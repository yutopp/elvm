#include <ir/ir.h>
#include <target/util.h>

#define UNIT_SIZE 32
#define UNIT_SIZE_STR "32"

static void erlang_init_state(Data* data) {
    emit_line("-module(elvm_erlang).");
    emit_line("-mode(compile).");

    emit_line("-record(state, {");
    inc_indent();
    for (int i = 0; i < 7; i++) {
        emit_line("%s = 0,", reg_names[i]);
    }

    //emit_line("mem = <<");
    //inc_indent();
    //int num = 0;
    //for (; data; data = data->next, ++num) {
    //    if (data->v) {
    //        emit_line("%d:" UNIT_SIZE_STR "/integer,", data->v);
    //    } else {
    //        emit_line("0:" UNIT_SIZE_STR "/integer,");
    //    }
    //}
    //emit_line("0:%d", ((1 << 24) - num) * UNIT_SIZE);
    //dec_indent();
    //emit_line(">>");
    //dec_indent();
    emit_line("mem = #{");
    inc_indent();
    for (int i = 0; data; data = data->next, ++i) {
        if (data->v) {
            emit_line("%d => %d%s", i, data->v, data->next ? "," : "");
        }
    }
    emit_line("}");
    dec_indent();
    emit_line("}).");
}

static void erlang_emit_prologue(void) {
    emit_line("run(State0) ->");
    emit_line(" case exec(State0) of");
    emit_line("  {ok, State1} ->");
    emit_line("   run(State1);");
    emit_line("  {stop, State1} ->");
    emit_line("  State1");
    emit_line(" end.");
}

static const char* erl_reg_name(Reg reg, int i) {
    return format("State%d#state.%s", i, reg_names[reg]);
}

static const char* erl_value_str(Value* v, int i) {
    if (v->type == REG) {
        return erl_reg_name(v->reg, i);
    } else if (v->type == IMM) {
        return format("%d", v->imm);
    } else {
        error("invalid value");
    }
}

static const char* erl_src_str(Inst* inst, int i) {
    return erl_value_str(&inst->src, i);
}

const char* erl_cmp_str(Inst* inst, int i) {
  int op = normalize_cond(inst->op, 0);
  const char* op_str;
  switch (op) {
    case JEQ:
      op_str = "=:="; break;
    case JNE:
      op_str = "=/="; break;
    case JLT:
      op_str = "<"; break;
    case JGT:
      op_str = ">"; break;
    case JLE:
      op_str = "=<"; break;
    case JGE:
      op_str = ">="; break;
    case JMP:
      return "true";
    default:
      error("oops");
  }
  return format("%s %s %s", erl_reg_name(inst->dst.reg, i), op_str, erl_src_str(inst, i));
}

static void erlang_emit_exec_begin(int pc) {
    emit_line("exec(State0 = #state{pc = %d}) ->", pc);
    inc_indent();
}

static void erlang_emit_inst(Inst* inst, int* state_count, int* stopped) {
    int cp = *state_count;
    int np = cp + 1;
    *stopped = 0;

    switch (inst->op) {
    case MOV:
        emit_line("State%d = State%d#state{%s = %s},",
                  np,
                  cp, reg_names[inst->dst.reg],
                  erl_src_str(inst, cp));
        ++(*state_count);
        break;

    case ADD:
        emit_line("State%d = State%d#state{%s = (State%d#state.%s + %s) band " UINT_MAX_STR "},",
                  np,
                  cp, reg_names[inst->dst.reg],
                  cp, reg_names[inst->dst.reg], erl_src_str(inst, cp));
        ++(*state_count);
        break;

    case SUB:
        emit_line("State%d = State%d#state{%s = (State%d#state.%s - %s) band " UINT_MAX_STR "},",
                  np,
                  cp, reg_names[inst->dst.reg],
                  cp, reg_names[inst->dst.reg], erl_src_str(inst, cp));
        ++(*state_count);
        break;

    case LOAD:
        //emit_line("Offset%d = %s * " UNIT_SIZE_STR ",", cp, erl_src_str(inst, cp));
        //emit_line("<<_:Offset%d, Val%d:" UNIT_SIZE_STR ", _/binary>> = State%d#state.mem,",
        //          cp, cp, cp);
        emit_line("Val%d = maps:get(%s, State%d#state.mem, 0),",
                  cp, erl_src_str(inst, cp), cp);
        emit_line("State%d = State%d#state{%s = Val%d},",
                  np,
                  cp, reg_names[inst->dst.reg],
                  cp);
        ++(*state_count);
        break;

    case STORE:
        //emit_line("Offset%d = %s,", cp, erl_src_str(inst, cp));
        //emit_line("<<Hd%d:Offset%d/binary-unit:" UNIT_SIZE_STR ", _:" UNIT_SIZE_STR ", Rest%d/binary>> = State%d#state.mem,", cp, cp, cp, cp);
        //emit_line("Bin%d = <<Hd%d/binary, (%s):" UNIT_SIZE_STR "/integer, Rest%d/binary>>,",
        //          cp,
        //          cp,
        //          erl_reg_name(inst->dst.reg, cp),
        //          cp);
        emit_line("Mem%d = maps:put(%s, %s, State%d#state.mem),",
                  cp, erl_src_str(inst, cp), erl_reg_name(inst->dst.reg, cp), cp);
        emit_line("State%d = State%d#state{mem = Mem%d},",
                  np,
                  cp,
                  cp);
        ++(*state_count);
        break;

    case PUTC:
        emit_line("io:format(\"~c\", [%s]),", erl_src_str(inst, cp));
        break;

    case GETC:
        emit_line("C_%d = io:get_chars(\"\", 1),", cp);
        emit_line("C%d = case C_%d of eof -> [0]; _ -> C_%d end,", cp, cp, cp);
        emit_line("State%d = State%d#state{%s = lists:nth(1, C%d)},",
                  np,
                  cp, reg_names[inst->dst.reg],
                  cp);
        ++(*state_count);
        break;

    case EXIT:
        *stopped = 1;
        break;

    case DUMP:
        break;

    case EQ:
    case NE:
    case LT:
    case GT:
    case LE:
    case GE:
        emit_line("State%d = State%d#state{%s = case %s of true -> 1; false -> 0 end},",
                  np,
                  cp, reg_names[inst->dst.reg],
                  erl_cmp_str(inst, cp));
        ++(*state_count);
        break;

    case JEQ:
    case JNE:
    case JLT:
    case JGT:
    case JLE:
    case JGE:
    case JMP:
        emit_line("State%d = case %s of", np, erl_cmp_str(inst, cp));
        inc_indent();
        emit_line("true  -> State%d#state{pc = %s - 1};",
                  cp, erl_value_str(&inst->jmp, cp));
        emit_line("false -> State%d", cp);
        dec_indent();
        emit_line("end,");
        ++(*state_count);
        break;

    default:
        error("oops");
    }
}

static void erlang_emit_exec_term(int state_count, int stopped) {
    if (stopped == 0) {
        emit_line("{ok, State%d#state{pc = State%d#state.pc + 1}};", state_count, state_count);
    } else {
        emit_line("{stop, State%d#state{pc = State%d#state.pc + 1}};", state_count, state_count);
    }
    dec_indent();
}

static void erlang_emit_main_loop(Inst* inst) {
    int prev_pc = -1;
    int state_count = 0;
    int stopped = 0;
    for (; inst; inst = inst->next) {
        if (prev_pc != inst->pc) {
            if (prev_pc != -1) {
                erlang_emit_exec_term(state_count, stopped);
            }
            state_count = 0;
            stopped = 0;
            erlang_emit_exec_begin(inst->pc);
        }
        prev_pc = inst->pc;
        erlang_emit_inst(inst, &state_count, &stopped);
    }
    erlang_emit_exec_term(state_count, stopped);
}

static void erlang_emit_epilogue() {
    emit_line("exec(State0) ->");
    emit_line(" {stop, State0}.");
}

void target_erl(Module* module) {
    erlang_init_state(module->data);
    erlang_emit_prologue();
    erlang_emit_main_loop(module->text);
    erlang_emit_epilogue();

    emit_line("main(_) ->");
    emit_line(" run(#state{}).");
}
