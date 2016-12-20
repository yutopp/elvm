#include <ir/ir.h>
#include <target/util.h>

static const char* rill_value_str(Value* v) {
  if (v->type == REG) {
    return format("(%s).to_uint32()", reg_names[v->reg]);
  } else if (v->type == IMM) {
    return format("%du", v->imm);
  } else {
    error("invalid value");
  }
}

static const char* rill_src_str(Inst* inst) {
  return rill_value_str(&inst->src);
}

static const char* rill_cmp_str(Inst* inst, const char* true_str) {
  int op = normalize_cond(inst->op, 0);
  const char* op_str;
  switch (op) {
    case JEQ:
      op_str = "=="; break;
    case JNE:
      op_str = "!="; break;
    case JLT:
      op_str = "<"; break;
    case JGT:
      op_str = ">"; break;
    case JLE:
      op_str = "<="; break;
    case JGE:
      op_str = ">="; break;
    case JMP:
      return true_str;
    default:
      error("oops");
  }
  return format("%s %s %s", reg_names[inst->dst.reg], op_str, rill_src_str(inst));
}

static void init_state(Data* data) {
    emit_line("import std.stdio;");
    emit_line("import std.libc;");

    emit_line("class state {");
    inc_indent();
    for (int i = 0; i < 7; i++) {
        emit_line("val %s: uint32;", reg_names[i]);
    }
    emit_line("val mutable mem: 'unmanaged raw_ptr!(mutable(array!(mutable(uint32), 16777216u)));");
    dec_indent();
    emit_line("}");

    emit_line("");

    emit_line("def init(): mutable(state) {");
    inc_indent();
    emit_line("val mutable s = state();");
    for (int i = 0; i < 7; i++) {
        emit_line("s.%s = 0u;", reg_names[i]);
    }
    emit_line("s.mem = new!(mutable(array!(mutable(uint32), 16777216u)))();");
    emit_line("memset(s.mem, 0u.to!(uint8)(), 16777216 * 4);");
    for (int mp = 0; data; data = data->next, mp++) {
        if (data->v) {
            emit_line("(*s.mem)[%du] = %du;", mp, data->v);
        }
    }
    emit_line("return s;");
    dec_indent();
    emit_line("}");
}

static void emit_func_prologue(int func_id) {
  emit_line("");
  emit_line("def func_%d!(S)(mutable s: S) {", func_id);
  inc_indent();
  for (int i = 0; i < 7; i++) {
      emit_line("ref mutable %s = s.%s;", reg_names[i], reg_names[i]);
  }
  emit_line("ref mutable mem = *s.mem;");
  emit_line("");
  emit_line("for(;%du <= pc && pc < %du;) with {",
            func_id * CHUNKED_FUNC_SIZE, (func_id + 1) * CHUNKED_FUNC_SIZE);
  inc_indent();
  emit_line("if (false) {");
  inc_indent();
  emit_line("// dummy");
}

static void emit_func_epilogue(void) {
  dec_indent();
  emit_line("}");
  emit_line("++pc;");
  dec_indent();
  emit_line("};");  // for
  dec_indent();
  emit_line("}");
}

static void emit_pc_change(int pc) {
  dec_indent();
  emit_line("} else if (pc == %du) {", pc);
  inc_indent();
}

static void emit_inst(Inst* inst) {
    switch (inst->op) {
    case MOV:
        emit_line("%s = %s.to_uint32();", reg_names[inst->dst.reg], src_str(inst));
        break;

    case ADD:
        emit_line("%s = (%s + %s.to_uint32()) & " UINT_MAX_STR "u;",
                  reg_names[inst->dst.reg],
                  reg_names[inst->dst.reg], src_str(inst));
        break;

    case SUB:
        emit_line("%s = (%s - %s.to_uint32()) & " UINT_MAX_STR "u;",
                  reg_names[inst->dst.reg],
                  reg_names[inst->dst.reg], src_str(inst));
        break;

    case LOAD:
        emit_line("%s = mem[%s];", reg_names[inst->dst.reg], src_str(inst));
        break;

    case STORE:
        emit_line("mem[%s] = %s;", src_str(inst), reg_names[inst->dst.reg]);
        break;

    case PUTC:
        emit_line("print(%s.to_uint32().to!uint8());", src_str(inst));
        break;

    case GETC:
        emit_line("with {val tmp = getchar();");
        emit_line("%s = (if (tmp == -1) 0 else tmp).to_uint32();};", reg_names[inst->dst.reg]);
        break;

    case EXIT:
        emit_line("exit(0);");
        break;

    case DUMP:
        break;

    case EQ:
    case NE:
    case LT:
    case GT:
    case LE:
    case GE:
        emit_line("%s = (%s).to_uint32();",
                  reg_names[inst->dst.reg], rill_cmp_str(inst, "1"));
        break;

    case JEQ:
    case JNE:
    case JLT:
    case JGT:
    case JLE:
    case JGE:
    case JMP:
        emit_line("if (%s) { pc = %s - 1u; }",
                  rill_cmp_str(inst, "true"), rill_value_str(&inst->jmp));
        break;

    default:
        error("oops");
    }
}

void target_rill(Module* module) {
    init_state(module->data);

    int num_funcs = emit_chunked_main_loop(module->text,
                                           emit_func_prologue,
                                           emit_func_epilogue,
                                           emit_pc_change,
                                           emit_inst);

    emit_line("def main() {");
    inc_indent();

    emit_line("ref mutable s = init();");
    emit_line("for(;;) with {");
    inc_indent();

    emit_line("if (false) { /* dummy */ }");
    for (int i = 0; i < num_funcs; i++) {
        emit_line("else if (s.pc / %du == %du) { func_%d(s); }", CHUNKED_FUNC_SIZE, i, i);
    }
    dec_indent();
    emit_line("};");    // for
    dec_indent();
    emit_line("}");
}
