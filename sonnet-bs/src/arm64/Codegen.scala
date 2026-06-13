package arm64

import tac.Tac
import tac.getTacValType
import scala.collection.mutable.{Map, ListBuffer}
import syntax.Size
import tac.isSigned

def codegenProgram(p: Tac.Program): Asm.Program = {
    val items = p.items.map {
        case f: Tac.FunctionDef => codegenFunction(f)
        case v: Tac.StaticVariable => {
            val s = Size.fromTacType(v.typ)
            Asm.StaticVariable(v.name, v.isGlobal, s, v.init)
        }
    }
    Asm.Program(items)
}

def codegenFunction(f: Tac.FunctionDef): Asm.FunctionDef = {
    val paramMoves = f.params.zipWithIndex.flatMap { case (paramVar, index) =>
        val asmType = paramVar.typ
        val size    = Size.fromTacType(paramVar.typ)
        if (index < 8) {
            List(Asm.Mov(Asm.Register(Asm.selectParamRegister(index, asmType)), Asm.PseudoReg(paramVar.value, size)))
        } else {
            val incomingOffset = ((index - 8) * 8) + 16
            List(Asm.Mov(Asm.StackSlot(incomingOffset, size), Asm.PseudoReg(paramVar.value, size)))
        }
    }
    val asmInstructions = paramMoves ++ f.body.flatMap(codegenInstruction)
    Asm.FunctionDef(f.name, f.isGlobal, asmInstructions)
}

def codegenInstruction(ins: Tac.Instruction): List[Asm.Instruction] = ins match {
    case Tac.Return(value) =>
        val t = getTacValType(value)
        List(
          Asm.Mov(codegenTacVal(value), Asm.Register(Asm.selectParamRegister(0, t))),
          Asm.Ret()
        )

    case Tac.Copy(src, dest) =>
        List(Asm.Mov(codegenTacVal(src), codegenTacVal(dest)))

    case Tac.SignExtend(src, dest) => {
        val srcType = getTacValType(src)
        val asmSrc  = codegenTacVal(src)
        val asmDst  = codegenTacVal(dest)
        srcType match {
            case Tac.I8()  => List(Asm.Sextb(asmSrc, asmDst))
            case Tac.I16() => List(Asm.Sexth(asmSrc, asmDst))
            case Tac.I32() => List(Asm.Sextw(asmSrc, asmDst))
            case Tac.I64() => List(Asm.Mov(asmSrc, asmDst))
        }
    }

    case Tac.ZeroExtend(src, dest) => {
        val srcType = getTacValType(src)
        val asmSrc  = codegenTacVal(src)
        val asmDst  = codegenTacVal(dest)
        srcType match {
            case Tac.U8() | Tac.I8()   => List(Asm.Uxtb(asmSrc, asmDst))
            case Tac.U16() | Tac.I16() => List(Asm.Uxth(asmSrc, asmDst))
            case Tac.U32() | Tac.I32() => List(Asm.Uxtw(asmSrc, asmDst))
            case Tac.U64() | Tac.I64() => List(Asm.Mov(asmSrc, asmDst))
        }
    }

    case Tac.Truncate(src, dest) =>
        List(Asm.Mov(codegenTacVal(src), codegenTacVal(dest)))

    case Tac.Unary(Tac.UnaryOp.Not, src, dest) =>
        val t       = getTacValType(src)
        val zeroReg = if (t == Tac.I64()) Asm.Reg.XZR else Asm.Reg.WZR
        List(
          Asm.Compare(codegenTacVal(src), Asm.Register(zeroReg)),
          Asm.ConditionalSet(Asm.ConditionCode.Equal, codegenTacVal(dest))
        )

    case Tac.Unary(op, src, dest) => {
        val asmOp  = convertUnOp(op)
        val asmSrc = codegenTacVal(src)
        val asmDst = codegenTacVal(dest)
        List(
          Asm.Mov(asmSrc, asmDst),
          Asm.Unary(asmOp, asmDst)
        )
    }

    case Tac.Binary(Tac.BinaryOp.Remainder, src1, src2, dest) => {
        val t  = getTacValType(dest)
        val s1 = codegenTacVal(src1)
        val s2 = codegenTacVal(src2)
        val d  = codegenTacVal(dest)

        val pseudoQuotient = d match {
            case Asm.PseudoReg(name, typ) => Asm.PseudoReg(s"${name}_quot", typ)
            case _                        => Asm.PseudoReg("rem_quot_temp", Size.fromTacType(t))
        }
        val divOp = if isSigned(t) then Asm.BinaryOp.Div else Asm.BinaryOp.UDiv
        List(
          Asm.Binary(divOp, s1, s2, pseudoQuotient),
          Asm.MultiplySubtract(pseudoQuotient, s2, s1, d)
        )
    }

    case Tac.Binary(op, src1, src2, dest) if isRelationalOp(op) => {
        List(
          Asm.Compare(codegenTacVal(src1), codegenTacVal(src2)),
          Asm.ConditionalSet(convertConditionCode(op, getTacValType(src1)), codegenTacVal(dest))
        )
    }

    case Tac.Binary(op, src1, src2, dest) => {
        List(Asm.Binary(convertBinOp(op, getTacValType(dest)), codegenTacVal(src1), codegenTacVal(src2), codegenTacVal(dest)))
    }

    case Tac.Label(name)  => List(Asm.Label(name))
    case Tac.Jump(target) => List(Asm.Branch(target.name))

    case Tac.JumpIfZero(cond, target) => {
        val t       = getTacValType(cond)
        val zeroReg = if (t == Tac.I64()) Asm.Reg.XZR else Asm.Reg.WZR
        List(
          Asm.Compare(codegenTacVal(cond), Asm.Register(zeroReg)),
          Asm.ConditionalBranch(Asm.ConditionCode.Equal, target.name)
        )
    }

    case Tac.JumpIfNotZero(cond, target) => {
        val t       = getTacValType(cond)
        val zeroReg = if (t == Tac.I64()) Asm.Reg.XZR else Asm.Reg.WZR
        List(
          Asm.Compare(codegenTacVal(cond), Asm.Register(zeroReg)),
          Asm.ConditionalBranch(Asm.ConditionCode.NotEqual, target.name)
        )
    }

    case Tac.FunctionCall(target, args, dest) => {
        val destType = getTacValType(dest)
        val argSetup = args.zipWithIndex.flatMap { case (argVal, index) =>
            val argType = getTacValType(argVal)
            if (index < 8) {
                List(Asm.Mov(codegenTacVal(argVal), Asm.Register(Asm.selectParamRegister(index, argType))))
            } else {
                List(Asm.Push(codegenTacVal(argVal)))
            }
        }

        val stackCleanup = if (args.length > 8) {
            val stackBytes = (args.length - 8) * 8
            List(Asm.DeallocateStack(pad16(stackBytes)))
        } else {
            Nil
        }

        argSetup ++ List(Asm.Call(target)) ++ stackCleanup ++ List(
          Asm.Mov(Asm.Register(Asm.selectParamRegister(0, destType)), codegenTacVal(dest))
        )
    }
}

def codegenTacVal(v: Tac.Val): Asm.Operand = v match {
    case Tac.Constant(syntax.Const.I8Lit(ival))  => Asm.Imm8(ival.toInt)
    case Tac.Constant(syntax.Const.U8Lit(uval))  => Asm.Imm8(uval.toInt)
    case Tac.Constant(syntax.Const.I16Lit(lval)) => Asm.Imm16(lval.toInt)
    case Tac.Constant(syntax.Const.U16Lit(uval)) => Asm.Imm16(uval.toInt)
    case Tac.Constant(syntax.Const.I32Lit(ival)) => Asm.Imm32(ival.toInt)
    case Tac.Constant(syntax.Const.U32Lit(uval)) => Asm.Imm32(uval.toInt)
    case Tac.Constant(syntax.Const.I64Lit(lval)) => Asm.Imm64(lval.toLong)
    case Tac.Constant(syntax.Const.U64Lit(uval)) => Asm.Imm64(uval.toLong)
    case Tac.Var(name, tacTyp)                   => Asm.PseudoReg(name, Size.fromTacType(tacTyp))
}

private def convertConditionCode(op: Tac.BinaryOp, operandType: Tac.Type): Asm.ConditionCode = {
    val unsigned = !isSigned(operandType)
    op match {
        case Tac.BinaryOp.Equal          => Asm.ConditionCode.Equal
        case Tac.BinaryOp.NotEqual       => Asm.ConditionCode.NotEqual
        case Tac.BinaryOp.LessThan       => if unsigned then Asm.ConditionCode.CarryClear else Asm.ConditionCode.LessThan
        case Tac.BinaryOp.LessOrEqual    => if unsigned then Asm.ConditionCode.LowerOrSame else Asm.ConditionCode.LessOrEqual
        case Tac.BinaryOp.GreaterThan    => if unsigned then Asm.ConditionCode.Higher else Asm.ConditionCode.GreaterThan
        case Tac.BinaryOp.GreaterOrEqual => if unsigned then Asm.ConditionCode.CarrySet else Asm.ConditionCode.GreaterOrEqual
        case _                           => throw new RuntimeException(s"$op is not a comparison.")
    }
}

private def convertUnOp(op: Tac.UnaryOp): Asm.UnaryOp = op match {
    case Tac.UnaryOp.Complement => Asm.UnaryOp.Not
    case Tac.UnaryOp.Negate     => Asm.UnaryOp.Neg
}

private def convertBinOp(op: Tac.BinaryOp, destType: Tac.Type): Asm.BinaryOp = op match {
    case Tac.BinaryOp.Add      => Asm.BinaryOp.Add
    case Tac.BinaryOp.Subtract => Asm.BinaryOp.Sub
    case Tac.BinaryOp.Multiply => Asm.BinaryOp.Mult
    case Tac.BinaryOp.Divide   => if isSigned(destType) then Asm.BinaryOp.Div else Asm.BinaryOp.UDiv
    case Tac.BinaryOp.BitAnd   => Asm.BinaryOp.BitAnd
    case Tac.BinaryOp.BitOr    => Asm.BinaryOp.BitOr
    case Tac.BinaryOp.BitXor   => Asm.BinaryOp.BitXor
    case Tac.BinaryOp.LShift   => Asm.BinaryOp.Lsl
    case Tac.BinaryOp.RShift   => if isSigned(destType) then Asm.BinaryOp.Asr else Asm.BinaryOp.Lsr
    case _                     => throw new RuntimeException(s"Unmappable: $op")
}

private def isRelationalOp(op: Tac.BinaryOp): Boolean = op match {
    case Tac.BinaryOp.Equal | Tac.BinaryOp.NotEqual | Tac.BinaryOp.LessThan | Tac.BinaryOp.LessOrEqual | Tac.BinaryOp.GreaterThan | Tac.BinaryOp.GreaterOrEqual => true
    case _                                                                                                                                                      => false
}
