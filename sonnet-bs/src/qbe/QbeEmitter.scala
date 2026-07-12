package qbe

import tac.Tac.*
import tac.{getTacValType, isSigned, isFloat}
import syntax.Const

class QbeEmitter {

    private var syntheticLabelCount                  = 0
    private var syntheticTempCount                   = 0
    private var stackPointerMap: Map[String, String] = Map.empty

    private def nextSyntheticLabel(): String = {
        syntheticLabelCount += 1
        s"synth_lbl_$syntheticLabelCount"
    }

    private def nextSyntheticTemp(): String = {
        syntheticTempCount += 1
        s"%synth_tmp_$syntheticTempCount"
    }

    def emitTac(prog: Program): String = {
        prog.items
            .map {
                case f: FunctionDef    => emitFunctionDef(f)
                case s: StaticVariable => emitStaticVariable(s)
            }
            .mkString("\n\n")
    }

    private def emitFunctionDef(f: FunctionDef): String = {
        val linkage   = if (f.isGlobal) "export " else ""
        val paramsStr = f.params.map(p => s"${getRegType(p.typ)} %${p.value}").mkString(", ")

        val allocations = f.body.foldLeft(Map.empty[String, Type]) { (acc, inst) =>
            inst match {
                case CopyToOffset(_, Var(name, typ), _) => acc + (name -> typ)
                case GetAddress(Var(name, typ), _)      => acc + (name -> typ)
                case _                                  => acc
            }
        }

        stackPointerMap = allocations.keys.map(name => name -> nextSyntheticTemp()).toMap

        val allocLines = allocations.map { case (name, typ) =>
            val is64Bit     = getRegType(typ) == "l" || getRegType(typ) == "d"
            val alignment   = if (is64Bit) "alloc8" else "alloc4"
            val maxOffset   = f.body.collect { case CopyToOffset(_, Var(n, _), offset) if n == name => offset }.maxOption.getOrElse(0)
            val elementSize = if (is64Bit) 8 else 4
            val totalBytes  = maxOffset + elementSize

            val ptrName = stackPointerMap(name)
            s"\t$ptrName =l $alignment $totalBytes"
        }.toList

        val bodyLines  = f.body.flatMap(emitInstruction)
        val allBodyStr = (allocLines ++ bodyLines).mkString("\n")

        stackPointerMap = Map.empty

        s"${linkage}function w $$${f.name}($paramsStr) {\n@start\n$allBodyStr\n}"
    }

    private def resolveVal(v: Val, setupInsts: collection.mutable.ListBuffer[String]): String = v match {
        case Var(name, typ) if stackPointerMap.contains(name) =>
            val tmp     = nextSyntheticTemp()
            val regType = getRegType(typ)
            val loadOp  = getLoadInstruction(typ)
            setupInsts += s"\t$tmp =$regType $loadOp ${stackPointerMap(name)}"
            tmp
        case _ => emitVal(v)
    }

    private def emitInstruction(inst: Instruction): List[String] = {
        val preInstrs = collection.mutable.ListBuffer[String]()

        val mainInstrs = inst match {
            case Return(value) =>
                List(s"\tret ${resolveVal(value, preInstrs)}")

            case GetAddress(Var(name, typ), dest) =>
                val ptrName = stackPointerMap(name)
                List(s"\t${emitVal(dest)} =l copy $ptrName")

            case GetAddress(src, dest) =>
                List(s"\t${emitVal(dest)} =l copy ${resolveVal(src, preInstrs)}")

            case AddPtr(ptr, index, scale, dest) =>
                val tmp  = nextSyntheticTemp()
                val rPtr = resolveVal(ptr, preInstrs)
                val rIdx = resolveVal(index, preInstrs)
                List(s"\t$tmp =l mul $rIdx, $scale", s"\t${emitVal(dest)} =l add $rPtr, $tmp")

            case CopyToOffset(src, Var(name, _), offset) =>
                val tmpPtr  = nextSyntheticTemp()
                val ptrName = stackPointerMap(name)
                val rSrc    = resolveVal(src, preInstrs)
                List(s"\t$tmpPtr =l add $ptrName, $offset", s"\t${getStoreInstruction(getTacValType(src))} $rSrc, $tmpPtr")

            case CopyToOffset(src, dest, offset) =>
                val tmpPtr = nextSyntheticTemp()
                val rSrc   = resolveVal(src, preInstrs)
                val rDest  = resolveVal(dest, preInstrs)
                List(s"\t$tmpPtr =l add $rDest, $offset", s"\t${getStoreInstruction(getTacValType(src))} $rSrc, $tmpPtr")

            case Load(srcPtr, dest) =>
                val rSrcPtr = resolveVal(srcPtr, preInstrs)
                val destTyp = getTacValType(dest)
                dest match {
                    case Var(name, _) if stackPointerMap.contains(name) =>
                        val tmp = nextSyntheticTemp()
                        List(
                          s"\t$tmp =${getRegType(destTyp)} ${getLoadInstruction(destTyp)} $rSrcPtr",
                          s"\t${getStoreInstruction(destTyp)} $tmp, ${stackPointerMap(name)}"
                        )
                    case _ =>
                        List(s"\t${emitVal(dest)} =${getRegType(destTyp)} ${getLoadInstruction(destTyp)} $rSrcPtr")
                }

            case Store(src, destPtr) =>
                List(s"\t${getStoreInstruction(getTacValType(src))} ${resolveVal(src, preInstrs)}, ${resolveVal(destPtr, preInstrs)}")

            case SignExtend(src, dest) =>
                val rSrc    = resolveVal(src, preInstrs)
                val destTyp = getTacValType(dest)
                dest match {
                    case Var(name, _) if stackPointerMap.contains(name) =>
                        val tmp = nextSyntheticTemp()
                        List(
                          s"\t$tmp =${getRegType(destTyp)} ${getSignExtendInstruction(getTacValType(src))} $rSrc",
                          s"\t${getStoreInstruction(destTyp)} $tmp, ${stackPointerMap(name)}"
                        )
                    case _ =>
                        List(s"\t${emitVal(dest)} =${getRegType(destTyp)} ${getSignExtendInstruction(getTacValType(src))} $rSrc")
                }

            case ZeroExtend(src, dest) =>
                val rSrc    = resolveVal(src, preInstrs)
                val destTyp = getTacValType(dest)
                dest match {
                    case Var(name, _) if stackPointerMap.contains(name) =>
                        val tmp = nextSyntheticTemp()
                        List(
                          s"\t$tmp =${getRegType(destTyp)} ${getZeroExtendInstruction(getTacValType(src))} $rSrc",
                          s"\t${getStoreInstruction(destTyp)} $tmp, ${stackPointerMap(name)}"
                        )
                    case _ =>
                        List(s"\t${emitVal(dest)} =${getRegType(destTyp)} ${getZeroExtendInstruction(getTacValType(src))} $rSrc")
                }

            case Truncate(src, dest) =>
                val rSrc    = resolveVal(src, preInstrs)
                val destTyp = getTacValType(dest)
                dest match {
                    case Var(name, _) if stackPointerMap.contains(name) =>
                        List(s"\t${getStoreInstruction(destTyp)} $rSrc, ${stackPointerMap(name)}")
                    case _ =>
                        List(s"\t${emitVal(dest)} =${getRegType(destTyp)} copy $rSrc")
                }

            case FloatToFloat(src, dest) =>
                val rSrc    = resolveVal(src, preInstrs)
                val destTyp = getTacValType(dest)
                val castOp  = if (getRegType(destTyp) == "d") "exts" else "truncd"
                dest match {
                    case Var(name, _) if stackPointerMap.contains(name) =>
                        val tmp = nextSyntheticTemp()
                        List(
                          s"\t$tmp =${getRegType(destTyp)} $castOp $rSrc",
                          s"\t${getStoreInstruction(destTyp)} $tmp, ${stackPointerMap(name)}"
                        )
                    case _ =>
                        List(s"\t${emitVal(dest)} =${getRegType(destTyp)} $castOp $rSrc")
                }

            case Unary(op, src, dest) =>
                val rSrc        = resolveVal(src, preInstrs)
                val destTyp     = getTacValType(dest)
                val destRegType = getRegType(destTyp)
                val instr = op match {
                    case UnaryOp.Complement => s"xor $rSrc, -1"
                    case UnaryOp.Negate     => s"neg $rSrc"
                    case UnaryOp.Not        => s"ceqw $rSrc, 0"
                }
                dest match {
                    case Var(name, _) if stackPointerMap.contains(name) =>
                        val tmp = nextSyntheticTemp()
                        List(
                          s"\t$tmp =$destRegType $instr",
                          s"\t${getStoreInstruction(destTyp)} $tmp, ${stackPointerMap(name)}"
                        )
                    case _ =>
                        List(s"\t${emitVal(dest)} =$destRegType $instr")
                }

            case Binary(op, src1, src2, dest) =>
                val rSrc1   = resolveVal(src1, preInstrs)
                val rSrc2   = resolveVal(src2, preInstrs)
                val destTyp = getTacValType(dest)
                dest match {
                    case Var(name, _) if stackPointerMap.contains(name) =>
                        val tmp = nextSyntheticTemp()
                        List(
                          s"\t$tmp =${getRegType(destTyp)} ${getBinaryOp(op, getTacValType(src1))} $rSrc1, $rSrc2",
                          s"\t${getStoreInstruction(destTyp)} $tmp, ${stackPointerMap(name)}"
                        )
                    case _ =>
                        List(s"\t${emitVal(dest)} =${getRegType(destTyp)} ${getBinaryOp(op, getTacValType(src1))} $rSrc1, $rSrc2")
                }

            case Copy(src, dest) =>
                val rSrc = resolveVal(src, preInstrs)
                dest match {
                    case Var(name, typ) if stackPointerMap.contains(name) =>
                        List(s"\t${getStoreInstruction(typ)} $rSrc, ${stackPointerMap(name)}")
                    case _ =>
                        List(s"\t${emitVal(dest)} =${getRegType(getTacValType(dest))} copy $rSrc")
                }

            case Label(name) =>
                List(s"@$name")

            case Jump(target) =>
                List(s"\tjmp @${target.name}")

            case JumpIfZero(cond, target) =>
                val rCond       = resolveVal(cond, preInstrs)
                val fallthrough = nextSyntheticLabel()
                List(s"\tjnz $rCond, @$fallthrough, @${target.name}", s"@$fallthrough")

            case JumpIfNotZero(cond, target) =>
                val rCond       = resolveVal(cond, preInstrs)
                val fallthrough = nextSyntheticLabel()
                List(s"\tjnz $rCond, @${target.name}, @$fallthrough", s"@$fallthrough")

            case FunctionCall(target, args, dest) =>
                val argsStr = args.map(a => s"${getRegType(getTacValType(a))} ${resolveVal(a, preInstrs)}").mkString(", ")
                val destTyp = getTacValType(dest)
                dest match {
                    case Var(name, _) if stackPointerMap.contains(name) =>
                        val tmp = nextSyntheticTemp()
                        List(
                          s"\t$tmp =${getRegType(destTyp)} call $$$target($argsStr)",
                          s"\t${getStoreInstruction(destTyp)} $tmp, ${stackPointerMap(name)}"
                        )
                    case _ =>
                        List(s"\t${emitVal(dest)} =${getRegType(destTyp)} call $$$target($argsStr)")
                }
            case _ => List.empty
        }

        preInstrs.toList ++ mainInstrs
    }

    private def emitStaticVariable(s: StaticVariable): String = {
        val linkage = if (s.isGlobal) "export " else ""
        val typObj  = getMemoryType(s.typ)
        val valueStr = s.init match {
            case Const.I8Lit(v)  => v.toString
            case Const.I16Lit(v) => v.toString
            case Const.I32Lit(v) => v.toString
            case Const.I64Lit(v) => v.toString
            case Const.U8Lit(v)  => v.toString
            case Const.U16Lit(v) => v.toString
            case Const.U32Lit(v) => v.toString
            case Const.U64Lit(v) => v.toString
            case Const.F16Lit(v) => s"s_$v"
            case Const.F32Lit(v) => s"s_$v"
            case Const.F64Lit(v) => s"d_$v"
        }
        s"${linkage}data $$${s.name} = { $typObj $valueStr }"
    }

    private def emitVal(v: Val): String = v match {
        case Var(name, _) => s"%$name"
        case Constant(c) =>
            c match {
                case Const.I8Lit(vl)  => vl.toString
                case Const.I16Lit(vl) => vl.toString
                case Const.I32Lit(vl) => vl.toString
                case Const.I64Lit(vl) => vl.toString
                case Const.U8Lit(vl)  => vl.toString
                case Const.U16Lit(vl) => vl.toString
                case Const.U32Lit(vl) => vl.toString
                case Const.U64Lit(vl) => vl.toString
                case Const.F16Lit(vl) => s"s_$vl"
                case Const.F32Lit(vl) => s"s_$vl"
                case Const.F64Lit(vl) => s"d_$vl"
            }
    }

    private def getRegType(t: Type): String = t match {
        case _: I8 | _: U8 | _: I16 | _: U16 | _: I32 | _: U32 => "w"
        case _: I64 | _: U64                                   => "l"
        case _: F16 | _: F32                                   => "s"
        case _: F64                                            => "d"
    }

    private def getMemoryType(t: Type): String = t match {
        case _: I8 | _: U8   => "b"
        case _: I16 | _: U16 => "h"
        case _: I32 | _: U32 => "w"
        case _: I64 | _: U64 => "l"
        case _: F16 | _: F32 => "s"
        case _: F64          => "d"
    }

    private def getLoadInstruction(t: Type): String = t match {
        case _: I8           => "loadsb"
        case _: U8           => "loadub"
        case _: I16          => "loadsh"
        case _: U16          => "loaduh"
        case _: I32          => "loadsw"
        case _: U32          => "loaduw"
        case _: I64 | _: U64 => "loadl"
        case _: F16 | _: F32 => "loads"
        case _: F64          => "loadd"
    }

    private def getStoreInstruction(t: Type): String = t match {
        case _: I8 | _: U8   => "storeb"
        case _: I16 | _: U16 => "storeh"
        case _: I32 | _: U32 => "storew"
        case _: I64 | _: U64 => "storel"
        case _: F16 | _: F32 => "stores"
        case _: F64          => "stored"
    }

    private def getSignExtendInstruction(t: Type): String = t match {
        case _: I8 | _: U8   => "extsb"
        case _: I16 | _: U16 => "extsh"
        case _: I32 | _: U32 => "extsw"
        case _               => "copy"
    }

    private def getZeroExtendInstruction(t: Type): String = t match {
        case _: I8 | _: U8   => "extub"
        case _: I16 | _: U16 => "extuh"
        case _: I32 | _: U32 => "extuw"
        case _               => "copy"
    }

    private def getBinaryOp(op: BinaryOp, srcType: Type): String = {
        val typeSuffix = getRegType(srcType)
        op match {
            case BinaryOp.Add                   => "add"
            case BinaryOp.Subtract              => "sub"
            case BinaryOp.Multiply              => "mul"
            case BinaryOp.Divide                => if (isSigned(srcType) || isFloat(srcType)) "div" else "udiv"
            case BinaryOp.Remainder             => if (isSigned(srcType)) "rem" else "urem"
            case BinaryOp.BitAnd | BinaryOp.And => "and"
            case BinaryOp.BitOr | BinaryOp.Or   => "or"
            case BinaryOp.BitXor                => "xor"
            case BinaryOp.LShift                => "shl"
            case BinaryOp.RShift                => if (isSigned(srcType)) "sar" else "shr"
            case BinaryOp.Equal                 => s"ceq$typeSuffix"
            case BinaryOp.NotEqual              => s"cne$typeSuffix"
            case BinaryOp.LessThan =>
                if (isFloat(srcType)) s"clt$typeSuffix"
                else if (isSigned(srcType)) s"cslt$typeSuffix"
                else s"cult$typeSuffix"
            case BinaryOp.LessOrEqual =>
                if (isFloat(srcType)) s"cle$typeSuffix"
                else if (isSigned(srcType)) s"csle$typeSuffix"
                else s"cule$typeSuffix"
            case BinaryOp.GreaterThan =>
                if (isFloat(srcType)) s"cgt$typeSuffix"
                else if (isSigned(srcType)) s"csgt$typeSuffix"
                else s"cugt$typeSuffix"
            case BinaryOp.GreaterOrEqual =>
                if (isFloat(srcType)) s"cge$typeSuffix"
                else if (isSigned(srcType)) s"csge$typeSuffix"
                else s"cuge$typeSuffix"
        }
    }
}
