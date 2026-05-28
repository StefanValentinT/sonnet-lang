package assembly

import syntax.*

def codegenProgram(p: Program): AsmProgram = AsmProgram(codegenFunction(p.items))

def codegenFunction(f: FunctionDef): AsmFunctionDef = AsmFunctionDef(f.name, codegenStmt(f.body))

def codegenStmt(s: Statement): List[AsmInstruction] = s match {
    case Return(exp) => List(Mov(codegenExpr(exp), Register()), Ret())
}

def codegenExpr(e: Expression): AsmOperand = e match {
    case Constant(ival) => Imm(ival)
}
