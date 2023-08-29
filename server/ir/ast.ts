import { Span, Token } from "./recursive-descent-parser";

export interface Program {
  funcs: Func[];
}

export interface Func extends Span {
  type: "func";
  main?: Token;
  kind: Token;
  name: Token;
  params: Param[];
  retTy: Token;
  body: Inst[];
}

export interface Param extends Span {
  type: "param";
  lhs: Token;
  ty: Token;
  optional?: Token;
  specParam?: unknown; // TODO
}

export type Inst =
  | IIf
  | ILoop
  | IExpr
  | ILet
  | IAssign
  | IDelete
  | IPush
  | IRemoveElem
  | IReturn
  | IAssert
  | IPrint
  | INop
  | ICall
  | IMethodCall
  | ISDOCall;
export interface IExpr extends Span {
  type: "expr";
  expr: Expr;
}
export interface ILet extends Span {
  type: "let";
  lhs: Token;
  expr: Expr;
}
export interface IAssign extends Span {
  type: "assign";
  ref: Token;
  expr: Expr;
}
export interface IDelete extends Span {
  type: "delete";
  ref: Token;
}
export interface IPush extends Span {
  type: "push";
  from: Expr;
  to: Expr;
  front: Token;
}
export interface IRemoveElem extends Span {
  type: "remove-elem";
  list: Expr;
  elem: Expr;
}
export interface IReturn extends Span {
  type: "return";
  expr: Expr;
}
export interface IAssert extends Span {
  type: "assert";
  expr: Expr;
}
export interface IPrint extends Span {
  type: "print";
  expr: Expr;
}
export interface INop extends Span {
  type: "nop";
}
export interface ICall extends Span {
  type: "call";
  lhs: Token;
  fexpr: Expr;
  args: Args;
}
export interface IMethodCall extends Span {
  type: "method-call";
  lhs: Token;
  base: Token;
  method: Token;
  args: Args;
}
export interface ISDOCall extends Span {
  type: "sdo-call";
  lhs: Token;
  base: Token;
  method: Token;
  args: Args;
}
export interface IIf extends Span {
  type: "if";
  cond: Expr;
  thenInst: Inst[];
  elseInst: Inst[];
}
export interface ILoop extends Span {
  type: "loop";
  kind: Token;
  cond: Expr;
  body: Inst[];
}
export interface Args extends Span {
  type: "args";
  args: Expr[];
}

export type Expr =
  | NormalExpr
  | AllocExpr
  | AstExpr
  | LiteralExpr;
export type NormalExpr =
  | EComp
  | EIsCompletion
  | EReturnIfAbrupt
  | EPop
  | EParse
  | ENt
  | ESourceText
  | EYet
  | EContains
  | ESubstring
  | ERef
  | EUnary
  | EBinary
  | EVariadic
  | EClamp
  | EMathOp
  | EConvert
  | ETypeOf
  | ETypeCheck
  | EDuplicated
  | EIsArrayIndex
  | EClo
  | ECont;
export interface EComp extends Span {
  type: "comp";
  // tyExpr: Expr;
  // valExpr: Expr;
  // tgtExpr: Expr;
}
export interface EIsCompletion extends Span {
  type: "is-completion";
  // expr: Expr;
}
export interface EReturnIfAbrupt extends Span {
  type: "return-if-abrupt";
  // expr: Expr;
  // check: Token;
}
export interface EPop extends Span {
  type: "pop";
  // list: Expr;
  // front: Token;
}
export interface EParse extends Span {
  type: "parse";
  // code: Expr;
  // rule: Expr;
}
export interface ENt extends Span {
  type: "nt";
  // name: Token;
  // params: Token[];
}
export interface ESourceText extends Span {
  type: "source-text";
  // expr: Expr;
}
export interface EYet extends Span {
  type: "yet";
  // msg: Token;
}
export interface EContains extends Span {
  type: "contains";
  // list: Expr;
  // expr: Expr;
  // field?: Token;
}
export interface ESubstring extends Span {
  type: "substring";
  // expr: Expr;
  // from: Expr;
  // to?: Expr;
}
export interface ERef extends Span {
  type: "ref";
  // ref: Token;
}
export interface EUnary extends Span {
  type: "unary";
  // uop: Token;
  // epxr: Expr;
}
export interface EBinary extends Span {
  type: "binary";
  // bop: Token;
  // left: Expr;
  // right: Expr;
}
export interface EVariadic extends Span {
  type: "variadic";
  // vop: Token;
  // exprs: Expr[];
}
export interface EClamp extends Span {
  type: "clamp";
  // target: Expr;
  // lower: Expr;
  // upper: Expr;
}
export interface EMathOp extends Span {
  type: "math-op";
  // mop: Token;
  // args: Expr[];
}
export interface EConvert extends Span {
  type: "convert";
  // cop: Token;
  // expr: Expr;
}
export interface ETypeOf extends Span {
  type: "type-of";
  // base: Expr;
}
export interface ETypeCheck extends Span {
  type: "type-check";
  // base: Expr;
  // tyExpr: Expr;
}
export interface EDuplicated extends Span {
  type: "duplicated";
  // list: Expr;
}
export interface EIsArrayIndex extends Span {
  type: "is-array-index";
  // expr: Expr;
}
export interface EClo extends Span {
  type: "clo";
  // fname: Token;
  // captured: Token[];
}
export interface ECont extends Span {
  type: "cont";
  // fname: Token;
}

// ast exrepssions
export type AstExpr =
  | ESyntactic
  | ELexical;
export interface ESyntactic extends Span {
  type: "syntactic";
  // name: Token;
  // args: Token[];
  // rhsIdx: Token;
  // children: Expr[];
}
export interface ELexical extends Span {
  type: "lexical";
  // name: Token;
  // expr: Expr;
}

// allocation expressions
export type AllocExpr =
  | EMap
  | EList
  | EListConcat
  | ESymbol
  | ECopy
  | EKeys
  | EGetChildren
  | EGetItems;
export interface EMap extends Span {
  type: "map";
  // tname: Token;
  // props: [Expr, Expr][];
}
export interface EList extends Span {
  type: "list";
  // exprs: Expr[];
}
export interface EListConcat extends Span {
  type: "list-concat";
  // exprs: Expr[];
}
export interface ESymbol extends Span {
  type: "symbol";
  // decs: Expr;
}
export interface ECopy extends Span {
  type: "copy";
  // obj: Expr;
}
export interface EKeys extends Span {
  type: "keys";
  // map: Expr;
  // intSorted: Token;
}
export interface EGetChildren extends Span {
  type: "get-children";
  // ast: Expr;
}
export interface EGetItems extends Span {
  type: "get-items";
  // nt: Expr;
  // ast: Expr;
}

// literals
export type LiteralExpr =
  | EMathVal
  | ENumber
  | EBigInt
  | EStr
  | EBool
  | EUndef
  | ENull
  | EAbsent
  | EConst
  | ECodeUnit;
export interface EMathVal extends Token {
  type: "math-val";
}
export interface ENumber extends Token {
  type: "number";
}
export interface EBigInt extends Token {
  type: "bigint";
}
export interface EStr extends Token {
  type: "str";
}
export interface EBool extends Token {
  type: "bool";
}
export interface EUndef extends Token {
  type: "undef";
}
export interface ENull extends Token {
  type: "null";
}
export interface EAbsent extends Token {
  type: "absent";
}
export interface EConst extends Token {
  type: "const";
}
export interface ECodeUnit extends Token {
  type: "code-unit";
}
