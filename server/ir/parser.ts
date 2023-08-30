import * as ast from "./ast";
import {
  createRecursiveDescentParser,
  Pattern,
  RecursiveDescentParser as Parser,
  Span,
  SyntaxError,
  Token,
} from "./recursive-descent-parser";

interface ParseResult {
  ast: ast.Program;
  parser: Parser;
}

export function parse(text: string): ParseResult {
  const parser = createRecursiveDescentParser(text, {debug: true});
  const funcs = acceptFuncs(parser);
  const ast = { funcs };
  return { ast, parser };
}

function acceptFuncs(parser: Parser) {
  const funcs: ast.Func[] = [];
  let node: ast.Func | undefined;
  while ((skipWsAndSweepComments(parser), node = acceptFunc(parser))) {
    funcs.push(node);
  }
  return funcs;
}

function acceptFunc(parser: Parser): ast.Func | undefined {
  const loc = parser.loc;
  const main = parser.accept("@main");
  skipWsAndSweepComments(parser);
  const head = parser.accept("def");
  if (!head) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  const kind = expectFuncKind(parser);
  const name = expectFuncName(parser);
  const params = expectFuncParams(parser);
  const retTy = parser.expect(/:[^{]+/);
  parser.expect("{");
  const body = expectInsts(parser);
  parser.expect("}");
  return {
    ...mergeSpans([main, kind, name, params, retTy, body]),
    type: "func",
    main,
    kind,
    name,
    params,
    retTy,
    body,
  };
}

function expectFuncKind(parser: Parser): Token {
  return parser.accept(/<\w+>/) || parser.expect("");
}

function expectFuncName(parser: Parser): Token {
  return parser.expect(/^[<>\w|:\.\[\],@]+/);
}

function expectFuncParams(parser: Parser): ast.Param[] {
  const params: ast.Param[] = [];
  parser.expect("(");
  while (true) {
    skipWsAndSweepComments(parser);
    const param = acceptFuncParam(parser);
    if (!param) break;
    params.push(param);
  }
  parser.expect(")");
  return params;
}

function acceptFuncParam(parser: Parser): ast.Param | undefined {
  const lhs = acceptIdent(parser);
  if (!lhs) return;
  skipWsAndSweepComments(parser);
  const optional = parser.accept("?");
  parser.expect(":");
  skipWsAndSweepComments(parser);
  const ty = parser.expect(/.+,\n/); // TODO
  skipWsAndSweepComments(parser);
  return {
    ...mergeSpans([lhs, optional, ty]),
    type: "param",
    lhs,
    ty,
    optional,
  };
}

function expectInsts(parser: Parser): ast.Inst[] {
  const insts: ast.Inst[] = [];
  while (true) {
    skipWsAndSweepComments(parser);
    const inst = acceptInst(parser);
    if (!inst) break;
    insts.push(inst);
  }
  return insts;
}

const acceptInst = choice<ast.Inst>([
  acceptIIf,
  acceptILoop,
  acceptICall,
  acceptIMethodCall,
  acceptISdoCall,
  acceptILet,
  acceptIDelete,
  acceptIPush,
  acceptIRemoveElem,
  acceptIReturn,
  acceptIAssert,
  acceptIPrint,
  acceptINop,
  acceptIAssign,
  acceptIExpr,
]);

function expectInst(parser: Parser): ast.Inst {
  const inst = acceptInst(parser);
  if (!inst) throw new SyntaxError(parser, []);
  return inst;
}

function acceptIIf(parser: Parser): ast.IIf | undefined {
  const head = parser.accept("if")
  if (!head) return;
  skipWsAndSweepComments(parser);
  const cond = expectExpr(parser);
  skipWsAndSweepComments(parser);
  const thenBlock = parser.accept("{");
  const thenInst = thenBlock ? expectInsts(parser) : [expectInst(parser)];
  if(thenBlock) parser.expect("}");
  skipWsAndSweepComments(parser);
  parser.expect("else");
  skipWsAndSweepComments(parser);
  const elseBlock = parser.accept("{");
  const elseInst = elseBlock ? expectInsts(parser) : [expectInst(parser)];
  if(elseBlock) parser.expect("}");
  return {
    ...mergeSpans([head, cond, thenInst, elseInst]),
    type: "if",
    cond,
    thenInst,
    elseInst,
  };
}

function acceptILoop(parser: Parser): ast.ILoop | undefined {
  const head = parser.accept("loop");
  if (!head) return;
  parser.expect("[");
  const kind = parser.expect(/^[^\]]+/);
  parser.expect("]");
  skipWsAndSweepComments(parser);
  const cond = expectExpr(parser);
  skipWsAndSweepComments(parser);
  parser.expect("{");
  const body = expectInsts(parser);
  parser.expect("}");
  return {
    ...mergeSpans([head, kind, cond, body]),
    type: "loop",
    kind,
    cond,
    body,
  };
}

function acceptICall(parser: Parser): ast.ICall | undefined {
  const head = parser.accept("call");
  if (!head) return;
  skipWsAndSweepComments(parser);
  const lhs = expectIdent(parser);
  skipWsAndSweepComments(parser);
  parser.expect("=");
  skipWsAndSweepComments(parser);
  const fexpr = expectExpr(parser);
  const args = expectArgs(parser);
  return {
    ...mergeSpans([lhs, fexpr, args]),
    type: "call",
    lhs,
    fexpr,
    args,
  };
}

function acceptIMethodCall(parser: Parser): ast.IMethodCall | undefined {
  const head = parser.accept("method-call");
  if (!head) return;
  skipWsAndSweepComments(parser);
  const lhs = expectIdent(parser);
  skipWsAndSweepComments(parser);
  parser.expect("=");
  skipWsAndSweepComments(parser);
  const base = expectERef(parser);
  parser.expect("->");
  const method = expectWord(parser);
  const args = expectArgs(parser);
  return {
    ...mergeSpans([head, lhs, base, method, args]),
    type: "method-call",
    lhs,
    base,
    method,
    args,
  };
}

function acceptISdoCall(parser: Parser): ast.ISDOCall | undefined {
  const head = parser.accept("sdo-call");
  if (!head) return;
  skipWsAndSweepComments(parser);
  const lhs = expectIdent(parser);
  skipWsAndSweepComments(parser);
  parser.expect("=");
  skipWsAndSweepComments(parser);
  const base = expectExpr(parser);
  parser.expect("->");
  const method = expectWord(parser);
  const args = expectArgs(parser);
  return {
    ...mergeSpans([head, lhs, base, method, args]),
    type: "sdo-call",
    lhs,
    base,
    method,
    args,
  };
}

function acceptILet(parser: Parser): ast.ILet | undefined {
  const head = parser.accept("let");
  if (!head) return;
  skipWsAndSweepComments(parser);
  const lhs = expectIdent(parser);
  skipWsAndSweepComments(parser);
  parser.expect("=");
  skipWsAndSweepComments(parser);
  const expr = expectExpr(parser);
  return {
    ...mergeSpans([head, lhs, expr]),
    type: "let",
    lhs,
    expr,
  };
}

function acceptIDelete(parser: Parser): ast.IDelete | undefined {
  const head = parser.accept("delete");
  if (!head) return;
  skipWsAndSweepComments(parser);
  const ref = expectIdent(parser);
  return {
    ...mergeSpans([head, ref]),
    type: "delete",
    ref,
  };
}

function acceptIPush(parser: Parser): ast.IPush | undefined {
  const head = parser.accept("push");
  if (!head) return;
  skipWsAndSweepComments(parser);
  const from = expectExpr(parser);
  skipWsAndSweepComments(parser);
  const front = parser.expect(/>|</);
  skipWsAndSweepComments(parser);
  const to = expectExpr(parser);
  skipWsAndSweepComments(parser);
  return {
    ...mergeSpans([head, from, to, front]),
    type: "push",
    from,
    to,
    front,
  };
}

function acceptIRemoveElem(parser: Parser): ast.IRemoveElem | undefined {
  const head = parser.accept("remove-elem");
  if (!head) return;
  skipWsAndSweepComments(parser);
  const list = expectExpr(parser);
  skipWsAndSweepComments(parser);
  const elem = expectExpr(parser);
  return {
    ...mergeSpans([head, list, elem]),
    type: "remove-elem",
    list,
    elem,
  };
}

function acceptIReturn(parser: Parser): ast.IReturn | undefined {
  const head = parser.accept("return");
  if (!head) return;
  skipWsAndSweepComments(parser);
  const expr = expectExpr(parser);
  return {
    ...mergeSpans([head, expr]),
    type: "return",
    expr,
  };
}

function acceptIAssert(parser: Parser): ast.IAssert | undefined {
  const head = parser.accept("assert");
  if (!head) return;
  skipWsAndSweepComments(parser);
  const expr = expectExpr(parser);
  return {
    ...mergeSpans([head, expr]),
    type: "assert",
    expr,
  };
}

function acceptIPrint(parser: Parser): ast.IPrint | undefined {
  const head = parser.accept("print");
  if (!head) return;
  skipWsAndSweepComments(parser);
  const expr = expectExpr(parser);
  return {
    ...mergeSpans([head, expr]),
    type: "print",
    expr,
  };
}

function acceptINop(parser: Parser): ast.INop | undefined {
  const nop = parser.accept("nop");
  if (!nop) return;
  return {
    ...mergeSpans([nop]),
    type: "nop",
  };
}

function acceptIAssign(parser: Parser): ast.IAssign | undefined {
  const ref = acceptERef(parser);
  if (!ref) return;
  skipWsAndSweepComments(parser);
  parser.expect("=");
  skipWsAndSweepComments(parser);
  const expr = expectExpr(parser);
  return {
    ...mergeSpans([ref, expr]),
    type: "assign",
    ref,
    expr,
  };
}

function acceptIExpr(parser: Parser): ast.IExpr | undefined {
  const expr = acceptExpr(parser);
  if (!expr) return;
  return {
    ...mergeSpans([expr]),
    type: "expr",
    expr,
  };
}

const acceptLiteralExpr = choice<ast.LiteralExpr>([
  acceptEBigInt,
  acceptEDouble,
  acceptECodeUnit,
  acceptEInf,
  acceptENaN,
  acceptEDecimal,
  acceptEString,
  acceptEBool,
  acceptEUndefined,
  acceptENull,
  acceptEAbsent,
  acceptEConst,
]);
const acceptAllocExpr = choice<ast.AllocExpr>([
  acceptEMap,
  acceptEList,
  acceptEListConcat,
  acceptESymbol,
  acceptECopy,
  acceptEKeys,
  acceptEGetChildren,
  acceptEGetItems,
]);
const acceptAstExpr = choiceLongest<ast.AstExpr>([
  acceptESyntactic,
  acceptELexical,
]);
const acceptExpr = choice<ast.Expr>([
  acceptEComp,
  acceptEIsCompletion,
  acceptEReturnIfAbrupt,
  acceptEPop,
  acceptEParse,
  acceptENt,
  acceptESourceText,
  acceptEYet,
  acceptEContains,
  acceptESubstring,
  acceptEUnary,
  acceptEBinary,
  acceptEVariadic,
  acceptEClamp,
  acceptEMathOp,
  acceptEConvert,
  acceptETypeOf,
  acceptETypeCheck,
  acceptEDuplicated,
  acceptEIsArrayIndex,
  acceptEClo,
  acceptECont,
  acceptLiteralExpr,
  acceptAllocExpr,
  acceptAstExpr,
  acceptERef,
]);

function expectExpr(parser: Parser): ast.Expr {
  const expr = acceptExpr(parser);
  if (!expr) throw new SyntaxError(parser, []);
  return expr;
}

function acceptEComp(parser: Parser): ast.EComp | undefined {
  const head = parser.accept("comp");
  if (!head) return;
  const head1 = parser.expect("[");
  const expr1 = expectExpr(parser);
  const slash = parser.expect("/");
  const expr2 = expectExpr(parser);
  const tail1 = parser.expect("]");
  const head2 = parser.expect("(");
  const expr3 = expectExpr(parser);
  const tail2 = parser.expect(")");
  return {
    ...mergeSpans([
      head,
      head1,
      expr1,
      slash,
      expr2,
      tail1,
      head2,
      expr3,
      tail2,
    ]),
    type: "comp",
    // tyExpr,
    // tgtExpr,
    // valExpr,
  };
}

function acceptEIsCompletion(parser: Parser): ast.EIsCompletion | undefined {
  const head = parser.accept("(comp?");
  if (!head) return;
  skipWsAndSweepComments(parser);
  const expr = expectExpr(parser);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, expr, tail]),
    type: "is-completion",
  };
}

function acceptEReturnIfAbrupt(
  parser: Parser,
): ast.EReturnIfAbrupt | undefined {
  const head = parser.accept(/^\[(\?|!)/);
  if (!head) return;
  skipWsAndSweepComments(parser);
  const expr = expectExpr(parser);
  const tail = parser.expect("]");
  return {
    ...mergeSpans([head, expr, tail]),
    type: "return-if-abrupt",
  };
}

function acceptEPop(parser: Parser): ast.EPop | undefined {
  const head = parser.accept("(pop");
  if (!head) return;
  skipWsAndSweepComments(parser);
  const front = parser.expect(/^(>|<)/);
  skipWsAndSweepComments(parser);
  const list = expectExpr(parser);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, front, list, tail]),
    type: "pop",
  };
}

function acceptEParse(parser: Parser): ast.EParse | undefined {
  const head = parser.accept("(parse");
  if (!head) return;
  skipWsAndSweepComments(parser);
  const code = expectExpr(parser);
  skipWsAndSweepComments(parser);
  const rule = expectExpr(parser);
  skipWsAndSweepComments(parser);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, code, rule, tail]),
    type: "parse",
  };
}

function acceptENt(parser: Parser): ast.ENt | undefined {
  const head = parser.accept("(nt");
  if (!head) return;
  skipWsAndSweepComments(parser);
  const head1 = parser.expect("|");
  const word = expectWord(parser);
  const tail1 = parser.expect("|");
  skipWsAndSweepComments(parser);
  const params = acceptParseParams(parser);
  skipWsAndSweepComments(parser);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, head1, word, tail1, params, tail]),
    type: "nt",
  };
}

function acceptESourceText(parser: Parser): ast.ESourceText | undefined {
  const head = parser.accept("(source-text");
  if (!head) return;
  skipWsAndSweepComments(parser);
  const expr = expectExpr(parser);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, expr, tail]),
    type: "source-text",
  };
}

function acceptEYet(parser: Parser): ast.EYet | undefined {
  const head = parser.accept("(yet");
  if (!head) return;
  skipWsAndSweepComments(parser);
  const msg = parser.expect(stringPattern);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, msg, tail]),
    type: "yet",
  };
}

function acceptEContains(parser: Parser): ast.EContains | undefined {
  const head = parser.accept("(contains");
  if (!head) return;
  skipWsAndSweepComments(parser);
  const expr = expectExpr(parser);
  skipWsAndSweepComments(parser);
  const from = expectExpr(parser);
  skipWsAndSweepComments(parser);
  const ty = parser.expect(/^[^\)]*/); // TODO
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, expr, from, ty, tail]),
    type: "contains",
  };
}

function acceptESubstring(parser: Parser): ast.ESubstring | undefined {
  const head = parser.accept("(substring");
  if (!head) return;
  skipWsAndSweepComments(parser);
  const expr = expectExpr(parser);
  skipWsAndSweepComments(parser);
  const from = expectExpr(parser);
  skipWsAndSweepComments(parser);
  const to = acceptExpr(parser);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, expr, from, to, tail]),
    type: "substring",
  };
}

function acceptEUnary(parser: Parser): ast.EUnary | undefined {
  const head = parser.accept(/^\((abs|floor|-|!|~)/);
  if (!head) return;
  skipWsAndSweepComments(parser);
  const expr = expectExpr(parser);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, expr, tail]),
    type: "unary",
  };
}

function acceptEBinary(parser: Parser): ast.EBinary | undefined {
  const loc = parser.loc;
  const head = parser.accept("(");
  const bop = parser.accept(/^(\+|-|\*|\/|%|=|&|\||\^|<|>)/);
  if (!head || !bop) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  const left = expectExpr(parser);
  skipWsAndSweepComments(parser);
  const right = expectExpr(parser);
  skipWsAndSweepComments(parser);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, bop, left, right, tail]),
    type: "binary",
  };
}

function acceptEVariadic(parser: Parser): ast.EVariadic | undefined {
  const loc = parser.loc;
  const head = parser.accept("(");
  const vop = choiceString([
    "min",
    "max",
    "concat"
  ])(parser);
  if (!head || !vop) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  let expr: ast.Expr | undefined;
  const exprs: ast.Expr[] = [];
  while (expr = acceptExpr(parser)) {
    exprs.push(expr);
    skipWsAndSweepComments(parser);
  }
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, vop, ...exprs, tail]),
    type: "variadic",
  };
}

function acceptEClamp(parser: Parser): ast.EClamp | undefined {
  const head = parser.accept("(clamp");
  if (!head) return;
  skipWsAndSweepComments(parser);
  const target = expectExpr(parser);
  skipWsAndSweepComments(parser);
  const lower = expectExpr(parser);
  skipWsAndSweepComments(parser);
  const upper = expectExpr(parser);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, target, lower, upper, tail]),
    type: "clamp",
  };
}

function acceptEMathOp(parser: Parser): ast.EMathOp | undefined {
  const loc = parser.loc;
  const head = parser.accept("(");
  const mop = parser.accept(/^\[math:\w+\]/);
  if (!head || !mop) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  let expr: ast.Expr | undefined;
  const exprs: ast.Expr[] = [];
  while (expr = acceptExpr(parser)) {
    exprs.push(expr);
    skipWsAndSweepComments(parser);
  }
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, mop, ...exprs, tail]),
    type: "math-op",
  };
}

function acceptEConvert(parser: Parser): ast.EConvert | undefined {
  const loc = parser.loc;
  const head = parser.accept("(");
  const cop = acceptCOp(parser);
  if (!head || !cop) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  const expr = expectExpr(parser);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, expr, tail]),
    type: "convert",
  };
}

function acceptCOp(parser: Parser): Token | undefined {
  const cop = parser.accept(/^\[(approx-number|number|bigInt|math)\]/)
  if (cop) return cop;
  const head = parser.accept("[str");
  if (!head) return;
  let expr: ast.Expr | undefined;
  const exprs: ast.Expr[] = [];
  while (expr = acceptExpr(parser)) {
    exprs.push(expr);
    skipWsAndSweepComments(parser);
  }
  const tail = parser.expect("]");
  return {
    ...mergeSpans([head, ...exprs, tail]),
    text: head.text // TODO + exprs.map((e) => e.text).join("") + tail.text,
  };
}


function acceptETypeOf(parser: Parser): ast.ETypeOf | undefined {
  const head = parser.accept("(typeof");
  if (!head) return;
  skipWsAndSweepComments(parser);
  const base = expectExpr(parser);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, base, tail]),
    type: "type-of",
  };
}

function acceptETypeCheck(parser: Parser): ast.ETypeCheck | undefined {
  const head = parser.accept("(?");
  if (!head) return;
  skipWsAndSweepComments(parser);
  const base = expectExpr(parser);
  skipWsAndSweepComments(parser);
  const colon = parser.expect(":");
  skipWsAndSweepComments(parser);
  const tyExpr = expectExpr(parser);
  skipWsAndSweepComments(parser);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, base, colon, tyExpr, tail]),
    type: "type-check",
  };
}

function acceptEDuplicated(parser: Parser): ast.EDuplicated | undefined {
  const head = parser.accept("(duplicated");
  if (!head) return;
  skipWsAndSweepComments(parser);
  const list = expectExpr(parser);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, list, tail]),
    type: "duplicated",
  };
}

function acceptEIsArrayIndex(parser: Parser): ast.EIsArrayIndex | undefined {
  const head = parser.accept("(array-index");
  if (!head) return;
  skipWsAndSweepComments(parser);
  const expr = expectExpr(parser);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, expr, tail]),
    type: "is-array-index",
  };
}

function acceptEClo(parser: Parser): ast.EClo | undefined {
  const head = parser.accept("clo<");
  if (!head) return;
  skipWsAndSweepComments(parser);
  const fname = parser.expect(fnamePattern);
  // TODO unused grammar exists in ESMeta
  const tail = parser.expect(">");
  return {
    ...mergeSpans([head, fname, tail]),
    type: "clo",
  };
}

function acceptECont(parser: Parser): ast.ECont | undefined {
  const head = parser.accept("cont<");
  if (!head) return;
  skipWsAndSweepComments(parser);
  const fname = parser.expect(fnamePattern);
  const tail = parser.expect(">");
  return {
    ...mergeSpans([head, fname, tail]),
    type: "cont",
  };
}

function acceptWord(parser: Parser): Token | undefined {
  return parser.accept(/^\w+/);
}

function expectWord(parser: Parser): Token {
  return parser.expect(/^\w+/);
}

function expectArgs(parser: Parser): ast.Args {
  const args: ast.Expr[] = [];
  parser.expect("(");
  while (true) {
    skipWsAndSweepComments(parser);
    const arg = acceptExpr(parser);
    if (!arg) break;
    parser.accept(",");
    args.push(arg);
  }
  parser.expect(")");
  return {
    ...mergeSpans(args),
    type: "args",
    args,
  };
}

// ELiteral parsers
const integerPattern = /^(0|-?[1-9]\d*)/;
const numberPattern = /^[+-]?(0|[1-9][0-9]*)(\.[0-9]+)?/;
function acceptEBigInt(parser: Parser): ast.EBigInt | undefined {
  const loc = parser.loc;
  const integer = parser.accept(integerPattern);
  const suffix = parser.accept("n");
  if (!integer || !suffix) {
    parser.loc = loc;
    return;
  }
  return {
    ...mergeSpans([integer, suffix]),
    text: integer.text + suffix.text,
    type: "bigint",
  };
}
function acceptEDouble(parser: Parser): ast.ENumber | undefined {
  const loc = parser.loc;
  const number = parser.accept(numberPattern);
  const suffix = parser.accept("f");
  if (!number || !suffix) {
    parser.loc = loc;
    return;
  }
  return {
    ...mergeSpans([number, suffix]),
    text: number.text + suffix.text,
    type: "number",
  };
}
function acceptECodeUnit(parser: Parser): ast.ECodeUnit | undefined {
  const loc = parser.loc;
  const codeUnit = parser.accept(integerPattern);
  const suffix = parser.accept("cu");
  if (!codeUnit || !suffix) {
    parser.loc = loc;
    return;
  }
  return {
    ...mergeSpans([codeUnit, suffix]),
    text: codeUnit.text + suffix.text,
    type: "code-unit",
  };
}
function acceptEInf(parser: Parser): ast.ENumber | undefined {
  const inf = parser.accept(/^(\+|-)?INF/);
  if (!inf) return;
  return {
    ...inf,
    type: "number",
  };
}
function acceptENaN(parser: Parser): ast.ENumber | undefined {
  const nan = parser.accept("NaN");
  if (!nan) return;
  return {
    ...nan,
    type: "number",
  };
}
function acceptEDecimal(parser: Parser): ast.EMathVal | undefined {
  const decimal = parser.accept(integerPattern);
  if (!decimal) return;
  return {
    ...decimal,
    type: "math-val",
  };
}
// TODO support \u0000-\u000F?
function acceptEString(parser: Parser): ast.EStr | undefined {
  const str = parser.accept(stringPattern,
  );
  if (!str) return;
  return {
    ...str,
    type: "str",
  };
}
function acceptEBool(parser: Parser): ast.EBool | undefined {
  const bool = parser.accept(/^(true|false)/);
  if (!bool) return;
  return {
    ...bool,
    type: "bool",
  };
}
function acceptEUndefined(parser: Parser): ast.EUndef | undefined {
  const undef = parser.accept("undefined");
  if (!undef) return;
  return {
    ...undef,
    type: "undef",
  };
}
function acceptENull(parser: Parser): ast.ENull | undefined {
  const token = parser.accept("null");
  if (!token) return;
  return {
    ...token,
    type: "null",
  };
}
function acceptEAbsent(parser: Parser): ast.EAbsent | undefined {
  const absent = parser.accept("absent");
  if (!absent) return;
  return {
    ...absent,
    type: "absent",
  };
}
function acceptEConst(parser: Parser): ast.EConst | undefined {
  const head = parser.accept("~");
  if(!head) return
  const body = parser.expect(/^[^~]+/);
  const tail = parser.expect("~");
  return {
    ...mergeSpans([head, body, tail]),
    text: head.text + body.text + tail.text,
    type: "const",
  };
}

// allocExpr parsers
function acceptEMap(parser: Parser): ast.EMap | undefined {
  const loc = parser.loc;
  const head = parser.accept("(new");
  skipWsAndSweepComments(parser);
  const tname = acceptWord(parser); // accept this for EList
  if (!head || !tname) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  const fields = acceptFields(parser);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, tname, tail]),
    type: "map",
    // tname,
    // fields,
  };
}

function acceptEList(parser: Parser): ast.EList | undefined {
  const loc = parser.loc;
  const head = parser.accept("(new");
  skipWsAndSweepComments(parser);
  const head2 = parser.accept("[");
  if (!head || !head2) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  const body = parser.expect(/^[^\]]*/);
  skipWsAndSweepComments(parser);
  const tail2 = parser.expect("]");
  skipWsAndSweepComments(parser);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, head2, body, tail2, tail]),
    type: "list",
    // body,
  };
}

function acceptEListConcat(parser: Parser): ast.EListConcat | undefined {
  const head = parser.accept("(list-concat");
  if (!head) return;
  skipWsAndSweepComments(parser);
  const body = parser.expect(/^[^\)]*/);
  skipWsAndSweepComments(parser);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, body, tail]),
    type: "list-concat",
    // body,
  };
}

function acceptESymbol(parser: Parser): ast.ESymbol | undefined {
  const head = parser.accept(/^\(new\s*'/);
  if (!head) return;
  skipWsAndSweepComments(parser);
  const expr = expectExpr(parser);
  skipWsAndSweepComments(parser);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, expr, tail]),
    type: "symbol",
    // body,
  };
}

function acceptECopy(parser: Parser): ast.ECopy | undefined {
  const head = parser.accept("(copy");
  if (!head) return;
  skipWsAndSweepComments(parser);
  const expr = expectExpr(parser);
  skipWsAndSweepComments(parser);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, expr, tail]),
    type: "copy",
    // body,
  };
}

function acceptEKeys(parser: Parser): ast.EKeys | undefined {
  const head = parser.accept("(keys");
  if (!head) return;
  skipWsAndSweepComments(parser);
  const intSorted = parser.accept("-int");
  skipWsAndSweepComments(parser);
  const expr = expectExpr(parser);
  skipWsAndSweepComments(parser);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, intSorted, expr, tail]),
    type: "keys",
  };
}

function acceptEGetChildren(parser: Parser): ast.EGetChildren | undefined {
  const head = parser.accept("(get-children");
  if (!head) return;
  skipWsAndSweepComments(parser);
  const expr = expectExpr(parser);
  skipWsAndSweepComments(parser);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, expr, tail]),
    type: "get-children",
  };
}

function acceptEGetItems(parser: Parser): ast.EGetItems | undefined {
  const head = parser.accept("(get-items");
  if (!head) return;
  skipWsAndSweepComments(parser);
  const expr = expectExpr(parser);
  skipWsAndSweepComments(parser);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, expr, tail]),
    type: "get-items",
  };
}

// AstExpr parsers
function acceptESyntactic(parser: Parser): ast.ESyntactic | undefined {
  const loc = parser.loc;
  const head1 = parser.accept("|");
  skipWsAndSweepComments(parser);
  const word = acceptWord(parser);
  skipWsAndSweepComments(parser);
  const tail1 = parser.accept("|");
  skipWsAndSweepComments(parser);
  const head2 = parser.try("[");
  if (!head1 || !word || !tail1 || !head2) {
    parser.loc = loc;
    return;
  }
  const params = acceptParseParams(parser);
  skipWsAndSweepComments(parser);
  const head3 = parser.expect("<");
  skipWsAndSweepComments(parser);
  const int = parser.expect(integerPattern);
  skipWsAndSweepComments(parser);
  const tail3 = parser.expect(">");
  skipWsAndSweepComments(parser);
  // TODO optional expr parser exists, but not used
  return {
    ...mergeSpans([
      head1,
      word,
      tail1,
      head2,
      params,
      head3,
      int,
      tail3,
    ]),
    type: "syntactic",
  };
}

function acceptParseParams(parser: Parser): Token | undefined {
  const head = parser.accept("[");
  if (!head) return;
  const params = parser.expect(/^(T|F)*/);
  const tail = parser.expect("]");
  return {
    ...mergeSpans([head, params, tail]),
    text: head.text + params.text + tail.text,
  };
}

function acceptELexical(parser: Parser): ast.ELexical | undefined {
  const loc = parser.loc;
  const head1 = parser.accept("|");
  skipWsAndSweepComments(parser);
  const word = acceptWord(parser);
  skipWsAndSweepComments(parser);
  const tail1 = parser.accept("|");
  skipWsAndSweepComments(parser);
  const head2 = parser.accept("(");
  if (!head1 || !word || !tail1 || !head2) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  const expr = expectExpr(parser);
  skipWsAndSweepComments(parser);
  const tail2 = parser.expect(")");
  skipWsAndSweepComments(parser);
  // TODO optional expr parser exists, but not used
  return {
    ...mergeSpans([head1, word, tail2, head2, expr, tail2]),
    type: "lexical",
  };
}

// ERef parsers
function acceptERef(parser: Parser): ast.ERef | undefined {
  const id = acceptRefId(parser);
  if (!id) return;
  const props = many(
    parser,
    choice<Token>([
      acceptRefDotProp,
      acceptRefIndexProp,
    ]),
  );
  return {
    ...mergeSpans([id, ...props]),
    type: "ref",
  };
}

function expectERef(parser: Parser): ast.ERef {
  const expr = acceptERef(parser);
  if (!expr) throw new SyntaxError(parser, []);
  return expr;
}

function acceptRefId(parser: Parser): Token | undefined {
  return parser.accept(idPattern) ?? parser.accept(localPattern) ??
    parser.accept(namePattern);
}

function acceptRefDotProp(parser: Parser): Token | undefined {
  const dot = parser.accept(".");
  if (!dot) return;
  const prop = expectIdent(parser);
  return {
    ...mergeSpans([dot, prop]),
    text: dot.text + prop.text,
  };
}

function acceptRefIndexProp(parser: Parser): Token | undefined {
  const head = parser.accept("[");
  if (!head) return;
  const expr = expectExpr(parser);
  const tail = parser.expect("]");
  return {
    ...mergeSpans([head, expr, tail]),
    text: head.text, // TODO + expr.text,
  };
}

// basic parsers
function acceptIdent(parser: Parser): Token | undefined {
  return parser.accept(identPattern) ?? parser.accept(idPattern) ??
    parser.accept(localPattern);
}

function expectIdent(parser: Parser): Token {
  const ident = acceptIdent(parser);
  if (!ident) {
    throw new SyntaxError(parser, [identPattern, idPattern, localPattern]);
  }
  return ident;
}

function acceptFields(parser: Parser): [ast.Expr, ast.Expr][] | undefined {
  const head = parser.accept("(");
  if (!head) return;
  const fields: [ast.Expr, ast.Expr][] = [];
  while (true) {
    skipWsAndSweepComments(parser);
    const field = acceptField(parser);
    if (!field) break;
    parser.accept(",");
    fields.push(field);
  }
  parser.expect(")");
  return fields;
}

function acceptField(parser: Parser): [ast.Expr, ast.Expr] | undefined {
  const loc = parser.loc;
  const key = acceptExpr(parser);
  skipWsAndSweepComments(parser);
  const arrow = parser.accept("->");
  if (!key || !arrow) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  const value = expectExpr(parser);
  return [key, value];
}

const whitespacePattern = /^\s+/;
const whitespaceWithoutNewlinePattern =
  /^[ \f\t\v\u00a0\u1680\u2000-\u200a\u2028\u2029\u202f\u205f\u3000\ufeff]+/;
const newlinePattern = /^\r?\n/;
const singlelineCommentPattern = /^\/\/.*(?:\r?\n|$)/;

const identPattern = /^[_a-zA-Z][_a-zA-Z0-9]*/i;
const idPattern = /^@[A-Za-z_]+/i;
const localPattern = /^%(0|[1-9][0-9]*)/i;
const namePattern = /^[_a-zA-Z][_a-zA-Z0-9]*/;
const stringPattern = /^\"([^"\x00-\x1F\x7F\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*\"/
const fnamePattern = /^[^<>, ]+/;

function skipWsAndSweepComments(parser: Parser) {
  parser.accept(whitespacePattern);
  while (true) {
    const whitespace = parser.accept(whitespaceWithoutNewlinePattern);
    if (whitespace) continue;
    const comment = parser.accept(singlelineCommentPattern);
    if (comment) continue;
    const newline = parser.accept(newlinePattern);
    if (newline) continue;
    break;
  }
}

function many<T>(
  parser: Parser,
  acceptFn: AcceptFn<T>,
): T[] {
  const nodes: T[] = [];
  let node: ReturnType<typeof acceptFn>;
  while (node = acceptFn(parser)) nodes.push(node);
  return nodes;
}


function choice<T>(acceptFns: AcceptFn<T>[]): AcceptFn<T> {
  return function accept(parser) {
    for (const acceptFn of acceptFns) {
      const node = acceptFn(parser);
      if (node) return node;
    }
  };
}

function choiceString(
  patterns: string[]
): AcceptFn<Token> {
  return function accept(parser) {
    for (const pattern of patterns) {
      const node = parser.accept(pattern);
      if (node) return node;
    }
  }
}

function choiceLongest<T extends Span>(
  acceptFns: AcceptFn<T>[],
): AcceptFn<T> {
  return function accept(parser) {
    return many(parser, choice(acceptFns)).sort((a, b) =>
      (b.end - b.start) - (a.end - a.start)
    )[0];
  };
}

function acceptSpecialToken<TType extends string>(
  parser: Parser,
  type: TType,
  pattern: Pattern = identPattern,
): (Token & { type: TType }) | undefined {
  const token = parser.accept(pattern);
  if (!token) return;
  return { type, ...token };
}

interface AcceptFn<T> {
  (parser: Parser): T | undefined;
}
function acceptPatternAndThen<T>(
  pattern: Pattern,
  then: (token: Token) => T,
): AcceptFn<T> {
  return function accept(parser) {
    const token = parser.accept(pattern);
    if (!token) return;
    return then(token);
  };
}

function mergeSpans(
  spans: (undefined | Span | (undefined | Span)[])[],
): Span {
  let start = Infinity;
  let end = -Infinity;
  for (let i = 0; i < spans.length; ++i) {
    if (spans[i] == null) continue;
    const span = Array.isArray(spans[i])
      ? mergeSpans(spans[i] as Span[])
      : spans[i] as Span;
    start = Math.min(start, span.start);
    end = Math.max(end, span.end);
  }
  return { start, end };
}

interface AcceptStatementFn<T extends Span> {
  (parser: Parser): T | undefined;
}
function acceptStatements<T extends Span>(
  parser: Parser,
  acceptStatementFns: AcceptStatementFn<T>[],
) {
  const statements: T[] = [];
  statements:
  while (true) {
    skipWsAndSweepComments(parser);
    for (const acceptStatementFn of acceptStatementFns) {
      const statement = acceptStatementFn(parser);
      if (statement) {
        statements.push(statement);
        continue statements;
      }
    }
    break;
  }
  return statements;
}
