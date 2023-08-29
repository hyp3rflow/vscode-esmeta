import * as ast from "./ast";
import {
  createRecursiveDescentParser,
  Pattern,
  RecursiveDescentParser as Parser,
  Span,
  SyntaxError,
  Token,
} from "./recursive-descent-parser";

export interface ParseResult {
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
  const loc = parser.loc;
  const lhs = acceptIdent(parser);
  if (!lhs) {
    parser.loc = loc;
    return;
  }
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

function acceptIIf(parser: Parser): ast.IIf | undefined {
  const loc = parser.loc;
  if (!parser.accept("if")) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  const cond = expectExpr(parser);
  skipWsAndSweepComments(parser);
  parser.expect("{");
  const thenInst = expectInsts(parser);
  parser.expect("}");
  skipWsAndSweepComments(parser);
  parser.expect("else");
  skipWsAndSweepComments(parser);
  parser.expect("{");
  const elseInst = expectInsts(parser);
  parser.expect("}");
  return {
    ...mergeSpans([cond, thenInst, elseInst]),
    type: "if",
    cond,
    thenInst,
    elseInst,
  };
}

function acceptILoop(parser: Parser): ast.ILoop | undefined {
  const loc = parser.loc;
  if (!parser.accept("loop")) {
    parser.loc = loc;
    return;
  }
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
    ...mergeSpans([kind, cond, body]),
    type: "loop",
    kind,
    cond,
    body,
  };
}

function acceptICall(parser: Parser): ast.ICall | undefined {
  const loc = parser.loc;
  const head = parser.accept("call");
  if (!head) {
    parser.loc = loc;
    return;
  }
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
  const loc = parser.loc;
  const head = parser.accept("method-call");
  if (!head) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  const lhs = expectIdent(parser);
  skipWsAndSweepComments(parser);
  parser.expect("=");
  skipWsAndSweepComments(parser);
  const base = expectIdent(parser);
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
  const loc = parser.loc;
  const head = parser.accept("sdo-call");
  if (!head) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  const lhs = expectIdent(parser);
  skipWsAndSweepComments(parser);
  parser.expect("=");
  skipWsAndSweepComments(parser);
  const base = expectIdent(parser);
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
  const loc = parser.loc;
  const head = parser.accept("let");
  if (!head) {
    parser.loc = loc;
    return;
  }
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
  const loc = parser.loc;
  const head = parser.accept("delete");
  if (!head) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  const ref = expectIdent(parser);
  return {
    ...mergeSpans([head, ref]),
    type: "delete",
    ref,
  };
}

function acceptIPush(parser: Parser): ast.IPush | undefined {
  const loc = parser.loc;
  const head = parser.accept("push");
  if (!head) {
    parser.loc = loc;
    return;
  }
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
  const loc = parser.loc;
  const head = parser.accept("remove-elem");
  if (!head) {
    parser.loc = loc;
    return;
  }
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
  const loc = parser.loc;
  const head = parser.accept("return");
  if (!head) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  const expr = expectExpr(parser);
  return {
    ...mergeSpans([head, expr]),
    type: "return",
    expr,
  };
}

function acceptIAssert(parser: Parser): ast.IAssert | undefined {
  const loc = parser.loc;
  const head = parser.accept("assert");
  if (!head) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  const expr = expectExpr(parser);
  return {
    ...mergeSpans([head, expr]),
    type: "assert",
    expr,
  };
}

function acceptIPrint(parser: Parser): ast.IPrint | undefined {
  const loc = parser.loc;
  const head = parser.accept("print");
  if (!head) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  const expr = expectExpr(parser);
  return {
    ...mergeSpans([head, expr]),
    type: "print",
    expr,
  };
}

function acceptINop(parser: Parser): ast.INop | undefined {
  const loc = parser.loc;
  const nop = parser.accept("nop");
  if (!nop) {
    parser.loc = loc;
    return;
  }
  return {
    ...mergeSpans([nop]),
    type: "nop",
  };
}

function acceptIAssign(parser: Parser): ast.IAssign | undefined {
  const loc = parser.loc;
  const ref = acceptIdent(parser);
  if (!ref) {
    parser.loc = loc;
    return;
  }
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
  const loc = parser.loc;
  const expr = acceptExpr(parser);
  if (!expr) {
    parser.loc = loc;
    return;
  }
  return {
    ...mergeSpans([expr]),
    type: "expr",
    expr,
  };
}

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
]);

function expectExpr(parser: Parser): ast.Expr {
  const expr = acceptExpr(parser);
  if (!expr) throw new SyntaxError(parser, []);
  return expr;
}

function acceptEComp(parser: Parser): ast.EComp | undefined {
  const loc = parser.loc;
  const head = parser.accept("comp");
  if (!head) {
    parser.loc = loc;
    return;
  }
  const tyExpr = parser.expect(/^\[[^\]]+\]/);
  const tgtExpr = tyExpr; // TODO
  const valExpr = parser.expect(/^\([^\)]+\)/);
  return {
    ...mergeSpans([head, tyExpr, tgtExpr, valExpr]),
    type: "comp",
    // tyExpr,
    // tgtExpr,
    // valExpr,
  };
}

function acceptEIsCompletion(parser: Parser): ast.EIsCompletion | undefined {
  const loc = parser.loc;
  const head = parser.accept("(comp?");
  if (!head) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  const expr = parser.expect(/^[^\)]+/);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, expr, tail]),
    type: "is-completion",
  };
}

function acceptEReturnIfAbrupt(
  parser: Parser,
): ast.EReturnIfAbrupt | undefined {
  const loc = parser.loc;
  const head = parser.accept(/^\[(\?|!)/);
  if (!head) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  const expr = parser.expect(/^[^\)]]+/);
  const tail = parser.expect("]");
  return {
    ...mergeSpans([head, expr, tail]),
    type: "return-if-abrupt",
  };
}

function acceptEPop(parser: Parser): ast.EPop | undefined {
  const loc = parser.loc;
  const head = parser.accept("(pop");
  if (!head) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  const front = parser.expect(/^(>|<)/);
  skipWsAndSweepComments(parser);
  const list = parser.expect(/^[^\)]+/);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, front, list, tail]),
    type: "pop",
  };
}

function acceptEParse(parser: Parser): ast.EParse | undefined {
  const loc = parser.loc;
  const head = parser.accept("(parse");
  if (!head) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  const code = parser.expect(/^[^\)]+/);
  const tail = parser.expect(")");
  const tail2 = parser.expect(")");
  return {
    ...mergeSpans([head, code, tail, tail2]),
    type: "parse",
  };
}

function acceptENt(parser: Parser): ast.ENt | undefined {
  const loc = parser.loc;
  const head = parser.accept("(nt");
  if (!head) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  const body = parser.expect(/^[^\)]+/);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, body, tail]),
    type: "nt",
  };
}

function acceptESourceText(parser: Parser): ast.ESourceText | undefined {
  const loc = parser.loc;
  const head = parser.accept("(source-text");
  if (!head) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  const body = parser.expect(/^[^\)]+/);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, body, tail]),
    type: "source-text",
  };
}

function acceptEYet(parser: Parser): ast.EYet | undefined {
  const loc = parser.loc;
  const head = parser.accept("(yet");
  if (!head) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  const body = parser.expect(/^[^\)]+/);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, body, tail]),
    type: "yet",
  };
}

function acceptEContains(parser: Parser): ast.EContains | undefined {
  const loc = parser.loc;
  const head = parser.accept("(contains");
  if (!head) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  const body = parser.expect(/^[^\)]+/);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, body, tail]),
    type: "contains",
  };
}

function acceptESubstring(parser: Parser): ast.ESubstring | undefined {
  const loc = parser.loc;
  const head = parser.accept("(substring");
  if (!head) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  const body = parser.expect(/^[^\)]+/);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, body, tail]),
    type: "substring",
  };
}

function acceptEUnary(parser: Parser): ast.EUnary | undefined {
  const loc = parser.loc;
  const head = parser.accept(/^\((abs|floor|-|!|~)/);
  if (!head) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  const body = parser.expect(/^[^\)]+/);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, body, tail]),
    type: "unary",
  };
}

function acceptEBinary(parser: Parser): ast.EBinary | undefined {
  const loc = parser.loc;
  const head = parser.accept("(+"); // TODO bop
  if (!head) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  const body = parser.expect(/^[^\)]+/);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, body, tail]),
    type: "binary",
  };
}

function acceptEVariadic(parser: Parser): ast.EVariadic | undefined {
  const loc = parser.loc;
  const head = parser.accept("(min"); // TODO vop
  if (!head) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  const body = parser.expect(/^[^\)]+/);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, body, tail]),
    type: "variadic",
  };
}

function acceptEClamp(parser: Parser): ast.EClamp | undefined {
  const loc = parser.loc;
  const head = parser.accept("(clamp"); // TODO vop
  if (!head) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  const body = parser.expect(/^[^\)]+/);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, body, tail]),
    type: "clamp",
  };
}

function acceptEMathOp(parser: Parser): ast.EMathOp | undefined {
  const loc = parser.loc;
  const head = parser.accept("([math:expm1]"); // TODO mop
  if (!head) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  const body = parser.expect(/^[^\)]+/);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, body, tail]),
    type: "math-op",
  };
}

function acceptEConvert(parser: Parser): ast.EConvert | undefined {
  const loc = parser.loc;
  const head = parser.accept("([approx-number]"); // TODO cop
  if (!head) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  const body = parser.expect(/^[^\)]+/);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, body, tail]),
    type: "convert",
  };
}

function acceptETypeOf(parser: Parser): ast.ETypeOf | undefined {
  const loc = parser.loc;
  const head = parser.accept("(typeof");
  if (!head) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  const body = parser.expect(/^[^\)]+/);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, body, tail]),
    type: "type-of",
  };
}

function acceptETypeCheck(parser: Parser): ast.ETypeCheck | undefined {
  const loc = parser.loc;
  const head = parser.accept("(?");
  if (!head) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  const body = parser.expect(/^[^\)]+/);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, body, tail]),
    type: "type-check",
  };
}

function acceptEDuplicated(parser: Parser): ast.EDuplicated | undefined {
  const loc = parser.loc;
  const head = parser.accept("(duplicated");
  if (!head) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  const body = parser.expect(/^[^\)]+/);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, body, tail]),
    type: "duplicated",
  };
}

function acceptEIsArrayIndex(parser: Parser): ast.EIsArrayIndex | undefined {
  const loc = parser.loc;
  const head = parser.accept("(array-index");
  if (!head) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  const body = parser.expect(/^[^\)]+/);
  const tail = parser.expect(")");
  return {
    ...mergeSpans([head, body, tail]),
    type: "is-array-index",
  };
}

function acceptEClo(parser: Parser): ast.EClo | undefined {
  const loc = parser.loc;
  const head = parser.accept("clo<");
  if (!head) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  const body = parser.expect(/^[^\>]+/);
  const tail = parser.expect(">");
  return {
    ...mergeSpans([head, body, tail]),
    type: "clo",
  };
}

function acceptECont(parser: Parser): ast.ECont | undefined {
  const loc = parser.loc;
  const head = parser.accept("cont<");
  if (!head) {
    parser.loc = loc;
    return;
  }
  skipWsAndSweepComments(parser);
  const body = parser.expect(/^[^\>]+/);
  const tail = parser.expect(">");
  return {
    ...mergeSpans([head, body, tail]),
    type: "cont",
  };
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

const whitespacePattern = /^\s+/;
const whitespaceWithoutNewlinePattern =
  /^[ \f\t\v\u00a0\u1680\u2000-\u200a\u2028\u2029\u202f\u205f\u3000\ufeff]+/;
const newlinePattern = /^\r?\n/;
const singlelineCommentPattern = /^\/\/.*(?:\r?\n|$)/;

const identPattern = /^[_a-zA-Z][_a-zA-Z0-9]*/i;
const idPattern = /^@[A-Za-z_]+/i;
const localPattern = /^%(0|[1-9][0-9]*)/i;

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

export function many<T>(
  parser: Parser,
  acceptFn: AcceptFn<T>,
): T[] {
  const nodes: T[] = [];
  let node: ReturnType<typeof acceptFn>;
  while (node = acceptFn(parser)) nodes.push(node);
  return nodes;
}

export function choice<T>(acceptFns: AcceptFn<T>[]): AcceptFn<T> {
  return function accept(parser) {
    for (const acceptFn of acceptFns) {
      const node = acceptFn(parser);
      if (node) return node;
    }
  };
}

export function acceptSpecialToken<TType extends string>(
  parser: Parser,
  type: TType,
  pattern: Pattern = identPattern,
): (Token & { type: TType }) | undefined {
  const token = parser.accept(pattern);
  if (!token) return;
  return { type, ...token };
}

export interface AcceptFn<T> {
  (parser: Parser): T | undefined;
}
export function acceptPatternAndThen<T>(
  pattern: Pattern,
  then: (token: Token) => T,
): AcceptFn<T> {
  return function accept(parser) {
    const token = parser.accept(pattern);
    if (!token) return;
    return then(token);
  };
}

export function mergeSpans(
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
