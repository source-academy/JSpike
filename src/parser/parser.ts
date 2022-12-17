// Node.js v17.6.0

import { display, error, head, is_null, map, parse, tail } from "sicp";

declare var require: any;

type List<A> = null | [A, List<A>];

type Value = undefined | string | number | boolean;

type Literal = { tag: "lit"; val: Value };
type Name = { tag: "nam"; sym: string };
type Application = { tag: "app"; fun: SyntaxTree; args: List<SyntaxTree> };
type LogicalComposition = {
  tag: "log";
  sym: string;
  frst: SyntaxTree;
  scnd: SyntaxTree;
};
type BinaryOperation = {
  tag: "binop";
  sym: string;
  frst: SyntaxTree;
  scnd: SyntaxTree;
};
type ArrayAccess = { tag: "arr_acc"; arr: SyntaxTree; ind: SyntaxTree };
type ArrayAssignment = {
  tag: "arr_assmt";
  arr: SyntaxTree;
  ind: SyntaxTree;
  expr: SyntaxTree;
};
type LiteralArray = { tag: "arr_lit"; elems: [SyntaxTree] };
type UnaryOperation = { tag: "unop"; sym: string; frst: SyntaxTree };
type Decl = string | { tag: "rest"; sym: string };
type Lambda = { tag: "lam"; prms: [Decl]; body: SyntaxTree };
type Sequence = { tag: "seq"; stmts: [SyntaxTree] };
type ConditionalExpression = {
  tag: "cond_expr";
  pred: SyntaxTree;
  cons: SyntaxTree;
  alt: SyntaxTree;
};
type ConditionalStatement = {
  tag: "cond_stmt";
  pred: SyntaxTree;
  cons: SyntaxTree;
  alt: SyntaxTree;
};
type While = { tag: "while"; pred: SyntaxTree; body: SyntaxTree };
type For = {
  tag: "for";
  init: SyntaxTree;
  pred: SyntaxTree;
  upd: SyntaxTree;
  body: SyntaxTree;
};
type Break = { tag: "break" };
type Continue = { tag: "cont" };
type Block = { tag: "blk"; body: SyntaxTree };
type Let = { tag: "let"; sym: string; expr: SyntaxTree };
type Assignment = { tag: "assmt"; sym: string; expr: SyntaxTree };
type Const = { tag: "const"; sym: string; expr: SyntaxTree };
type Function = { tag: "fun"; sym: string; prms: [Decl]; body: SyntaxTree };
type Return = { tag: "ret"; expr: SyntaxTree };

type Import = { tag: "import"; syms: List<string>; from: string };
type This = { tag: "this" };
type Spread = { tag: "spread"; sym: string };
type Property = { tag: "prop"; sym: string };
type SyntaxTree =
  | Literal
  | Name
  | Application
  | BinaryOperation
  | UnaryOperation
  | LogicalComposition
  | Lambda
  | Sequence
  | ConditionalExpression
  | ConditionalStatement
  | While
  | Break
  | Continue
  | For
  | Block
  | Let
  | Const
  | Assignment
  | ArrayAccess
  | ArrayAssignment
  | LiteralArray
  | Function
  | Return
  | Import
  | This
  | Spread
  | Property;

function list_to_array(xs) {
  return is_null(xs) ? [] : [head(xs)].concat(list_to_array(tail(xs)));
}

function parameters(xs) {
  return map(
    (x) =>
      head(x) === "rest_element"
        ? { tag: "rest", sym: head(tail(head(tail(x)))) }
        : head(tail(x)),
    xs
  );
}

// turn tagged list syntax from parse into object
function objectify(t): SyntaxTree {
  //  display_list(t, "in objectify");
  switch (head(t)) {
    case "literal":
      return { tag: "lit", val: head(tail(t)) };
    case "name":
      return { tag: "nam", sym: head(tail(t)) };
    case "application":
      return {
        tag: "app",
        fun: objectify(head(tail(t))),
        args: list_to_array(map(objectify, head(tail(tail(t))))),
      };
    case "logical_composition":
      return {
        tag: "log",
        sym: head(tail(t)),
        frst: objectify(head(tail(tail(t)))),
        scnd: objectify(head(tail(tail(tail(t))))),
      };
    case "binary_operator_combination":
      return {
        tag: "binop",
        sym: head(tail(t)),
        frst: objectify(head(tail(tail(t)))),
        scnd: objectify(head(tail(tail(tail(t))))),
      };
    case "object_access":
      return {
        tag: "arr_acc",
        arr: objectify(head(tail(t))),
        ind: objectify(head(tail(tail(t)))),
      };
    case "object_assignment":
      return {
        tag: "arr_assmt",
        arr: objectify(head(tail(head(tail(t))))),
        ind: objectify(head(tail(tail(head(tail(t)))))),
        expr: objectify(head(tail(tail(t)))),
      };
    case "array_expression":
      return {
        tag: "arr_lit",
        elems: list_to_array(map(objectify, head(tail(t)))),
      };
    case "unary_operator_combination":
      return {
        tag: "unop",
        sym: head(tail(t)),
        frst: objectify(head(tail(tail(t)))),
      };
    case "lambda_expression":
      return {
        tag: "lam",
        prms: list_to_array(parameters(head(tail(t)))),
        body: objectify(head(tail(tail(t)))),
      };
    case "sequence":
      return {
        tag: "seq",
        stmts: list_to_array(map(objectify, head(tail(t)))),
      };
    case "block":
      return {
        tag: "blk",
        body: objectify(head(tail(t))),
      };
    case "variable_declaration":
      return {
        tag: "let",
        sym: head(tail(head(tail(t)))),
        expr: objectify(head(tail(tail(t)))),
      };
    case "constant_declaration":
      return {
        tag: "const",
        sym: head(tail(head(tail(t)))),
        expr: objectify(head(tail(tail(t)))),
      };
    case "assignment":
      return {
        tag: "assmt",
        sym: head(tail(head(tail(t)))),
        expr: objectify(head(tail(tail(t)))),
      };
    case "conditional_statement":
      return {
        tag: "cond_stmt",
        pred: objectify(head(tail(t))),
        cons: objectify(head(tail(tail(t)))),
        alt: objectify(head(tail(tail(tail(t))))),
      };
    case "while_loop":
      return {
        tag: "while",
        pred: objectify(head(tail(t))),
        body: objectify(head(tail(tail(t)))),
      };
    case "for_loop":
      return {
        tag: "for",
        init: objectify(head(tail(t))),
        pred: objectify(head(tail(tail(t)))),
        upd: objectify(head(tail(tail(tail(t))))),
        body: objectify(head(tail(tail(tail(tail(t)))))),
      };
    case "break_statement":
      return {
        tag: "break",
      };
    case "continue_statement":
      return {
        tag: "cont",
      };
    case "conditional_expression":
      return {
        tag: "cond_expr",
        pred: objectify(head(tail(t))),
        cons: objectify(head(tail(tail(t)))),
        alt: objectify(head(tail(tail(tail(t))))),
      };
    case "function_declaration":
      return {
        tag: "fun",
        sym: head(tail(head(tail(t)))),
        prms: list_to_array(parameters(head(tail(tail(t))))),
        body: objectify(head(tail(tail(tail(t))))),
      };
    case "return_statement":
      return {
        tag: "ret",
        expr: objectify(head(tail(t))),
      };
    case "import_declaration":
      return {
        tag: "import",
        syms: map((x) => head(tail(x)), head(tail(t))),
        from: head(tail(tail(t))),
      };
    case "this_expression":
      return {
        tag: "this",
      };
    case "spread_element":
      return {
        tag: "spread",
        sym: head(tail(head(tail(t)))),
      };
    case "property":
      return {
        tag: "prop",
        sym: head(tail(t)),
      };
    default:
      throw error(t, "unknown syntax:");
  }
}

export function parse_into_json(program) {
  let obj = objectify(parse(program));
  let json = JSON.stringify(obj);
  let fs = require("fs");
  display(json, "json: ");
  fs.writeFile("../source.json", json, "utf8", () => true);
}

//parse_into_json("42;");

//parse_into_json("42 - 7;");

//parse_into_json("((x, ...y) => ((a,b,c) => a+b+c)(...y))(1,2,3,4);");

//parse_into_json("1; 2; 3;");

//parse_into_json("undefined;");

//parse_into_json("true;");

//parse_into_json("1;");

//parse_into_json("math_pow(2, 3);");

//parse_into_json("'asdf';");

//parse_into_json("'as\"df';");

//parse_into_json("true ? 1 : 2;");

//parse_into_json("22 <= 44 ? 1 : 2;");

//parse_into_json("let x = 1; x + 2;");

parse_into_json("a.b.c;");

/*
const fact = `
function factorial(n) {
  return n === 1
       ? 1
       : n * factorial(n - 1);
}
factorial(4);
`
parse_into_json(fact)
*/

/*
const fact2 = `
function factorial(n) {
  if (n === 1) {
    return 1;
  } else {
    return n * factorial(n - 1);
  }
}
factorial(20);
`
parse_into_json(fact2)
*/

//parse_into_json("a[b];");
// expected: a and b are names. currently: error (fixed in Source parser)

//parse_into_json("a[b] = c;");
// expected: a and b are names. currently: error (fixed in Source parser)

// parse_into_json("[1, 2, 3];");

// parse_into_json("[1, 2, 3][1];");

// parse_into_json("const a = [1, 2, 3]; a[1] = 42; a;");

// parse_into_json("import { show } from 'rune';");

//parse_into_json("while (false) { 1; }");

//parse_into_json("let x = 0; x = 1; while (x < 10) { 1; x = x + 1; } x;");

//parse_into_json("let x = 0; for (x = 1; x < 10; x = x + 1) { 1; } x;");

//parse_into_json("for (let x = 1; x < 10; x = x + 1) { 1; } x;");

/*
const loop = `
let x = 0;
for (let i = 0; i < 1000; i = i + 1) {
  for (let j = 0; j < 100; j = j + 1) {
    x = x + i + j;
  }
}
x;
`
parse_into_json(loop);
*/

/*
const loop = `
let x = 0;
let i = 0;
while (i < 100) {
  let j = 0;
  while (j < 100) {
    x = x + i + j;
    j = j + 1;
  }
  i = i + 1;
}
x;
`
parse_into_json(loop);
*/

/*
const loop = `
let x = 0;
let i = 0;
while (i < 1000) {
  const y = 1;
  i = i + 1;
}
i;
`
parse_into_json(loop);
*/

// parse_into_json("while (false) { 1; }");

// parse_into_json("let i = 0; while (i < 10) { i = i + 1; }");

//parse_into_json("let i = 0; i = i + 1;");

// parse_into_json("for (x = 1; x < 10; x = x + 1) { break; }");

// parse_into_json("let x = 1;");

// parse_into_json("x = 1;");

// parse_into_json("const x = 1;");

// parse_into_json("{ const x = 1; x + 2; } x;");

//parse_into_json("x; let x = 1;");
// expected: Error: unbound local: "x"

//parse_into_json("x;");
// expected: Error: unbound name: "x"

// parse_into_json("! 2;");
// expected: Error: unknown operator ">"

/*
const prelude = 
`
// Prelude


// equal computes the structural equality
// over its arguments

function equal(xs, ys) {
  return is_pair(xs)
  ? (is_pair(ys) &&
  equal(head(xs), head(ys)) &&
  equal(tail(xs), tail(ys)))
  : is_null(xs)
  ? is_null(ys)
  : is_number(xs)
  ? (is_number(ys) && xs === ys)
  : is_boolean(xs)
  ? (is_boolean(ys) && ((xs && ys) || (!xs && !ys)))
  : is_string(xs)
  ? (is_string(ys) && xs === ys)
  : is_undefined(xs)
  ? is_undefined(ys)
  : is_function(xs)
  // we know now that xs is a function,
  // but we use an if check anyway to make use of the type predicate
  ? (is_function(ys) && xs === ys)
  : false;
}


// returns the length of a given argument list
// assumes that the argument is a list

function $length(xs, acc) {
  return is_null(xs) ? acc : $length(tail(xs), acc + 1);
}
function length(xs) {
  return $length(xs, 0);
}

// map applies first arg f, assumed to be a unary function,
// to the elements of the second argument, assumed to be a list.
// f is applied element-by-element:
// map(f, list(1, 2)) results in list(f(1), f(2))

function $map(f, xs, acc) {
  return is_null(xs)
       ? reverse(acc)
       : $map(f, tail(xs), pair(f(head(xs)), acc));
}
function map(f, xs) {
  return $map(f, xs, null);
}

// build_list takes a a function fun as first argument, 
// and a nonnegative integer n as second argument,
// build_list returns a list of n elements, that results from
// applying fun to the numbers from 0 to n-1.

function $build_list(i, fun, already_built) {
  return i < 0 ? already_built : $build_list(i - 1, fun, pair(fun(i), already_built));
}

function build_list(fun, n) {
  return $build_list(n - 1, fun, null);
}

// for_each applies first arg fun, assumed to be a unary function,
// to the elements of the second argument, assumed to be a list.
// fun is applied element-by-element:
// for_each(fun, list(1, 2)) results in the calls fun(1) and fun(2).
// for_each returns true.

function for_each(fun, xs) {
  if (is_null(xs)) {
  return true;
  } else {
  fun(head(xs));
  return for_each(fun, tail(xs));
  }
}

// list_to_string returns a string that represents the argument list.
// It applies itself recursively on the elements of the given list.
// When it encounters a non-list, it applies to_string to it.

function $list_to_string(xs, cont) {
  return is_null(xs)
    ? cont("null")
    : is_pair(xs)
    ? $list_to_string(
        head(xs),
        x => $list_to_string(
             tail(xs),
             y => cont("[" + x + "," + y + "]")))
    : cont(stringify(xs));
}

function list_to_string(xs) {
  return $list_to_string(xs, x => x);
}

// reverse reverses the argument, assumed to be a list

function $reverse(original, reversed) {
  return is_null(original)
       ? reversed
       : $reverse(tail(original), pair(head(original), reversed));
}

function reverse(xs) {
  return $reverse(xs, null);
}

// append first argument, assumed to be a list, to the second argument.
// In the result null at the end of the first argument list
// is replaced by the second argument, regardless what the second
// argument consists of.

function $append(xs, ys, cont) {
  return is_null(xs)
       ? cont(ys)
       : $append(tail(xs), ys, zs => cont(pair(head(xs), zs)));
}

function append(xs, ys) {
  return $append(xs, ys, xs => xs);
}

// member looks for a given first-argument element in the
// second argument, assumed to be a list. It returns the first
// postfix sublist that starts with the given element. It returns null if the
// element does not occur in the list

function member(v, xs) {
  return is_null(xs)
     ? null
	 : v === head(xs)
	 ? xs
	 : member(v, tail(xs));
}

// removes the first occurrence of a given first-argument element
// in second-argument, assmed to be a list. Returns the original
// list if there is no occurrence.

function $remove(v, xs, acc) {
  // Ensure that typechecking of append and reverse are done independently
  const app = append;
  const rev = reverse;
  return is_null(xs)
     ? app(rev(acc), xs)
     : v === head(xs)
     ? app(rev(acc), tail(xs))
     : $remove(v, tail(xs), pair(head(xs), acc));
}

function remove(v, xs) {
  return $remove(v, xs, null);
}

// Similar to remove, but removes all instances of v
// instead of just the first

function $remove_all(v, xs, acc) {
  // Ensure that typechecking of append and reverse are done independently
  const app = append;
  const rev = reverse;
  return is_null(xs)
     ? app(rev(acc), xs)
     : v === head(xs)
     ? $remove_all(v, tail(xs), acc)
     : $remove_all(v, tail(xs), pair(head(xs), acc));
}

function remove_all(v, xs) {
  return $remove_all(v, xs, null);
}

// filter returns the sublist of elements of the second argument
// (assumed to be a list), for which the given predicate function
// returns true.

function $filter(pred, xs, acc) {
  return is_null(xs)
  ? reverse(acc)
  : pred(head(xs))
  ? $filter(pred, tail(xs), pair(head(xs), acc))
  : $filter(pred, tail(xs), acc);
}

function filter(pred, xs) {
  return $filter(pred, xs, null);
}

// enumerates numbers starting from start, assumed to be a number,
// using a step size of 1, until the number exceeds end, assumed
// to be a number

function $enum_list(start, end, acc) {
  // Ensure that typechecking of reverse are done independently
  const rev = reverse;
  return start > end
     ? rev(acc)
     : $enum_list(start + 1, end, pair(start, acc));
}

function enum_list(start, end) {
  return $enum_list(start, end, null);
}

// Returns the item in xs (assumed to be a list) at index n,
// assumed to be a nonnegative integer.
// Note: the first item is at position 0

function list_ref(xs, n) {
  return n === 0
     ? head(xs)
     : list_ref(tail(xs), n - 1);
}

// accumulate applies an operation op (assumed to be a binary function)
// to elements of sequence (assumed to be a list) in a right-to-left order.
// first apply op to the last element and initial, resulting in r1, then to
// the  second-last element and r1, resulting in r2, etc, and finally
// to the first element and r_n-1, where n is the length of the
// list.
// accumulate(op, zero, list(1, 2, 3)) results in
// op(1, op(2, op(3, zero)))

function $accumulate(f, initial, xs, cont) {
  return is_null(xs)
       ? cont(initial)
       : $accumulate(f, initial, tail(xs), x => cont(f(head(xs), x)));
}

function accumulate(f, initial, xs) {
  return $accumulate(f, initial, xs, x => x);
}


// Supporting streams in the Scheme style, following
// "stream discipline"

// is_stream recurses down the stream and checks that it ends with the
// empty list null

function is_stream(xs) {
  return is_null(xs) ||
  (is_pair(xs) &&
  is_function(tail(xs)) &&
  arity(tail(xs)) === 0 &&
  is_stream(stream_tail(xs)));
}

// A stream is either null or a pair whose tail is
// a nullary function that returns a stream.

function list_to_stream(xs) {
  return is_null(xs)
  ? null
  : pair(head(xs),
    () => list_to_stream(tail(xs)));
}

// stream_to_list transforms a given stream to a list
// Lazy? No: stream_to_list needs to force the whole stream
function stream_to_list(xs) {
  return is_null(xs)
  ? null
  : pair(head(xs), stream_to_list(stream_tail(xs)));
}

// stream_length returns the length of a given argument stream
// throws an exception if the argument is not a stream
// Lazy? No: The function needs to explore the whole stream
function stream_length(xs) {
  return is_null(xs)
  ? 0
  : 1 + stream_length(stream_tail(xs));
}

// stream_map applies first arg f to the elements of the second
// argument, assumed to be a stream.
// f is applied element-by-element:
// stream_map(f,list_to_stream(list(1,2)) results in
// the same as list_to_stream(list(f(1),f(2)))
// stream_map throws an exception if the second argument is not a
// stream, and if the second argument is a nonempty stream and the
// first argument is not a function.
// Lazy? Yes: The argument stream is only explored as forced by
//      the result stream.
function stream_map(f, s) {
  return is_null(s)
  ? null
  : pair(f(head(s)),
    () => stream_map(f, stream_tail(s)));
}

// build_stream takes a function fun as first argument, 
// and a nonnegative integer n as second argument,
// build_stream returns a stream of n elements, that results from
// applying fun to the numbers from 0 to n-1.
// Lazy? Yes: The result stream forces the applications of fun
//      for the next element
function build_stream(fun, n) {
  function build(i) {
  return i >= n
    ? null
    : pair(fun(i),
    () => build(i + 1));
  }
  return build(0);
}

// stream_for_each applies first arg fun to the elements of the stream
// passed as second argument. fun is applied element-by-element:
// for_each(fun,list_to_stream(list(1, 2,null))) results in the calls fun(1)
// and fun(2).
// stream_for_each returns true.
// stream_for_each throws an exception if the second argument is not a
// stream, and if the second argument is a nonempty stream and the
// first argument is not a function.
// Lazy? No: stream_for_each forces the exploration of the entire stream
function stream_for_each(fun, xs) {
  if (is_null(xs)) {
  return true;
  } else {
  fun(head(xs));
  return stream_for_each(fun, stream_tail(xs));
  }
}

// stream_reverse reverses the argument stream
// stream_reverse throws an exception if the argument is not a stream.
// Lazy? No: stream_reverse forces the exploration of the entire stream
function stream_reverse(xs) {
  function rev(original, reversed) {
  return is_null(original)
    ? reversed
    : rev(stream_tail(original),
    pair(head(original), () => reversed));
  }
  return rev(xs, null);
}

// stream_append appends first argument stream and second argument stream.
// In the result, null at the end of the first argument stream
// is replaced by the second argument stream
// stream_append throws an exception if the first argument is not a
// stream.
// Lazy? Yes: the result stream forces the actual append operation
function stream_append(xs, ys) {
  return is_null(xs)
  ? ys
  : pair(head(xs),
    () => stream_append(stream_tail(xs), ys));
}

// stream_member looks for a given first-argument element in a given
// second argument stream. It returns the first postfix substream
// that starts with the given element. It returns null if the
// element does not occur in the stream
// Lazy? Sort-of: stream_member forces the stream only until the element is found.
function stream_member(x, s) {
  return is_null(s)
  ? null
  : head(s) === x
    ? s
    : stream_member(x, stream_tail(s));
}

// stream_remove removes the first occurrence of a given first-argument element
// in a given second-argument list. Returns the original list
// if there is no occurrence.
// Lazy? Yes: the result stream forces the construction of each next element
function stream_remove(v, xs) {
  return is_null(xs)
  ? null
  : v === head(xs)
    ? stream_tail(xs)
    : pair(head(xs),
    () => stream_remove(v, stream_tail(xs)));
}

// stream_remove_all removes all instances of v instead of just the first.
// Lazy? Yes: the result stream forces the construction of each next element
function stream_remove_all(v, xs) {
  return is_null(xs)
  ? null
  : v === head(xs)
    ? stream_remove_all(v, stream_tail(xs))
    : pair(head(xs), () => stream_remove_all(v, stream_tail(xs)));
}

// filter returns the substream of elements of given stream s
// for which the given predicate function p returns true.
// Lazy? Yes: The result stream forces the construction of
//      each next element. Of course, the construction
//      of the next element needs to go down the stream
//      until an element is found for which p holds.
function stream_filter(p, s) {
  return is_null(s)
  ? null
  : p(head(s))
    ? pair(head(s),
    () => stream_filter(p, stream_tail(s)))
    : stream_filter(p, stream_tail(s));
}

// enumerates numbers starting from start,
// using a step size of 1, until the number
// exceeds end.
// Lazy? Yes: The result stream forces the construction of
//      each next element
function enum_stream(start, end) {
  return start > end
  ? null
  : pair(start,
    () => enum_stream(start + 1, end));
}

// integers_from constructs an infinite stream of integers
// starting at a given number n
// Lazy? Yes: The result stream forces the construction of
//      each next element
function integers_from(n) {
  return pair(n,
  () => integers_from(n + 1));
}

// eval_stream constructs the list of the first n elements
// of a given stream s
// Lazy? Sort-of: eval_stream only forces the computation of
//        the first n elements, and leaves the rest of
//        the stream untouched.
function eval_stream(s, n) {
  function es(s, n) {
    return n === 1 
         ? list(head(s))
         : pair(head(s), 
            es(stream_tail(s), n - 1));
  }
  return n === 0 
       ? null
       : es(s, n);
}

// Returns the item in stream s at index n (the first item is at position 0)
// Lazy? Sort-of: stream_ref only forces the computation of
//        the first n elements, and leaves the rest of
//        the stream untouched.
function stream_ref(s, n) {
  return n === 0
  ? head(s)
  : stream_ref(stream_tail(s), n - 1);
}
`
parse_into_json(prelude)
*/
