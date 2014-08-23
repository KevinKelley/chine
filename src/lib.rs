#![feature(struct_variant)]
#![allow(dead_code)]

///////////////////////////////////////////////////////////////////////////////
// Opcode
//   these are a dozen primitive instructions that implement scheme-like
// semantics.  This is applicative-order lambda calculus with lexically-scoped
// environments: everything reduces to function calls where arguments are
// evaluated before application of function to arguments; variables are
// bound in their static (lexical) scope like Scheme, not in their dynamic
// (runtime) scope like earlier Lisps.
// Execution model is heap-based and there is support for call-with-current-continuation
// so exception semantics can be implemented easily in terms of call/cc.
//
enum Opcode {
  HALT     {unused:bool},
  REFER    {var: String, k: Code},
  CONSTANT {obj: Obj, k: Code},
  CLOSE    {vars: Vec<String>, body: Code, k: Code},
  TEST     {thenc: Code, elsec: Code},
  ASSIGN   {var: String, k: Code},
  CONTI    {k: Code},
  NUATE    {s: Frame, var: String},
  FRAME    {k: Code, ret: Code},
  ARGUMENT {k: Code},
  APPLY    {unused:bool},
  INVOKE   {method: String, k: Code},
  RETURN   {unused:bool},
}
type Code = Box<Opcode>;

enum Obj {
	Int(i32),
	Float(f32),
	Str(String)
}

struct Frame {
	code: Code
}