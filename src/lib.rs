#![feature(struct_variant)]
#![allow(dead_code)]
#![allow(uppercase_variables)]
#![allow(unused_variable)]

use std::collections::hashmap::HashMap;


// some kinda source-language

#[deriving(Eq,PartialEq,Ord,PartialOrd)]
enum Expr {
    EVar{x:String},            // variable
    ELit{v:Lit},            // literal
    EApp{f:Exp, e:Exp},        // application, f(e)
    EAbs{f:String, e:Exp},    // abstraction
    ELet{x:String, e:Exp, e2:Exp}
}
type Exp = Box<Expr>;

#[deriving(Eq,PartialEq,Ord,PartialOrd)]
enum Lit {
    LInt(i32),
    LBool(bool)
}


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
#[deriving(Clone)]
pub enum Opcode {
    HALT     {unused:bool},
    REFER    {var: String, k: Code},
    CONSTANT {obj: Obj, k: Code},
    CLOSE    {vars: Vec<String>, body: Code, k: Code},
    TEST     {kthen: Code, kelse: Code},
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

#[deriving(Clone)]
enum Obj {
    OBool(bool),
    OInt(i32),
    OFloat(f32),
    OStr(String),
    OClosure(Closure)
}

///  Scope is a dynamic environment: a set of bindings, implemented
/// as a map from variable names (as Str, representing symbols)
/// to runtime value (Obj? if not typing; or AxonVal derivatives)
#[deriving(Clone)]
struct Scope {
    parent: Option<Box<Scope>>,  // link to enclosing scope
    local: HashMap<String, Obj>// local vars (conceptually includes fn params)
}
impl Scope
{
    fn make(parent:Option<Box<Scope>>) -> Scope {
        Scope { parent:parent, local:HashMap::new() }
    }

    fn get(&self, var: &String) -> Option<Obj> {
        None
    }
    fn set(&self, var: &String, val: Obj) {

    }
    fn extend(&self, vars: Vec<String>, vals: Vec<Obj>) -> Scope {

        let extended = Scope::make(Some(box self.clone()));
        //...
        extended
    }

//    @Operator Obj? get(Str var) {
//        if (local.containsKey(var)) return local[var]
//        if (parent != null) return parent.get(var)
//        return null    // TODO throw, to require def-before-use?
//    }
//
//    /// this is "set!", the scheme function that alters a binding's value.
//    /// it is required that the binding exist, whether in current scope or
//    /// in an enclosing scope doesn't matter.
//    @Operator This set(Str var, Obj? val) {
//        if (local.containsKey(var)) {
//            local[var] = val;
//            return this
//        }
//        if (parent != null) {
//            return parent.set(var, val)
//        }
//        throw Err("$var not defined")
//        //return this
//    }
//
//    /// define a variable binding in current scope.
//    This def(Str var, Obj? val) {
//        if (local.containsKey(var)) throw Err("'$var' already defined!")
//        //local[var] = val
//        return this
//    }
//
//    /// lookup a var starting in local scope, and return the scope that
//    /// contains that var definition, or null if not found
//    Scope? lookup(Str var) {
//        if (local.containsKey(var)) return this
//        if (parent != null) return parent.lookup(var)
//        return null
//    }
//
//    /// extend this environment with a new scope, and add vars/vals to it
//    This extend(Str[]? vars, Obj?[]? vals) {
//        scope := make(this) // create a new scope w/ this as its parent
//        vars?.each |var, i| {
//            val := (vals != null && vals.size > i) ? vals[i] : null
//            scope.def(var, val)
//        }
//        return scope
//    }
//
//    static Scope makeEmptyRoot() { Scope() }
//
//    static Scope makeWithFantomImplementedAxonFuncs(Scope? parent := null) {
//        scope := Scope(parent)
//        funcs := Introspect.allAxonFanFuncs
//        funcs.each |func| {
//            qname := func.qname
//            primfunc := func //|Obj?[] args->Obj?| { func.call(args) }
//            scope.def(qname, primfunc)
//        }
//        return scope
//    }
}


/// Frame is the dynamic (runtime) representation of a function
/// execution.  It captures the caller-frame and return-address,
/// so the complete dynamic context can be traced by walking back
/// thru the caller links; a bindings context (which pushes and
/// pops local scopes for variable definitions); the AST for the
/// function's code; and the instruction-pointer which indicates
/// the current point of execution in the code.
#[deriving(Clone)]
struct Frame {
    // *X*  when this frame returns, exec. resumes from caller.code[ret] (ie. ret is index into code of caller)
    ret: Code,
    // *E*  parms,locals
    bindings: Scope,
    // *R*  accumulator of arg vals, to be combined w/ param names in extending env
    valueRib: Vec<Obj>,
    // *S*  previous frame
    caller: Option<Box<Frame>>,
    //code: Code //belongs in Frame (there's a frame for every lambda definition)
}
impl Frame {
    fn make(env:Scope, rib: Vec<Obj>, ret: Code, caller: Option<Box<Frame>>)
    -> Frame
    {
        Frame { bindings:env, valueRib:rib, ret:ret, caller:caller }
    }
}

/// closure captures the environment where it was created; when called,
/// it binds its params to actual-arg values (in left-to-right listed order)
/// and extends its environment with those bindings, and executes its
/// body with that extended environment.
#[deriving(Clone)]
struct Closure {
    // names of parameters to be applied to closure
    params: Vec<String>,
    // static environment (lexical scope, captures scopes enclosing definition)
    env: Scope,
    // code implementing body of closure.
    body: Code
}
impl Closure {
    fn make(params: Vec<String>, env: Scope, body: Code) -> Closure {
        Closure { params:params, env:env, body:body }
    }
}


/// The VM below is fundamentally a state machine, of course, and
/// the five registers capture the entire current-state of that machine.
struct VMState
{
    /////////////////////////////////////////////////////////////////////
    // Machine Registers

    // accumulator (most-recently-evaluated-expression value)
    A: Obj,

    // next instruction to be executed (source is compiled into a directed-graph of Opcode)
    X: Code,

    // current (lexical) environment (bindings map, context, ...)
    E: Scope,

    // value rib (accumulator for values of arguments to a fn application)
    R: Vec<Obj>,

    // control stack (ptr to top call frame; frames have link to prev frame)
    S: Frame
}
impl VMState {
    fn make(a:Obj, x:Code, e:Scope, r:Vec<Obj>, s:Frame) -> VMState {
        VMState { A:a, X:x, E:e, R:r, S:s }
    }

    fn accumulator(&self) -> &Obj         { &self.A }
    fn program(&self)     -> &Code        { &self.X }
    fn environment(&self) -> &Scope       { &self.E }
    fn arguments(&self)   -> &Vec<Obj>    { &self.R }
    fn stackframe(&self)  -> &Frame       { &self.S }
}


///////////////////////////////////////////////////////////////////////////////
// axon machine: definition and implementation of virtual machine for
// scheme-like semantics
//
// let code be an in-memory graph (DAG) of instructions, where the entry-point
// to a sub-program is a single instruction.
// let instruction be a composition of:
// - opcode, an enumeration identifying its type
// - operands, compile-time constant arguments to the instruction
// - links, 0, 1, or 2 links to successor-instructions.
// note the single exception: the 'nuate instruction takes a Frame
// argument.  This means that (as written), compiled code that
// includes call/cc won't be serializable, because the live control-stack
// frames aren't serializable.  This only matters if we start thinking
// about serializing execution-in-process code and moving it to a
// different machine for resumption.
// ...
// ...
// A VM with 5 registers, 12 primitive instructions, and
// 3 basic data structures:
// - Frame captures a call-frame and maintains a dynamic control stack
// - Scope manages bindings of variables to values in lexically nested scopes
// - Closure binds parameters to actual args and executes code

struct Machine {
    state: VMState
}
impl Machine
{
    fn init(state: VMState) -> Machine { Machine { state:state } }

    fn step(&mut self) -> Option<Obj> {
        let (mut A,mut X,mut E,mut R,mut S) = (
            self.state.A.clone(),
            self.state.X.clone(),
            self.state.E.clone(),
            self.state.R.clone(),
            self.state.S.clone()
        );

        match *X {
//  case HALT    : return // and return A
HALT {..} => {},
//  case REFER   : I: REFER   ; A = E[I.var]; X = I.next
REFER {var:ref var, k:ref k} => {
    A = E.get(var).expect("yowza");
    //X = *k;
},
//  case CONSTANT: I: CONSTANT; A = I.obj; X = I.next
CONSTANT {obj:ref obj, k:ref k} => {
    A = obj.clone();
    //X = *k;
},
//  case CLOSE   : I: CLOSE   ; A = AxonClosure(I.vars, E, I.body); X = I.next
CLOSE {vars:ref vars, body:ref body, k:ref k} => {
    let a = Closure { params:vars.clone(), env:E.clone(), body:body.clone() };
    A = OClosure(a);
    //X = *k;
},
//  case TEST    : I: TEST    ; X = (A == true) ? I.thenc : I.elsec
TEST {kthen:ref kthen, kelse:ref kelse} => {
    let k = //if A == true { kthen } else { kelse };
        match A {
            OBool(true) => { kthen },
            //OBool(false) => { kelse },
            _ => { kelse }
        };
    //X = k;
},
//  case ASSIGN  : I: ASSIGN  ; E[I.var] = A; X = I.next
ASSIGN {var:ref var, k:ref k} => {
    E.set(var, A.clone());
    //X = k;
},
//  case CONTI   : I: CONTI   ; A = capture_cc(S); X = I.next
CONTI {k:ref k} => {
    let a = Machine::capture_cc(&S);
    A = OClosure(a);
    //X = k;
},
//  case NUATE   : I: NUATE   ; A = E[I.var]; X = RETURN;
NUATE {s:ref s, var:ref var} => {
    A = E.get(var).expect("yup");
    let k = box RETURN {unused:true};
    //X = k;
},
//  case FRAME   : I: FRAME   ; S = Frame(E, R, I.ret, S); R = [,]; X = I.next
FRAME {k:ref k, ret:ref ret} => {
    let s = Frame {
        ret: ret.clone(),
        bindings: E.clone(),
        valueRib: R.clone(),
        caller:Some(box S.clone())
    };
    S = s;
    R = vec!();
    // X = k;
},
//  case ARGUMENT: I: ARGUMENT; R.add(A); X = I.next
ARGUMENT {k:ref k} => {
    R.push(A.clone());
    // X = k;

},
//  case APPLY   : I: APPLY   ; closure := (AxonClosure) A
//                              vals := R
//                              vars := closure.params
//                              E = closure.env.extend(vars, vals)
//                              R = [,]
//                              X = closure.body
APPLY {..} => {
    let closure = match A {
        OClosure(ref clo) => { clo.clone() },
        _ => fail!("no clo")
    };
    let vals = R.clone();
    let vars = closure.params.clone();
    E = closure.env.extend(vars, vals);
    R = vec!();
    X = closure.body;
},
//  case INVOKE  : I: INVOKE  ; obj := A
//                              // meth := obj.typeof.slot[I.method]
//                              args := (Obj?[]) R
//                              // A = meth.invoke(obj, args)
//                              R = [,]
//                              X = I.next
INVOKE {method:ref method, k:ref code} => {
    //TODO
},
//  case RETURN  : I: RETURN  ; X = S.ret; E = S.bindings; R = S.valueRib; S = S.caller
RETURN {..} => {
    //TODO
},
        }

        let retval = A.clone();
        self.state = VMState {
            A:A,
            X:X,
            E:E,
            R:R,
            S:S
        };
        //notifyObservers
        Some(retval)
    }
    fn done(&self) -> bool {
        match *self.state.X {
            HALT {..} => { true },
            _ => { false }
        }
    }
    fn run(&mut self) -> Option<Obj> {
        loop {
            let retval = self.step();
            if self.done() {
                return retval;
            }
        }
    }


  /// a continuation is a closure that in addition has access to the frame
  /// in which it was created (where call/cc was called).
  /// the body of a continuation closure, when executed, restores the
  /// saved frame (which includes its calling frames)  (pg. 50)
  ///
  /// a continuation generates a closure that captures
  /// the current control stack; the body of the generated
  /// closure is an instruction that will restore the
  /// captured stack.
  fn capture_cc(s: &Frame) -> Closure {
    let v = "__V__";  // param name used by the 'nuate instruction
                      // to access the first (only) argument
                      // to the continuation.
                      // this comes from some source like  call/cc exp
    let body = box NUATE{ s:s.clone(), var:v.to_string() };
    let env  = Scope::make(None);
    let vars = vec!(v.to_string());
    Closure { params:vars, env:env, body:body }
  }

}