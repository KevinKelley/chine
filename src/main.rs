
fn main() {}


//enum Values {
//    VSymbol(String),
//    VNumber(i64),
//    VString(String),
//    VPair(Value, Value)
//};
//type Value = Box<Values>;


//enum Syn {
//    Sym(String),
//    Lam(Exp, Exp),  // (λ var, body)
//    App(Exp, Exp)   // f(e)
//}
//fn eval(sx: CoreLanguage, env: Scope) -> Obj {
//    match sx {
//        Sym(ref sym) => env.get(sym).expect(""),
//        Lam(var, body) => { ONil },
//        App(f, e) => { ONil }
//    }
//}

// some kinda source-language

//#[deriving(Eq,PartialEq,Ord,PartialOrd)]
//enum Expr {
//    EVar{x:String},            // variable/symbol
//    ELit{v:Lit},               // literal
//    EApp{f:Exp, e:Exp},        // application, (f e) or f(e)
//    EAbs{f:String, e:Exp},     // abstraction
//    ELet{x:String, e:Exp, e2:Exp}
//}
//type Exp = Box<Expr>;
//
//#[deriving(Eq,PartialEq,Ord,PartialOrd)]
//enum Lit {
//    LInt(i32),
//    LBool(bool)
//}
