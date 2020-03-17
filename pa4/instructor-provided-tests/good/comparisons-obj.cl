class A {
   foo(a:A, b:B, c:C):Bool { 
        not 
        (c <= b)
   } ;
   bar(a:A, b:B):Bool { 
    let c : SELF_TYPE <- self in
        not 
        (c <= b)
   } ;
};
class B inherits A {
  bbb : Int ; 
};
class C inherits A {
  ccc : Int ; 
};
class Main { main () : Int { 6 }; };

