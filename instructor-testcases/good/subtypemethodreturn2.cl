class A {
    foo() : A {
       let x : A in {
          x <- new A;
          x.foo();
       }
    };
  
};

class B inherits A {

    foo() : A {
       new B
    };
  
};

class Main { main () : Int { 6 }; };
