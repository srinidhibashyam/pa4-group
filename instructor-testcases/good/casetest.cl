class Main inherits IO{
 a:A;
 y:String;
 main(): Object {{ 
                  case a of b:B => new B; 
                            c:C => new C;
                            d:Int => new D;
                  esac;
                }};
};

class A {
};

class B inherits A {
};

class C inherits B {
};

class D inherits A {
};





