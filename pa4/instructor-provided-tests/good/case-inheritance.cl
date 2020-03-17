class B {};
class C inherits B {};
class D inherits B {};
class E inherits B {};

class Hello {
   foo: Int;
   bar: String;
   
   how(a:String, b:Int) : B {
     case new Object of
       a: C => new C;
       b: D => new D;
       c: E => new E;
     esac
   };
   
};

      
	
class Main { main () : Int { 6 }; };
