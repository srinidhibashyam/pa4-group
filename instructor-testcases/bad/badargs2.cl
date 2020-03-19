class Main { main() : Object {0}; };

class A {
      bar() : Object {
	    (new B).foo(self)
      };
};

class B inherits A {
      foo(b:Object, x:Int) : String { "moo" };
};


