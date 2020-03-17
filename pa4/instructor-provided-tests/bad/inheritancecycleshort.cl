class Main { main() : Int {0 }; };

class A inherits B{
  foo(x:Int) : Object {self};
};

class B inherits A {};
