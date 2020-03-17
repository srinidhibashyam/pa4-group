

class A {
  x:String;
  foo(x:A) : A {x};
};

class B inherits A {
  food(x:A,y:A) : A {self};
};
