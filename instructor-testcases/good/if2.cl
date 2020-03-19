class A { };
class B inherits A { };
class C inherits A { };
class D inherits C { };

--test LUB
class Main inherits IO{
 x: Int;
 main(): Object {{ 
		   if (new A = new A) then new B else new D fi;
                }};
 foo(): String {"test"};
};
