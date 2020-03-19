class A { x: Int; y: String; foo : Bool; };

class L inherits A { foo() : Int {x}; bar : Bool <- foo; };

class Main { main() : Int { (new L).foo() }; };
