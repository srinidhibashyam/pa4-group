class A { x: Int; y: String; void : Bool; };

class L inherits A { void() : Int {x}; bar : Bool <- void; };

class Main { main() : Int { (new L).void() }; };
