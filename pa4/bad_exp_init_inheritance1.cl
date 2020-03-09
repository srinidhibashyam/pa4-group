class Main {
	main() : Object { 5 };
	x : String <- "5";
};

class Child inherits Main {
	s : Int <- x;
};