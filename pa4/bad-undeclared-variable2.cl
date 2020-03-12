class Main {
	main() : Object { 5 };
	y : List <- s;
	x : List <- y;
};

class List {
	list(s: String, i: Int) : Object { 5 };	
};

class Nil inherits List {
	nil(s: String, i: Int) : Object { 5 };	
};