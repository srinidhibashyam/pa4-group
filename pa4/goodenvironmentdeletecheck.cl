class Main {
	main() : Object { 5 };
	y : Int <- 6;
	x : Int <- 
		let y : Int <- 5, 
			z : Int, 
			f : Bool <- true
		in y;
	z : Int <- y;
};