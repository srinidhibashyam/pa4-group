Class Main inherits Object {
	 c : Int <- 5;
	 s : Int<- 2;

	main () : Object {
	 	3
	 };

	method3(i: Int, j: Int) : String {
	 	"3"
	};
};

Class Second inherits Main {
	
	method4(i: Int, j: String) : String {
	 	"3"
	};
};


Class Third inherits Second {
	
	type_name(i: Int) : String {
	 	"3"
	};
};