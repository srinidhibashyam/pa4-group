(* This tests what happens if the wrong number of parameters are provided when performing a dynamic dispatch *)
Class Main {
	main(): Object {
		let exists: String <- "test" in
		let thingy_object: Thingy <- new Thingy in
		let test: String <- thingy_object.our_method() in
		"test"
	};
};
Class Thingy {
	our_method(string_paramter: String): String {
		string_paramter
	};
};