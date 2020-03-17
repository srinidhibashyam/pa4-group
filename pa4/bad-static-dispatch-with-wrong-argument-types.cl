(* This tests what happens when a static dispatch is issued with an incorrect 
	arugment type, given the expected paramter type. *)

Class Main {
	main(): String {
		let test: String <- (new OtherClass)@OtherClass.extremely_complex_method(1) in
		"Whales"
	};
};
Class OtherClass {
	extremely_complex_method(key_to_the_city: String): String {
		"nope"
	};
};