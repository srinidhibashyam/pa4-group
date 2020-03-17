(* This tests what happens when a self dispatch is issued with an incorrect 
	arugment type, given the expected paramter type. *)

Class Main {
	main(): String {
		let test: String <- extremely_complex_method(1) in
		"Whales"
	};
	extremely_complex_method(key_to_the_city: String): String {
		"nope"
	};
};