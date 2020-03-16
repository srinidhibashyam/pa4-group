(* This tests what happens when try to static dispatch on a expression that will not resolve
	to the contracted type. *)
Class Main {
	main(): Object {
		let exists: String <- "test" in
		let test: String <- (1 + 2)
				@SpecialType.fire() in
		"test"
	};
};
Class SpecialType {
	fire(): String {
		"and wind" 
	};
};