(* This tests what happens when try to static dispatch on a expression that will produce
	the required type, where the type is freshly constructed. *)
Class Main {
	main(): Object {
		let test: String <- (new SpecialType)
				.fire("Blue") in
		"test"
	};
};
Class SpecialType {
	fire(color: String): String {
		"and wind" 
	};
};