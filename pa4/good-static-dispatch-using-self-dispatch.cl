(* This tests what happens when try to static dispatch on a expression that will produce
	the required type, from a value returned by a self method. *)
Class Main {
	main(): Object {
		let test: String <- (do_some_weird_stuff())
				@SpecialType.fire() in
		"test"
	};
	do_some_weird_stuff(): SpecialType {
		new SpecialType	
	};
};
Class SpecialType {
	fire(): String {
		"and wind" 
	};
};