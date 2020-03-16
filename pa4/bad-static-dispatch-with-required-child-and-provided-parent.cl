(* This tests what happens when try to static dispatch on a expression that will produce
	the parent of the required type instead of the required type. *)
Class Main {
	main(): Object {
		let test: String <- (new SpecialType)
				@SpecialFireType.fire() in
		"test"
	};
};
Class SpecialType {
	fire(): String {
		"and wind" 
	};
};
Class SpecialFireType inherits SpecialType {
};