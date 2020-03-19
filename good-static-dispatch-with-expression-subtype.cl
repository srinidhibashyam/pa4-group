(* This tests what happens when try to static dispatch on a expression that will produce
	the required type and the caller type is a subtype of the required type. *)
Class Main {
	main(): Object {
		let test: String <- (new SpecialFireType)
				@SpecialType.fire() in
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