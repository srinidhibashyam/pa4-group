(* This tests what happens when the required type for a static dispatch is not a valid
	type *)
Class Main {
	main(): Object {
		let exists: String <- "test" in
		let test: String <- ("test")
				@DoesNotExist.fire() in
		"test"
	};
};