(* This tests what happens when try use SELF_TYPE as the required type in
	static dispatch. This should fail, since it's illegal. *)
Class Main {
	main(): Object {
		let exists: String <- "test" in
		let test: String <- self
				@SELF_TYPE.fire() in
		"test"
	};
};