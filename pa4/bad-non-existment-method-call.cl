(* This tests what happens when we call a non-existant method in a dynamic dispatch *)
Class Main {
	main(): Object {
		let exists: String <- "test" in
		let test: String <- exists.not_a_method() in
		"test"
	};
};