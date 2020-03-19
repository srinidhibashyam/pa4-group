(* This tests what happens when try to have a class inherit from another class which
	inherits from the another class, so three layers of inheritence. *)
Class Main {
	main(): Object {
		let test: GrandmaClass <- new BabyClass in
		"test"
	};
};
Class GrandmaClass {
};
Class MamaClass inherits GrandmaClass {
};
Class BabyClass inherits MamaClass {
};