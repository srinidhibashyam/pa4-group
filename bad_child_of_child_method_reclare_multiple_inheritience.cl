Class Main inherits IO {
	main(): String {
		"test"	
	};
};

Class Thing {
	apple(): String {
		"An Orange"
	};
};

Class Bar inherits Thing {

};

Class Food inherits Bar {
	apple(): Int {
		"The Apple"
	};
};

Class Drink inherits Bar {
};
