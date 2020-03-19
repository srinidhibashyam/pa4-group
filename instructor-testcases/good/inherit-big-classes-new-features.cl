class Hello {
   foo: Int;
   bar: String <- "7";
   
   moo() : Int {
      if isvoid 6 then 3 else 6 fi
   };
   
   cow(a:Int) : Object {
      moo()
   };
   
   how(a:String, b:Int) : Hoho {
   	  { self.cow(5); new Hoho; }
   };
   
   moo2(a:String, b:Int, c:Int, d:Int) : Hello {
      { foo; self; }
   };
   
};

class Hoho { foo: Int; };

class Goodbye {
  hello: Hello;
};

class HelloAgain inherits Hello {
   hel5675: IO;
   moo: SELF_TYPE;
  
  cow(a: Int) : Object {
    {	moo@Hello.how("Hello", 5); self; }
  };
  
  moo2(a:String, b:Int, c:Int, d:Int) : Hello {
     moo <- self
  };
  

};

class GoodbyeForGood inherits Goodbye {
   moo() : Int {
      if 7 < 6 then 12 else 6 + 8 fi
   };
   
   hoho() : Int {
     { hello.moo2("george", 1, 2, 3);
   	   5 * (6 + 7); }
   };
   
   

};


      
	
class Main { main () : Int { 6 }; };
