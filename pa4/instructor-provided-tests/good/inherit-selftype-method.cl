class Main inherits IO {
  main() : Object {
   true
  } ;
   test() : SELF_TYPE {
      self
   };
} ;

class Bob inherits Main{
   toddler : Int;
   test() : SELF_TYPE {
      self
   };
};
