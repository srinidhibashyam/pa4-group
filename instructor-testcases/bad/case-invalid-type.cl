(* mark berry, Tue Mar 25 19:13:55 EDT 2008 *)
class Main inherits IO {
   main() : Int {
      case 5 of
         i : Int           => { out_string("i"); 0;};
         j : Pink_Elephant => { out_string("j"); 0;};
      esac
   };
};
