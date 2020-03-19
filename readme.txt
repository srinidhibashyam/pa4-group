Students: 
  Thân Việt Cường, 
  Bashyam Srinidhi,
  Micheal Kovar,
  Alex Malyshev
------------------------------
PA4 - Semantic Analyzer
-------------------------------

Usage:
  You can compile the program with the following command:
    make
  This will generate a checker-pa4.exe file. This can be run using:
    ./checker-pa4 <input Cool ast file>
  You can also run the program with a Cool file using the following command from the Makefile:
    make test <input Cool file>
  Lastly, you can run the above command against the reference compiler by doing:
    make diff <input Cool file>

Design decisions:

Our program follows the general process of reading in the Cool AST and converting it into our internal representation, going through the AST and ensuring that there are no declaration errors, then going through the AST and type checking everything, and finally outputing our new AST with type annotations. 

Our code for reading in the AST and storing it is fairly simple and follows the normal pattern we have for the lexing and parsing code seen before.

For our code to check the declaration of classes and methods for errors we first start by parsing the AST again to store parent-child relationships, which is determined using a simple topological sort, as well as what methods and attributes each class has. After we setup this data we then check the declaration of each class to ensure that things like invalid class names or class redeclations do not occur. We then go on to populate the object environment for each class with their attributes. After populating a class's attributes, we the move onto the next stage.

After we populate a class's attributes and have ensured that the declaration of the class itself is fine, we go on to type check each method. For the type checking in here we recursively call our expression type checking method and make use of the method, object, and class (just stores the name of the class) environments to perform all of the nessacary type checking for each expression type. The only notable type checking tricks that we used is that all three of the Dispatch expressions use the same core helper method to check their arguments against the formal definitions and to check that the given method is present. Another notable expression in type checking is the Let expression. The main reason that the Let expression is interesting is that we had to populate a new object environment based upon the old object environment with all of the new variables that were present, so that the expressions after the Let expression were informed of the new varibles declared in it.

After we complete all of the type checking we follow the same general strategy used for outputing tokens in PA2 and PA3 to output our completed type-checked AST. 

We believe that our Semantic Analyzer is currently feature complete (past some cleanup and optimaization) and should fufill all of the functional requirements, based upon our testing with our own test cases and the instructor provided ones.