README
PA4c: The Semantic Analyzer
Students: Bashyam Srinidhi & Micheal Kovar
------------------------------
PA4 c
-------------------------------

Design decisions:

The Cool AST file from the PA3 parser is read using few reader routines and converted to its Ocaml type representation consisting of list of cool classes(class,features,etc). The Cool base classes like Bool, IO, etc. are added to the list of user classes. 

The list of classes is iterated over and each class is type checked for inheritance related inconsistencies in class, method, and attribute.

After the completion of type checks, the list of classes is again iterated over to emit class map as per the required format.

In this version, the list of type-checks performed on the Cool AST file is as follows:

1: Inherits undeclared class
2: Inherits non-inheritable classes like String, Bool, and Int
3: Object class's method illegal overrides like abort(), type_name(), and copy()
4: Duplicate class declaration 
5: Duplicate method declaration 
6: No Main class
7: No main method
8: Illegal method redefines - different formal size
9: Illegal method redefines - different formal types
10: Redefining classes like Int, String, etc.


List of files:
  1: readme.txt: this README file
  2: Source files: semantic.ml

Instructions to run the program:
  1: Run the Makefile: 
  	Commands:  
    To clean: make clean
    To build: make   
    Result: Creates semantic.exe, and semantic.o

  2: Test the analyzer: semantic.exe
  	Command:  
    $ cool --parse file.cl
    $ semantic.exe file.cl-ast

References:
1: Westley Weimer's - Video Guide - PA4c - OCaml at https://www.youtube.com/watch?v=Wa9zMygcv_M