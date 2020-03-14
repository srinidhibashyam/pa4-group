(* PA-4 Semantic Anlyzer checkpoint *)
open Printf 

(* create a static type for Cool expression *)
type static_type = 
	| Class of string (*"Int" or "Object" *)
	| SELF_TYPE of string (*SELF_TYPE_c *)

(* Procedure to print these *)
let type_to_str t = match t with
	| Class(x) -> x (*"Int" or "Object" *)
	| SELF_TYPE(c) -> "SELF_TYPE" (*SELF_TYPE_c *)

(* Operations to be performed on these types *)
(* <= --- subtyping (Liskov Substitution Principle)*)
(* Int <= Object *)
(* String <= String *)
(* Dog <= Animal if Dog inherits Animal *)
let parent_map = Hashtbl.create 255;;

let rec is_subtype t1 t2 =    (*checking if t1 is subtype of t2*)
	match t1, t2 with
	| Class(x), Class(y) when x=y -> true  (*like String <= String*)
	| Class(x), Class("Object") -> true  (*this is always true in Cool*)
	| Class(x), Class(y) -> begin
		(*check the parent map*)
		try
			let parents = Hashtbl.find parent_map x in
			(List.mem y parents)
		with
		| _ -> false (* x is undefined- Int, String, Bool*)
		end
	| _, _ -> (*TODO: check the class note like for SELF_TYPE*) false   

exception LowerBoundFound of string;;

let rec lowest_upper_bound t1 t2 =
	if (is_subtype t1 t2) then 
		t2 
	else if (is_subtype t2 t1) then 
		t1 
	else begin
		try
			let x = type_to_str t1 in
			(* get parent maps of t1 and t2 *)
			let t1_parents = Hashtbl.find parent_map x in
			(* Iterate through t1_parents and find the lowest match in t2_parents*)
			List.iter(fun cls ->
				if (is_subtype t2 (Class cls)) then raise (LowerBoundFound cls) 
			) t1_parents;
			(Class "Object"); (* should not reach this; but for completion, return object*)	

		with
			| LowerBoundFound(bound) -> (Class bound)
			| _ -> (Class "Object")(* undefined - Int, String, Bool -> return object*)	
	end


(* mapping from object identifier (names) to types *)
type obj_env = (string, static_type) Hashtbl.t
let empty_obj_env () = Hashtbl.create 255


type class_method = string * string
(* Maps a (class_name, method_name) -> [..List of formal types, Method return type] *)
and method_env = (class_method, (static_type list)) Hashtbl.t
let empty_method_env () = Hashtbl.create 255


type cool_program = cool_class list
and line_number = string
and identifier = line_number * string
and cool_type = identifier
and cool_class = identifier * (identifier option) * (feature list)
and feature = 
	| Attribute of identifier * cool_type * (expression option)
	| Method of identifier * (formal list) * cool_type * expression
and formal = identifier * cool_type

(* Annotate the expression with its static type. 
 * Initial type is assumed to be absent/option, 
 * later we will discover it by typechecking. It will be Mutable.*)

and expression = {
	line_number: line_number;
	expression_type: expression_type;
	mutable static_type: static_type option; (*this can change *)
}
and expression_type = 
	| Plus of expression * expression
    | Minus of expression * expression
    | Times of expression * expression
    | Divide of expression * expression
    | Identifier of identifier
    | Integer of string
    | String of string
    | True 
    | False
    | If of expression * expression * expression
    | While of expression * expression
    | Assign of identifier * expression
    | LessThan of expression * expression
    | LessThanOrEq of expression * expression
    | Equals of expression * expression
    | Not of expression
    | Negate of expression
    | IsVoid of expression
    | New of identifier
    | Block of expression list
    | DynamicDispatch of expression * identifier * expression list
    | SelfDispatch of identifier * expression list
    | StaticDispatch of expression * cool_type * identifier * expression list
    | Let of (binding list) * expression
    | Case of expression * (case_element list)
and binding =
    | BindingNoInit of identifier * cool_type
    | BindingInit of identifier * cool_type * expression
and case_element = 
    | CaseElement of identifier * cool_type * expression


exception MethodRedefineError of string;;
exception AttributeRedefineError of string;;

let dummy_exp_body = {line_number= "0"; expression_type= String(""); static_type= None;}
let int_exp_body = {line_number= "0"; expression_type= Integer("0"); static_type= None;}
let string_exp_body = {line_number= "0"; expression_type= String(""); static_type= None;}
(* TODO:  length of self *)
let string_length_exp_body = {line_number = "0"; expression_type= Integer("99"); static_type= None;}

let int_features = [];;
let bool_features = [];;
let io_features = [ 
				Method(("0","out_string"), [(("0","x"),("0","String"))], ("0","SELF_TYPE"), dummy_exp_body);
				Method(("0","out_int"), [(("0","x"),("0","Int"))], ("0","SELF_TYPE"), dummy_exp_body);
				Method(("0","in_string"), [], ("0","String"), dummy_exp_body);
				Method(("0","in_int"), [], ("0","Int"), dummy_exp_body);
				];;
let obj_features = [ 
				Method(("0","abort"), [], ("0","Object"), dummy_exp_body);
				Method(("0","type_name"), [], ("0","String"), dummy_exp_body);
				Method(("0","copy"), [], ("0","SELF_TYPE"), dummy_exp_body);
				];;	
let string_features = [ 
				Method(("0","length"), [], ("0","Int"), string_length_exp_body);
				Method(("0","concat"), [(("0","x"),("0","String"))], ("0","String"), dummy_exp_body);
				Method(("0","substr"), [(("0","i"),("0","Int"));(("0","l"),("0","Int"))], ("0","String"), dummy_exp_body);
				];;	

(* Lists all parent classes of a given class in given ast list (including IO and Object) *)
let rec get_super_classes input_class_name ast_list visited = begin
	try
		let ((cloc,_),inherits,_) = List.find(fun ((_,cname),_,_)-> input_class_name = cname) ast_list in
		match inherits with
		| None -> 
			(*return object*) 
			let class_object = ((cloc, "Object"), None, obj_features) in
			[class_object];
		| Some(iloc, iname) -> 
			try
				(* Check for forbidden super classes *)
				if (List.mem iname ["Bool"; "Int"; "String"]) then begin
					printf "ERROR: %s: Type-Check: class %s inherits from forbidden class %s.\n" iloc input_class_name iname;
					exit 1 
				end ;

				(* Now find the super class *)
				let super_class = List.find(fun ((_,cname2),_,_)-> iname = cname2) ast_list in
				(* Update visited set with input class name*)
				let visited2 = 
					if (List.mem input_class_name visited) then visited
					else input_class_name:: visited
				in
				(* Inheritance cycle - check visited set*)
				if (List.mem iname visited2) then begin
					printf "ERROR: 0: Type-Check: inheritance cycle: %s\n" (String.concat " " visited2) ;
					exit 1
				end;

				(* Update visited set with super class name*)
				let visited3 = iname::visited2 in
				let super_list = get_super_classes iname ast_list visited3 in (* recursively find more super class here *)
				super_class :: super_list; (*append super class to the list and return*)	
			with
			| _ -> 
				(* Inheritance from Undeclared class *)
				match iname with
				| "IO" -> 
					let class_io = ((iloc, iname), None, io_features) in
					let class_object = ((iloc, "Object"), None, obj_features) in
					[class_io; class_object]; (* return IO and Object *)
				| "Object" -> 
					let class_object = ((cloc, "Object"), None, obj_features) in
					[class_object]; (* return Object*)
				| _ ->
					printf "ERROR: %s: Type-Check: Class %s Inherits from undefined class %s\n" 
						iloc input_class_name iname;
					exit 1
	with
	| _ -> failwith "cannot happen: class not found";

end ;;

(* Lists all methods in a given class (excludes inherited methods) *)
let get_own_methods input_class = begin
	let  ((cls_loc, class_name), _, features) = input_class in
	(* Only include Method features *)
	let method_names: string list ref = ref [] in
	let own_methods = List.filter (
		fun feature -> 
			match feature with
				| Method ((method_loc, method_name), _, _, _) -> 
					(* Check for duplicate method in the same class. *)
				if (List.mem method_name !method_names) then begin
					printf "ERROR: %s: Type-Check: class %s redefines method %s\n" method_loc class_name method_name ;
					exit 1
				end else
					(* Add this method to our method names list so that we can track it *)
					method_names := method_name :: !method_names
				;
				true
				| _        -> false
	) features in
	(* Check for a missing method main in class Main. *)
	if (class_name = "Main") && (not (List.mem "main" !method_names)) then begin
		printf "ERROR: 0: Type-Check: method main not found in class Main\n";
		exit 1
	end;	
	own_methods;
end ;;


(* Lists all attributes in a given class (excludes inherited attributes) *)
let get_own_attributes input_class = begin
	let  ((cls_loc, class_name), _, features) = input_class in
	(* Only include Method features *)
	let attribute_names: string list ref = ref [] in
	let own_attributes = List.filter (
		fun feature -> 
			match feature with
				| Attribute ((attribute_loc, attribute_name), _, _) -> 
					(* Check for duplicate method in the same class. *)
				if (List.mem attribute_name !attribute_names) then begin
					printf "ERROR: %s: Type-Check: class %s redefines attribute %s\n" 
						attribute_loc class_name attribute_name ;
					exit 1
				end else
					(* Add this method to our method names list so that we can track it *)
					attribute_names := attribute_name :: !attribute_names
				;
				true
				| _        -> false
	) features in
	own_attributes;
end ;;

let add_attribute_types o input_class = begin
	(* Add current class attributes to object environment*)	
	let own_attributes =  get_own_attributes input_class in
	List.iter(fun own_attr -> 
		match own_attr with
		| Attribute ((attr_loc, attr_name), (decl_loc, decl_type), exp) ->
			(*add attr_name to O -- add it to current scope *)
			Hashtbl.add o attr_name (Class decl_type);		
		| Method _ -> ();
	) own_attributes;
end;;



let add_method_types meth_env input_class = begin
	(* Add input_class methods to object environment*)	
	let own_methods =  get_own_methods input_class in
	let ((_, class_name), _, _) = input_class in
	List.iter(fun own_feat -> 
		match own_feat with
		|  Method ((_, m_name), formals, (_, return_type),_) -> 
			let formal_types = List.map (fun (_, (_, fname)) -> Class fname) formals in
			let key: string * string = (class_name, m_name) in
			let value: static_type list= formal_types  @  [Class return_type] in
			Hashtbl.add meth_env key value
		| Attribute _ -> ();
	) own_methods;
end;;

(* finds a given method in the class - returns option (feature) *)
let find_method method_name in_class = begin
	let own_methods = get_own_methods in_class in
	try 
		let result = List.find(fun feature ->
			match feature with
			| Attribute _ ->  false	
			| Method ((_, m_name), formals, (_, return_type),_) -> 
				method_name = m_name
		) own_methods in	
		Some(result);
	with
	| _ -> None;		
end ;;

(* finds a given attribute in the class - returns option (feature) *)
let find_attribute attribute_name in_class = begin
	let own_attributes = get_own_attributes in_class in
	try 
		let result = List.find(fun feature ->
			match feature with
			| Method _ ->  false	
			| Attribute ((_, a_name), _, _) -> 
				attribute_name = a_name
		) own_attributes in	
		Some(result);
	with
	| _ -> None;		
end ;;

(* Type check redefines related errors for a child method in a given parent class*)
(* On mismatch, throws MethodRedefineError *)
let check_method_redefines in_method in_class = begin
	let ((_, in_method_name), in_formals, (_, in_return_type),_) = in_method in
	let parent_method = find_method in_method_name in_class in
	let all_good = match parent_method with
		| Some(feature) -> 
			let res = match feature with
				| Attribute _ -> failwith "cannot happen: found attribute"	
				| Method (_, parent_formals, (_, parent_return_type),_) -> 
					(* Number of Formals should match *)
					if not (List.length parent_formals = List.length in_formals) then begin
						raise (MethodRedefineError "changes number of formals")
					end;

					(* We combine the formal lists together (maps each entry to the corresponding one 
					in the other list) so that we can test for equality between all of the formals. *)
					let combined_formals = List.combine parent_formals in_formals in
					List.iter ( fun (((_, parent_formal_name),(_, parent_formal_type)), (_,(_, formal_type))) -> 
						if not (parent_formal_type = formal_type) then begin
							raise (MethodRedefineError("changes type of formal " ^ parent_formal_name))
						end
					) combined_formals ;

					(* Check changes return type (Ex: from SELF_TYPE to String *)	
					if not (in_return_type = parent_return_type) then begin
						raise (MethodRedefineError( sprintf "changes return type (from %s to %s) "
							 parent_return_type in_return_type));
					end;

					();
			in
			res;
		| None -> ();
	in
	all_good	
end;;


(* Type check redefines related errors for a child attribute in a given parent class*)
(* On mismatch, throws AttributeRedefineError *)
let check_attribute_redefines in_attribute in_class = begin
	let ((_, in_attribute_name), _, _) = in_attribute in
	let parent_attribute = find_attribute in_attribute_name in_class in
	let all_good = match parent_attribute with
		| Some(feature) -> 
			let res = match feature with
				| Method _ -> failwith "cannot happen: found method"	
				| Attribute _ -> 
					raise (AttributeRedefineError("redefines attribute "^in_attribute_name))
			in
			res;
		| None -> ();
	in
	all_good	
end;;



(* Expression Type Checking 
* Iterete over every class
*		Then over every feature
*			Then typecheck the experessions in that feature.
* We implement type check procedure by reading in rules in the CRM
*)
	(* TODO: M C *)
let rec exp_typecheck(o_e: obj_env) (m_e: method_env) (exp: expression) : static_type = begin
	printf "Doing an expression type check\n" ;
	let static_type = match exp.expression_type with
	| DynamicDispatch(caller_expression, method_identifier, parameter_expressions) -> begin
		printf "Doing a Dynamic Dispatch\n" ;
		(* We're assuming that this should return the Static Type (so the most general class that applies) *)
		let caller_type: static_type = exp_typecheck o_e m_e caller_expression in
		(* We now check if the given method belongs to this class type. This first requires that we
		retrieve the caller_type_name and the method_name, so that we can look it up. *)
		let caller_type_name: string = match caller_type with
		| Class(type_name) -> type_name
		| SELF_TYPE(_) -> begin
			(* FIXME Is this correct? *)
			printf "Illegal: Self type can not be invoked as the caller for a dynamic dispatch\n" ;
			exit 1
		end
		in 
		let (method_identifier_line_number, method_name): (string * string) = method_identifier in
		try
			(* This will return a list of ClassTypes, with the tail being the return type of the method
			and the rest being parameter types *)
			let method_information: static_type list = Hashtbl.find m_e (caller_type_name, method_name) in
			(* We will return this later as the type for this DynamicDispatch, if we're successful *)
			let return_type = List.nth method_information ((List.length method_information) - 1) in
			(* We're now going to see if the parameters we parsed match what the definition expects.
			For the number of parameters, we have to account for the return type being included in
			the method information, so we subtract one from that. *)
			let expected_parameter_count = (List.length method_information) - 1 in
			let actual_parameter_count = List.length parameter_expressions in
			if actual_parameter_count <> expected_parameter_count then begin
				printf "ERROR: %s: Type-Check: wrong number of actual arguments (%d vs. %d)\n"
						caller_expression.line_number actual_parameter_count expected_parameter_count ;
				exit 1
			end ;
			
			return_type
		with Not_found -> begin
			(* ERROR: 4: Type-Check: unknown method five in dispatch on String *)
			printf "ERROR: %s: Type-Check: unknown method %s in dispatch on %s\n" 
					method_identifier_line_number method_name caller_type_name ;
			exit 1
		end
	end
	| New(class_identifier) -> begin
		let (_, class_name) = class_identifier in
		Class(class_name)
	end
	| Integer(i) -> begin
		printf "Doing a Integer\n" ;
		Class("Int")
	end
	| String(s) -> begin
		printf "Doing a String\n" ;
		Class("String")
	end
	| True | False -> begin
		printf "Doing a Boolean\n" ;
		Class("Bool")
	end
	| Plus(e1, e2) | Minus(e1, e2) | Times(e1, e2) | Divide(e1, e2) -> 
		printf "Doing a Math Symbol\n" ;
		let t1 = exp_typecheck o_e m_e e1 in
		if t1 <> (Class "Int") then begin
			printf "ERROR: %s: Type-Check: arithmetic on Int %s instead of Ints\n" 
				exp.line_number (type_to_str t1);
			exit 1	
		end;
		let t2 = exp_typecheck o_e m_e e2 in
		if t2 <> (Class "Int") then begin
			printf "ERROR: %s: Type-Check: arithmetic on Int %s instead of Ints\n"
				exp.line_number (type_to_str t2);
			exit 1	
		end;
		Class("Int")
	| Equals(e1, e2) | LessThan(e1, e2)  | LessThanOrEq(e1, e2)-> 
		printf "Doing a Equality\n" ;
		let eq_class_list = [(Class "Int");(Class "String");(Class "Bool")] in
		let t1 = exp_typecheck o_e m_e e1 in
		if not (List.mem t1 eq_class_list) then begin
			printf "ERROR: %s: Type-Check: comparison on %s not allowed\n" 
				exp.line_number (type_to_str t1);
			exit 1	
		end;
		let t2 = exp_typecheck o_e m_e e2 in
		if t1 <> t2 then begin
			printf "ERROR: %s: Type-Check: comparison between %s and %s\n" 
				exp.line_number (type_to_str t1) (type_to_str t2);
			exit 1	
		end;
		Class("Bool")
	| Not(e1) -> 
		printf "Doing a Not\n" ;
		let t1 = exp_typecheck o_e m_e e1 in
		if t1 <> (Class "Bool") then begin
			printf "ERROR: %s: Type-Check: not applied to type %s instead of Boolean \n" 
				exp.line_number (type_to_str t1);
			exit 1
		end;
		Class("Bool")
	| Negate(e1) -> 
		printf "Doing a Negate\n" ;
		let t1 = exp_typecheck o_e m_e e1 in
		if t1 <> (Class "Int") then begin
			printf "ERROR: %s: Type-Check: negate applied to type %s instead of Int \n" 
				exp.line_number (type_to_str t1);
			exit 1
		end;
		Class("Int")
	| IsVoid(e1) -> 
		printf "Doing a Isvoid\n" ;
		let t1 = exp_typecheck o_e m_e e1 in
		Class("Bool")
	| Identifier((vloc, vname)) ->
		printf "Doing a Identifier\n" ;
		if Hashtbl.mem o_e vname then
			Hashtbl.find o_e vname
	 	else begin
	 		printf "ERROR: %s: Type-Check: Undeclared variable %s\n" vloc vname;
			exit 1
	 	end
	| Assign((id_location, id_name), exp) ->
		printf "Doing a Assign\n" ;
		if Hashtbl.mem o_e id_name then 
            let tid = Hashtbl.find o_e id_name in 
            let te = exp_typecheck o_e m_e exp in 
            if is_subtype te tid then
                te
            else begin
                printf "ERROR: %s: Assignment does not conform: %s has type %s\n"  id_location id_name (type_to_str tid);
                exit 1
        end
        else begin
                printf "ERROR: %s: Type-check: undeclared variable %s\n"  id_location id_name;
                exit 1
        end
	| If(predicate, then_exp, else_exp) ->
		printf "Doing a If\n" ;
		let predicate_type = exp_typecheck o_e m_e predicate in
    	if predicate_type <> (Class "Bool") then begin
    		printf "ERROR: %s: If statement's predicate expects type Bool, not type %s\n" exp.line_number (type_to_str predicate_type);
    		exit 1
    	end
    	else
    		let then_exp_type = exp_typecheck o_e m_e then_exp in 
    		let else_exp_type = exp_typecheck o_e m_e else_exp in
    			lowest_upper_bound then_exp_type else_exp_type
    | Block(exps) ->
    	printf "Doing a Block\n" ;
		let list_type = ref (Class "Object") in 
    	let check_type = List.iter(fun e -> 
        		list_type := exp_typecheck o_e m_e e
    	) exps in 
    		!list_type
    | Let(bindings, exp) ->
    	printf "Doing a Let\n" ;
		let new_o = o_e in  
		List.iter(fun binding -> 
			match binding with 
			| BindingNoInit((id_location, id_name), (type_location, init_type)) -> 
				Hashtbl.add new_o id_name (Class init_type)
			| BindingInit((id_location, id_name), (type_location, init_type), init_exp) ->
				let init_exp_type = exp_typecheck o_e m_e init_exp in 
					if is_subtype init_exp_type (Class init_type) then 
						Hashtbl.add new_o id_name (Class init_type)
					else begin
						printf "ERROR: %s: Type-Check: initializer for %s was %s, did not match declared %s\n" 
							id_location id_name (type_to_str init_exp_type) init_type;
						exit 1
					end
		) bindings;
		exp_typecheck new_o m_e exp
	| _ -> begin
		printf "FATAL ERROR: Unmatched expression type\n" ;
		exit 1
	end
	in
	(* annotate AST with the new-found static type *)
	exp.static_type <- Some(static_type); (* Node => havent done tc, Some => done with tc *)
	static_type

end;;


let topo_sort vertices = begin

    (*let vertices = List.map (fun ((_,cls_name),_,_) -> cls_name) ast in*)

    let deg = Hashtbl.create 64 in

    (* counting the number of incoming edges *)
    List.iter (fun (v, d) -> 
            let cur = Hashtbl.find_opt deg v in
            match cur with 
            | None -> Hashtbl.replace deg v 1
            | degree -> Hashtbl.replace deg v (Option.get degree + 1);
    ) vertices ;

    (* adding missing vertices. Init them with 0 degree *)
    List.iter (fun (v, d) -> 
            let missing_vert = Hashtbl.find_opt deg d in
            match missing_vert with 
            | None -> Hashtbl.replace deg d 0
            | _ -> ();
    ) vertices ;

    (* find starting point  *)
    let topo = ref (Hashtbl.fold (fun v d acc -> 
            if d = 0 then v :: acc else acc
    ) deg []) in



    (* traverse the graph *)
    let result = ref [] in
    while (List.length !topo) > 0 do
            let sorted = List.sort compare !topo in
            topo := sorted;
            let hd = List.hd !topo in
            (* printf "head %s\n" head; *)
            (*List.iter (fun x ->*)
                (* printf "topo %s\n" x; *)
            (* !topo;*)
            result := hd :: !result;
            List.iter (fun (in_, out) ->
                    if hd = out then begin
                            let d = Option.get (Hashtbl.find_opt deg in_) in
                            Hashtbl.replace deg in_ (d-1);
                            if (d-1) = 0 then begin
                                    topo := in_ :: !topo
                            end;
                    end;
            ) vertices;
            let new_topo = List.filter (fun x -> 
                            x <> hd) !topo in
            topo := new_topo
    done;
    Hashtbl.iter (fun v d -> 
            if (d != 0) then begin
                (* TODO: add more details about the cycle *)
                printf "ERROR: 0: Type-Check: inheritance cycle: %s\n" v;
                exit 1
            end;
    ) deg ;

    !result;
end;;

let add_missing_edges vertices = begin
    let result = ref vertices in
    let missed_classes = ["Bool"; "Int"; "IO"; "String"] in 
    List.iter (fun cls ->
       result := ("Object", cls) :: !result
    ) missed_classes;
    !result;
end;;

let get_ast_with_base_classes ast base_classes = begin
        let ast_with_base_classes = ref ast in
        let obj_name = "Object" in
        let unused_location = "-1" in
        List.iter (fun class_name ->
                let initialized_class = match class_name with
                | "Bool" -> ((unused_location, class_name), Some(unused_location, obj_name), bool_features)
                | "Int" -> ((unused_location, class_name), Some(unused_location, obj_name), int_features)
                | "IO" -> ((unused_location, class_name), Some(unused_location, obj_name), io_features)
                | "String" -> ((unused_location, class_name), Some(unused_location, obj_name), string_features)
                | "Object" -> ((unused_location, class_name), None, obj_features)
                in
                ast_with_base_classes := initialized_class :: !ast_with_base_classes
        ) base_classes ;
        ! ast_with_base_classes
end;;
let main () = begin 

	(* De-Serialize the cl-ast file *)
	let input_file_name = Sys.argv.(1) in
	let input_file = open_in input_file_name in

	let read () = 
		input_line input_file (* FIX ME - \r\n *)
	in
	let rec range k = 
		if k <= 0 then []
		else k :: (range (k-1))
	in 

	(* This is a generic helper method used to read a list using a custom parsing function *)
	let read_list worker  =
		let n = int_of_string (read()) in
		let mapping_indexes = range n in
		(* This tries to parse each of the items individually and places them together in a list *)
		List.map (fun _ -> worker()) mapping_indexes
	in

	let rec read_cool_program () =
		let cool_classes = read_list read_cool_class in
		cool_classes

	and read_id () = 
		let line_number  = read() in
		let name  = read() in
		(line_number, name)

	and read_cool_class () =  (* Cool class *)
		let class_identifier = read_id() in 
		let inherits = match read() with
		| "no_inherits" -> None
		| "inherits" -> 
			let super = read_id() in
			Some(super)
		| x -> failwith ("cannot happen:inherits " ^x)
		in 
		let features = read_list read_feature in 
		(class_identifier, inherits, features)

	and read_feature() = 
		match read() with
		| "attribute_no_init" -> 
			let feature_name = read_id() in
			let feature_type = read_id() in
			Attribute(feature_name, feature_type, None)
		| "attribute_init"-> 
			let feature_name = read_id() in
			let feature_type = read_id() in
			let initialization_expression = read_exp() in
			Attribute(feature_name, feature_type, (Some initialization_expression))
		| "method"-> 
			let method_name = read_id() in
			let formals = read_list read_formal in 
			let return_type = read_id() in 
			let method_body = read_exp () in 
			Method(method_name, formals, return_type, method_body)
		| x -> failwith ("cannot happen:feature " ^ x)

	and read_formal () = 
		let formal_name = read_id() in
	    let formal_type = read_id() in 
	    (formal_name, formal_type)
	and read_binding () = match read() with
		| "let_binding_init" -> 
						let id = read_id() in 
						let init_type = read_id() in 
						let exp = read_exp() in 
						BindingInit(id, init_type, exp)
		| "let_binding_no_init" -> 
						let id = read_id() in 
						let init_type = read_id() in 
						BindingNoInit(id, init_type)
	and read_case_element () = 
		let id = read_id() in 
		let init_type = read_id() in
		let e = read_exp() in 
		CaseElement(id, init_type, e)
	and read_exp () = 
		let line_number = read() in
		let expression_type = match read () with
			| "integer" ->
			                let i_val = read () in
			                Integer(i_val)
			| "string" ->
			                let s_val = read () in
			                String(s_val)

			| "assign" -> 
			                let id_name = read_id() in 
			                let expr = read_exp() in 
			                Assign(id_name, expr)
			| "identifier" -> 
			                let id = read_id() in 
			                Identifier(id)
			| "dynamic_dispatch" ->
			                let expr = read_exp() in 
			                let meth = read_id() in
			                let expr_list = read_list read_exp in 
			                DynamicDispatch(expr, meth, expr_list)
			| "static_dispatch" ->
			                 let expr = read_exp() in 
			                 let typ = read_id() in 
			                 let type_loc, type_name = typ in
			                        if type_name = "SELF_TYPE" then begin
			                                printf "Error: %s: Type-Check: SELF_TYPE does not conform to SELF_TYPE in static dispatch %s\n" type_loc type_name;
			                                exit 1;
			                        end ;
			                 let meth = read_id() in 
			                 let expr_list = read_list read_exp in 
			                 StaticDispatch (expr, typ, meth, expr_list)
			| "self_dispatch" -> 
			                let meth = read_id() in 
			                let expr_list = read_list read_exp in
			                SelfDispatch(meth, expr_list)
			| "if" -> 
			                let predicate = read_exp() in 
			                let then_exp = read_exp() in
			                let else_exp = read_exp() in
			                If(predicate, then_exp, else_exp)
			| "while" -> 
			                let predicate = read_exp() in 
			                let body = read_exp() in 
			                While(predicate, body)
			| "block" -> 
			                let exp_list = read_list read_exp in 
			                Block(exp_list)
			| "new" ->  
			                let class_name = read_id() in
			                New(class_name)
			| "plus" -> 
			                let a = read_exp() in 
			                let b = read_exp() in
			                Plus(a, b)
			| "minus" -> 
			                let a = read_exp() in 
			                let b = read_exp() in
			                Minus(a, b)
			| "times" -> 
			                let a = read_exp() in 
			                let b = read_exp() in
			                Times(a, b)
			| "divide" -> 
			                let a = read_exp() in 
			                let b = read_exp() in
			                Divide(a, b)
			| "isvoid" -> 
			                let e = read_exp() in
			                IsVoid(e)
			| "lt" -> 
			                let a = read_exp() in 
			                let b = read_exp() in
			                LessThan(a, b)
			| "le" -> 
			                let a = read_exp() in 
			                let b = read_exp() in
			                LessThanOrEq(a, b)
			| "eq" -> 
			                let a = read_exp() in 
			                let b = read_exp() in
			                Equals(a, b)
			| "not" -> 
			                let e = read_exp() in
			                Not(e)
			| "negate" -> 
			                let e = read_exp() in
			                Negate(e)
			| "true" -> 	True
			| "false" -> 	False
			| "let" -> 
							let bindings = read_list read_binding in 
							let e = read_exp() in 
							Let(bindings, e)
			| "case" -> 
							let e = read_exp() in
							let elements = read_list read_case_element in 
							Case(e, elements)
		| x -> (*TODO: other exp *)
			failwith ("cannot happen:expr " ^x)	
		in	
		{
			line_number = line_number;
			expression_type = expression_type;
			static_type = None; (*not annotated it yet*)
		}

	in


	let ast = read_cool_program () in
	close_in input_file; 

	(* Consolidated List of User and Base classes *)
    (* This adds the bases classes to our class inheritence mapping so that they aren't treated as
-	undeclared classes. *)
    let base_classes = ["Bool"; "IO"; "Int"; "String"; "Object"] in
    let user_classes = List.map (fun ((_,cname),_,_) -> cname) ast in
	let all_classes = base_classes @ user_classes in
	let all_classes = List.sort compare all_classes in
    let vertices = ref [] in

	(* Check of Main class, else return *)
	if not (List.mem "Main" user_classes) then begin
		printf "ERROR: 0: Type-Check: class Main not found\n";
		exit 1
	end;

	(* Set up global maps *)
	let m_e = empty_method_env () in (* add features to obj env *)
	List.iter(fun current_class ->
		(* populate parent map *)
		let ((class_line_number, class_name), inherits, features) = current_class in
		let parents = get_super_classes class_name ast [] in
		let parents_names = List.map (fun ((_,cname),_,_) -> cname) parents in
		Hashtbl.add parent_map class_name parents_names;

		(* populate method environment *)
		(* Add parent methods *)
		List.iter(fun super_class ->
			add_method_types m_e super_class;
		) parents;
		(* Add self methods *)
		add_method_types m_e current_class;		

	) ast;

	(* Look for errors in Class Declarations*)	
	let declared_classes = ref [] in
	List.iter(fun current_class ->
		let ((class_line_number, class_name), inherits, features) = current_class in
		(* Look for redefined Basic Classes or already existing class*)
		if ((List.mem class_name base_classes) ||
				(List.mem class_name !declared_classes)) then begin
			printf "ERROR: %s: Type-Check: class %s redefined\n" class_line_number class_name;
			exit 1
		end;
		declared_classes := class_name :: !declared_classes;

		let supclasses = get_super_classes class_name ast [] in
		let o_e = empty_obj_env () in (* add features to obj env *)
		(* Add super class attributes to object environment*)
		List.iter(fun super_class ->
			add_attribute_types o_e super_class;
		) supclasses;
		(* Add current class attributes to object environment*)
		add_attribute_types o_e current_class;
		
	
		(* Type check own methods *)
		let own_methods = get_own_methods current_class in
		List.iter(fun own_method -> 
			match own_method with
			| Method ((method_line_number,method_name), formals, ret_type, exp) -> 
				let in_method = ((method_line_number,method_name), formals, ret_type, exp) in

				(* Check for duplicate formals in the method. *)
				let formal_names = ref [] in
				let new_o_env = Hashtbl.copy o_e in
				List.iter( fun ((_, formal_name),(_, formal_type)) ->
					if (List.mem formal_name !formal_names) then begin
						printf "ERROR: %s: Type-Check: class %s has method %s with duplicate formal parameter named %s\n" 
							method_line_number class_name method_name formal_name;
						exit 1
					end;
					(* Collect all formal names *)
					formal_names :=  formal_name :: !formal_names;
					(* Add formal type to the local extended object enviroment *)
					Hashtbl.add new_o_env formal_name (Class formal_type);
				) formals ;

				(* Check for method override errors in super classes *)
				List.iter(fun super_class ->
					try
						check_method_redefines in_method super_class;		
					with
					| MethodRedefineError(msg) -> 
						printf "ERROR: %s: Type-Check: class %s redefines method %s and %s\n" 
								method_line_number class_name method_name msg;
						exit 1;
					| _ -> failwith "cannot happen: Unknown Method Redefines Error";
				) supclasses;


				(* Type check method expression *)

				let init_type = exp_typecheck new_o_env m_e exp in
	  			let (return_loc, return_type) = ret_type in
	  			if not (is_subtype init_type (Class return_type)) then begin
	  				printf "ERROR: %s: Type-Check: %s does not conform to %s in method %s\n" 
	  					method_line_number (type_to_str init_type) return_type method_name;
					exit 1;
				end;

			| _ -> failwith "cannot happen: found attribute"
			
		) own_methods;

		(* Type check own attributes *)
		let own_attributes = get_own_attributes current_class in	
		List.iter(fun own_attr -> 
			match own_attr with
			| Attribute ((attr_loc, attr_name), (dtloc, decl_type), exp) -> 
				let in_attr = ((attr_loc,attr_name),(dtloc, decl_type), exp) in
				(* Check for attribute override errors in super classes *)
				List.iter(fun super_class ->
					try
						check_attribute_redefines in_attr super_class;					
					with
					| AttributeRedefineError(msg) -> 
						printf "ERROR: %s: Type-Check: class %s redefines attribute %s\n" 
								attr_loc class_name attr_name;
						exit 1;
					| _ -> failwith "cannot happen: Unknown Attribute Redefines Error";
				) supclasses;

				begin match exp with
					| Some(init_exp) -> 
						(* x: Int <- 5 + 3 *)
			  			let init_type = exp_typecheck o_e m_e init_exp in
			  			if not (is_subtype init_type (Class decl_type)) then begin 
			  				printf "ERROR: %s: Type-Check: initializer for %s was %s, did not match declared %s\n" 
			  					attr_loc attr_name (type_to_str init_type) decl_type ;
							exit 1
						end;

					| None -> ()
				end;

			| _ -> failwith "cannot happen: found method"


		) own_attributes;



	) ast;

	(* DONE WITH ERROR CHECKING*)

	(* Emit the CL-Type file *)

	(* PA4-c we emit only class map - TODO ? *)
	let class_map_file_name = (Filename.chop_extension input_file_name) ^ ".cl-type" in
	let f_out = open_out class_map_file_name in

	let rec output_exp e =
                fprintf f_out "%s\n" e.line_number;
                (match e.static_type  with
                | None -> failwith  "FATAL ERROR: Can not have a non-static type"
                | Some(Class(c)) -> fprintf f_out "%s\n" c
                | Some(SELF_TYPE(c)) -> fprintf f_out "SELF_TYPE\n"
                );
                match e.expression_type with
                | Integer(int_val) -> fprintf f_out "integer\n%s\n" int_val
                | String(str_val) -> fprintf f_out "string\n%s\n" str_val
                | True -> fprintf f_out "true\n" 
                | False -> fprintf f_out "false\n" 
                | Identifier(id) ->
                                fprintf f_out "identifier\n";
                                output_identifier id
                | Plus(expr1, expr2) -> 
                                fprintf f_out "plus\n" ;
                                output_exp expr1 ;
                                output_exp expr2
                | Times(expr1, expr2) -> 
                                fprintf f_out "times\n" ;
                                output_exp expr1;
                                output_exp expr2
                | Minus(expr1, expr2) -> 
                                fprintf f_out "minus\n" ;
                                output_exp expr1;
                                output_exp expr2
                | Divide(expr1, expr2) -> 
                                fprintf f_out "divide\n" ;
                                output_exp expr1;
                                output_exp expr2
                | If(precondition, then_expr, else_expr) ->
                                fprintf f_out "if\n" ;
                                output_exp precondition;
                                output_exp then_expr;
                                output_exp else_expr
                | While(precondition, body_expr) -> 
                                fprintf f_out "while\n" ;
                                output_exp precondition;
                                output_exp body_expr
                | Assign(expr1, expr2) ->
                                fprintf f_out "assign\n" ;
                                output_identifier expr1;
                                output_exp expr2
                | LessThan(expr1, expr2) -> 
                                fprintf f_out "lt\n" ;
                                output_exp expr1;
                                output_exp expr2
                | LessThanOrEq(expr1, expr2) -> 
                                fprintf f_out "le\n" ;
                                output_exp expr1;
                                output_exp expr2
                | Equals(expr1, expr2) -> 
                                fprintf f_out "eq\n" ;
                                output_exp expr1;
                                output_exp expr2
                | Not(expr)  ->    
                                fprintf f_out "not\n" ;
                                output_exp expr
                | Negate(expr)  ->    
                                fprintf f_out "negate\n" ;
                                output_exp expr
                | IsVoid(expr) ->
                                fprintf f_out "isvoid\n" ;
                                output_exp expr
                | New(expr) ->
                                fprintf f_out "new\n" ;
                                output_identifier expr
                | Block(expr) ->
                                fprintf f_out "block\n" ;
                                fprintf f_out "%d\n" (List.length expr);
                                List.iter output_exp expr
                | DynamicDispatch(expr, id, args) -> 
                                fprintf f_out "dynamic_dispatch\n";
                                output_exp expr;
                                output_identifier id;
                                fprintf f_out "%d\n" (List.length args);
                                List.iter output_exp args
                | SelfDispatch(id, args) -> 
                                fprintf f_out "self_dispatch\n";
                                output_identifier id;
                                fprintf f_out "%d\n" (List.length args);
                                List.iter output_exp args
                | StaticDispatch(expr, typ, id, args) -> 
                                fprintf f_out "static_dispatch\n";
                                output_exp expr;
                                output_type typ;
                                output_identifier id;
                                fprintf f_out "%d\n" (List.length args);
                                List.iter output_exp args
                | Let(bindings, expr)     ->
                                fprintf f_out "let\n";
                                fprintf f_out "%d\n" (List.length bindings);
                                List.iter output_binding bindings;
                                output_exp expr
                | Case(expr, elelist)   ->
                                fprintf f_out "case\n";
                                output_exp expr;
                                fprintf f_out "%d\n" (List.length elelist);
                                List.iter output_ele elelist
        and     output_identifier (line_number, string_lexeme) =
                        fprintf f_out "%s\n%s\n" line_number string_lexeme

        and     output_type (line_number, string_type) =
                        fprintf f_out "%s\n%s\n" line_number string_type
        and     output_binding res =
                match res with 
                | BindingNoInit(id, typ) ->
                        fprintf f_out "let_binding_no_init\n";
                        output_identifier id;
                        output_type typ
                | BindingInit(id, typ, exp) ->
                        fprintf f_out "let_binding_init\n";
                        output_identifier id;
                        output_type typ;
                        output_exp exp
        (*Print case element in case expression*)
        and     output_ele res =
                match res with
                | CaseElement(id, typ, expr) ->
                        output_identifier id;
                        output_type typ;
                        output_exp expr
        (*Print each type of expression*)
	in

	fprintf f_out "class_map\n%d\n" (List.length all_classes);
	(* Iterate over all classes *)
	List.iter (fun class_name ->
		(* name of the class, # of attributes, each attr=feature in turn *)
		fprintf f_out "%s\n" class_name;

		(* Get list of attributes in this class with name class_name using find fn.*)
		let attributes = 
			(* TODO: consider INHERITED attributes:
				1: construct a mapping from child to parent
					a: use Toposort here to find the right order of traversal
						or to detect inheritance cycles
				2: recursively walk up that mapping until we hit object
				3: add in all of the attributes we find
					4: while in 3, look for attribute override problems
			 *)

			try
				let  (_, inherits, features) = List.find(
					fun ((_,class_name_candidate),_,_)-> class_name_candidate = class_name
				) ast in
				(*Filter only Attribute features *)
				List.filter (
					fun feature -> match feature with
					| Attribute _ -> true
					| Method _ -> false
				) features;

			with
			| _ -> (*bool/int/object*) []
		in
		fprintf f_out "%d\n" (List.length attributes);
		List.iter ( fun attribute -> match attribute with
		| Attribute ((_,attribute_name), (_, attribute_type), None) -> 
			fprintf f_out "no_initializer\n%s\n%s\n" attribute_name attribute_type;
		| Attribute ((_,attribute_name), (_, attribute_type), (Some initialization_expression)) ->
			fprintf f_out "initializer\n%s\n%s\n" attribute_name attribute_type;
			output_exp initialization_expression;
		| Method _ -> failwith "method unexpected"

		) attributes;

		(* Get list of methods in this class.*)
		let methods = try
				let  (_, _, features) = List.find(
					fun ((_,candidate_class_name),_,_) -> 
						candidate_class_name = class_name
					) ast 
				in
				(*Filter only Method features *)
				List.filter (fun feature -> match feature with
				| Attribute _ -> false
				| Method _ -> true) features;
 
			with
			| _ -> (*bool/int/object*) []
		in
		List.iter ( fun meth -> match meth with
		| Attribute _ -> failwith "attribute unexpected"
		| Method ((_,mname), features, mtype, exp) -> ()
			(* TODO: create impl. list *)
		) methods;

        ) all_classes ;

        let ast_with_base_classes = get_ast_with_base_classes ast base_classes in

        let get_number_of_methods lst = 
                let num = ref 0 in
                List.iter (fun ((_,_),_,features) ->
                        num := (List.length features) + !num
                ) lst ;
                !num 
        in

        List.iter (fun ((cls_loc, cls_name), inherits, features) ->
                match inherits with
                | None -> begin 
                    vertices := ("Object", cls_name) :: !vertices;
                    ()
                end;
                | Some(inh_loc, inh_name) -> 
                                vertices := (inh_name, cls_name) :: !vertices;
        ) ast ;
        vertices := add_missing_edges !vertices;
        let sorted_classes = topo_sort !vertices in

        (*List.iter (fun cls -> *)
                (*printf "%s\n" cls*)
        (*) sorted_classes ;*)

        let does_method_exist_in_list target_method_name feature_list =
                let result = ref false in
                List.iter (fun feature ->
                        match feature with
                        | Attribute _ -> ()
                        | Method ((_,method_name), _, _, _) -> begin
                                if method_name = target_method_name then
                                        result := true;
                                        ()
                        end
                ) feature_list ;
                !result
        in

        let output_formal_name formals =
                List.iter( fun ((_, formal_name),_) ->
                        fprintf f_out "%s\n" formal_name
                ) formals                 
        in

        (* retreive the hashtable with method name as a key and tuple (formals, class_name, exp) as a value.
         * It is needed to conveniently print out implementation map. 
         * What it does? it itrates over class names (that are in topological order) and saves all
         * methods that it declares and override.*)
        let get_parents_methods_tbl class_names super_class_list =
            let methods_map = Hashtbl.create 255 in
            List.iter (fun class_name ->
                    let super_class_option = List.find_opt (fun ((_,cname),_,_)-> class_name = cname) super_class_list in
                    if not (Option.none = super_class_option) then begin
                            let ((_, _),_,features) = Option.get super_class_option in
                            (* add methods to hashtbl starting from the ultimate class that override method *)
                            List.iter (fun current_method ->
                                    match current_method with
                                    | Attribute _ -> ()
                                    | Method ((_,method_name), formals, method_type, exp) -> begin
                                            let cur = Hashtbl.find_opt methods_map method_name in
                                            match cur with 
                                            | None -> Hashtbl.replace methods_map method_name (formals, class_name, exp);
                                            | _ -> () 
                                    end
                            ) features ;
                    end;
            ) class_names ;
            methods_map
        in

        (* get sorted list of names from a hastable *)
        let get_method_names_from_tbl tbl =
            let parents_method_list = ref (Hashtbl.fold (fun method_name value acc -> 
                                    method_name :: acc 
            ) tbl []) in
            List.sort compare !parents_method_list
        in

        (* Output each method in turn (in order of appearance, with inherited or overridden methods from a superclass coming first; internal methods are defined to appear in ascending alphabetical order) *)
        let output_parent_methods parents_methods_tbl  =
            let sorted_methods = get_method_names_from_tbl parents_methods_tbl in
            List.iter (fun method_name ->
                    let formals, owner_class, exp = (Hashtbl.find parents_methods_tbl method_name) in
                    fprintf f_out "%s\n" method_name;
                    fprintf f_out "%d\n" (List.length formals);
                    output_formal_name formals;
                    fprintf f_out "%s\n" owner_class;
                    fprintf f_out "!TODO: expression\n";
            ) sorted_methods
        in

        (* print out methods of the current class in the order they appear in source filess *)
        let output_own_methods current_class own_features parents_method_list = 
            List.iter (fun current_method ->
                    match current_method with
                    | Attribute _ -> ()
                    | Method ((_,method_name), formals, method_type, exp) -> begin
                            fprintf f_out "%s\n" method_name;
                            fprintf f_out "%d\n" (List.length formals);
                            output_formal_name formals;
                            if not (List.mem method_name parents_method_list) then begin
                                    fprintf f_out "%s\n" current_class;
                                    if not (exp.static_type = None) then output_exp exp
                                    else fprintf f_out "expression is None\n"
                            end;
                    end
            ) own_features
        in

	fprintf f_out "implementation_map\n%d\n" (List.length all_classes);
	(* Iterate over all classes *)
	List.iter (fun class_name ->
        (* output the class name *)
        fprintf f_out "%s\n" class_name;
        let super_class_list = get_super_classes class_name ast_with_base_classes [] in
        
        let (_,inherits,features) = List.find (fun ((_,cname),_,_)-> class_name = cname) ast_with_base_classes in
        fprintf f_out "%d\n" ((get_number_of_methods super_class_list) + (List.length features));
        (* output the number of methods *)
        let map = get_parents_methods_tbl (List.rev sorted_classes) super_class_list in
        (*fprintf f_out "size: %d " (Hashtbl.length map);*)
        let parents_methods_tbl = map in
        output_parent_methods parents_methods_tbl;
        let sorted_method_list = get_method_names_from_tbl parents_methods_tbl in
        output_own_methods class_name features sorted_method_list;
        (*List.iter (fun target_class ->*)
                (*let option_super_class = List.find_opt (fun ((_,cname),_,_)-> target_class = cname) super_class_list in*)
                (*if not (option_super_class = Option.none) then begin*)
                        (*let ((_, super_class_name),_,super_features) = (Option.get option_super_class) in*)
                        (*output_parent_methods super_class_name super_features class_name features;*)
                (*end*)
        (*) sorted_classes ;*)
        (*fprintf f_out "%s\n" super_class_name;*)
                
    ) all_classes ;

	fprintf f_out "parent_map\n%d\n" (List.length all_classes - 1);
	(* Iterate over all classes *)
	List.iter (fun class_name ->
        let obj_name = "Object" in
                if not (obj_name = class_name) 
                        then fprintf f_out "%s\n" class_name;
        
        if (List.mem class_name base_classes) then begin
                if not (obj_name = class_name) 
                        then fprintf f_out "%s\n" obj_name
        end else begin
                let (_,inherits,_) = List.find (fun ((_,cname),_,_)-> class_name = cname) ast in
                match inherits with
                | None -> 
                        fprintf f_out "%s\n" obj_name
                | Some(_, iname) ->
                        fprintf f_out "%s\n" iname
        end
    ) all_classes ;


end ;;
main () ;;
