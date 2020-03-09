open Printf


(* ----------------------------------------------------------------------------------- *)
(* Type declaration *)
type static_type = 
        | Class of string (*Int or Object*)
        | SELF_TYPE of string
let type_to_str t = match t with  
        | Class(x) -> x
        | SELF_TYPE(c) -> "SELF_TYPE"
let rec is_subtype t1 t2  =
    match t1, t2 with 
        | Class(x), Class(y) when x = y -> true
        | Class(x), Class("Object") -> true
        | Class(x), Class(y) -> false
        | _, _ -> false 

type object_environment = 
    (string, static_type) Hashtbl.t

let empty_object_environment ()  = Hashtbl.create 255

type cool_program = cool_class list
and location = string 
and id = location * string
and cool_type = location * string
and cool_class = id * (id option) * feature list
and feature = 
        | Attribute of id * cool_type * (exp option)
        | Method of id * (formal list) * cool_type * exp
and formal = id * cool_type
and exp = {
    location : location;
    exp_kind : exp_kind;
    mutable static_type : static_type option ; (*This can change *)
}
and exp_kind = 
        | Plus of exp * exp
        | Minus of exp * exp
        | Times of exp * exp
        | Divide of exp * exp
        | Identifier of id
        | Integer of string
        | String of string
        | True 
        | False
        | If of exp * exp * exp
        | While of exp * exp
        | Assign of id * exp
        | Lt of exp * exp
        | Le of exp * exp
        | Eq of exp * exp
        | Not of exp
        | Negate of exp
        | Isvoid of exp
        | New of id
        | Block of exp list
        | Dyn_Dispatch of exp * id * exp list
        | Self_Dispatch of id * exp list
        | Static_Dispatch of exp * cool_type * id * exp list
        | Let of (binding list) * exp
        | Case of exp * (case_ele list)
and binding =
        | BindingNoInit of id * cool_type
        | BindingInit of id * cool_type * exp
and case_ele = 
        | CaseEle of id * cool_type * exp

(* End of declaration *)
(* ---------------------------------------------------------------------------------------------------------------------- *)

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
(* ---------------------------------------------------------------------------------------------------------------------- *)

let add_missing_edges vertices = begin
    let result = ref vertices in
    let missed_classes = ["Bool"; "Int"; "IO"; "String"] in 
    List.iter (fun cls ->
       result := ("Object", cls) :: !result
    ) missed_classes;
    !result;
end;;


let main () = begin
    (*-----------------------------------------------------------------------------------------------------------------------*)
        (*Reading from the .cl-ast file*)
        let f_name = Sys.argv.(1) in
        let f_in = open_in f_name in

        let read () = 
                input_line f_in
        in

        let rec range k =
                if k<= 0 then []
                else k :: (range (k - 1))
        in

        let read_list worker = 
                let k = int_of_string (read ()) in
                printf "read_list of %d\n" k;
                let lst = range k in
                List.map (fun _ -> worker ()) lst
        in

        (* read in the .cl-ast file *)
        let rec read_cool_program () = 
                (* TODO: read in alphabetical order *)
                read_list read_cool_class

        and read_id () = 
                let location = read() in 
                let name = read () in
                (location, name)

        and read_cool_class () =
                let cls_name = read_id () in
                let inherits = match 
                read () with
                | "no_inherits" -> None
                | "inherits" ->
                                let super = read_id () in
                                Some(super)
                | x -> failwith ("!error: " ^ x)
                in
                let features = read_list read_features in
                (cls_name, inherits, features)

        and read_features () = 
                match read() with  
                | "attribute_no_init" -> 
                                let f_name = read_id () in
                                let f_type = read_id () in
                                Attribute(f_name, f_type, None)
                | "attribute_init" ->
                                let f_name = read_id () in
                                let f_type = read_id () in
                                let f_init = read_exp () in
                                Attribute(f_name, f_type, (Some f_init))
                | "method" ->
                                let m_name = read_id () in
                                let formals = read_list read_formal in
                                let m_type = read_id () in
                                let m_body = read_exp () in
                                Method(m_name, formals, m_type, m_body)
                | x ->
                                failwith ("fearue unhandled: " ^ x)
        and read_formal () = 
                                let f_name = read_id () in
                                let f_type = read_id () in
                                (f_name, f_type)
        and read_exp () =
                let eloc = read () in
                let ekind = match read () with
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
                                Dyn_Dispatch(expr, meth, expr_list)
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
                                 Static_Dispatch (expr, typ, meth, expr_list)
                | "self_dispatch" -> 
                                let meth = read_id() in 
                                let expr_list = read_list read_exp in
                                Self_Dispatch(meth, expr_list)
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
                                Isvoid(e)
                | "lt" -> 
                                let a = read_exp() in 
                                let b = read_exp() in
                                Lt(a, b)
                | "le" -> 
                                let a = read_exp() in 
                                let b = read_exp() in
                                Le(a, b)
                | "eq" -> 
                                let a = read_exp() in 
                                let b = read_exp() in
                                Eq(a, b)
                | "not" -> 
                                let e = read_exp() in
                                Not(e)
                | "negate" -> 
                                let e = read_exp() in
                                Negate(e)
                | "true" -> True
                | "false" -> False
                | x ->
                                failwith ("exp kind unhandled: " ^ x)
                in
                {
                    location = eloc;
                    exp_kind = ekind;
                    static_type = None; 
                }

        (*Finish reading*)
(*--------------------------------------------------------------------------------------------------------------------------------------------------------------------*)
        in

        let ast = read_cool_program () in
        close_in f_in;
        printf ".cl-ast deserialized, %d classes\n" (List.length ast);

        (* check for class-related errors *)

        let base_classes = [ "Int"; "String" ; "Bool" ; "IO"; "Object" ] in
        let user_classes = List.map (fun ((_,cls_name),_,_) -> cls_name) ast in 
        let all_classes = base_classes @ user_classes in
        let vertices = ref [] in

        (* Look for Inheritance from Int *)
        List.iter (fun ((cls_loc, cls_name), inherits, features) ->
                if cls_name = "SELF_TYPE" then begin
                        printf "Error: %s: Type-Check: class named %s\n" cls_loc cls_name;
                        exit 1;
                end ;
                (* check redefinition of base clases. Look at the list of classes*)
                match inherits with
                | None -> begin 
                    vertices := ("Object", cls_name) :: !vertices;
                    ()
                end;
                | Some(inh_loc, inh_name) -> 
                                vertices := (inh_name, cls_name) :: !vertices;
                                (* TODO: add check inheritance of String *)
                                (* TODO: add check cycles *)
                                if (inh_name = "Int") || (inh_name = "Bool") || (inh_name = "String") then begin
                                        printf "Error: %s: Type-check: inheriting from forbidden class %s\n" inh_loc inh_name;
                                        exit 1;
                                end ;
                                if not (List.mem inh_name all_classes) then begin
                                        printf "Error: %s: Type-check: inheriting from undefined class %s\n" inh_loc inh_name ;
                                        exit 1;
                                end ;
                List.iter (fun feature -> 
                (*(fun ((m_loc, m_name), formals, (f_loc, f_name), exp) ->*)
                        match feature with
                        (* check redefinition of method  *)
                        | Method((m_loc, m_name), formals, m_type, m_body) -> 
                                        if (m_name = "main") && (List.length formals > 0) then begin
                                                printf "ERROR: 0: Type-Check: class Main method main with 0 parameters not found\n" ;
                                                exit 1;
                                        end ;
                        | Attribute(_, _, _) -> 
                                                ()
                ) features ;
        ) ast ;
        
        (* --------------------------------------------------------------------------------------------------------------------------- *)
        (* Toposort *)
        (* add missing edges *)
        vertices := add_missing_edges !vertices;
        let sorted_classes = topo_sort !vertices in
        printf "class hierarchy (number of classes is %d): \n" (List.length sorted_classes);
        List.iter (fun cls -> 
                printf "%s\n" cls
        ) sorted_classes ;

        (*  let check_duplicates lst1 lst2 =
            List.iter (fun elem1 ->

            ) lst1;*)

        (*let get_full_attributes = 
                List.iter(fun curr_class -> 
                    (* get class from ast *)
                    List.iter(fun ((cls_loc, cls_name), inherits, features) -> 
                        if cls_name = curr_class then 
                        begin
                            match inherits with
                            | None -> ()
                            | Some(inh_loc, inh_name) -> 
                                List.iter(fun ((cls_loc_parent, cls_name_parent), inherits_parent, features_parent) -> 
                                    if inh_name = cls_name_parent then begin
                                        printf "inh name: %s, parent name: %s\n" cls_name cls_name_parent;
                                    end;

                            ) ast;
                        end;
                    ) ast;
                ) sorted_classes;*)
(*----------------------------------------------------------------------------------------------------------------------------------------------*)
        (*Type checking*)
        let rec typecheck (o: object_environment) (exp : exp) : static_type = 
            let static_type = match exp.exp_kind with 
            | Integer(i) -> (Class "Int")
            | Plus(e1, e2) ->
                let t1 = typecheck o e1 in 
                    if t1 <> (Class "Int") then begin 
                        printf "ERROR: %s: Type-check: Adding %s instead of Int\n"  exp.location (type_to_str t1);
                        exit 1; 
                    end;
                let t2 = typecheck o e2 in 
                    if t2 <> (Class "Int") then begin 
                        printf "ERROR: %s: Type-check: Adding %s instead of Int\n"  exp.location (type_to_str t2);
                        exit 1; 
                end;
                (Class "Int")
            | Minus(e1, e2) ->
                let t1 = typecheck o e1 in 
                    if t1 <> (Class "Int") then begin 
                        printf "ERROR: %s: Type-check: Subtracting %s instead of Int\n"  exp.location (type_to_str t1);
                        exit 1; 
                    end;
                let t2 = typecheck o e2 in 
                    if t2 <> (Class "Int") then begin 
                        printf "ERROR: %s: Type-check: Subtracting %s instead of Int\n"  exp.location (type_to_str t2);
                        exit 1; 
                end;
                (Class "Int")
            | Times(e1, e2) ->
                let t1 = typecheck o e1 in 
                    if t1 <> (Class "Int") then begin 
                        printf "ERROR: %s: Type-check: Multiplying %s instead of Int\n"  exp.location (type_to_str t1);
                        exit 1; 
                    end;
                let t2 = typecheck o e2 in 
                    if t2 <> (Class "Int") then begin 
                        printf "ERROR: %s: Type-check: Multiplying %s instead of Int\n"  exp.location (type_to_str t2);
                        exit 1; 
                end;
                (Class "Int")
            | Divide(e1, e2) ->
                let t1 = typecheck o e1 in 
                    if t1 <> (Class "Int") then begin 
                        printf "ERROR: %s: Type-check: Dividing %s instead of Int\n"  exp.location (type_to_str t1);
                        exit 1; 
                    end;
                let t2 = typecheck o e2 in 
                    if t2 <> (Class "Int") then begin 
                        printf "ERROR: %s: Type-check: Dividing %s instead of Int\n"  exp.location (type_to_str t2);
                        exit 1; 
                end;
                (Class "Int")
            | Identifier((vloc, vname)) -> 
                if Hashtbl.mem o vname then 
                    Hashtbl.find o vname
                else begin
                    printf "ERROR: %s: Type-check: undeclared variable %s\n"  vloc vname;
                    exit 1
                end
            | String(s) -> (Class "String")
            | True -> (Class "Bool")
            | False -> (Class "Bool")
            | Assign((loc, name), exp) -> 
                if Hashtbl.mem o name then 
                    let tid = Hashtbl.find o name in 
                    let te = typecheck o exp in 
                    if is_subtype te tid then
                        te
                    else begin
                        printf "ERROR: %s: Assignment does not conform: %s has type %s\n"  loc name (type_to_str tid);
                        exit 1
                end
                else begin
                    printf "ERROR: %s: Type-check: undeclared variable %s\n"  loc name;
                    exit 1
                end 
            in
                exp.static_type <- Some(static_type) ;
                static_type
            in
        let type_list = ref [] in
        List.iter (fun ((cls_loc, cls_name), inherits, features) ->
                type_list := (Class cls_name) ::!type_list;
            ) ast;
        List.iter (fun ((cls_loc, cls_name), inherits, features) ->
            let o = empty_object_environment () in 
            List.iter(fun feat ->
                let curr_class = (Class cls_name) in 
                match feat with 
                | Attribute((nameloc, name), (dcl_loc, declare_type), Some(init_exp)) -> 
                    let init_type = typecheck o init_exp in
                    if is_subtype init_type (Class declare_type) then begin
                        Hashtbl.add o name (Class declare_type)
                    end
                    else begin 
                        printf "ERROR: %s: Initializer for %s was %s not %s\n"  nameloc name (type_to_str init_type) declare_type;
                        exit 1
                    end
                | _ -> ()  
            ) features;
        ) ast;    
(*---------------------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*Printing to the .cl-type file*)
        let cls_map_name = (Filename.chop_extension f_name) ^ ".cl-type" in
        let f_out = open_out cls_map_name in

        let rec output_exp e =
                fprintf f_out "%s\n" e.location;
                (match e.static_type  with
                | None -> failwith  "typechecking"
                | Some(Class(c)) -> fprintf f_out "%s\n" c
                | Some(SELF_TYPE(c)) -> fprintf f_out "SELF_TYPE\n"
                );
                match e.exp_kind with
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
                | Lt(expr1, expr2) -> 
                                fprintf f_out "lt\n" ;
                                output_exp expr1;
                                output_exp expr2
                | Le(expr1, expr2) -> 
                                fprintf f_out "le\n" ;
                                output_exp expr1;
                                output_exp expr2
                | Eq(expr1, expr2) -> 
                                fprintf f_out "eq\n" ;
                                output_exp expr1;
                                output_exp expr2
                | Not(expr)  ->    
                                fprintf f_out "not\n" ;
                                output_exp expr
                | Negate(expr)  ->    
                                fprintf f_out "negate\n" ;
                                output_exp expr
                | Isvoid(expr) ->
                                fprintf f_out "isvoid\n" ;
                                output_exp expr
                | New(expr) ->
                                fprintf f_out "new\n" ;
                                output_identifier expr
                | Block(expr) ->
                                fprintf f_out "block\n" ;
                                fprintf f_out "%d\n" (List.length expr);
                                List.iter output_exp expr
                | Dyn_Dispatch(expr, id, args) -> 
                                fprintf f_out "dynamic_dispatch\n";
                                output_exp expr;
                                output_identifier id;
                                fprintf f_out "%d\n" (List.length args);
                                List.iter output_exp args
                | Self_Dispatch(id, args) -> 
                                fprintf f_out "self_dispatch\n";
                                output_identifier id;
                                fprintf f_out "%d\n" (List.length args);
                                List.iter output_exp args
                | Static_Dispatch(expr, typ, id, args) -> 
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
                | CaseEle(id, typ, expr) ->
                        output_identifier id;
                        output_type typ;
                        output_exp expr
        (*Print each type of expression*)
        in
        
        fprintf f_out "class_map\n%d\n"  (List.length all_classes);
        List.iter (fun cls_name ->
                (* name of class, # attrs, each attr=feature in turn *)
                fprintf f_out "%s\n" cls_name ;

                let attributes = 
                try
                        let _, inhertis, features = List.find (fun ((_,cls_name2),_,_) -> cls_name = cls_name2) ast in
                List.filter (fun feature -> match feature with
                        | Attribute _ -> true
                        | Method _ -> false
                ) features
                with Not_found ->  (* bool/int *)
                        []
        in

        fprintf f_out "%d\n" (List.length attributes) ;
        List.iter (fun attr -> match attr with
                | Attribute((_,attr_name),(_,attr_type),None) ->
                                fprintf f_out "no_initializer\n%s\n%s\n" attr_name attr_type
                | Attribute((_,attr_name),(_,attr_type),(Some init)) ->
                                fprintf f_out "initializer\n%s\n%s\n" attr_name attr_type ;
                                output_exp init
                | Method _ -> failwith "method unexpected"
                ) attributes ;
        ) all_classes ;

        close_out f_out ;
end;;
main();;

