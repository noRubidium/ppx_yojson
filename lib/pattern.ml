open Ppxlib

let expand_string ~loc s = [%pat? `String [%p Ast_builder.Default.pstring ~loc s]]

let expand_intlit ~loc s = [%pat? `Intlit [%p Ast_builder.Default.pstring ~loc s]]

let expand_int ~loc ~ppat_loc s =
  match Ocaml_compat.int_of_string_opt s with
  | Some i -> [%pat? `Int [%p Ast_builder.Default.pint ~loc i]]
  | None when Integer_const.is_binary s -> Raise.unsupported_payload ~loc:ppat_loc
  | None when Integer_const.is_octal s -> Raise.unsupported_payload ~loc:ppat_loc
  | None when Integer_const.is_hexadecimal s -> Raise.unsupported_payload ~loc:ppat_loc
  | None -> expand_intlit ~loc s

let expand_float ~loc s = [%pat? `Float [%p Ast_builder.Default.pfloat ~loc s]]

let expand_var ~loc var = Ast_builder.Default.ppat_var ~loc var

let rec expand ~loc ~path pat =
  match pat with
  | [%pat? _] -> [%pat? _]
  | [%pat? None] -> [%pat? `Null]
  | [%pat? true] -> [%pat? `Bool true]
  | [%pat? false] -> [%pat? `Bool false]
  | {ppat_desc = Ppat_constant (Pconst_string (s, None)); _} -> expand_string ~loc s
  | {ppat_desc = Ppat_constant (Pconst_integer (s, None)); ppat_loc; _}
    ->
    expand_int ~loc ~ppat_loc s
  | {ppat_desc = Ppat_constant (Pconst_integer (s, Some ('l' | 'L' | 'n'))); _}
    ->
    expand_intlit ~loc s
  | {ppat_desc = Ppat_constant (Pconst_float (s, None)); _} -> expand_float ~loc s
  | {ppat_desc = Ppat_var v; _} -> expand_var ~loc v
  | [%pat? [%p? left] | [%p? right]]
    ->
    ([%pat? [%p expand ~loc ~path left] | [%p expand ~loc ~path right]])
  | {ppat_desc = Ppat_alias (pat, var); _}
    ->
    let pat = expand ~loc ~path pat in
    Ast_builder.Default.ppat_alias ~loc pat var
  | [%pat? []] -> [%pat? `List []]
  | [%pat? [%p? _]::[%p? _]] -> [%pat? `List [%p expand_list ~loc ~path pat]]
  | {ppat_desc = Ppat_record (l, Closed); ppat_loc; _} -> expand_record ~loc ~ppat_loc ~path l ~type_:`Closed
  | {ppat_desc = Ppat_record (l, Open); ppat_loc; _} -> expand_record ~loc ~ppat_loc ~path l ~type_:`Open
  | {ppat_loc = loc; _} -> Raise.unsupported_payload ~loc
and expand_list ~loc ~path = function
  | [%pat? []] -> [%pat? []]
  | [%pat? [%p? hd]::[%p? tl]]
    ->
    let json_hd = expand ~loc ~path hd in
    let json_tl = expand_list ~loc ~path tl in
    [%pat? [%p json_hd]::[%p json_tl]]
  | _ -> assert false
and expand_record ~loc ~ppat_loc ~path ~type_ l =
  let field = function
    | {txt = Lident s; _} -> [%pat? [%p Ast_builder.Default.pstring ~loc s]]
    | {txt = _; loc} -> Raise.unsupported_record_field ~loc
  in
  let expand_one (f, p) =
    [%pat? ([%p field f], [%p expand ~loc ~path p])]
  in
  let rec plist' ~loc l =
    let open Ast_builder.Default in
      match l with
      | [] ->
        [%pat? _]
      | x :: l ->
        ppat_construct ~loc (Located.mk ~loc (Longident.Lident "::"))
          (Some (ppat_tuple ~loc [x; plist' ~loc l]))
    in
  let assoc_pattern pat_list =
    match type_ with
    |`Open    -> [%pat? `Assoc [%p plist' ~loc pat_list]]
    | `Closed -> [%pat? `Assoc [%p Ast_builder.Default.plist ~loc pat_list]]
  in
  if List.length l > 4 then
    Raise.too_many_fields_in_record_pattern ~loc:ppat_loc
  else
    let pat_list = List.map expand_one l in
    let permutations = Utils.permutations pat_list in
    let permutations =
      match type_ with
      | `Closed -> permutations
      | `Open -> List.map (Utils.permutations_with_padding ~length:5 ~padding:[%pat? _]) permutations |> List.concat
    in
    let assoc_patterns = List.map assoc_pattern permutations in
    match assoc_patterns with
    | [] -> assert false
    | [single] -> single
    | hd::tl -> List.fold_left (fun acc elm -> [%pat? [%p acc] | [%p elm]]) hd tl
