
let pp_lambda' = Printlambda.lambda

let classify_lambda l =
  let open Lambda in
  match l with
  | Lvar _ -> "Lvar"
  | Lconst _ -> "Lconst"
  | Lapply _ -> "Lapply"
  | Lfunction _ -> "Lfunction"
  | Llet _ -> "Llet"
  | Lletrec _ -> "Lletrec"
  | Lprim _ -> "Lprim"
  | Lswitch _ -> "Lswitch"
  | Lstringswitch _ -> "Lstringswitch"
  | Lstaticraise _ -> "Lstaticraise"
  | Lstaticcatch _ -> "Lstaticcatch"
  | Ltrywith _ -> "Ltrywith"
  | Lifthenelse _ -> "Lifthenelse"
  | Lsequence _ -> "Lsequence"
  | Lwhile _ -> "Lwhile"
  | Lfor _ -> "Lfor"
  | Lassign _ -> "Lassign"
  | Lsend _ -> "Lsend"
  | Levent _ -> "Levent"
  | Lifused _ -> "Lifused"


let example1 = {|
  type point = {
    x: option(float),
    y: float
  };

  let force = fun
    | Some(x) => x
    | None => invalid_arg("no");

  let someFunc = ({x} as point, num) => {
    let x = force(point.x) +. num;
    switch (Some(x)) {
    | Some(something) => something +. num
    | None => point.y
    };
  };

  print_float(someFunc({ x: Some(42.0), y: 0.0}, 100.0));
|}

let example2 = {|
  let sum = (x, y) => x + y;
|}


let primitives = {|
func add_int(x interface{}, y interface{}) int {
  return x.(int) + y.(int)
}
|}


let parse code =
  code
  |> Lexing.from_string
  |> Reason_toolchain.RE.implementation_with_comments


let boxed_integer_name = function
  | Lambda.Pnativeint -> "nativeint"
  | Pint32 -> "int32"
  | Pint64 -> "int64"

let field_kind = function
  | Lambda.Pgenval -> "*"
  | Pintval -> "int"
  | Pfloatval -> "float"
  | Pboxedintval bi -> boxed_integer_name bi


let block_shape ppf shape = match shape with
  | None | Some [] -> ()
  | Some l when List.for_all ((=) Lambda.Pgenval) l -> ()
  | Some [elt] ->
      Format.fprintf ppf " (%s)" (field_kind elt)
  | Some (h :: t) ->
      Format.fprintf ppf " (%s" (field_kind h);
      List.iter (fun elt ->
          Format.fprintf ppf ",%s" (field_kind elt))
        t;
      Format.fprintf ppf ")"

let pp_primitive formatter p0 =
  let pr format = Fmt.pf formatter format in
  let open Lambda in
  match p0 with
  | Pidentity -> pr "id"
  | Pbytes_to_string -> pr "bytes_to_string"
  | Pbytes_of_string -> pr "bytes_of_string"
  | Pignore -> pr "ignore"
  | Prevapply -> pr "revapply"
  | Pdirapply -> pr "dirapply"
  (* | Ploc kind -> pr "%s" (string_of_loc_kind kind) *)
  | Pgetglobal id -> pr "global %a" Ident.print id
  | Psetglobal id -> pr "setglobal %a" Ident.print id
  | Pmakeblock(tag, Immutable, shape) ->
      pr "makeblock %i%a" tag block_shape shape
  (* | Pmakeblock(tag, Mutable, shape) -> *)
  (*     pr "makemutable %i%a" tag block_shape shape *)
  | Pfield n -> pr "field %i" n
  (* | Psetfield(n, ptr, init) -> *)
  (*     let instr = *)
  (*       match ptr with *)
  (*       | Pointer -> "ptr" *)
  (*       | Immediate -> "imm" *)
  (*     in *)
  (*     let init = *)
  (*       match init with *)
  (*       | Initialization -> "(init)" *)
  (*       | Assignment -> "" *)
  (*     in *)
  (*     pr "setfield_%s%s %i" instr init n *)
  | Pfloatfield n -> pr "floatfield %i" n
  (* | Psetfloatfield (n, init) -> *)
  (*     let init = *)
  (*       match init with *)
  (*       | Initialization -> "(init)" *)
  (*       | Assignment -> "" *)
  (*     in *)
  (*     pr "setfloatfield%s %i" init n *)
  (* | Pduprecord (rep, size) -> pr "duprecord %a %i" record_rep rep size *)
  | Plazyforce -> pr "force"
  | Pccall p -> pr "%s" p.prim_name
  | Praise k -> pr "%s" (Lambda.raise_kind k)
  | Psequand -> pr "&&"
  | Psequor -> pr "||"
  | Pnot -> pr "not"
  | Pnegint -> pr "~"
  | Paddint -> pr "add_int"
  | Psubint -> pr "-"
  | Pmulint -> pr "*"
  | Pdivint Safe -> pr "/"
  | Pdivint Unsafe -> pr "/u"
  | Pmodint Safe -> pr "mod"
  | Pmodint Unsafe -> pr "mod_unsafe"
  | Pandint -> pr "and"
  | Porint -> pr "or"
  | Pxorint -> pr "xor"
  | Plslint -> pr "lsl"
  | Plsrint -> pr "lsr"
  | Pasrint -> pr "asr"
  | Pintcomp(Ceq) -> pr "=="
  | Pintcomp(Cneq) -> pr "!="
  | Pintcomp(Clt) -> pr "<"
  | Pintcomp(Cle) -> pr "<="
  | Pintcomp(Cgt) -> pr ">"
  | Pintcomp(Cge) -> pr ">="
  | Poffsetint n -> pr "%i+" n
  | Poffsetref n -> pr "+:=%i"n
  | Pintoffloat -> pr "int_of_float"
  | Pfloatofint -> pr "float_of_int"
  | Pnegfloat -> pr "~."
  | Pabsfloat -> pr "abs."
  | Paddfloat -> pr "add_float"
  | Psubfloat -> pr "sub_float"
  | Pmulfloat -> pr "mul_float"
  | Pdivfloat -> pr "div_float"
  | Pfloatcomp(Ceq) -> pr "==."
  | Pfloatcomp(Cneq) -> pr "!=."
  | Pfloatcomp(Clt) -> pr "<."
  | Pfloatcomp(Cle) -> pr "<=."
  | Pfloatcomp(Cgt) -> pr ">."
  | Pfloatcomp(Cge) -> pr ">=."
  | Pstringlength -> pr "string.length"
  | Pstringrefu -> pr "string.unsafe_get"
  | Pstringrefs -> pr "string.get"
  | Pbyteslength -> pr "bytes.length"
  | Pbytesrefu -> pr "bytes.unsafe_get"
  | Pbytessetu -> pr "bytes.unsafe_set"
  | Pbytesrefs -> pr "bytes.get"
  | Pbytessets -> pr "bytes.set"

  (* | Parraylength k -> pr "array.length[%s]" (array_kind k) *)
  (* | Pmakearray (k, Mutable) -> pr "makearray[%s]" (array_kind k) *)
  (* | Pmakearray (k, Immutable) -> pr "makearray_imm[%s]" (array_kind k) *)
  (* | Pduparray (k, Mutable) -> pr "duparray[%s]" (array_kind k) *)
  (* | Pduparray (k, Immutable) -> pr "duparray_imm[%s]" (array_kind k) *)
  (* | Parrayrefu k -> pr "array.unsafe_get[%s]" (array_kind k) *)
  (* | Parraysetu k -> pr "array.unsafe_set[%s]" (array_kind k) *)
  (* | Parrayrefs k -> pr "array.get[%s]" (array_kind k) *)
  (* | Parraysets k -> pr "array.set[%s]" (array_kind k) *)
  | Pctconst c ->
     let const_name = match c with
       | Big_endian -> "big_endian"
       | Word_size -> "word_size"
       | Int_size -> "int_size"
       | Max_wosize -> "max_wosize"
       | Ostype_unix -> "ostype_unix"
       | Ostype_win32 -> "ostype_win32"
       | Ostype_cygwin -> "ostype_cygwin"
       | Backend_type -> "backend_type" in
     pr "sys.constant_%s" const_name
  | Pisint -> pr "isint"
  | Pisout -> pr "isout"
  | Pbittest -> pr "testbit"
  (* | Pbintofint bi -> print_boxed_integer "of_int" ppf bi *)
  (* | Pintofbint bi -> print_boxed_integer "to_int" ppf bi *)
  (* | Pcvtbint (bi1, bi2) -> print_boxed_integer_conversion ppf bi1 bi2 *)
  (* | Pnegbint bi -> print_boxed_integer "neg" ppf bi *)
  (* | Paddbint bi -> print_boxed_integer "add" ppf bi *)
  (* | Psubbint bi -> print_boxed_integer "sub" ppf bi *)
  (* | Pmulbint bi -> print_boxed_integer "mul" ppf bi *)
  (* | Pdivbint { size = bi; is_safe = Safe } -> *)
  (*     print_boxed_integer "div" ppf bi *)
  (* | Pdivbint { size = bi; is_safe = Unsafe } -> *)
  (*     print_boxed_integer "div_unsafe" ppf bi *)
  (* | Pmodbint { size = bi; is_safe = Safe } -> *)
  (*     print_boxed_integer "mod" ppf bi *)
  (* | Pmodbint { size = bi; is_safe = Unsafe } -> *)
  (*     print_boxed_integer "mod_unsafe" ppf bi *)
  (* | Pandbint bi -> print_boxed_integer "and" ppf bi *)
  (* | Porbint bi -> print_boxed_integer "or" ppf bi *)
  (* | Pxorbint bi -> print_boxed_integer "xor" ppf bi *)
  (* | Plslbint bi -> print_boxed_integer "lsl" ppf bi *)
  (* | Plsrbint bi -> print_boxed_integer "lsr" ppf bi *)
  (* | Pasrbint bi -> print_boxed_integer "asr" ppf bi *)
  (* | Pbintcomp(bi, Ceq) -> print_boxed_integer "==" ppf bi *)
  (* | Pbintcomp(bi, Cneq) -> print_boxed_integer "!=" ppf bi *)
  (* | Pbintcomp(bi, Clt) -> print_boxed_integer "<" ppf bi *)
  (* | Pbintcomp(bi, Cgt) -> print_boxed_integer ">" ppf bi *)
  (* | Pbintcomp(bi, Cle) -> print_boxed_integer "<=" ppf bi *)
  (* | Pbintcomp(bi, Cge) -> print_boxed_integer ">=" ppf bi *)
  (* | Pbigarrayref(unsafe, _n, kind, layout) -> *)
  (*     print_bigarray "get" unsafe kind ppf layout *)
  (* | Pbigarrayset(unsafe, _n, kind, layout) -> *)
  (*     print_bigarray "set" unsafe kind ppf layout *)
  | Pbigarraydim(n) -> pr "Bigarray.dim_%i" n
  | Pstring_load_16(unsafe) ->
     if unsafe then pr "string.unsafe_get16"
     else pr "string.get16"
  | Pstring_load_32(unsafe) ->
     if unsafe then pr "string.unsafe_get32"
     else pr "string.get32"
  | Pstring_load_64(unsafe) ->
     if unsafe then pr "string.unsafe_get64"
     else pr "string.get64"
  | Pstring_set_16(unsafe) ->
     if unsafe then pr "string.unsafe_set16"
     else pr "string.set16"
  | Pstring_set_32(unsafe) ->
     if unsafe then pr "string.unsafe_set32"
     else pr "string.set32"
  | Pstring_set_64(unsafe) ->
     if unsafe then pr "string.unsafe_set64"
     else pr "string.set64"
  | Pbigstring_load_16(unsafe) ->
     if unsafe then pr "bigarray.array1.unsafe_get16"
     else pr "bigarray.array1.get16"
  | Pbigstring_load_32(unsafe) ->
     if unsafe then pr "bigarray.array1.unsafe_get32"
     else pr "bigarray.array1.get32"
  | Pbigstring_load_64(unsafe) ->
     if unsafe then pr "bigarray.array1.unsafe_get64"
     else pr "bigarray.array1.get64"
  | Pbigstring_set_16(unsafe) ->
     if unsafe then pr "bigarray.array1.unsafe_set16"
     else pr "bigarray.array1.set16"
  | Pbigstring_set_32(unsafe) ->
     if unsafe then pr "bigarray.array1.unsafe_set32"
     else pr "bigarray.array1.set32"
  | Pbigstring_set_64(unsafe) ->
     if unsafe then pr "bigarray.array1.unsafe_set64"
     else pr "bigarray.array1.set64"
  | Pbswap16 -> pr "bswap16"
  (* | Pbbswap(bi) -> print_boxed_integer "bswap" ppf bi *)
  | Pint_as_pointer -> pr "int_as_pointer"
  | Popaque -> pr "opaque"
  | p -> Fmt.pr "XXX: %s@." (Printlambda.name_of_primitive p)


let rec struct_const formatter const =
  let pr format = Fmt.pf formatter format in
  let open Lambda in
  match const with
  | Const_base(Const_int n) -> pr "%i" n
  | Const_base(Const_char c) -> pr "%C" c
  | Const_base(Const_string (s, _)) -> pr "%S" s
  | Const_immstring s -> pr "#%S" s
  | Const_base(Const_float f) -> pr "%s" f
  | Const_base(Const_int32 n) -> pr "%lil" n
  | Const_base(Const_int64 n) -> pr "%LiL" n
  | Const_base(Const_nativeint n) -> pr "%nin" n
  | Const_pointer n -> pr "%ia" n
  | Const_block(tag, []) ->
      pr "[%i]" tag
  | Const_block(tag, sc1::scl) ->
      let sconsts = Fmt.(list ~sep:(unit "@ ") struct_const) in
      pr "@[<1>[%i:@ @[%a%a@]]@]" tag struct_const sc1 sconsts scl
  | Const_float_array [] ->
      pr "[| |]"
  (* | Const_float_array (f1 :: fl) -> *)
  (*     let floats ppf fl = *)
  (*       List.iter (fun f -> pr "@ %s" f) fl in *)
  (*     pr "@[<1>[|@[%s%a@]|]@]" f1 floats fl *)
  | c -> Fmt.pr "XXX: %a@." Printlambda.structured_constant c


let rec pp_lambda formatter l0 =
  let pr format = Fmt.pf formatter format in
  let open Lambda in
  match l0 with
  | Lsequence ((Lconst (Const_pointer _n) as l1), l2) ->
    Fmt.pr "Skipping: %s %a@." (classify_lambda l1) pp_lambda' l1;
    pp_lambda formatter l2

  | Lsequence (l1, l2) ->
    pr "{{{@.%a@,%a@,}}}"
      pp_lambda l1
      pp_lambda l2

  | Llet (_kind, _val_kind, id, Lfunction { params; body = fbody; _ }, _lbody) ->
    pr "@[<v4>func %s(%a) interface{} {@,%a@]@,}@."
      (Ident.name id)
      Fmt.(list ~sep:(unit ", ") string) (List.map (fun p -> Ident.name p ^ " interface{}") params)
      pp_lambda fbody

  | Llet (_kind, _val_kind, id, l1, l2) ->
    pr "@[<v4>{@,%s := %a;@,%a@]@,}"
      (Ident.name id)
      pp_lambda l1
      pp_lambda l2

  | Lprim (Pfield n, ls, _loc) ->
    pr "field(%d, %a)" n Fmt.(list ~sep:(unit ", ") pp_lambda) ls

  | Lprim (prim, ls, _loc) ->
    pr "%a(%a)"
      pp_primitive prim
      Fmt.(list ~sep:(unit ", ") pp_lambda) ls

  | Lvar id ->
    pr "%s" (Ident.name id)

  | Lconst c ->
    struct_const formatter c

  | Lifthenelse(cond, true_branch, false_branch) ->
    pr "var if_res interface{}@,";
    pr "@[<v4>if %a {@,if_res = %a@]@,@[<v4>} else {@,if_res = %a@]@,}"
      pp_lambda cond
      pp_lambda true_branch
      pp_lambda false_branch

  | Lapply {ap_func; ap_args; _} ->
    pr "apply(%a, %a)" pp_lambda ap_func Fmt.(list ~sep:(unit ", ") pp_lambda) ap_args

  | _ -> pr "XXX: %s@." (classify_lambda l0)


open Migrate_parsetree

module To_current = Convert(OCaml_404)(OCaml_current)

let main () =
  Compmisc.init_path false;
  let module_name = "Hello" in
  let env = Compmisc.initial_env () in
  Env.set_unit_name module_name;

  let (untyped_structure, _comments) = parse example1 in
  let untyped_structure = To_current.copy_structure untyped_structure in
  let (typed_structure, _signature, _env') = Typemod.type_toplevel_phrase env untyped_structure in
  typed_structure
    |> Translmod.transl_toplevel_definition
    |> Simplif.simplify_lambda ""
    |> Fmt.pr "%s@.%a@." primitives pp_lambda


let () =
  Printexc.record_backtrace true;
  try main () with
  | Typetexp.Error(_loc, env, err) ->
    Typetexp.report_error env Format.std_formatter err
  | Typecore.Error(_loc, env, err) ->
    Typecore.report_error env Format.std_formatter err
  | Reason_syntax_util.Error (_loc, Syntax_error err) ->
    Fmt.pr "Syntax error: %s@." err
  | exn ->
    Fmt.pr "Syntax error: %a@." Fmt.exn exn


