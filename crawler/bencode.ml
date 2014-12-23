module Dict = Map.Make(String)

type t =
| String of string
| Int of int
| List of t list
| Dict of t Dict.t

let dict_of_list l = List.fold_left (fun d (k, v) -> Dict.add k v d) Dict.empty l

let encode x =
  let open Buffer in
  let buf = create 42 in
  let rec encode = function
  | String s -> begin
    s
    |> String.length
    |> string_of_int
    |> add_string buf;
    add_char buf ':';
    add_string buf s;
  end
  | Int n -> begin
    add_char buf 'i';
    n
    |> string_of_int
    |> add_string buf;
    add_char buf 'e';
  end
  | List l -> begin
    add_char buf 'l';
    List.iter encode l;
    add_char buf 'e';
  end
  | Dict d -> begin
    add_char buf 'd';
    d
    |> Dict.bindings
    |> List.iter (fun (k, v) -> encode (String k); encode v);
    add_char buf 'e';
  end
  in
  encode x;
  contents buf

exception Parse_error

let decode s =
  let pos = ref 0 in
  let rec parse_int () =
    let start = !pos in
    incr pos;
    while '0' <= s.[!pos] && s.[!pos] <= '9' do incr pos done;
    String.sub s start (!pos - start)
    |> int_of_string
  in
  let rec decode () =
    match s.[!pos] with
    | '-' | '0'..'9' -> begin
      let len = parse_int () in
      let start = !pos + 1 in
      pos := !pos + len + 1;
      String (String.sub s start len)
    end
    | 'i' -> begin
      incr pos;
      let n = parse_int () in
      incr pos;
      Int n
    end
    | 'l' -> begin
      incr pos;
      List (parse_list ())
    end
    | 'd' -> begin
      incr pos;
      Dict (parse_dict ())
    end
    | _ -> raise Parse_error
  and parse_list () =
    if s.[!pos] = 'e' then begin
      incr pos;
      []
    end else
      let h = decode () in
      h :: parse_list ()
  and parse_dict () =
    if s.[!pos] = 'e' then begin
      incr pos;
      Dict.empty
    end else
      match decode () with
      | String k ->
        let v = decode () in
        Dict.add k v (parse_dict ())
      | _ -> raise Parse_error
  in
  decode ()
