type header = {
  magic : string;
  (** Magic number of the marshaller *)
  length : int;
  (** Size on disk in bytes *)
  size32 : int;
  (** Size in words when loaded on 32-bit systems *)
  size64 : int;
  (** Size in words when loaded on 64-bit systems *)
  objects : int;
  (** Number of blocks defined in the marshalled structure *)
}
[@@deriving show { with_path = false }]

type address = int [@printer fun fmt v -> fprintf fmt "0x%x" v]
[@@deriving show { with_path = false }]

type digest = Digest.t [@printer fun fmt v -> fprintf fmt "Digest.of_hex \"%s\"" (Digest.to_hex v)]
[@@deriving show { with_path = false }]

type code =
| Code_int8                   [@value 0x00]
| Code_int16                  [@value 0x01]
| Code_int32                  [@value 0x02]
| Code_int64                  [@value 0x03]
| Code_shared8                [@value 0x04]
| Code_shared16               [@value 0x05]
| Code_shared32               [@value 0x06]
| Code_block32                [@value 0x08]
| Code_block64                [@value 0x13]
| Code_string8                [@value 0x09]
| Code_string32               [@value 0x0A]
| Code_double_big             [@value 0x0B]
| Code_double_little          [@value 0x0C]
| Code_double_array8_big      [@value 0x0D]
| Code_double_array8_little   [@value 0x0E]
| Code_double_array32_big     [@value 0x0F]
| Code_double_array32_little  [@value 0x07]
| Code_codepointer            [@value 0x10]
| Code_infixpointer           [@value 0x11]
| Code_custom                 [@value 0x12]
[@@deriving show { with_path = false }, enum]

type repr =
| Repr_int    of int
| Repr_block  of { tag: int; size: int }
| Repr_string of string
| Repr_ptr    of { offset: int }
| Repr_code   of { address: address; digest: digest }
[@@deriving show { with_path = false }]

type data =
| Data_invalid
| Data_int    of int
| Data_ptr    of { absolute: int }
| Data_atom   of { tag: int }
| Data_fun    of { address: address; digest: digest }
[@@deriving show { with_path = false }]

type obj =
| Obj_invalid
| Obj_struct  of { tag: int; block: data array }
| Obj_string  of string
[@@deriving show { with_path = false }]

type file = {
  header: header;
  memory: obj array;
  value:  data;
}
[@@deriving show { with_path = false }]

let input_header chan =
  let magic   = really_input_string chan 4 in
  let length  = input_binary_int chan in
  let objects = input_binary_int chan in
  let size32  = input_binary_int chan in
  let size64  = input_binary_int chan in
  { magic; length; size32; size64; objects }

let input_char chan =
  Char.chr (input_byte chan)

let input_string len chan =
  String.init len (fun _ -> input_char chan)

let input_unsigned n chan =
  let rec iter n v =
    if n = 0 then v
    else iter (n - 8) ((v lsl 8) lor input_byte chan)
  in iter n 0

let input_signed n chan =
  let v = input_unsigned n chan in
  if v land (1 lsl n - 1) = 0 then v
  else v lor ((-1) lsl n - 1)

let input_header32 chan =
  let i = input_byte chan in
  let j = input_byte chan in
  let k = input_byte chan in
  let l = input_byte chan in
  let tag = l in
  let size = (i lsl 14) lor (j lsl 6) lor (k lsr 2) in
  (tag, size)

let input_header64 chan =
  let i = input_byte chan in
  let j = input_byte chan in
  let k = input_byte chan in
  let l = input_byte chan in
  let m = input_byte chan in
  let n = input_byte chan in
  let o = input_byte chan in
  let p = input_byte chan in
  let tag = p in
  let size =
    (i lsl 46) lor (j lsl 38) lor (k lsl 30) lor (l lsl 22) lor
    (m lsl 14) lor (n lsl 6) lor (o lsr 2)
  in
  (tag, size)

let prefix_small_block  = 0x80
let prefix_small_int    = 0x40
let prefix_small_string = 0x20

let input_repr chan =
  let data = input_byte chan in
  if prefix_small_block <= data then
    let tag = data land 0x0F in
    let size = (data lsr 4) land 0x07 in
    Repr_block { tag; size }
  else if prefix_small_int <= data then
    Repr_int (data land 0x3F)
  else if prefix_small_string <= data then
    let size = data land 0x1F in
    Repr_string (input_string size chan)
  else if data > max_code then
    assert false
  else match code_of_enum data with
  | Some Code_int8 ->
    Repr_int (input_signed 8 chan)
  | Some Code_int16 ->
    Repr_int (input_signed 16 chan)
  | Some Code_int32 ->
    Repr_int (input_signed 32 chan)
  | Some Code_int64 ->
    Repr_int (input_signed 64 chan)
  | Some Code_shared8 ->
    Repr_ptr { offset = input_unsigned 8 chan }
  | Some Code_shared16 ->
    Repr_ptr { offset = input_unsigned 16 chan }
  | Some Code_shared32 ->
    Repr_ptr { offset = input_unsigned 32 chan }
  | Some Code_block32 ->
    let tag, size = input_header32 chan in
    Repr_block { tag; size }
  | Some Code_block64 ->
    let tag, size = input_header64 chan in
    Repr_block { tag; size }
  | Some Code_string8 ->
    let size = input_unsigned 8 chan in
    Repr_string (input_string size chan)
  | Some Code_string32 ->
    let size = input_unsigned 32 chan in
    Repr_string (input_string size chan)
  | Some Code_codepointer ->
    let address = input_unsigned 32 chan in
    let digest = Digest.input chan in
    Repr_code { address; digest }
  | Some Code_double_big
  | Some Code_double_little
  | Some Code_double_array8_big
  | Some Code_double_array8_little
  | Some Code_double_array32_big
  | Some Code_double_array32_little
  | Some Code_infixpointer
  | Some Code_custom
  | None ->
    Format.printf "Unhandled code %04x (%s)\n%!"
                  data ([%derive.show: code option] (code_of_enum data));
    assert false

let magic_number = "\132\149\166\190"

let input_file chan =
  let header = input_header chan in
  assert (header.magic = magic_number);
  let memory = Array.make header.objects Obj_invalid in
  let cursor = ref 0 in
  let fill_obj repr =
    match repr with
    | Repr_ptr { offset } ->
      let data = Data_ptr { absolute = !cursor - offset } in
      data, None
    | Repr_int value ->
      let data = Data_int value in
      data, None
    | Repr_string value ->
      let data = Data_ptr { absolute = !cursor } in
      Array.set memory !cursor (Obj_string value);
      incr cursor;
      data, None
    | Repr_block { tag; size = 0 } ->
      let data = Data_atom { tag } in
      data, None
    | Repr_block { tag; size } ->
      let data = Data_ptr { absolute = !cursor } in
      let content = Array.make size Data_invalid in
      Array.set memory !cursor (Obj_struct { tag; block = content });
      incr cursor;
      data, Some content
    | Repr_code { address; digest } ->
      let data = Data_fun { address; digest } in
      data, None
  in
  let rec fill_block block offset stack =
    if Array.length block = offset then
      match stack with
      | [] -> ()
      | (block, offset) :: stack ->
        fill_block block offset stack
    else
      let data, contents = fill_obj (input_repr chan) in
      block.(offset) <- data;
      let block, offset, stack =
        match contents with
        | None ->
          block, succ offset, stack
        | Some contents ->
          contents, 0, ((block, succ offset) :: stack)
      in
      fill_block block offset stack
  in
  let result = [|Data_invalid|] in
  fill_block result 0 [];
  { header; memory; value = result.(0) }
