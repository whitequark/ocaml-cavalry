(** Dynamically typed, marshal-safe values *)

(* This module implements dynamic types providing the following guarantees:
    * a freshly created tag is an unique physical object;
    * creating an equality witness is a single physical equality operation;
    * a value written with an `int tag` cannot be read as `string tag`,
      even by changing the type in the source code and recompiling;
    * marshalled representations of tags never collide;
    * a boxed marshalled object may be unmarshalled and used without any
      further action, and tag identity is preserved.

   And it achieves all these remarkable properties with just one set_field!
   (Without marshalling, it would be completely type-safe, too.)

   Here's how it works. Each time a statement like `type tag += Tag` is
   translated, the OCaml compiler inserts code amounting to:

     let _Tag = new_block Object_tag 2
     _Tag.0 := "Tag"
     _Tag.1 := %caml_fresh_oo_id ()

   Each time a statement like `let tag = Tag arg1` is translated, the compiler
   inserts code amounting to:

     let tag = new_block No_tag 2
     tag.0 := _Tag
     tag.1 := arg1
     (tag.2 := ...)

   Matching on extensible variants cannot be optimized due to their design
   and so by necessity, any expression like `match e with Tag _ -> x` is
   translated to:

     let e_tag = e.0
     if e_tag == _Tag then x

   So, the representation of `Box (Tag (fun () -> "string"), "foo")` is
   as follows:

   Box
   +-----+-------+
   | tag | value |---->"foo"
   +-----+-------+
     |
     |   Tag (the value)      fun () -> "string"
     |   +--------+-----+     +=============+------+-----+
     \-->| constr | key |---->| Closure_tag | code | env |---->unit
         +--------+-----+     +=============+------+-----+
           |                                  |
           |   Tag (the constructor)          v
           |   +============+------+-------+  +--------------+
           \-->| Object_tag | name | index |  | PUSHCONST... |
               +============+------+-------+  +--------------+
                              |      |
                              v      v
                              "Tag"  5 (e.g.)

   Because the heap object representing the constructor (ab)uses Object_tag,
   when the box is marshalled and unmarshalled, an entire new tree is created,
   and a fresh index is generated. Now the unmarshalled value cannot be
   unboxed, and the unmarshalled tag cannot be matched on, and it doesn't
   even compare equal to the original tag! What can we do about this?

   Well, there is one part of the tree that was not duplicated. That is,
   the closure code pointer. Let's use it in place of the constructor!
   To do this, we'll put the freshly created Tag constructor into a module
   that has nothing else, pack it as a first-class module, and mutate
   the module so that the constructor points to the code pointer instead.
   Let's also get rid of the inner box, since it would be redundant
   with the actual closure.

   The resulting representation would look something like this:

      Box
   +-----+-------+
   | tag | value |---->"foo"
   +-----+-------+
     |
     |   Tag (the "constructor")
     |   +--------------+
     \-->| PUSHCONST... |
         +--------------+

   "But lo!" you may exclaim. "This all seems very unsafe." Not so fast!
   The OCaml language has another feature, namely extension constructor
   rebinding, that in combination with functors pretty much pins down
   this specific runtime representation or its equivalent. Namely,
   it is legal to do:

     type tag += Foo.Tag

   where Foo.Tag is a constructor declared elsewhere. Foo may be a functor
   and Tag becomes available for rebinding again from whichever context
   this statement appears. So, OCaml is obliged to drag the tag around
   as an abstract value, cannot optimize matches, and can't do anything
   smarter than comparing this abstract value by physical equality.

   The only thing that *does* become unsafe here is calling
   Obj.extension_name (Obj.extension_constructor tag). Doing that will
   result in a crash. Don't do it.
*)

type key = unit -> string
type _ tag = ..

let code (f: key): Obj.t =
  Obj.field (Obj.repr f) 0

module KeyHashtbl = Hashtbl.Make(struct
  type t = key
  let equal a b = code a == code b
  let hash = Hashtbl.hash
end)
let key_set: unit KeyHashtbl.t = KeyHashtbl.create 0

type (_, _) eq =
| Eq : ('a, 'a) eq
| Ne : ('a, 'b) eq
type 't field = {
  key: key;
  tag: 't tag;
  eq:  'v. 'v tag -> ('t, 'v) eq;
}
type any = Any : _ field -> any
let create (type t) key: t field =
  let module M = struct
    module type S = sig
      type _ tag += Tag : t tag
    end
    module T = struct
      type _ tag += Tag : t tag
    end

    let () =
      if not (KeyHashtbl.mem key_set key) then
        KeyHashtbl.add key_set key ()
      else raise (Invalid_argument "Dyn.create");
      (* type safety added and removed here *)
      let repr_T = Obj.repr (module T : S) in
      Obj.set_field repr_T 0 (code key)

    let tag = T.Tag
    let eq (type v) (tag: v tag): (t, v) eq =
      match tag with
      | T.Tag -> Eq
      | _     -> Ne
  end in
  M.{ key; tag; eq }

exception Invalid_proj

type box = Box : 'v tag * 'v -> box
let inj : type t. t field -> t -> box =
  fun field value ->
  Box (field.tag, value)
let proj : type t. t field -> box -> t =
  fun field (Box (tag, value)) ->
  match field.eq tag with
  | Eq -> value
  | Ne -> raise Invalid_proj
