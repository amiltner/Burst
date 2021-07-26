open CoreAndMore

module type IdGenerator =
sig
  val fresh : unit -> Id.t
end

module Make(D:Data)(IDG:IdGenerator)(FName : Singleton with type t = string) =
struct
  module D =
  struct
    include D
    let sexp_of_t _ = failwith "ah"
  end

  let dd = Hashtbl.create (module D)
  let idd = Hashtbl.create (module Id)

  let to_readable_string
    ()
    : string =
    let ids =
      List.sort
        ~compare:Id.compare
        (Hashtbl.keys
           idd)
    in
    let kvp_strings =
      List.map
        ~f:(fun k ->
            string_of_pair
              Id.to_string
              D.show
              (k,Hashtbl.find_exn idd k))
        ids
    in
    String.concat ~sep:"\n" kvp_strings

  let print () : unit =
    let s = to_readable_string () in
    SimpleFile.write_to_file ~fname:FName.value ~contents:s

  let get_id
      (d:D.t)
    : Id.t =
    let ido =
      Hashtbl.find
        dd
        d
    in
    begin match ido with
      | None ->
        let id = IDG.fresh () in
        Hashtbl.set
          ~key:d
          ~data:id
          dd;
        Hashtbl.set
          ~key:id
          ~data:d
          idd;
        if !Consts.print_mapping then print ();
        id
      | Some id -> id
    end

  let get_d
      (id:Id.t)
    : D.t =
    Hashtbl.find_exn
      idd
      id
end
