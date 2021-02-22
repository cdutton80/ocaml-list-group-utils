module List_group_utils :
sig
  (* Values that will be available outside of the module. *)
  val group : ('a -> 'b) -> 'a list -> 'a list list
  val group_by : ('a -> 'b) -> 'a list -> ('b * 'a list) list
  val group_by_named : 
    ('a -> 'b) -> ('a -> 'c) -> ('a list) -> ('c * 'a list) list
  val transformed_group_by_named : 
    ('a -> 'b) -> ('a -> 'c) -> ('a -> 'd) -> 'a list -> ('c * 'd list) list
  
  val strip_keys : ('a * 'b) list -> 'b list
   
  module Sort :
  sig
    val group :
      ('a -> 'b) ->
      ('b -> 'b -> int) ->
      ('a -> 'a -> int) -> 'a list -> 'a list list
    val flat_group : 
      ('a -> 'b) ->
      ('b -> 'b -> int) ->
      ('a -> 'a -> int) -> 'a list -> 'a list
    val group_by :
      ('a -> 'b) ->
      ('b -> 'b -> int) ->
      ('a -> 'a -> int) -> 'a list -> ('b * 'a list) list
    val group_by_named :
      ('a -> 'b) ->
      ('a -> 'c) ->
      ('b -> 'b -> int) ->
      ('a -> 'a -> int) -> 'a list -> ('c * 'a list) list
    val transformed_group_by_named :
      ('a -> 'b) ->
      ('a -> 'c) ->
      ('a -> 'd) ->
      ('b -> 'b -> int) ->
      ('d -> 'd -> int) -> 'a list -> ('c * 'd list) list
  end
end =
struct
  (* Implementation of the signature of this module. *)

  (* This module will use the List and Hashtbl modules from the standard 
     library. *)
  open List
  open Hashtbl

  (* An identity function. What goes in comes right out.
     This is handy because every function in this comes from a base utility 
     function. In some cases, we simply pass an identity function instead of a 
     meaningful one. *)
  let id x = x
  
  let strip_keys lst =
    List.map (fun (k, v) -> v) lst
  
  (* Implements the real logic of the module.
  
     First argument: Function that determines the value to group by.
     Second argument: The naming function. Determines the name given to each 
     group.
     Third argument: Transform function that is applied to the grouped items.
     Fourth and last argument: The input list of values to group. *)
  let base_fun f nf tf lst =
    (* We'll use a hashtable from the standard library to group items. 
       Hash tables are mutable. *)
    let ht = Hashtbl.create 123456 in
    (* Internal recursive helper function. Because the hashtable already exists, 
       this can be modified across calls to the helper function. *)
    let rec base_fun' f nf lst =
      match lst with
      (* When we get to the end of the list, return the hashtable. *)
      | [] -> ht
      (* Otherwise we're not at the end! *)
      | x :: xs ->
        (* The key for our hastable is a tuple formed by applying the function 
           to determine the value to group by and the naming function. The first 
           of these will later be discarded. *)
        let key = (f x, nf x) in
        (* We'll use exception handling to try to add the current value "x" to a 
           list containing the other values corresponding to the same key. If 
           nothing corresponds to that key yet, a Not_found exception is raised, 
           and we'll create an entry for that key with the current value "x". 
           The hashtable will contain a list ref so the list can be updated. *)
        (try
           let v = Hashtbl.find ht key in
           v.contents <- List.append v.contents [x]
         with Not_found -> 
           Hashtbl.add ht key (ref [x]));
        (* Once we have handled the hashtable update, run the same function on 
           the rest of the input list. *)
        base_fun' f nf xs in
    (* Call the helper and capture the resulting hashtable. *)
    let group_ht = base_fun' f nf lst in
    (* Fold the hashtable into a list.
       Opportunity to slim down to just the group name part of the key.
       Also each group's list of values is derefenced fom a "list ref" to a 
       "list". *)
    Hashtbl.fold (fun (k, nk) v acc -> (nk, !v |> List.map tf) :: acc) 
                 group_ht []
                 
  (* No group names. The list map removes the group name part of the tuple. *)
  let group f lst =
    lst |> base_fun f f id |> strip_keys 
    
  (* No special naming function. *)
  let group_by f lst =
    lst |> base_fun f f id 
    
  (* Uses the naming function but no transform on the values. *)
  let group_by_named f nf lst =
    lst |> base_fun f nf id
    
  (* Exposes the full functionality of the module. *)
  let transformed_group_by_named =
    base_fun
    
  (* Equivalent functions but designed with built-in sorting. 
     Two sort functions are passed to each. 
     The first sorts the keys that we group based on. Where we provide a naming 
     function to govern the final name given to each group, the sort happens 
     based on the raw grouping criteria. 
     The second sort function (sfi or "sort function internal") governs how the 
     list of values in each group is sorted. 
     
     As in the main module, implementation happens in a base_fun function which 
     is not directly exposed outside of the module. The signature to control 
     this is specified for the List_group_utils module above. *)
  module Sort =
  struct
    (* Very similar structure to base_fun, but we are keeping both the key and 
       named key around so we can sort on the key before stripping it out. *)
    let sort_base_fun f nf tf sf sfi lst =
      let ht = Hashtbl.create 123456 in
      let rec base_fun' f nf lst =
        match lst with
        | [] -> ht
        | x :: xs ->
          let key = (f x, nf x) in
          (try
             let v = Hashtbl.find ht key in
             v.contents <- List.append v.contents [x]
           with Not_found -> 
             Hashtbl.add ht key (ref [x]));
          base_fun' f nf xs in
          let group_ht = base_fun' f nf lst in
      Hashtbl.fold (fun (k, nk) v acc -> ((k, nk), !v |> List.map tf) :: acc) 
                   group_ht []
      |> List.sort (fun ((k1, _), _) ((k2, _), _) -> sf k1 k2)
      |> List.map (fun ((_, nk), lst) -> (nk, lst |> List.sort sfi))

    let group f sf sfi lst =
      sort_base_fun f f id lst |> strip_keys
      
    let flat_group f sf sfi lst =
      lst |> group f sf sfi |> List.flatten

    let group_by f sf sfi lst =
      sort_base_fun f f id sf sfi lst
      
    let group_by_named f nf sf sfi lst =
      sort_base_fun f nf id sf sfi lst
      
    let transformed_group_by_named =
      sort_base_fun
  end
end
