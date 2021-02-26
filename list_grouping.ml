module List_grouping = 
struct
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
    
  type ('a, 'b, 'c) name_key_type = 
  | Same_as_key
  | Name_key of ('a -> 'b)
  | Modified_key of ('b -> 'c)
    
  module Sort =
  struct
    let sort_base_fun f nf tf sfs sfis lst =
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
      let raw_results = 
        Hashtbl.fold 
          (fun (k, nk) v acc -> ((k, nk), !v) :: acc) 
          group_ht 
          []
      in
      sfs 
        |> List.fold_left
             (fun results f -> results |> List.sort (fun ((k1, _), _) ((k2, _), _) ->  f k1 k2))
             raw_results
        |> List.map (fun ((_, nk), lst) -> (nk, lst))
        |> List.map (fun (nk, g) -> (nk, sfis |> List.fold_left (fun acc f -> List.sort f acc) g |> List.map tf))
        
    let group f sfs sfis lst =
      lst |> sort_base_fun f f id sfs sfis |> strip_keys
      
    let flat_group f sfs sfis lst =
      lst |> group f sfs sfis |> List.flatten
      
    let group_by f sfs sfis lst =
      lst |> sort_base_fun f f id sfs sfis
      
    let group_by_named f nf lst sfs sfis lst =
      lst |> sort_base_fun f nf id sfs sfis
      
    let transformed_group_by_named =
      sort_base_fun
  end
  
  module Labl_sort =
  struct
    let transformed_group_by_named 
      ~key ~name_key ~item_transform ~key_sort ~item_sort ~items =
      Sort.transformed_group_by_named 
        key 
        (match name_key with 
         | Same_as_key -> key 
         | Name_key f -> f
         | Modified_key f -> (fun x -> f (key x)))
        item_transform 
        key_sort 
        item_sort 
        items
        
    let group ~key ~key_sort ~item_sort ~items =
      transformed_group_by_named 
        ~key 
        ~name_key: Same_as_key 
        ~item_transform: id
        ~key_sort
        ~item_sort
        ~items
      |> strip_keys
      
    let flat_group ~key ~key_sort ~item_sort ~items =
      group ~key ~key_sort ~item_sort ~items |> List.flatten
      
    let group_by ~key ~key_sort ~item_sort ~items =
      transformed_group_by_named 
        ~key 
        ~name_key: Same_as_key 
        ~item_transform: id
        ~key_sort
        ~item_sort
        ~items
        
    let group_by_named ~key ~name_key ~key_sort ~item_sort ~items =
      transformed_group_by_named 
        ~key 
        ~name_key 
        ~item_transform: id
        ~key_sort
        ~item_sort
        ~items
  end
  
  module Iter =
  struct    
    let flat_iter_on_named_groups f lst =
      lst |> List.iter (fun (k, v) -> v |> List.iter (fun x -> f (k, x)))
    
    let flat_iteri_on_named_groups f lst =
      lst |> List.iteri (fun i1 (k, v) -> v |> List.iteri (fun i2 x -> f (i1, i2, k, x)))
    
    let flat_iteri2_on_named_groups f lst =
      let c = ref (-1) in
      lst |> List.iteri (fun i1 (k, v) -> v |> List.iteri (fun i2 x -> c := !c + 1; f (i1, i2, !c, k, x)))
    
    let flat_iterix_on_named_groups f lst =
      lst |> flat_iteri2_on_named_groups (fun (_, _, c, k, v) -> f (c, k, v))
  end

  
  let base_fun f nf tf lst =
    Sort.sort_base_fun f nf tf [] [] lst 
                 
  (* No group names. The list map removes the group name part of the tuple. *)
  let group f lst =
    lst |> base_fun f f id |> strip_keys
    
  let flat_group f lst =
    lst |> group f |> List.flatten
    
  (* No special naming function. *)
  let group_by f lst =
    lst |> base_fun f f id 
    
  (* Uses the naming function but no transform on the values. *)
  let group_by_named f nf lst =
    lst |> base_fun f nf id
    
  (* Exposes the full functionality of the module. *)
  let transformed_group_by_named =
    base_fun
end
