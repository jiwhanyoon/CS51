(**********************************************************************
 * CS51 Problem Set 5, 2016 - Moogle
 * crawl.ml: the crawler, which builds a dictionary from words to sets
 *           of links
 **********************************************************************)

(* Rename modules for convenience *)
module CS = Crawler_services ;;
module PR = Pagerank ;; 

(* Specify the web page ranking method to be used. If you do the
   Challenge section 6, you'll want to modify this. *)
module MoogleRanker
  = PR.InDegreeRanker (PR.PageGraph) (PR.PageScore)
  (*
     = PR.RandomWalkRanker (PR.PageGraph) (PR.PageScore) (struct
         let do_random_jumps = Some 0.20
         let num_steps = 1000
       end)
  *)

  (*
     = PR.QuantumRanker (PR.PageGraph) (PR.PageScore) (struct
         let alpha = 0.01
         let num_steps = 1
         let debug = true
       end) 
  *)

(* Dictionaries mapping words (strings) to sets of crawler links *)
module WordDict = Dict.Make(
  struct
    type key = string
    type value = PR.LinkSet.set
    let compare = Order.string_compare
    let string_of_key = (fun s -> s)
    let string_of_value = PR.LinkSet.string_of_set

    (* These functions are for testing purposes *)
    let gen_key () = ""
    let gen_key_gt _ () = gen_key ()
    let gen_key_lt _ () = gen_key ()
    let gen_key_random () = gen_key ()
    let gen_key_between _ _ () = None
    let gen_value () = PR.LinkSet.empty
    let gen_pair () = (gen_key(),gen_value())
  end)

(* A query module that uses LinkSet and WordDict *)
module Q = Query.Query(
  struct
    module S = PR.LinkSet
    module D = WordDict
  end)

(***********************************************************************)
(*    Section 1: CRAWLER                                               *)
(***********************************************************************)

(* TODO: Replace the implementation of the crawl function (currently
   just a stub returning the empty dictionary) with a proper index of
   crawled pages. Build an index as follows:
 
   Remove a link from the frontier (the set of links that have yet to
   be visited), visit this link, add its outgoing links to the
   frontier, and update the index so that all words on this page are
   mapped to linksets containing this url.
 
   Keep crawling until we've reached the maximum number of links (n) or
   the frontier is empty. 
 *)

let rec crawl (n:int)
    (frontier: PR.LinkSet.set)
    (visited : PR.LinkSet.set)
    (d:WordDict.dict) : WordDict.dict =
    (*first make sure the number or the frontier isn't empty*)
    if n = 0 then d 
    else
    match (PR.LinkSet.choose frontier) with
      | None -> d
      | Some (lnk, _ ) -> 
        
        (* if i have visited the link before, 
        remove it from the frontier and move on *)
        if (PR.LinkSet.member visited lnk) then 
          crawl (n) (PR.LinkSet.remove lnk frontier) visited d 
        
        else
          
          (* since we need the info on the link, 
          change it to a page, in which we can extract data from *)
          (match (CS.get_page lnk) with
            
            (* not convertible? remove and insert accordingly and move on *)
            | None -> crawl (n) (PR.LinkSet.remove lnk frontier)
             (PR.LinkSet.insert lnk visited) d

            (* convertible to page? make the actual dictionary *)
            | Some page -> 
              (crawl 
              (n-1) 
              (  List.fold_left (fun x y -> PR.LinkSet.insert y x) 
                 (PR.LinkSet.remove lnk frontier) page.links  )
                
                (PR.LinkSet.insert lnk visited)
                
                ( List.fold_left (fun dict word -> 
                  match WordDict.lookup dict word with
                    | Some x -> WordDict.insert dict word 
                                (PR.LinkSet.insert page.url x)
                    | None -> WordDict.insert dict word 
                              (PR.LinkSet.singleton page.url))
                              d page.words  )))
;;

let crawler () =
  crawl CS.num_pages_to_search
  (PR.LinkSet.singleton CS.initial_link)
  PR.LinkSet.empty
  WordDict.empty ;;



(* Debugging note: if you set debug=true in moogle.ml, it will print
   out your index after crawling. *)

  
(* If you want to test your crawler,
 * uncomment the following tests and run them. *)


let crawler_tests () = 
  let link_present crawler key link = 
    match WordDict.lookup crawler key with
    | Some v -> PR.LinkSet.member v {host = ""; port = 80; path = link}
    | _ -> false in 
  let i = crawler () in 
  (link_present i "DOCTYPE" "./simple-html/cow.html") 
  && (link_present i "DOCTYPE" "./simple-html/index.html") 
  && (link_present i "DOCTYPE" "./simple-html/moo.html") 
  && (link_present i "DOCTYPE" "./simple-html/ocaml.html") 
  && (link_present i "DOCTYPE" "./simple-html/functor.html") 
  && (link_present i "DOCTYPE" "./simple-html/42.html") 
  && (link_present i "DOCTYPE" "./simple-html/abstraction.html") 
  && not (link_present i "universe" "./simple-html/cow.html") 
  && not (link_present i "universe" "./simple-html/index.html") 
  && not (link_present i "universe" "./simple-html/moo.html") 
  && not (link_present i "universe" "./simple-html/ocaml.html") 
  && not (link_present i "universe" "./simple-html/functor.html") 
  && not (link_present i "universe" "./simple-html/abstraction.html") 
  && (link_present i "universe" "./simple-html/42.html") ;;
crawler_tests () ;;
