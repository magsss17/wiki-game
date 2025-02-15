open! Core

(* [get_linked_articles] should return a list of wikipedia article lengths
   contained in the input.

   Note that [get_linked_articles] should ONLY return things that look like
   wikipedia articles. In particular, we should discard links that are: -
   Wikipedia pages under special namespaces that are not articles (see
   https://en.wikipedia.org/wiki/Wikipedia:Namespaces) - other Wikipedia
   internal URLs that are not articles - resources that are external to
   Wikipedia - page headers

   One nice think about Wikipedia is that stringent content moderation
   results in uniformity in article format. We can expect that all Wikipedia
   article links parsed from a Wikipedia page will have the form
   "/wiki/<TITLE>". *)
let get_linked_articles contents : string list =
  let open Soup in
  parse contents
  $$ "a[href]"
  |> to_list
  |> List.map ~f:(fun a -> R.attribute "href" a)
  |> List.filter ~f:(fun url ->
       match Wikipedia_namespace.namespace url with
       | Some _ -> false
       | None -> String.is_prefix ~prefix:"/wiki/" url)
  |> Set.of_list (module String)
  |> Set.to_list
;;

let print_links_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Print all of the valid wiki page links on a page"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_linked_articles contents) ~f:print_endline]
;;

(* [visualize] should explore all linked articles up to a distance of
   [max_depth] away from the given [origin] article, and output the result as
   a DOT file. It should use the [how_to_fetch] argument along with
   [File_fetcher] to fetch the articles so that the implementation can be
   tested locally on the small dataset in the ../resources/wiki directory. *)

module G = Graph.Imperative.Graph.Concrete (String)

module Dot = Graph.Graphviz.Dot (struct
  include G

  let edge_attributes _ = [ `Dir `Back ]
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
  let vertex_name v = v
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)

let get_linked_articles_wrapper ~node ~how_to_fetch =
  let abs_url =
    match how_to_fetch with
    | File_fetcher.How_to_fetch.Local _ -> node
    | Remote -> "https://en.wikipedia.org" ^ node
  in
  let contents = File_fetcher.fetch_exn how_to_fetch ~resource:abs_url in
  get_linked_articles contents
;;

let chop_article article =
  Str.global_replace (Str.regexp {|/|}) "" article
  |> Str.global_replace (Str.regexp {|wiki|}) ""
  |> Str.global_replace (Str.regexp {|(|}) ""
  |> Str.global_replace (Str.regexp {|)|}) ""
;;

let chop_articles articles : string list =
  List.map articles ~f:(fun article -> chop_article article)
;;

let rec create_graph ~graph ~node ~visited ~depth ~how_to_fetch =
  let visited = Set.add visited node in
  if depth >= 0
  then
    List.fold
      (get_linked_articles_wrapper ~node ~how_to_fetch)
      ~init:visited
      ~f:(fun acc v ->
      G.add_edge graph (chop_article node) (chop_article v);
      if not (Set.mem visited v)
      then
        create_graph
          ~graph
          ~node:v
          ~visited:acc
          ~depth:(depth - 1)
          ~how_to_fetch
      else acc)
  else visited
;;

let visualize ?(max_depth = 3) ~origin ~output_file ~how_to_fetch () : unit =
  let graph = G.create () in
  let visited = String.Set.of_list [] in
  let _ =
    create_graph ~graph ~node:origin ~visited ~depth:max_depth ~how_to_fetch
  in
  Dot.output_graph
    (Out_channel.create (File_path.to_string output_file))
    graph
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        visualize ~max_depth ~origin ~output_file ~how_to_fetch ();
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

(* [find_path] should attempt to find a path between the origin article and
   the destination article via linked articles.

   [find_path] should use the [how_to_fetch] argument along with
   [File_fetcher] to fetch the articles so that the implementation can be
   tested locally on the small dataset in the ../resources/wiki directory.

   [max_depth] is useful to limit the time the program spends exploring the
   graph. *)

let bfs start_node depth end_node ~how_to_fetch =
  let visited = String.Set.of_list [] in
  let parent = String.Map.empty in
  let rec traverse ~to_visit ~visited ~parent ~depth =
    match depth, to_visit with
    | 0, _ | _, [] -> None
    | _, hd :: tl ->
      if not (Set.mem visited hd)
      then
        if String.equal hd end_node
        then Some parent
        else (
          let visited = Set.add visited hd in
          let children =
            get_linked_articles_wrapper ~how_to_fetch ~node:hd
          in
          let parent =
            List.fold children ~init:parent ~f:(fun acc child ->
              if not (Map.mem acc child)
              then Map.add_exn acc ~key:child ~data:hd
              else acc)
          in
          traverse ~to_visit:(tl @ children) ~visited ~parent ~depth)
      else traverse ~to_visit:tl ~visited ~parent ~depth
  in
  traverse ~to_visit:[ start_node ] ~visited ~parent ~depth
;;

let backtrack_map map start dest : string list =
  let rec helper curr_node list =
    if String.equal curr_node start
    then chop_articles (curr_node :: list)
    else helper (Map.find_exn map curr_node) (curr_node :: list)
  in
  helper dest []
;;

let find_path ?(max_depth = 3) ~origin ~destination ~how_to_fetch () =
  let origin' = String.drop_prefix origin 24 in
  let destination' = String.drop_prefix destination 24 in
  let parent_map = bfs origin' max_depth destination' ~how_to_fetch in
  match parent_map with
  | None -> None
  | Some map -> Some (backtrack_map map origin' destination')
;;

let find_path_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "Play wiki game by finding a link between the origin and destination \
       pages"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and destination =
        flag "destination" (required string) ~doc:" the destination page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      in
      fun () ->
        match find_path ~max_depth ~origin ~destination ~how_to_fetch () with
        | None -> print_endline "No path found!"
        | Some trace -> List.iter trace ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"wikipedia game commands"
    [ "print-links", print_links_command
    ; "visualize", visualize_command
    ; "find-path", find_path_command
    ]
;;
