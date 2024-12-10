open Soup
open Lwt.Infix

(* Function to fetch the HTML body of a URL *)
let fetch_html url =
  Cohttp_lwt_unix.Client.get (Uri.of_string url) >>= fun (_, body) ->
  Cohttp_lwt.Body.to_string body

(* Function to extract synonyms from the page *)
let extract_synonyms html =
  let soup = Soup.parse html in
  (* Replace with the correct CSS selector after inspecting the page
     structure *)
  soup $$ ".thes-list .sim-list-scored" |> to_list
  |> List.map (fun synonym -> R.leaf_text synonym |> String.trim)

(* Main function to fetch and display synonyms *)
let () =
  let url = "https://www.merriam-webster.com/thesaurus/pig" in
  let html = Lwt_main.run (fetch_html url) in
  let synonyms = extract_synonyms html in
  Printf.printf "%i\n" (List.length synonyms)

(* List.iter (fun synonym -> Printf.printf "%s\n" synonym) synonyms *)
