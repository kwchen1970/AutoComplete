(* We take in a word <word>, webscrape the Merriam Webster dictionary page for
   that word: https://www.merriam-webster.com/dictionary/<word> and then
   retrieve all the content under "Synonym" card from inspect. *)

(* (* Import necessary libraries *) open Lwt open Cohttp open Cohttp_lwt_unix

   (* Function to fetch and print the HTML content from a URL *) let fetch_url
   url = let%lwt uri = Lwt.return (Uri.of_string url) in let%lwt response, body
   = Client.get uri in let%lwt body_string = Cohttp_lwt.Body.to_string body in
   Lwt_io.printf "Response body:\n%s\n" body_string

   (* Entry point of the program *) let () = let url = "https://example.com" in
   Lwt_main.run (fetch_url url)

   Add synonym words into a hashmap. *)
