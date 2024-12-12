open Lwt.Infix
open Cohttp_lwt_unix
open Yojson.Basic

let gpt2_url =
  "https://api-inference.huggingface.co/models/openai-community/gpt2"

(* let llama3_2_url =
   "https://api-inference.huggingface.co/models/meta-llama/Llama-3.2-1B" *)

let api_key = "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"

let headers =
  Cohttp.Header.of_list
    [
      ("Authorization", "Bearer " ^ api_key);
      ("Content-Type", "application/json");
      ("x-wait-for-model", "true");
    ]

(* TODO: use additional input params here. DOC:
   https://huggingface.co/docs/api-inference/tasks/text-generation *)
let payload prompt =
  `Assoc
    [
      ("inputs", `String prompt);
      ( "parameters",
        `Assoc
          [
            ("max_new_tokens", `Int 10);
            ("temperature", `Float 0.7);
            ("top_p", `Float 0.9);
            ("do_sample", `Bool true);
            ("seed", `Int (Random.int 10000));
          ] );
    ]

let post_request api_url prompt =
  let body = Yojson.Basic.to_string (payload prompt) in
  let uri = Uri.of_string api_url in
  let%lwt resp, body =
    Client.post ~headers ~body:(Cohttp_lwt.Body.of_string body) uri
  in
  let%lwt body_str = Cohttp_lwt.Body.to_string body in
  Lwt.return (Yojson.Basic.from_string body_str)

let extract_generated org_text gen_text =
  if String.starts_with ~prefix:org_text gen_text then
    String.sub gen_text (String.length org_text)
      (String.length gen_text - String.length org_text)
  else gen_text

(** [complete_sentence prompt] is the auto-completed sentence following
    [prompt]. *)
let complete_sentence prompt =
  print_endline ("Input to complete_sentence: " ^ prompt);
  let%lwt res_json = post_request gpt2_url prompt in
  match res_json with
  | `Assoc [ ("generated_text", `String result) ] ->
      Printf.printf "Response: \n%s\n" result;
      Lwt.return (extract_generated prompt result)
  | `List [ `Assoc [ ("generated_text", `String result) ] ] ->
      Printf.printf "Response: \n%s" result;
      Lwt.return (extract_generated prompt result)
  | _ ->
      print_endline
        ("Unexpected response: " ^ Yojson.Basic.pretty_to_string res_json);
      Lwt.return ""

let extract_first_word text =
  try
    let space = Str.regexp "[ \n\r\x0c\t]+" in
    let end_i = Str.search_forward space text 0 in
    if end_i = 0 then
      let next_start = Str.match_end () in
      let next_end = Str.search_forward space text next_start in
      String.sub text next_start (next_end - next_start)
    else String.sub text 0 end_i
  with Not_found -> text

(** [complete_next_word prompt] is the auto-completed next word following
    [prompt]. *)
let complete_next_word prompt =
  let%lwt res_json = post_request gpt2_url prompt in
  match res_json with
  | `List [ `Assoc [ ("generated_text", `String result) ] ] ->
      let gen_text = extract_generated prompt result in
      Lwt.return (extract_first_word gen_text)
  | _ -> Lwt.return ""
