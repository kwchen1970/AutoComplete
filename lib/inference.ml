open Lwt.Infix
open Cohttp_lwt_unix
open Yojson.Basic

let gpt2_url =
  "https://api-inference.huggingface.co/models/openai-community/gpt2"

(* let api_url =
   "https://api-inference.huggingface.co/models/meta-llama/Llama-3.2-1B" *)

let api_key = "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"

let headers =
  Cohttp.Header.of_list
    [
      ("Authorization", "Bearer " ^ api_key);
      ("Content-Type", "application/json");
    ]

let payload prompt = `Assoc [ ("inputs", `String prompt) ]

let post_request api_url prompt =
  let body = Yojson.Basic.to_string (payload prompt) in
  let uri = Uri.of_string api_url in
  let%lwt resp, body =
    Client.post ~headers ~body:(Cohttp_lwt.Body.of_string body) uri
  in
  let%lwt body_str = Cohttp_lwt.Body.to_string body in
  Lwt.return (Yojson.Basic.from_string body_str)

let complete_sentence prompt =
  let%lwt res_json = post_request gpt2_url prompt in
  match res_json with
  | `List [ `Assoc [ ("generated_text", `String result) ] ] -> Lwt.return result
  | _ -> Lwt.return ""
