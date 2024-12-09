(*
 * Note: This file is currently broken, since Reason removed
 * Reason_syntax_util.Error in favor of Reerror's `Printexc.to_string e`
*)

open Js_of_ocaml

module RE = Reason_toolchain.RE
module ML = Reason_toolchain.ML


let syntaxerr_error_to_string = function
  | Syntaxerr.Unclosed _ -> "Unclosed"
  | Expecting _ -> "Expecting"
  | Not_expecting _ -> "Not expecting"
  | Variable_in_scope _ -> "Variable in scope"
  | Ill_formed_ast _ -> "Ill formed ast"
  | Invalid_package_type _ -> "Invalid package type"
  | Removed_string_set _ -> "Removed string set"
  | Applicative_path _ -> "Applicative path"
  | _ -> "Unknown error"

let reason_error_to_string = function
  | Reason_errors.Lexing_error Illegal_character _e -> "Illegal character"
  | Lexing_error Illegal_escape _ -> "Illegal escape"
  | Lexing_error Unterminated_comment _ -> "Unterminated comment"
  | Lexing_error Unterminated_string -> "Unterminated string"
  | Lexing_error Unterminated_string_in_comment _ -> "Unterminated string in comment"
  | Lexing_error Keyword_as_label _ -> "Keyword as label"
  | Lexing_error Invalid_literal _ -> "Invalid literal"
  | Parsing_error _ -> "Parsing error"
  | Ast_error Not_expecting _ -> "Not expecting"
  | Ast_error Other_syntax_error _ -> "Other syntax error"
  | Ast_error Variable_in_scope _ -> "Variable in scope"
  | Ast_error Applicative_path _ -> "Applicative path"

let locationToJsObj (loc: Astlib.Location.t) =
  
  let (_file, start_line, start_char) = Location.get_pos_info loc.loc_start in
  let (_, end_line, end_char) = Location.get_pos_info loc.loc_end in
  (* The right way of handling ocaml syntax error locations. Do do this at home
    copied over from
    https://github.com/BuckleScript/bucklescript/blob/2ad2310f18567aa13030cdf32adb007d297ee717/jscomp/super_errors/super_location.ml#L73
  *)
  let normalizedRange =
    if start_char == -1 || end_char == -1 then
      (* happens sometimes. Syntax error for example *)
      None
    else if start_line = end_line && start_char >= end_char then
      (* in some errors, starting char and ending char can be the same. But
         since ending char was supposed to be exclusive, here it might end up
         smaller than the starting char if we naively did start_char + 1 to
         just the starting char and forget ending char *)
      let same_char = start_char + 1 in
      Some ((start_line, same_char), (end_line, same_char))
    else
      (* again: end_char is exclusive, so +1-1=0 *)
      Some ((start_line, start_char + 1), (end_line, end_char))
  in
  match normalizedRange with
  | None -> Js.undefined
  | Some ((start_line, start_line_start_char), (end_line, end_line_end_char)) ->
    let intToJsFloatToAny i =
      i |> float_of_int |> Js.number_of_float |> Js.Unsafe.inject
    in
    Js.def (Js.Unsafe.obj [|
      ("startLine", intToJsFloatToAny start_line);
      ("startLineStartChar", intToJsFloatToAny start_line_start_char);
      ("endLine", intToJsFloatToAny end_line);
      ("endLineEndChar", intToJsFloatToAny end_line_end_char)
    |])

let warningToJsObj (warning: Location.t) =
  Astlib.Location.{
    loc_start = warning.loc_start;
    loc_end = warning.loc_end;
    loc_ghost = warning.loc_ghost;
  }


let parseWith f code =
  let throwAnything = Js.Unsafe.js_expr "function(a) {throw a}" in
  try
    (code
    |> Lexing.from_string
    |> f)
  with
  (* from ocaml and reason *)
  | Syntaxerr.Error (err) ->
    let location: Location.t = Syntaxerr.location_of_error err in
    let jsLocation = locationToJsObj (warningToJsObj location) in
    let errorString = syntaxerr_error_to_string err in
    let jsError =
      Js.Unsafe.obj [|
        ("message", Js.Unsafe.inject (Js.string errorString));
        ("location", Js.Unsafe.inject jsLocation);
      |]
    in
    Js.Unsafe.fun_call throwAnything [|Js.Unsafe.inject jsError|]
  | Reason_errors.Reason_error (err, loc) ->
    let jsLocation = locationToJsObj loc in
    let errorString = reason_error_to_string err in
    let jsError =
      Js.Unsafe.obj [|
        ("message", Js.Unsafe.inject (Js.string errorString));
        ("location", Js.Unsafe.inject jsLocation);
      |]
    in
    Js.Unsafe.fun_call throwAnything [|Js.Unsafe.inject jsError|]

let ast_string exp =
  let config = Ppxlib.Pp_ast.Config.make () in
  Format.asprintf "%a" (Ppxlib.Pp_ast.structure ~config) exp

let parse f code =
  let structure, _ = parseWith f code in
  ast_string structure

let parse_reason code =
  parse RE.implementation_with_comments code

let parse_ocaml code =
  parse ML.implementation_with_comments code

let _ = Js.export "parseReason" parse_reason
let _ = Js.export "parseOcaml" parse_ocaml