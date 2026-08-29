open Core
open Ppxlib
open Ast_builder.Default

(* Resolve the user-supplied path relative to the directory of the source file
   containing the extension point, rather than relative to the ppx process's
   working directory.  Dune runs ppx rewriters with the working directory set to
   the build-context root, which is the enclosing workspace root — not the
   package root.  A path resolved against the working directory therefore only
   works when the package happens to sit at the workspace root (as in the
   original monorepo); it breaks when the package is vendored under a
   subdirectory.  Resolving against the source file's directory makes embedded
   paths (e.g. [../foo/bar.js]) work identically whether the package is built
   standalone or nested inside another workspace. *)
let resolve_relative_to_source ~loc path =
  if Filename.is_absolute path
  then path
  else (
    let source_dir = Filename.dirname loc.loc_start.pos_fname in
    Filename.concat source_dir path)
;;

let file_path_to_absolute_string ~loc compile_time_file_path =
  let open (val Ast_builder.make loc) in
  let compile_time_file_path = resolve_relative_to_source ~loc compile_time_file_path in
  let entire_file = In_channel.with_file compile_time_file_path ~f:In_channel.input_all in
  estring entire_file
;;

let file_path_to_absolute_string_with_filename ~loc compile_time_file_path =
  let resolved_file_path = resolve_relative_to_source ~loc compile_time_file_path in
  let entire_file = In_channel.with_file resolved_file_path ~f:In_channel.input_all in
  [%expr
    [%e estring ~loc:{ loc with loc_ghost = true } compile_time_file_path]
    , [%e estring ~loc entire_file]]
;;

let embed_file_as_string =
  Extension.V3.declare
    "embed_file_as_string"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    (fun ~ctxt relative_file_path ->
      file_path_to_absolute_string
        ~loc:(Expansion_context.Extension.extension_point_loc ctxt)
        relative_file_path)
  |> Ppxlib.Context_free.Rule.extension
;;

let embed_file_as_string_with_filename =
  Extension.V3.declare
    "embed_file_as_string_with_filename"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    (fun ~ctxt relative_file_path ->
      file_path_to_absolute_string_with_filename
        ~loc:(Expansion_context.Extension.extension_point_loc ctxt)
        relative_file_path)
  |> Ppxlib.Context_free.Rule.extension
;;

let () =
  Driver.register_transformation
    ~rules:[ embed_file_as_string; embed_file_as_string_with_filename ]
    "ppx_embed_file"
;;
