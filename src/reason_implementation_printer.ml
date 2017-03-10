open Migrate_parsetree
open Ast_404

module Reason_implementation_printer : Printer_maker.PRINTER =
    struct
        type t = Parsetree.structure
        let err = Printer_maker.err

        (* Note: filename should only be used with .ml files. See reason_toolchain. *)
        let defaultImplementationParserFor use_stdin filename =
          let open Reason_toolchain in
          let (_parser, thing) =
            if Filename.check_suffix filename ".re"
            then (JS.canonical_implementation_with_comments, false)
            else if Filename.check_suffix filename ".ml"
            then (ML.canonical_implementation_with_comments, true)
            else err ("Cannot determine default implementation parser for filename '" ^ filename ^ "'.")
          in
          _parser (setup_lexbuf use_stdin filename), thing, false

        let parse filetype use_stdin filename =
            let ((ast, comments), parsedAsML, parsedAsInterface) =
            (match filetype with
            | `Auto -> defaultImplementationParserFor use_stdin filename
            | `BinaryReason -> Printer_maker.reasonBinaryParser use_stdin filename
            | `Binary -> Printer_maker.ocamlBinaryParser use_stdin filename
            | `ML ->
                    let lexbuf = Reason_toolchain.setup_lexbuf use_stdin filename in
                    let impl = Reason_toolchain.ML.canonical_implementation_with_comments in
                    (impl lexbuf, true, false)
            | `Reason ->
                    let lexbuf = Reason_toolchain.setup_lexbuf use_stdin filename in
                    let impl = Reason_toolchain.JS.canonical_implementation_with_comments in
                    (impl lexbuf, false, false))
            in
            if parsedAsInterface then
              err "The file parsed does not appear to be an implementation file."
            else if !Reason_config.add_printers then
              (* NB: Not idempotent. *)
              ((Printer_maker.str_ppx_show_runtime::ast, comments), parsedAsML)
            else
              ((ast, comments), parsedAsML)

        let print printtype filename parsedAsML output_chan output_formatter =
            match printtype with
            | `BinaryReason -> fun (ast, comments) -> (
              (* Our special format for interchange between reason should keep the
               * comments separate.  This is not compatible for input into the
               * ocaml compiler - only for input into another version of Reason. We
               * also store whether or not the binary was originally *parsed* as an
               * interface file.
               *)
              output_value output_chan (
                Config.ast_impl_magic_number, filename, ast, comments, parsedAsML, false
              );
            )
            | `Binary -> fun (ast, comments) -> (
               Ast_io.to_channel output_chan filename
                 (Ast_io.Impl ((module OCaml_current),
                               Reason_toolchain.To_current.copy_structure ast))
            )
            | `AST -> fun (ast, comments) -> (
              Printast.implementation output_formatter
                (Reason_toolchain.To_current.copy_structure ast)
            )
            (* If you don't wrap the function in parens, it's a totally different
             * meaning #thanksOCaml *)
            | `None -> (fun (ast, comments) -> ())
            | `ML -> Reason_toolchain.ML.print_canonical_implementation_with_comments output_formatter
            | `Reason -> Reason_toolchain.JS.print_canonical_implementation_with_comments output_formatter
    end;;
