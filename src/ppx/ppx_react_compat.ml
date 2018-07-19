module P = Ppx_react (* make sure we link this module *)

let () = Migrate_parsetree_driver.run_as_ppx_rewriter ()
