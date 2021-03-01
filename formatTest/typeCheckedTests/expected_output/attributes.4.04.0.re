[@reason.version 3.7];
/* Pexp_letexception with attributes */
let () = {
  [@attribute]
  exception E;
  raise(E);
};

/** Different payloads **/

/* Empty signature */

[@haha:]
let x = 5;

/* signature_item */
[@haha: let x: option(int)]
let x = 5;

/* Signature */
[@haha: type t; let x: option(t)]
let x = 5;
