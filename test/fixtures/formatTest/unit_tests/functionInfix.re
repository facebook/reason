let entries = ref([]);

let all = ref(0);

/*
 * >>= is left associative, and higher precedence than =>
 */
let (>>=)(a,b) = b(a);

let fff = ();


/** Parse tree */
(fff >>= (xx(yy) >>= aa(bb)));

/* Minimum parenthesis */
fff >>= xx(yy) >>= aa(bb);

/* Actually printed parenthesis */
fff >>= (xx(yy) >>= aa(bb));


/** Parse tree */
fff >>= ((fun(xx)=> 0) >>= (fun(aa) => 10));

/* Minimum parenthesis */
fff >>= ((fun(xx)=> 0) >>= (fun(aa) => 10));

/* Actually printed parenthesis */
fff >>= ((fun(xx)=> 0) >>= (fun(aa) => 10));


/** Parse tree */
((fff >>= (fun(xx) => 0)) >>= (fun(aa) => 10));

/* Minimum parenthesis */
/* It is very difficult to actually achieve this. */
fff >>= (fun(xx) => 0) >>= fun(aa) => 10;

/* Actually printed. */
fff >>= (fun(xx) => 0) >>= (fun(aa) => 10);


/** Parse tree */
(fff >>= (fun(xx) => (0 >>= (fun(aa,cc) => 10))));

/* Minimum parens - grouping the zero */
/* Difficult to achieve. */
fff >>= fun(xx) => 0 >>= (fun(aa,cc) => 10);

/* Actually printed parenthesis. */
fff >>= (fun(xx) => 0) >>= (fun(aa,cc) => 10);

/* Another way you could also write it it */
(fff >>= fun(xx) => 0) >>= (fun(aa,cc) => 10);


/** Parse tree */
(fff >>= (fun(xx) => 0));

/* Minimum parens - grouping the zero */
fff >>= fun(xx) => 0;

/* Printed parens - see how more are printed than necessary. */
fff >>= (fun(xx) => 0);
