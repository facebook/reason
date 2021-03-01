let x = { "obj": obj };

let x = { "key": key, "keyTwo": keyTwo };

let x = { ...x, "key": key };

let x = { ...x, "key": key, "keyTwo": keyTwo };

type t = {. "x": int };

type t('a) = {.. "x": int } as 'a;

type t = {. "x": (int, int) };

type t('a) = {.. "x": (int, int) } as 'a;

let x = { "obj": 0 };

let x = { "key": 0, "keyTwo": 1 };

let x = { ...x, "key": 0 };

let x = { ...x, "key": 0, "keyTwo": 1 };

type t = {. "x": int };

type t('a) = {.. "x": int } as 'a;

type t = {. "x": (int, int) };

type t('a) = {.. "x": (int, int) } as 'a;
