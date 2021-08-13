1392
((3) 0 () 1 ((q lib "macro-help/main.rkt")) () (h ! (equal) ((c def c (c (? . 0) q stx-map)) q (1264 . 4)) ((c def c (c (? . 0) q stx-rec-findb)) q (1588 . 4)) ((c def c (c (? . 0) q stx-list*)) q (680 . 4)) ((c def c (c (? . 0) q stx-length)) q (777 . 3)) ((c def c (c (? . 0) q stx-null)) q (517 . 2)) ((c def c (c (? . 0) q stx-splitf-at)) q (1872 . 4)) ((c def c (c (? . 0) q stx-foldl)) q (1350 . 5)) ((c def c (c (? . 0) q stx-cdr)) q (459 . 3)) ((c def c (c (? . 0) q stx-append)) q (859 . 4)) ((c def c (c (? . 0) q stx-dropf)) q (1778 . 4)) ((c def c (c (? . 0) q stx-cons)) q (311 . 4)) ((c def c (c (? . 0) q stx-list?)) q (553 . 3)) ((c def c (c (? . 0) q stx-car)) q (401 . 3)) ((c def c (c (? . 0) q stx-group-adjacent-by)) q (1130 . 5)) ((c def c (c (? . 0) q stx-genids)) q (2187 . 4)) ((c def c (c (? . 0) q stx-list)) q (613 . 3)) ((c def c (c (? . 0) q stx-partition)) q (1021 . 4)) ((c def c (c (? . 0) q genids)) q (2051 . 4)) ((c def c (c (? . 0) q stx-foldr)) q (1469 . 5)) ((c def c (c (? . 0) q stx-pair?)) q (189 . 3)) ((c def c (c (? . 0) q stx-null?)) q (250 . 3)) ((c def c (c (? . 0) q stx-identifier?)) q (0 . 3)) ((c def c (c (? . 0) q genid)) q (1970 . 3)) ((c def c (c (? . 0) q stx-keyword?)) q (125 . 3)) ((c def c (c (? . 0) q stx-literal?)) q (61 . 3)) ((c def c (c (? . 0) q stx-takef)) q (1684 . 4)) ((c def c (c (? . 0) q stx-reverse)) q (955 . 3))))
procedure
(stx-identifier? v) -> boolean?
  v : any/c
procedure
(stx-literal? stx) -> boolean?
  stx : syntax?
procedure
(stx-keyword? stx) -> boolean?
  stx : syntax?
procedure
(stx-pair? stx) -> boolean?
  stx : syntax?
procedure
(stx-null? stx) -> boolean?
  stx : syntax?
procedure
(stx-cons stx1 stx2) -> stx-pair?
  stx1 : syntax?
  stx2 : syntax?
procedure
(stx-car stx) -> syntax?
  stx : syntax?
procedure
(stx-cdr stx) -> syntax?
  stx : syntax?
value
stx-null : syntax? = #'()
procedure
(stx-list? stx) -> syntax?
  stx : syntax?
procedure
(stx-list stx ...) -> stx-list??
  stx : syntax?
procedure
(stx-list* h-stx stx ...) -> stx-list??
  h-stx : syntax?
  stx : syntax?
procedure
(stx-length stx) -> exact-nonnegative-integer?
  stx : stx-list?
procedure
(stx-append stx1 stx2) -> stx-list?
  stx1 : stx-list?
  stx2 : stx-list?
procedure
(stx-reverse stx) -> stx-list?
  stx : stx-list?
procedure
(stx-partition pred stx) -> stx-list? stx-list?
  pred : procedure?
  stx : stx-list?
procedure
(stx-group-adjacent-by key stx same) -> stx-list?
  key : procedure?
  stx : stx-list?
  same : procedure?
procedure
(stx-map f stx) -> stx-list?
  f : procedure?
  stx : stx-list?
procedure
(stx-foldl f stx-init stx) -> any/c
  f : procedure?
  stx-init : syntax?
  stx : stx-list?
procedure
(stx-foldr f stx-init stx) -> any/c
  f : procedure?
  stx-init : syntax?
  stx : stx-list?
procedure
(stx-rec-findb v stx) -> boolean?
  v : stx-identifier?
  stx : stx-list?
procedure
(stx-takef stx pred) -> stx-list?
  stx : stx-list?
  pred : procedure?
procedure
(stx-dropf stx pred) -> stx-list?
  stx : stx-list?
  pred : procedure?
procedure
(stx-splitf-at stx pred) -> stx-list?
  stx : stx-list?
  pred : procedure?
procedure
(genid base) -> stx-identifier?
  base : (or string? symbol?)
procedure
(genids num base) -> (listof stx-identifier?)
  num : exact-nonnegative-integer?
  base : (or string? symbol?)
procedure
(stx-genids num base) -> stx-list?
  num : exact-nonnegative-integer?
  base : (or string? symbol?)
