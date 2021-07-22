#lang scribble/manual

@(require scribble/eval
          (for-label racket/base
                     macro-help))


@(define helper-eval (make-base-eval))
@interaction-eval[#:eval helper-eval
                  (require macro-help)]


@title{Macro help}

@author[(author+email "Héctor Galbis Sanchis" "hectometrocuadrado@gmail.com")]

@defmodule[macro-help]

Esta librería ofrece numerosas funciones auxiliares para el manejo de objetos sintácticos.
Cada una de las funciones se asegura de mantener el contexto en el que se crearon
cada uno de los objetos.

@section{Tipos de objetos sintácticos}

Las siguientes funciones permiten al programador distinguir diferentes tipos de 
objetos sintácticos.

@defproc[(stx-identifier? [v any/c])
         boolean?]{
    Devuelve @racket[#t] si @racket[v] es un objeto sintáctico y @racket[(syntax-e v)] produce un símbolo.

    @examples[
        #:eval helper-eval
        (stx-identifier? #'hola)
        (stx-identifier? #'(casa hola))
        (stx-identifier? #'3)
    ]
}

@defproc[(stx-literal? [stx syntax?])
         boolean?]{
    Devuelve @racket[#t] si @racket[(syntax->datum stx)] verifica que no es @racket[null?]
    y no es @racket[pair?]. 

    @examples[
        #:eval helper-eval
        (stx-literal? #'hola)
        (stx-literal? #'(casa hola))
        (stx-literal? #'3)
    ]
}

@defproc[(stx-keyword? [stx syntax?])
         boolean?]{
    Devuelve @racket[#t] si @racket[stx] contiene un keyword.

    @examples[
        #:eval helper-eval
        (stx-keyword? #'#:hola)
        (stx-keyword? #'(hola))
        (stx-keyword? #'hola)
    ]

}

@section{Constructores y selectores de pares}

@defproc[(stx-pair? [stx syntax?])
         boolean?]{
    Devuelve @racket[#t] si @racket[stx] contiene un datum que es @racket[pair?].

    @examples[
        #:eval helper-eval
        (stx-pair? #'(a . b))
        (stx-pair? #'(a b c))
        (stx-pair? #'hola)
    ]

}

@defproc[(stx-null? [stx syntax?])
         boolean?]{
    Devuelve @racket[#t] si @racket[stx] contiene un datum que es @racket[null?].

    @examples[
        #:eval helper-eval
        (stx-null? #'())
        (stx-null? #'(a b c))
        (stx-null? #'hola)
    ]

}

@defproc[(stx-cons [stx1 syntax?] [stx2 syntax?])
         stx-pair?]{
    Devuelve el objeto sintáctico formado por un par con los objetos @racket[stx1] y @racket[stx2].

    @examples[
        #:eval helper-eval
        (stx-cons #'a #'b)
        (stx-cons #'hola #'())
        (stx-cons #'a #'(b c))
    ]

}

@defproc[(stx-car [stx syntax?])
         syntax?]{
    Devuelve el objeto sintáctico situado a la izquierda de un @racket[stx-pair?].

    @examples[
        #:eval helper-eval
        (stx-car #'(a . b))
        (stx-car #'(hola adios casa))
        (stx-car #'(bla))
    ]

}

@defproc[(stx-cdr [stx syntax?])
         syntax?]{
    Devuelve el objeto sintáctico situado a la derecha de un @racket[stx-pair?].

    @examples[
        #:eval helper-eval
        (stx-cdr #'(a . b))
        (stx-cdr #'(hola adios casa))
        (stx-cdr #'(bla))
    ]

}

@defthing[stx-null syntax? #:value #'()]{
    Alias de @racket[#'()].

    @examples[
        #:eval helper-eval
        stx-null
    ]

}

@defproc[(stx-list? [stx syntax?])
         syntax?]{
    Devuelve @racket[#t] si el datum que contiene es @racket[list?].

    @examples[
        #:eval helper-eval
        (stx-list? #'(a . b))
        (stx-list? #'(hola adios casa))
        (stx-list? stx-null)
    ]

}

@defproc[(stx-list [stx syntax?] ...)
         stx-list??]{
    Crea un objeto sintáctico que contiene una lista con cada uno de los @racket[stx ...].

    @examples[
        #:eval helper-eval
        (stx-list #'a #'b #'c)
        (stx-list)
        (stx-list #'(1 2 3) #'(a b c))
    ]

}

@defproc[(stx-list* [h-stx syntax?] [stx syntax?] ...)
         stx-list??]{
    Crea un objeto sintáctico que contiene una lista no adecuada con @racket[h-stx] y cada uno de los @racket[stx ...].

    @examples[
        #:eval helper-eval
        (stx-list* #'a #'b #'c)
        (stx-list* #'a #'b #'(c d e f))
        (stx-list* #'(1 2 3) #'(a b c))
    ]

}

@section{Operaciones sobre listas}

@defproc[(stx-length [stx stx-list?])
         exact-nonnegative-integer?]{
    Retorna el número de elementos de un @racket[stx-list?].

    @examples[
        #:eval helper-eval
        (stx-length #'())
        (stx-length #'(a b c d e))
        (stx-length #'(hola casa))
    ]

}

@defproc[(stx-append [stx1 stx-list?] [stx2 stx-list?])
         stx-list?]{
    Concatena varias @racket[stx-list?].

    @examples[
        #:eval helper-eval
        (stx-append #'(a) #'(b c))
        (stx-append #'(a b c d e) #'())
        (stx-append #'(Para) #'(todo) #'(epsilon mayor) #'(que cero))
    ]

}

@defproc[(stx-reverse [stx stx-list?])
         stx-list?]{
    Invierte el orden de los elementos de una @racket[stx-list?].

    @examples[
        #:eval helper-eval
        (stx-reverse #'(a b c))
        (stx-reverse #'(hola y adios))
        (stx-reverse #'(1 2 3 2 1))
    ]

}

@defproc[(stx-partition [pred procedure?] [stx stx-list?])
         (values stx-list? stx-list?)]{
    Parte @racket[stx] en dos. Primero se retornan los elementos que devuelven @racket[#t]
    usando @racket[pred], y luego se devuelve el resto.

    @examples[
        #:eval helper-eval
        (stx-partition stx-keyword? #'(#:hola a b #:adios))
        (stx-partition stx-list? #'((hola adios) casa manzana (1 2 3)))
        (stx-partition stx-literal? #'(1 2 (pfff blaaa) hola (brrr brrr) "holi"))
    ]
}

@defproc[(stx-group-adjacent-by [key procedure?] [stx stx-list?] [same procedure?])
         stx-list?]{
    Agrupa los elementos adyacentes de la lista que sean equivalentes. Dos elementos @racket[a] y @racket[b]
    son equivalentes si verifican @racket[(same? (key a) (key b))].

    @examples[
        #:eval helper-eval
        (require racket/bool)
        (stx-group-adjacent-by stx-keyword? #'(#:hola #:adios b c d #:casa) (lambda (s1 s2) (not (xor s1 s2))))
    ]
}

@section{Iteración sobre listas}

@defproc[(stx-map [f procedure?] [stx stx-list?])
         stx-list?]{
    Ejecuta la función @racket[f] sobre cada uno de los elementos de @racket[stx] y devuelve una @racket[stx-list?]
    con los resultados.

    @examples[
        #:eval helper-eval
        (stx-map stx-keyword? #'(#:hola a b #:adios))
        (stx-map stx-car #'((hola adios) (a . b) (1 2 3)))
        (stx-map stx-reverse #'((1 2 3) (a b c d) (bla ble bli blo blu))
    ]
}

@defproc[(stx-map [f procedure?] [stx stx-list?])
         stx-list?]{
    Ejecuta la función @racket[f] sobre cada uno de los elementos de @racket[stx] y devuelve una @racket[stx-list?]
    con los resultados.

    @examples[
        #:eval helper-eval
        (stx-map stx-keyword? #'(#:hola a b #:adios))
        (stx-map stx-car #'((hola adios) (a . b) (1 2 3)))
        (stx-map stx-reverse #'((1 2 3) (a b c d) (bla ble bli blo blu))
    ]
}