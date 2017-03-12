(define-module text.znclog

  (export
    default-path-of
    parse-line
    parse-file$
    parse-file
    parse-yyyymmdd
    )

  (use util.match)
  (use file.util)
  (use gauche.generator)

  )

(select-module text.znclog)

(define (parse-line line)
  (rxmatch-case line
    [ #/^\[(\d\d):(\d\d):(\d\d)\] (.*)$/ ( #f HH MM SS line )
      (list*
        `(HH . ,HH) `(MM . ,MM) `(SS . ,SS)
        (rxmatch-case line
          [ #/^\*\*\* (.*)$/ ( #f msg )
            (list `(type . "server-message")
                  `(message . ,msg)
                  `(parsed . #t)
                  )
            ]
          [ #/^<([^ ]+)> (.*)$/ ( #f nick msg )
            (list `(type . "user-message")
                  `(message . ,msg)
                  `(from . ,nick)
                  )
            ]
          [ else
            ;; unknown message type
            (list `(raw-line . ,line)
                  )
            ]
          ) ) ]
    [ else
      ;; unknown line type
      (list `(raw-line . ,line))
      ]
    ) )

(define (default-path-of logroot nw ch yyyy mm dd)
  ($ build-path logroot
     $ format "~a_~a_~4,'0d~2,'0d~2,'0d.log"
     nw ch yyyy mm dd
     ) )

(define ((%call-with-input-file* fallback) filename . args)
  (cond [ (file-exists? filename)
         (apply call-with-input-file filename . args) ]
        [ else fallback ] ) )

(define ((parse-file$ path-of) logroot nw ch yyyy mm dd)
  ($ reverse $ (cut vector-ref <> 1)
     $ call-with-input-file (path-of logroot nw ch yyyy mm dd)
     (^(in)
       (port-fold
         (match-lambda*
           [ ( line #( lineno parsed-lines ) )
            (vector
              (+ lineno 1)
              (cons (list* `(nw . ,nw)
                           `(ch . ,ch)
                           `(yyyy . ,yyyy)
                           `(mm . ,mm)
                           `(dd . ,dd)
                           `(lineno . ,lineno)
                           (parse-line line)
                           )
                    parsed-lines) )
            ]
           )
         '#( 0 () )
         (pa$ read-line in)
         ) ) ) )

(define parse-file (parse-file$ default-path-of))

(define (parse-yyyymmdd yyyymmdd)
  (rxmatch-case yyyymmdd
    [ #/^(\d\d\d\d)(\d\d)(\d\d)$/ (#f yyyy-s mm-s dd-s)
      (list (string->number yyyy-s 10)
            (string->number mm-s 10)
            (string->number dd-s 10)
            ) ]
    [ else #f ] ) )

(define (%generate-file-args
          :key
          [ count-limit 100 ]
          date-since
          date-until
          )
  (generate
    (^(yield)
      (match `#(,date-since ,date-until)
        [ #(_ _)
          (error "not implemented")
          ]
        ) ) ) )
