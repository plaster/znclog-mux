(define-module text.znclog.parser

  (export
    default-path-of
    parse-line
    parse-file$
    parse-file
    )

  (use util.match)
  (use file.util)

  )

(select-module text.znclog.parser)

(define (parse-line line)
  (rxmatch-case line
    [ #/^\[(\d\d):(\d\d):(\d\d)\] (.*)$/ ( #f HH MM SS line )
      (rxmatch-case line
        [ #/^\*\*\* (.*)$/ ( #f msg )
          (list HH MM SS
                'server-message
                msg)
          ]
        [ #/^<([^ ]+)> (.*)$/ ( #f nick msg )
          (list HH MM SS
                'user-message
                msg nick)
          ]
        [ else
          ;; unknown message type
          (list HH MM SS
                #f
                line)
          ]
        ) ]
    [ else
      ;; unknown line type
      (list #f #f #f
            #f
            line)
      ]
    ) )

(define (default-path-of logroot nw ch yyyy mm dd)
  ($ build-path logroot
     $ format "~a_~a_~4,'0d~2,'0d~2,'0d.log"
     nw ch yyyy mm dd
     ) )

(define ((parse-file$ path-of) logroot nw ch yyyy mm dd)
  ($ reverse $ (cut vector-ref <> 1)
     $ call-with-input-file (path-of logroot nw ch yyyy mm dd)
     (^(in)
       (port-fold
         (match-lambda*
           [ ( line #( line-no parsed-lines ) )
            (vector
              (+ line-no 1)
              (cons (match (parse-line line)
                      [ ( HH MM SS type . args )
                       `( ,nw ,ch ,yyyy ,mm ,dd ,HH ,MM ,SS ,line-no ,type ,@args )
                       ] )
                    parsed-lines) )
            ]
           )
         '#( 0 () )
         (pa$ read-line in)
         ) ) ) )

(define parse-file (parse-file$ default-path-of))
