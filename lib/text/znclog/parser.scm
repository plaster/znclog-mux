(define-module text.znclog.parser
  (export
    parse-line
    ))

(select-module text.znclog.parser)

(define (parse-line line)
  (rxmatch-case line
    [ #/^\[(\d\d):(\d\d):(\d\d)\] (.*)$/ ( #f hh mm ss line )
      (rxmatch-case line
        [ #/^\*\*\* (.*)$/ ( #f msg )
          (values hh mm ss 'server-message msg)
          ]
        [ #/^<([^ ]+)> (.*)$/ ( #f nick msg )
          (values hh mm ss 'user-message nick msg)
          ]
        [ else
          ;; unknown message type
          (values hh mm ss #f line)
          ]
        ) ]
    [ else
      ;; unknown line type
      (values #f #f #f #f line)
      ]
    ) )
