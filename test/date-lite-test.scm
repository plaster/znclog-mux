(use gauche.test)
(use data.date-lite)

(test-start "data.date-lite")

(test-module 'data.date-lite)

(define yyyymmdd+-specs
  '(( "20160101" "20150101" 365 )
    ( "20160101" "20151231" 1 )
    ( "20151231" "20160101" -1 )
    ( "20170101" "20160101" 366 )
    ))

(define (test-yyyymmdd+-spec expect op0 op1)
  (test "yyyymmdd+" expect (pa$ yyyymmdd+ op0 op1) string=?))

(for-each (pa$ apply test-yyyymmdd+-spec)
          yyyymmdd+-specs)

(test-end :exit-on-failure 1)
