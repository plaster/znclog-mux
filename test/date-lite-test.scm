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

(define yyyymmdd-iota-specs
  '(( () "20160101" 0)
    ( ("20160101") "20160101" 1)
    ( ("20160101" "20160102" "20160103" "20160104" "20160105"
       "20160106" "20160107" "20160108" "20160109" "20160110"
       )
     "20160101" 10)
    ( ("20151227" "20151228" "20151229" "20151230" "20151231"
       "20160101" "20160102" "20160103" "20160104" "20160105"
       )
     "20160101" 10 -5)
    ))

(define (test-yyyymmdd-iota-spec expect . args)
  (test "yyyymmdd-iota" expect (pa$ apply yyyymmdd-iota args) equal?))

(for-each (pa$ apply test-yyyymmdd-iota-spec)
          yyyymmdd-iota-specs)

(test-end :exit-on-failure 1)
