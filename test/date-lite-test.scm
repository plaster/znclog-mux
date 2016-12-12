(use gauche.test)
(use gauche.generator)
(use data.date-lite)

(test-start "data.date-lite")

(test-module 'data.date-lite)

(define (test-specs spec-list tester)
  (for-each (pa$ apply tester) spec-list))

(test-specs
  '(( "20160101" "20150101" 365 )
    ( "20160101" "20151231" 1 )
    ( "20151231" "20160101" -1 )
    ( "20170101" "20160101" 366 )
    )
  (^ (expect . args)
     (test "yyyymmdd+" expect (pa$ apply yyyymmdd+ args) string=?) ) )

(test-specs
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
    )
  (^ (expect . args)
     (test "yyyymmdd-iota" expect (pa$ apply yyyymmdd-iota args) equal?) ) )

(test-specs
  '(( () 42 "20160101" 0)
    ( ("20151227" "20151228" "20151229" "20151230" "20151231"
       "20160101" "20160102" "20160103" "20160104" "20160105"
       )
     42
     "20160101" 10 -5)
    )
  (^ (expect ntake . args)
     (test "yyyymmdd-giota" expect (^()
                                     (generator->list
                                       (gtake (apply yyyymmdd-giota args)
                                              ntake) ) )
           equal?)))


(test-end :exit-on-failure 1)
