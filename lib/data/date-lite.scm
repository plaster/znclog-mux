(define-module data.date-lite

  ;;; "yyyymmdd" string operator / generator

  (export
    yyyymmdd+
    yyyymmdd-iota
    yyyymmdd-giota
    yyyymmdd-between
    yyyymmdd-gbetween
    )

  (use gauche.generator)

  ;;; implemented as srfi-19 date wrapper
  (use srfi-19
    :only ( make-date
            date-year
            date-month
            date-day
            date->time-utc
            time-utc->date
            make-time
            time-duration
            add-duration
            )
    )
  )

(select-module data.date-lite)

(define tz-offset 0)

;; TODO: purge
(define (%decompose->yyyy+mm+dd yyyymmdd)
  (rxmatch-case yyyymmdd
    [ #/^(\d\d\d\d)(\d\d)(\d\d)$/ (#f yyyy-s mm-s dd-s)
      (values (string->number yyyy-s 10)
              (string->number mm-s 10)
              (string->number dd-s 10)
              ) ]
    [ else (errorf "invalid: ~s" yyyymmdd) ]
    ) )

;; TODO: purge
(define (%date<-yyyy+mm+dd yyyy mm dd)
  (make-date 0 0 0 0 dd mm yyyy tz-offset)
  )

;; TODO: use string->date
(define %parse (.$ %date<-yyyy+mm+dd %decompose->yyyy+mm+dd))

;; TODO: purge
(define (%yyyy+mm+dd<-date date)
  (values (date-year date)
          (date-month date)
          (date-day date)
          ) )

;; TODO: purge
(define (%compose yyyy mm dd)
  (format "~4,'0d~2,'0d~2,'0d" yyyy mm dd) )

;; TODO: use date->string
(define %deparse (.$ %compose %yyyy+mm+dd<-date))

(define (%duration<-days days)
  (make-time time-duration 0 (* days 24 60 60)))

(define (%date+ date days)
  (time-utc->date
    (add-duration
      (date->time-utc date)
      (%duration<-days days)
      )
    tz-offset ) )

(define (yyyymmdd+ yyyymmdd days)
  ((.$ %deparse
       (cut %date+ <> days)
       %parse
       )
   yyyymmdd
   ))

;; (define %date-iota (error "not implemented"))
;; (define %date-giota (error "not implemented"))
;; (define %date-between (error "not implemented"))
;; (define %date-gbetween (error "not implemented"))

;; (define yyyymmdd-iota (error "not implemented"))
;; (define yyyymmdd-giota (error "not implemented"))
;; (define yyyymmdd-between (error "not implemented"))
;; (define yyyymmdd-gbetween (error "not implemented"))
