
;;;
;;; Very simple TSV parser
;;;
;;; Copyright 2016 Jason K. MacDuffie
;;; License: GPLv3+
;;;

(define valid-tsv?
  (case-lambda
   ;; Ensure that all lines are the same length  (case-lambda
   ((l) (valid-tsv? (cdr l) (length (car l))))
   ((l sz)
    (or (null? l)
        (and (= (length (car l)) sz)
             (valid-tsv? (cdr l) sz))))))

(define tsv-read-line
  (case-lambda
   (() (tsv-read-line (current-input-port)))
   ((port)
    (define s (read-line port))
    (if (eof-object? s)
        s
        (string-split s "\t")))))

(define (tsv-read-core port)
  (let loop ((out '()))
    (define s (tsv-read-line port))
    (if (eof-object? s)
        (if (null? (car out))
            ;; Allow the last line to be blank
            (reverse (cdr out))
            (reverse out))
        (loop (cons s out)))))

(define tsv-read
  (case-lambda
   (() (tsv-read (current-input-port)))
   ((port)
    (define result (tsv-read-core port))
    (if (valid-tsv? result)
        result
        (error "tsv-read" "All columns must be the same size")))))

(define (tsv-read-file path)
  (define p (open-input-file path))
  (define t (tsv-read p))
  (close-input-port p)
  t)

(define (tsv-read-string s)
  (define p (open-input-string s))
  (define t (tsv-read p))
  (close-input-port p)
  t)

(define tsv-write-line
  (case-lambda
   ((t) (tsv-write-line t (current-output-port)))
   ((t port)
    (define l (string-join t "\t"))
    (display l port)
    (newline port))))

(define (tsv-write-core t port)
  (for-each (lambda (tline)
              (tsv-write-line tline port))
            t))

(define tsv-write
  (case-lambda
   ((t) (tsv-write t (current-output-port)))
   ((t port)
    (if (valid-tsv? t)
        (tsv-write-core t port)
        (error "tsv-write" "All columns must be the same size")))))

(define (tsv-write-string t)
  (define p (open-output-string))
  (tsv-write t p)
  (let ((s (get-output-string p)))
    (close-output-port p)
    s))

(define (tsv-write-file t path)
  (define p (open-output-file path))
  (tsv-write t p)
  (close-output-port p))

