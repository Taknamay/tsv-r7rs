
;;;
;;; Basic querying, for use in conjunction with (macduffie tsv)
;;;
;;; Copyright 2016 Jason K. MacDuffie
;;; License: GPLv3+
;;;

(define (tsv-index l field)
  (define result
    (list-index (lambda (s)
                  (string=? field s))
                (car l)))
  (if result result (error "tsv-index" "Field not found")))

(define (columns l . collist)
  ;; Implements part of SQL "SELECT"
  (define column-order
    (list->vector
     (map (lambda (field)
            (tsv-index l field))
          collist)))
  (define sz (length collist))
  (define (rewrite-record rec)
    (define vecrec (list->vector rec))
    (define newvec (make-vector sz))
    (let loop ((i 0))
      (when (< i sz)
        (vector-set! newvec
                     i
                     (vector-ref vecrec
                                 (vector-ref column-order i)))
        (loop (+ i 1))))
    (vector->list newvec))
  (map rewrite-record l))

(define (only t field pred)
  (define i (tsv-index t field))
  (cons
   (car t)
   (filter (lambda (rec)
             (pred (list-ref rec i)))
           (cdr t))))

(define (union t1 t2)
  (if (equal? (car t1) (car t2))
      (let loop ((out (cdr t1))
                 (in (cdr t2)))
        (if (null? in)
            (cons (car t1) out)
            (loop (if (null? (filter (lambda (l)
                                       (equal? (car t2) l))))
                      (cons (car in) out)
                      out)
                  (cdr in))))
      (error "union" "Fields not identical")))

