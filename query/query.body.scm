
;;;
;;; Basic querying, for use in conjunction with (macduffie tsv)
;;;
;;; Copyright 2016 Jason K. MacDuffie
;;; License: GPLv3+
;;;

(define (columns l . collist)
  ;; Similar to SQL "SELECT"
  (define column-order
    (list->vector
     (map (lambda (field)
            (list-index (lambda (s)
                          (string=? field s)) (car l)))
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

