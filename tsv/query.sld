
(define-library (macduffie tsv query)
  (import (scheme base)
          (scheme write)
          (scheme read)
          (scheme case-lambda)
          (srfi 1)
          (srfi 95))
  (export tsv-index tsv-sort columns only union insert)
  (include "query.body.scm"))

