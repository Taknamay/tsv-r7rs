
(define-library (macduffie tsv query)
  (import (scheme base)
          (scheme write)
          (scheme read)
          (scheme case-lambda)
          (srfi 1))
  (export tsv-index columns only union insert)
  (include "query.body.scm"))

