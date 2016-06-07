
(define-library (macduffie tsv query)
  (import (scheme base)
          (scheme write)
          (scheme read)
          (srfi 1))
  (export tsv-index columns only)
  (include "query.body.scm"))

