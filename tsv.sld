
(define-library (macduffie tsv)
  (import (scheme base)
          (scheme case-lambda)
          (scheme file)
          (scheme read)
          (scheme write)
          (srfi 130))
  (export tsv-read-line tsv-read tsv-read-string tsv-read-file
          tsv-write-line tsv-write tsv-write-string tsv-write-file)
  (include "tsv.body.scm"))

