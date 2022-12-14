(load "archive/file")
(load "archive/uef")
(load "archive/gzipped-uef")

(import
  gzipped-uef
  uef
  coops)

(define (main)
  (let ((gzipped-uef-archive (make <gzipped-uef-archive>)))
    (open-file "Forth_E.uef" gzipped-uef-archive)
    (display (iterate gzipped-uef-archive))))

(main)