(load "file")
(load "uef")
(load "gzipped-uef")

(import
  (aat gzipped-uef)
  (aat uef)
  coops)

(define (main)
  (let ((gzipped-uef-archive (make <gzipped-uef-archive>)))
    (open-file "Forth_E.uef" gzipped-uef-archive)
    (display (iterate gzipped-uef-archive))))

(main)