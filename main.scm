(load "file")
(load "uef")

(import
  (aat file)
  (aat uef)
  coops)

(define (main)
  (let ((archive (make <uef>)))
    (open-file "media/Forth_E.uef" archive)
    (display (iterate archive))))

(main)