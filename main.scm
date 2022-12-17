(load "file")
(load "uef")

(import
  (aat file)
  (aat uef)
  (chicken format)
  coops)

(define (main)
  (let ((archive (make <uef>)))
    (open-file "media/Forth_E.uef" archive)
    (map
      (lambda (file)
        (format #t "filename: ~A~%" (attribute 'filename file)))
      (iterate archive))))

(main)