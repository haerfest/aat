(load "file")
(load "uef")

(import
  (aat file)
  (aat uef)
  (chicken format)
  coops)

(define (main)
  (let ((archive (make <uef>)))
    (open-file "media/Hopper_E.uef" archive)
    (format #t "UEF version: ~A~%" (version archive))
    (for-each
      (lambda (file)
        (format #t "[[ ~A ]]~%" (fmt file)))
      (files archive))))

(main)