(load "storage")
(load "file-storage")
(load "memory-storage")

(import
  (aat file-storage)
  (chicken format)
  coops)

(define (main)
  (let ((beeb.mmb (make <file-storage> 'filepath "media/BEEB.MMB")))
    (st-open beeb.mmb 'rdonly)
    (format #t "Size: ~A~%" (st-size beeb.mmb))
    (st-close beeb.mmb)))

(main)