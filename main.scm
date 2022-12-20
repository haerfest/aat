(import
  (chicken format)
  coops)

;; =============================================================================

;; Modules.
(load "file")
(load "archive")
(load "dfs")
(load "mmfs")
(load "uef")

(import (aat file))
(import (aat dfs))
(import (aat mmfs))
(import (aat uef))

;; =============================================================================

(define (main)
  (with-input-from-file "media/BEEB.MMB"
    (lambda ()
      (let ((mmb (make <mmfs>)))
        (read-port mmb (current-input-port))
        (for-each
          (lambda (file)
            (format #t "~A: ~A~%"
              (get-meta file 'slot)
              (get-meta file 'discname)))
          ((members mmb)))))))

;; =============================================================================

(main)