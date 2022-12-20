(import
  (chicken format)
  coops)

;; =============================================================================

;; Modules.
(load "file")
(load "archive")
(load "dfs")
(load "uef")

(import (aat file))
(import (aat dfs))
(import (aat uef))

;; =============================================================================

(define (main)
  (with-input-from-file "media/Elite_E.uef"
    (lambda ()
      (let ((tape (make <uef>)))
        (read-port tape (current-input-port))
        (for-each
          (lambda (file)
            (format #t "~A~%" (get-meta file 'filename)))
          ((members tape)))))))

;; =============================================================================

(main)