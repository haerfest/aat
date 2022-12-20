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
            (format #t "~A #x~X #x~X #x~X(~A)~%"
              (get-meta file 'filename)
              (get-meta file 'load-addr)
              (get-meta file 'exec-addr)
              (get-meta file 'size)
              (get-meta file 'size)))
          ((members tape)))))))

;; =============================================================================

(main)