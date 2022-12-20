(import
  (chicken format)
  coops)

;; =============================================================================

;; Modules.
(load "file")
(load "archive")
(load "dfs")

(import (aat file))
(import (aat dfs))

;; =============================================================================

(define (main)
  (with-input-from-file "media/Elite.ssd"
    (lambda ()
      (let ((disc (make <dfs>)))
        (read-port disc (current-input-port))
        (for-each
          (lambda (file)
            (format #t "~A~%" (get-meta file 'filename)))
          ((members disc)))))))

;; =============================================================================

(main)