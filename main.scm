(import
  (chicken format)
  coops)

;; =============================================================================

;; Modules.
(load "file")
(load "fs")
(load "dfs")

(import (aat file))
(import (aat fs))
(import (aat dfs))

;; =============================================================================

(define (main)
  (with-input-from-file "media/Elite.ssd"
    (lambda ()
      (let ((disc (make <dfs>)))
        (mount disc (current-input-port))
        (format #t "~S (~A)  *OPT4,~A  ~A sectors~%"
          (title disc)
          (write-cycle-count disc)
          (opt-4 disc)
          (sector-count disc))
        (for-each
          (lambda (file)
            (format #t "~A\t&~X\t&~X\t~A\t~C~C~%"
              (filename file)
              (load-addr file)
              (exec-addr file)
              (size file)
              (if (readable? file) #\R #\space)
              (if (writable? file) #\W #\space)
              ))
          (members disc))
        (unmount disc)))))

;; =============================================================================

(main)