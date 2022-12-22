(import
  (chicken format)
  coops)

;; =============================================================================

;; Modules.
(load "file")
(load "fs")
(load "dfs")
(load "uef")

(import (aat file))
(import (aat fs))
(import (aat dfs))
(import (aat uef))

;; -----------------------------------------------------------------------------

(define (print-file file)
  (format #t "~A\t&~X\t&~X\t~A\t~C~%"
    (filename  file)
    (load-addr file)
    (exec-addr file)
    (size      file)
    (if (locked? file) #\L #\space)))

(define (test-dfs)
  (call-with-input-file "media/Elite.ssd"
    (lambda (port)
      (let ((disc (make <dfs>)))
        (mount disc port)
        (format #t "~S (~A)  *OPT4,~A  ~A sectors~%"
          (title             disc)
          (write-cycle-count disc)
          (opt-4             disc)
          (sector-count      disc))
        (for-each print-file (members disc))
        (unmount disc)))))

(define (test-uef)
  (call-with-input-file "media/Hopper_E.uef"
    (lambda (port)
      (let ((tape (make <uef>)))
        (mount tape port)
        (for-each print-file (members tape))
        (unmount tape)))))

(define (main)
  (test-dfs)
  (test-uef))

(main)