(import
  (chicken format)
  coops
  srfi-13
  srfi-14)

;; =============================================================================

;; Modules.
(load "identifiable")
(load "fs")
(load "file")
(load "dfs")
(load "uef")

(import
  (aat identifiable)
  (aat fs)
  (aat file)
  (aat dfs)
  (aat uef))

;; -----------------------------------------------------------------------------

(define (print-file file)
  (format #t "~S\t&~X\t&~X\t~A\t~C ~S~%"
    (filename  file)
    (load-addr file)
    (exec-addr file)
    (size      file)
    (if (locked? file) #\L #\space)
    (id        file)))

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
        (for-each print-file (items disc))
        (unmount disc)))))

(define (test-uef)
  (call-with-input-file "media/Hopper_E.uef"
    (lambda (port)
      (let ((tape (make <uef>)))
        (mount tape port)
        (for-each print-file (items tape))
        (unmount tape)))))

(define (main)
  (test-dfs)
  (test-uef))

(main)