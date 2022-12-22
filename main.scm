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
(load "mmfs")
(load "uef")

(import
  (aat identifiable)
  (aat fs)
  (aat file)
  (aat dfs)
  (aat mmfs)
  (aat uef))

;; -----------------------------------------------------------------------------

(define (print-file file)
  (format #t "~S\t&~X\t&~X\t~A\t~C\t~S~%"
    (filename  file)
    (load-addr file)
    (exec-addr file)
    (size      file)
    (if (locked? file) #\L #\space)
    (id        file)))

(define (print-dfs disc)
  (mount disc)
  (format #t "~S (~A)  *OPT4,~A  ~A sectors  ~S~%"
    (title             disc)
    (write-cycle-count disc)
    (opt-4             disc)
    (sector-count      disc)
    (id                disc))
  (for-each print-file (items disc))
  (unmount disc))

(define (test-dfs filepath)
  (call-with-input-file filepath
    (lambda (port)
      (let ((disc (make <dfs> 'source port)))
        (print-dfs disc)))))

(define (test-uef filepath)
  (call-with-input-file filepath
    (lambda (port)
      (let ((tape (make <uef> 'source port)))
        (mount tape)
        (for-each print-file (items tape))
        (unmount tape)))))

(define (test-mmfs filepath)
  (call-with-input-file filepath
    (lambda (port)
      (let ((fs (make <mmfs> 'source port)))
        (mount fs)
        (format #t "*ONBOOT 0 ~A  *ONBOOT 1 ~A  *ONBOOT 2 ~A  *ONBOOT 3 ~A~%"
          (on-boot-0 fs) (on-boot-1 fs) (on-boot-2 fs) (on-boot-3 fs))
        (for-each print-dfs (items fs))
        (unmount fs)))))

(define (main)
  (test-dfs  "media/Elite.ssd")
  (test-uef  "media/Hopper_E.uef")
  (test-mmfs "media/BEEB.MMB"))

(main)