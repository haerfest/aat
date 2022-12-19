(import
  (chicken format)
  coops)

;; =============================================================================

;; Accesses the contents of a file.
(define-generic (contents file))

;; Accesses the meta-information of a file.
(define-generic (meta file))

;; Represents a file, consisting of its contents and meta information such as a
;; filename, load- and execution addresses, and what not.
(define-class <file> ()
  ((contents initform: "" accessor: contents)
   (meta initform: '() accessor: meta)))

;; =============================================================================

;; Modules.
(load "archive")
(load "mmfs")

(import (aat mmfs))

;; =============================================================================

(define (main)
  (with-input-from-file "media/BEEB.MMB"
    (lambda ()
      (let ((mmfs (make <mmfs>)))
        (read-port mmfs (current-input-port))
        (for-each
          (lambda (x) (format #t "~A~%" x))
          (members mmfs))))))

;; =============================================================================

(main)