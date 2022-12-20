;; -----------------------------------------------------------------------------
;; Represents a file. Each archive is a file itself.
;; -----------------------------------------------------------------------------

(module (aat file)
  (<file> contents meta get-meta set-meta!)

  (import
    coops
    coops-primitive-objects
    scheme)

  (define-generic (contents archive))

  (define-generic (meta archive))

  (define-class <file> ()
    ((contents initform: "" accessor: contents)
     (meta initform: '() accessor: meta)))

  (define-method (get-meta (file <file>) (key <symbol>))
    (let ((existing (assq key (meta file))))
      (and existing (cdr existing))))

  (define-method (set-meta! (file <file>) (key <symbol>) (value #t))
    (let ((existing (assq key (meta file))))
      (if existing
        (set! (cdr existing) value)
        (set! (meta file)
          (cons (cons key value) (meta file)))))))