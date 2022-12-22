;; -----------------------------------------------------------------------------
;; Represents a file.
;; -----------------------------------------------------------------------------

(module (aat file)
  (<file>
   id filename load-addr exec-addr size locked? contents
   meta get-meta set-meta!)

  (import
    (aat identifiable)
    coops
    coops-primitive-objects
    scheme)

  (define-class <file> (<identifiable>)
    ((filename  initform: ""  accessor: filename)
     (load-addr initform: 0   accessor: load-addr)
     (exec-addr initform: 0   accessor: exec-addr)
     (size      initform: 0   accessor: size)
     (locked?   initform: #f  accessor: locked?)
     (contents  initform: ""  accessor: contents)
     (meta      initform: '() accessor: meta)))

  (define-method (get-meta (file <file>) (key <symbol>))
    (let ((existing (assq key (meta file))))
      (and existing (cdr existing))))

  (define-method (set-meta! (file <file>) (key <symbol>) (value #t))
    (let ((existing (assq key (meta file))))
      (if existing
        (set! (cdr existing) value)
        (set! (meta file)
          (cons (cons key value) (meta file)))))))