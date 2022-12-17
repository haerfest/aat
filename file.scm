(module (aat file)
  (<file> data attribute attributes append-data! set-attribute!)

  (import
    (chicken io)
    (chicken pathname)
    coops
    scheme)

  (define-class <file> ()
    ((data initform: "" accessor: data)
     (attributes initform: '() accessor: attributes)))

  (define-method (append-data! (new-data #t) (file <file>))
    (set! (data file) (string-append (data file) new-data)))
  
  (define-method (attribute (attr #t) (file <file>))
    (assq attr (attributes file)))

  (define-method (set-attribute! (attr #t) (value #t) (file <file>))
    (let ((existing (assq attr (attributes file))))
      (if (not existing)
        (set! (attributes file)
              (cons (cons attr value) (attributes file)))
        (set! (cdr existing) value)))))