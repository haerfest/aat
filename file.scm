(module (aat file)
  (<file> data attributes append-data! set-attribute!)

  (import
    (chicken io)
    (chicken pathname)
    coops
    scheme)

  (define-class <file> ()
    ((data accessor: data)
     (attributes initform: '() accessor: attributes)))

  (define-method (append-data! (data #t) (file <file>))
    #f)
  
  (define-method (set-attribute! (attribute #t) (value #t) (file <file>))
    (let ((existing (assq attribute (attributes file))))
      (if (not existing)
        (set! (attributes file)
              (cons (cons attribute value) (attributes file)))
        (set! (cdr existing) value)))))