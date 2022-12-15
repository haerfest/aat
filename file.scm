(module (aat file)
  (<file> contents attributes set-attribute)

  (import
    (chicken io)
    (chicken pathname)
    coops
    scheme)

  (define-class <file> ()
    ((contents initform: '() accessor: contents)
     (attributes initform: '() accessor: attributes)))

  (define-method (set-attribute (attribute #t) (value #t) (file <file>))
    (let ((existing (assq attribute (attributes file))))
      (if (not existing)
        (set! (attributes file)
              (cons (cons attribute value) (attributes file)))
        (set! (cdr existing) value)))))