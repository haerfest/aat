(module (aat file)
  (<file> data fmt attribute attributes append-data! set-attribute!)

  (import
    (chicken base)
    (chicken format)
    (chicken io)
    (chicken pathname)
    coops
    scheme)

  (define-class <file> ()
    ((data initform: "" accessor: data)
     (attributes initform: '() accessor: attributes)))

  (define-method (fmt (file <file>))
    (apply string-append
      (map
        (lambda (attr)
          (let ((value (cdr attr)))
            (if (fixnum? value)
              (format #f " ~S:#x~X" (car attr) value)
              (format #f " ~S:~A" (car attr) value))))
        (reverse (attributes file)))))
  
  (define-method (append-data! (new-data #t) (file <file>))
    (set! (data file) (string-append (data file) new-data))
    (set-attribute! 'size (string-length (data file)) file))
  
  (define-method (attribute (attr #t) (file <file>))
    (assq attr (attributes file)))

  (define-method (set-attribute! (attr #t) (value #t) (file <file>))
    (let ((existing (assq attr (attributes file))))
      (if (not existing)
        (set! (attributes file)
              (cons (cons attr value) (attributes file)))
        (set! (cdr existing) value)))))