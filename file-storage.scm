(module (aat file-storage)
  (<file-storage>
   st-mode st-open st-close st-read st-write st-size st-tell st-seek)

  (import
    (aat storage)
    bitstring
    (chicken base)
    (chicken file posix)
    coops
    scheme)

  (define-class <file-storage> (<storage>)
    ((filepath initform: #f  accessor: filepath)
     (port     initform: #f  accessor: port)
     (mode)))

  (define-method (st-mode (storage <file-storage>))
    (slot-value storage 'mode))

  (define-method (st-open (storage <file-storage>) mode)
    (set! (port storage)
          (file-open (filepath storage)
                     (+ open/binary
                        (cond
                         ((eq? mode 'rdonly) open/rdonly)
                         ((eq? mode 'wronly) open/wronly)
                         ((eq? mode 'rdwr)   open/rdwr)
                         (else               open/rdonly)))))
    (set! (slot-value storage 'mode) mode))

  (define-method (st-close (storage <file-storage>))
    (file-close (port storage))
    (set! (port storage) #f))

  (define-method (st-read (storage <file-storage>) count)
      (car (file-read (port storage) count)))

  (define-method (st-write (storage <file-storage>) data)
    (file-write
      (port storage)
      (if (bitstring? data) (bitstring->u8vector data) data)))

  (define-method (st-size (storage <file-storage>))
    (file-size (port storage)))

  (define-method (st-tell (storage <file-storage>))
    (file-position (port storage)))

  (define-method (st-seek (storage <file-storage>) position)
    (set-file-position! (port storage) position seek/set)))
