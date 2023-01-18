(module (aat file-storage)
  (<file-storage>
  st-open st-close st-read st-write st-size st-tell st-seek)

  (import
    (aat storage)
    bitstring
    (chicken base)
    (chicken file posix)
    coops
    scheme)

  (define-class <file-storage> (<storage>)
    ((filepath initform: #f  accessor: filepath)
     (port     initform: #f  accessor: port)))

  (define-method (st-open (storage <file-storage>))
    (set! (port storage)
      (file-open (filepath storage)
                 (+ open/binary open/rdwr open/creat))))

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
