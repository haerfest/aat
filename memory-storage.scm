(module (aat memory-storage)
  (<memory-storage>
   st-open st-close st-read st-write st-size st-tell st-seek)

  (import
    (aat storage)
    (chicken base)
    coops
    scheme)

  (define-class <memory-storage> (<storage>)
    ((buffer  initform: "" accessor: buffer)
     (backend initform: #f accessor: backend)
     (mode)))

  (define-method (st-mode (storage <memory-storage>))
    (slot-value storage 'mode))

  (define-method (st-open (storage <memory-storage>) mode)
    (when (backend storage)
      (st-open (backend storage) mode)
      (set! (buffer storage) (st-read (backend storage)
                                      (st-tell (backend storage)))))
    (set! (slot-value storage 'mode) mode))

  (define-method (st-close (storage <memory-storage>))
    (when (backend storage)
      (when (or (eq? (st-mode (backend storage)) 'wr)
                (eq? (st-mode (backend storage)) 'rdwr))
        (st-seek (backend storage) 0)
        (st-write (backend storage) (buffer storage)))
      (st-close (backend storage))
      (set! (backend storage) #f))
    (set! (buffer storage) ""))

  (define-method (st-seek (storage <memory-storage>) position)
    'ok)

  (define-method (st-read (storage <memory-storage>) count)
    'ok)

  (define-method (st-write (storage <memory-storage>) data)
    'ok)

  (define-method (st-size (storage <memory-storage>))
    'ok)

  (define-method (st-tell (storage <memory-storage>))
    'ok)

  (define-method (st-seek (storage <memory-storage>) position)
    'ok))
