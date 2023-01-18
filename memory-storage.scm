(module (aat memory-storage)
  (<memory-storage>
   st-open st-close st-read st-write st-size st-tell st-seek)

  (import
    (aat storage)
    (chicken base)
    coops
    scheme)

  (define-class <memory-storage> (<storage>)
    ((backend initform: #f accessor: backend)
     (buffer)
     (position)))

  (define-method (st-open (storage <memory-storage>))
    (set! (slot-value storage 'position) 0)
    (when (backend storage)
      (st-open (backend storage))
      (set! (slot-value storage 'buffer)
            (st-read (backend storage) (st-size (backend storage))))
      (st-close (backend storage))))

  (define-method (st-close (storage <memory-storage>))
    (when (backend storage)
      (st-open (backend storage))
      (st-write (backend storage) (slot-value storage 'buffer))
      (st-close (backend storage))))

  (define-method (st-read (storage <memory-storage>) count)
    (let ((position (slot-value storage 'position)))
      (substring (slot-value storage 'buffer) position (+ position count))))

  (define-method (st-write (storage <memory-storage>) data)
    (error "st-write not implemented"))

  (define-method (st-size (storage <memory-storage>))
    (length (slot-value storage 'buffer)))

  (define-method (st-tell (storage <memory-storage>))
    (slot-value storage 'position))

  (define-method (st-seek (storage <memory-storage>) position)
    (set! (slot-value storage 'position) position)))
