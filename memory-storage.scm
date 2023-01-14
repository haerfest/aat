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
     (position)
     (mode)))

  (define-method (st-mode (storage <memory-storage>))
    (slot-value storage 'mode))

  (define-method (st-open (storage <memory-storage>) mode)
    (set! (slot-value storage 'mode) mode)
    (set! (slot-value storage 'position) 0)
    (when (and (backend storage) (or (eq? mode 'rd) (eq? mode 'rdwr)))
      (st-open (backend storage) mode)
      (set! (slot-value storage 'buffer)
            (st-read (backend storage) (st-size (backend storage))))
      (st-close (backend storage))))

  (define-method (st-close (storage <memory-storage>))
    (let ((mode (slot-value storage 'mode)))
      (when (and (backend storage) (or (eq? mode 'wr) (eq? mode 'rdwr)))
          (st-open (backend storage) mode)
          (st-write (backend storage) (slot-value storage 'buffer))
          (st-close (backend storage)))
      (set! (slot-value storage 'buffer) "")))

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
