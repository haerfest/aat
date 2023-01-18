(module (aat storage)
  (<storage>
   st-open st-close st-read st-write st-size st-tell st-seek)

  (import
    coops
    scheme)

  (define-class <storage> ())

  (define-generic (st-open (storage <storage>)))

  (define-generic (st-close (storage <storage>)))

  (define-generic (st-read (storage <storage>) count))

  (define-generic (st-write (storage <storage>) data))

  (define-generic (st-size (storage <storage>)))

  (define-generic (st-tell (storage <storage>)))

  (define-generic (st-seek (storage <storage>) position)))
