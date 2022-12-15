(module (aat file)
  (<file-archive> open-port open-file iterate)

  (import
    (chicken io)
    (chicken pathname)
    coops
    scheme)

  (define-class <file-archive> ()
    ((contents '())))

  (define-method (open-port (port #t) (archive <file-archive>))
    (set! (slot-value archive 'contents) (read-string #f port)))

  (define-method (open-file (filepath #t) (archive <file-archive>))
    (call-with-input-file
      filepath
      (lambda (port) (open-port port archive))
      #:binary))
  
  (define-method (iterate (archive <file-archive>))
    (list archive)))