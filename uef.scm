(module uef
  (<uef-archive> open-port open-file)

  (import
    (chicken io)
    (chicken pathname)
    coops
    scheme)

  (define-class <uef-archive> ())

  (define-method (open-port (port #t) (archive <uef-archive>))
    (and
      (string=? "UEF File!" (read-string 9 port))
      (= 0 (read-byte port))))

  (define-method (open-file (filepath #t) (archive <uef-archive>))
    (and
      (string-ci=? "uef" (pathname-extension filepath))
      (call-with-input-file filepath
        (lambda (port) (open-port port archive))
        #:binary))))