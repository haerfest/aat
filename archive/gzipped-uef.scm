(module gzipped-uef
  (<gzipped-uef-archive> open-port open-file iterate)

  (import
    (chicken io)
    (chicken pathname)
    coops
    scheme
    uef
    z3
    zlib)

  (define-class <gzipped-uef-archive> ()
    ((uef-archive (make <uef-archive>))))

  (define-method (open-port (port #t) (archive <gzipped-uef-archive>))
    (open-port port (slot-value archive 'uef-archive)))

  (define-method (open-file (filepath #t) (archive <gzipped-uef-archive>))
    (and
      (string-ci=? "uef" (pathname-extension filepath))
      (let ((port (z3:open-compressed-input-file filepath)))
        (open-port port archive))))
  
  (define-method (iterate (archive <gzipped-uef-archive>))
    (iterate (slot-value archive 'uef-archive))))