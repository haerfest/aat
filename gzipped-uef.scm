(module gzipped-uef
  (<gzipped-uef-archive> open-port open-file)

  (import
    (chicken io)
    (chicken pathname)
    coops
    scheme
    uef
    z3
    zlib)

  (define-class <gzipped-uef-archive> ())

  (define-method (open-port (port #t) (archive <gzipped-uef-archive>))
    (let ((uef-archive (make <uef-archive>)))
      (open-port port uef-archive)))

  (define-method (open-file (filepath #t) (archive <gzipped-uef-archive>))
    (and
      (string-ci=? "uef" (pathname-extension filepath))
      (let ((port (z3:open-compressed-input-file filepath)))
        (open-port port archive)))))