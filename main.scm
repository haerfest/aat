(import
  (chicken io)
  (chicken pathname)
  coops
  z3
  zlib)

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
      #:binary)))

(define-class <gzipped-uef-archive> ())

(define-method (open-port (port #t) (archive <gzipped-uef-archive>))
  (let ((uef-archive (make <uef-archive>)))
    (open-port port uef-archive)))

(define-method (open-file (filepath #t) (archive <gzipped-uef-archive>))
  (and
    (string-ci=? "uef" (pathname-extension filepath))
    (let ((port (z3:open-compressed-input-file filepath)))
      (open-port port archive))))

(define (main)
  (let ((gzipped-uef-archive (make <gzipped-uef-archive>)))
    (open-file "Forth_E.uef" gzipped-uef-archive)))

(display (main))
(newline)