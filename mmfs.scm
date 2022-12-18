(module (aat mmfs)
  (<mmfs> open-port open-file disks)

  (import
    (aat file)
    bitstring
    (chicken base)
    (chicken bitwise)
    (chicken format)
    (chicken io)
    (chicken pathname)
    (chicken port)
    coops
    list-comprehensions
    scheme
    srfi-1)
  
  (define-class <mmfs> ()
    ((disks initform: (make-vector 511 #f) accessor: disks)))

  (define (null-terminated-bitstring->string bstr)
    (list->string
      (map
        (lambda (ascii) (integer->char ascii))
        (take-while
          (lambda (ascii) (not (zero? ascii)))
          (bitstring->list bstr 8)))))

  ;; https://sweh.spuddy.org/Beeb/mmb_utils.html

  (define (parse-catalog catalog archive #!optional (disk-number 0))
    (when (< disk-number 511)
      (bitmatch catalog
        (((DiskName (* 12 8) bitstring)
          (#x00) (#x00) (#x00)
          (Status 8)
          (Remaining bitstring))
         (begin
          (when (or (= Status #x00) (= Status #x0F))
            (vector-set! (disks archive) disk-number (cons disk-number (null-terminated-bitstring->string DiskName))))
          (parse-catalog Remaining archive (+ disk-number 1)))))))

  (define (parse-content header archive)
    (bitmatch header
      (((#x00) (#x01) (#x02) (#x03) (#x00)(#x00)(#x00)(#x00)(#x00)(#x00)(#x00)(#x00)(#x00)(#x00)(#x00)(#x00)
        (Catalog bitstring))
       (parse-catalog Catalog archive))))

  (define-method (open-port (port #t) (archive <mmfs>))
    (parse-content (read-string 8192 port) archive))
  
  (define-method (open-file (filepath #t) (archive <mmfs>))
    (and
      (string-ci=? "mmb" (pathname-extension filepath))
      (call-with-input-file filepath
        (lambda (port) (open-port port archive))
        #:binary))))
