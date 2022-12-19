;; -----------------------------------------------------------------------------
;; Represents an MMFS archive.
;;
;; See:
;; - https://github.com/hoglet67/MMFS
;; - https://sweh.spuddy.org/Beeb/mmb_utils.html
;; -----------------------------------------------------------------------------

(module (aat mmfs)
  (<mmfs> read-port members)

  (import
    (aat archive)
    bitstring
    (chicken base)
    (chicken io)
    coops
    scheme
    srfi-1)
  
  (define-generic (disks archive))

  (define-class <mmfs> (<archive>)
    ((disks initform: (make-vector 511 #f) accessor: disks)))

  (define-method (read-port (archive <mmfs>) (port #t))
    (parse-content (read-string (* 100 1024 1024) port) archive))

  (define-method (members (archive <mmfs>))
    (filter (lambda (disk) disk) (vector->list (disks archive))))

;; -----------------------------------------------------------------------------

  (define (null-terminated-bitstring->string bstr)
    (list->string
      (map
        (lambda (ascii) (integer->char ascii))
        (take-while
          (lambda (ascii) (not (zero? ascii)))
          (bitstring->list bstr 8)))))

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
        (Catalog (* 8 (- 8192 16)) bitstring)
        (Disks bitstring))
       (parse-catalog Catalog archive)))))
