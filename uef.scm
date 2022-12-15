;; http://electrem.emuunlim.com/UEFSpecs.htm

(module (aat uef)
  (<uef-archive> open-port open-file iterate)

  (import
    (aat file)
    (chicken base)
    (chicken io)
    (chicken format)
    (chicken pathname)
    (chicken port)
    coops
    scheme)

  (define-class <uef-archive> ()
    ((version '())
     (chunks '())))

  (define (read-version port)
    (let ((minor (read-byte port))
          (major (read-byte port)))
      (cons major minor)))
  
  (define (read-u16 port)
    (let ((lo (read-byte port))
          (hi (read-byte port)))
      (+ (* hi 256) lo)))
  
  (define (read-u32 port)
    (let ((lo (read-u16 port))
          (hi (read-u16 port)))
      (+ (* hi 65536) lo)))
  
  (define (read-chunk port)
    (let* ((identifier (read-u16 port))
           (length     (read-u32 port))
           (content    (read-string length port)))
      (list identifier length content)))

  (define (read-chunks port)
    (port-fold cons '() (lambda ()
                          (if (eq? #!eof (peek-char port))
                            #!eof
                            (read-chunk port)))))
          
  (define-method (open-port (port #t) (archive <uef-archive>))
    (and
      (string=? "UEF File!" (read-string 9 port))
      (= 0 (read-byte port))
      (set! (slot-value archive 'version) (read-version port))
      (set! (slot-value archive 'chunks) (read-chunks port))))

  (define-method (open-file (filepath #t) (archive <uef-archive>))
    (and
      (string-ci=? "uef" (pathname-extension filepath))
      (call-with-input-file filepath
        (lambda (port) (open-port port archive))
        #:binary)))
  
  (define (parse-block block)
    (define (find-null index)
      (if (or (= index (string-length block))
              (eq? #\null (string-ref block index)))
        index
        (find-null (+ index 1))))
    (let ((n (find-null 0)))
      (substring block 0 (- n 1))))
  
  (define (parse-chunk chunk)
    (let ((identifier (car chunk))
          (length     (cadr chunk))
          (content   (caddr chunk)))
      (if (or (= #x0100 identifier) (= #x0102 identifier))
        (parse-block content)
        (format #f "0x~X" identifier))))

  (define-method (iterate (archive <uef-archive>))
    (map parse-chunk (slot-value archive 'chunks))))