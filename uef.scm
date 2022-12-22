;; -----------------------------------------------------------------------------
;; Represents a UEF (Unified Emulator Format) tape image.
;;
;; See:
;; - http://electrem.emuunlim.com/UEFSpecs.htm
;; - https://beebwiki.mdfs.net/Acorn_cassette_format
;; -----------------------------------------------------------------------------

(module (aat uef)
  (<uef> mount unmount members)

  (import
    (aat file)
    (aat fs)
    bitstring
    (chicken base)
    (chicken format)
    (chicken io)
    coops
    coops-primitive-objects
    scheme
    srfi-1
    srfi-13)

  (define-class <uef> (<fs>)
    ((version initform: '() accessor: version)))

  (define-method (mount (tape <uef>) (port <port>))
    (parse tape (read-string #f port)))

  (define-method (unmount (tape <uef>))
    #f)

;; -----------------------------------------------------------------------------

  ; https://daviddavidson.website/threading-macros-scheme/
  (define-syntax ->>
    (syntax-rules ()
      ((->> value)
        value)
      ((->> value (fn args ...) rest ...)
        (->> (fn args ... value) rest ...))
      ((->> value fn rest ...)
        (->> (fn value) rest ...))))

  (define (parse tape bitstr)
    (bitmatch bitstr
      ((("UEF File!")
        (#x00)
        (VersionMinor 8 unsigned)
        (VersionMajor 8 unsigned)
        (Remaining    bitstring))
       (begin
        (set! (version tape) (cons VersionMajor VersionMinor))
        (set!
          (members tape)
          (->> Remaining
            parse-chunks
            (filter block?)
            (map parse-block)
            blocks->files))))))

  (define-class <chunk> ()
    ((id       accessor: id)
     (size     accessor: size)
     (contents accessor: contents)))

  (define (parse-chunks bitstr #!optional (chunks '()))
    (bitmatch bitstr
      (((Id        16 little unsigned)
        (Size      32 little unsigned)
        (Contents  (* 8 Size) bitstring)
        (Remaining bitstring))
       (let ((chunk (make <chunk>)))
        (set! (id chunk)       Id)
        (set! (size chunk)     Size)
        (set! (contents chunk) Contents)
        (parse-chunks Remaining (cons chunk chunks))))
      (else (reverse chunks))))

  (define (block? chunk)
    (and
      (or (= (id chunk) #x0100)
          (= (id chunk) #x0102))
      (not (zero? (size chunk)))
      (let ((first-byte (string-ref (bitstring->string (contents chunk)) 0)))
        (eq? #\* first-byte))))

  (define-class <block> ()
    ((filename     accessor: filename)
     (load-addr    accessor: load-addr)
     (exec-addr    accessor: exec-addr)
     (number       accessor: number)
     (size         accessor: size)
     (last?        accessor: last?)
     (locked?      accessor: locked?)
     (header-crc   accessor: header-crc)
     (contents     accessor: contents)
     (contents-crc accessor: contents-crc)))

  (define (parse-block chunk)
    (let* ((bitstr (contents chunk))
           (index  (list-index zero? (take (bitstring->list bitstr 8) 12)))
           (n      (- index 1)))
      (bitmatch bitstr
        (((#\*)
          (Filename        (* 8 n) bitstring)
          (#x00)
          (LoadAddr        32 little unsigned)
          (ExecAddr        32 little unsigned)
          (Number          16 little unsigned)
          (Size            16 little unsigned)
          (Last?           1) (_ 6) (Locked? 1)
          (AddressNextFile 32 little unsigned)
          (HeaderCrc       16 little unsigned)
          (ContentsAndCrc  bitstring))
         (let ((block (make <block>)))
          (set! (filename   block) (string-trim-right
                                    (bitstring->string Filename)))
          (set! (load-addr  block) LoadAddr)
          (set! (exec-addr  block) ExecAddr)
          (set! (number     block) Number)
          (set! (size       block) Size)
          (set! (last?      block) (= Last? 1))
          (set! (locked?    block) (= Locked? 1))
          (set! (header-crc block) HeaderCrc)
          (if (not (zero? Size))
            (bitmatch ContentsAndCrc
              (((Contents    (* 8 Size) bitstring)
                (ContentsCrc 16 little unsigned))
               (begin
                (set! (contents     block) Contents)
                (set! (contents-crc block) ContentsCrc))))
            (begin
              (set! (contents     block) (string->bitstring ""))
              (set! (contents-crc block) 0)))
          block)))))

  (define (blocks->files blocks
                         #!optional (files '()) (file #f) (expected-nr 0))
    (if (null? blocks)
      (reverse files)
      (let ((block     (car blocks))
            (remaining (cdr blocks)))
        (cond
          ; finished any previous file, and start of a new file?
          ((and (not file) (= (number block) 0))
            (let ((file (make <file>)))
              (set! (filename  file) (filename  block))
              (set! (load-addr file) (load-addr block))
              (set! (exec-addr file) (exec-addr block))
              (set! (locked?   file) (locked?   block))
              (set! (contents  file) (contents  block))
              (if (last? block)
                (begin
                  (set! (size file) (/ (bitstring-length (contents file)) 8))
                  (blocks->files remaining (cons file files)))
                (blocks->files remaining files file 1))))
          ; continuing a file, and expected next block number?
          ((and file
                (string=? (filename block) (filename file))
                (= (number block) expected-nr))
           (set!
            (contents file)
            (bitstring-append (contents file) (contents block)))
           (if (last? block)
            (begin
              (set! (size file) (/ (bitstring-length (contents file)) 8))
              (blocks->files remaining (cons file files)))
            (blocks->files remaining files file (+ expected-nr 1))))
          ; neither, skip this block
          (else
            (blocks->files remaining files file expected-nr)))))))