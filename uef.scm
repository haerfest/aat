;; -----------------------------------------------------------------------------
;; Represents a UEF (Unified Emulator Format) tape image.
;;
;; See:
;; - http://electrem.emuunlim.com/UEFSpecs.htm
;; - https://beebwiki.mdfs.net/Acorn_cassette_format
;; -----------------------------------------------------------------------------

(module (aat uef)
  (<uef> read-port members)

  (import
    (aat archive)
    (aat file)
    bitstring
    (chicken base)
    (chicken io)
    coops
    scheme
    srfi-1)

  (define-class <uef> (<archive>))

  (define-method (read-port (tape <uef>) (port #t))
    (set! (contents tape) (read-string #f port))
    (parse-contents tape))

;; -----------------------------------------------------------------------------

  (define-method (parse-contents (tape <uef>))
    (bitmatch (contents tape)
      ((("UEF File!")
        (#x00)
        (VersionMinor 8 unsigned)
        (VersionMajor 8 unsigned)
        (Remaining bitstring))
       (begin
        (set-meta! tape 'version (cons VersionMajor VersionMinor))
        (set!
          (members tape)
          (blocks->files
            (map parse-block (filter block? (parse-chunks Remaining)))))))))

  (define-class <chunk> ()
    ((id accessor: id)
     (size accessor: size)
     (data accessor: data)))

  (define (parse-chunks bitstr #!optional (chunks '()))
    (bitmatch bitstr
      (((Id 16 little unsigned)
        (Size 32 little unsigned)
        (Data (* 8 Size) bitstring)
        (Remaining bitstr))
       (let ((chunk (make <chunk>)))
        (set! (id chunk) Id)
        (set! (size chunk) Size)
        (set! (data chunk) Data)
        (parse-chunks Remaining (cons chunk chunks))))
      (else (reverse chunks))))

  (define (block? chunk)
    (and
      (or (= (id chunk) #x0100)
          (= (id chunk) #x0102))
      (not (zero? (size chunk)))
      (let ((first-byte (string-ref (bitstring->string (data chunk) 8) 0)))
        (eq? #\* first-byte))))

  (define-class <block> ()
    ((filename accessor: filename)
     (load-addr accessor: load-addr)
     (exec-addr accessor: exec-addr)
     (number accessor: number)
     (size accessor: size)
     (last? accessor: last?)
     (locked? accessor: locked?)
     (header-crc accessor: header-crc)
     (data accessor: data)
     (data-crc accessor: data-crc)))

  (define (find-null bitstr #!optional (index 0))
    (bitmatch bitstr
      (((#x00) (_ bitstring)) index)
      (((_ 8) (Remaining bitstring)) (find-null Remaining (+ index 1)))))

  (define (parse-block bitstr)
    (let ((filename-length (- (find-null bitstr 1) 1)))
      (bitmatch bitstr
        (((#\*)
          (Filename (* 8 filename-length) bitstring)
          (#x00)
          (LoadAddr 32 little unsigned)
          (ExecAddr 32 little unsigned)
          (Number 16 little unsigned)
          (Size 16 little unsigned)
          (Last? 1) (_ 6) (Locked? 1)
          (AddressNextFile 32 little unsigned)
          (HeaderCrc 16 little unsigned)
          (DataAndCrc bitstring))
         (let ((block (make <block>)))
          (set! (filename block) (bitstring->string Filename))
          (set! (load-addr block) LoadAddr)
          (set! (exec-addr block) ExecAddr)
          (set! (number block) Number)
          (set! (size block) Size)
          (set! (last? block) Last?)
          (set! (locked? block) Locked?)
          (set! (header-crc block) HeaderCrc)
          (when (not (zero? Size))
            (bitmatch DataAndCrc
              (((Data (* 8 Size) bitstring)
                (DataCrc 16 little unsigned))
               (begin
                (set! (data block) Data)
                (set! (data-crc block) DataCrc)))))
          block)))))

  (define (blocks->files blocks #!optional (files '()) (file #f) (expected-nr 0))
    (if (null? blocks)
      (reverse files)
      (let ((block (car blocks))
            (remaining (cdr blocks)))
        (cond
          ; finished any previous file, and start of a new file?
          ((and (not file) (= (number block) 0))
            (let ((file (make <file>)))
              (set-meta! file 'filename  (filename block))
              (set-meta! file 'load-addr (load-addr block))
              (set-meta! file 'exec-addr (exec-addr block))
              (set-meta! file 'locked?   (= (locked? block) 1))
              (set! (contents file) (data block))
              (if (last? block)
                (blocks->files remaining (cons file files))
                (blocks->files remaining files file 1))))
          ; completing a file, and expected next block number?
          ((and file (= (number block) expected-nr))
           (set! (contents file) (bitstring-append (contents file) (data block)))
           (if (last? block)
            (blocks->files remaining (cons file files))
            (blocks->files remaining files file (+ expected-nr 1))))
          ; neither, skip this block
          (else
            (blocks->files remaining files file expected-nr)))))))