(module (aat uef)
  (<uef> open-port open-file iterate)

  (import
    (aat file)
    bitstring
    (chicken base)
    (chicken bitwise)
    (chicken io)
    (chicken format)
    (chicken pathname)
    (chicken port)
    coops
    scheme)

  ;; The chunk class represents a UEF chunk. Each chunk consists of a numeric
  ;; identifier, a length, and a payload. The identifier determines how the
  ;; payload should be interpreted.
  (define-class <chunk> ()
    ((identifier accessor: identifier)
     (size accessor: size)
     (data accessor: data)))

  ;; A file is usually stored in multiple blocks on tape. A single block is
  ;; represented by this block class. See page 177 of the Acorn Electron
  ;; Advanced User Guide, describing the *ROM filing system data format.
  (define-class <block> ()
    ((filename accessor: filename)
     (load-addr accessor: load-addr)
     (exec-addr accessor: exec-addr)
     (number accessor: number)
     (size accessor: size)
     (flag accessor: flag)
     (header-crc accessor: header-crc)
     (data accessor: data)
     (data-crc accessor: data-crc)))

  (define (filename-length data #!optional (index 1))
    (cond
      ((= index (min 11 (string-length data))) (- index 1))
      ((eq? #\null (string-ref index data)) (- index 1))
      (else (filename-length data (+ index 1)))))
  
  (define-class <uef> ()
    ((version accessor: version)
     (files initform: '() accessor: files)))

  (define (read-version port)
    (bitmatch (read-string 2 port)
      (((minor 8 unsigned) (major 8 unsigned)) (cons major minor))))
  
  (define (read-chunk port)
    (if (eq? #!eof (peek-char port))
      #!eof
      (let ((chunk (make <chunk>)))
        (bitmatch (read-string 6 port)
          (((Identifier 16 little unsigned) (Size 32 little unsigned))
            (begin
              (set! (identifier chunk) Identifier)
              (set! (size chunk) Size)
              (set! (data chunk) (read-string Size port))
              chunk))))))

  (define (parse-block data)
    (let ((n (filename-length data))
          (block (make <block>)))
      (display (char->integer (string-ref data 0))) (newline)
      (bitmatch data
        (((#x2A)
          (Filename (* 8 n) bitstring)
          (#x00)
          (LoadAddr 32 little unsigned)
          (ExecAddr 32 little unsigned)
          (Number 16 little unsigned)
          (Size 16 little unsigned)
          (Flag 8 unsigned)
          (AddressNextFile 16 little unsigned)
          (HeaderCrc 16 little unsigned)
          (Data (* 8 size) bitstring)
          (DataCrc 16 little unsigned))
         (begin
          (set! (slot-value block 'filename) Filename)
          (set! (slot-value block 'load-addr) LoadAddr)
          (set! (slot-value block 'exec-addr) ExecAddr)
          (set! (slot-value block 'number) Number)
          (set! (slot-value block 'size) Size)
          (set! (slot-value block 'flag) Flag)
          (set! (slot-value block 'header-crc) HeaderCrc)
          (set! (slot-value block 'data) Data)
          (set! (slot-value block 'data-crc) DataCrc)
          block)))))

  (define (assemble-file-chunks chunk port)
    (let ((file (make <file>)))
      (define (assemble-blocks block)
        (append-data! (data block) file)
        (if (bitwise-and #x80 (flag block))
          file
          (assemble-blocks (parse-block (data (read-next-file-chunk port))))))
      (let ((block (parse-block (data chunk))))
        (set-attribute! 'filename (filename block) file)
        (set-attribute! 'load-addr (load-addr block) file)
        (set-attribute! 'exec-addr (exec-addr block) file)
        (assemble-blocks block))))

  (define (file-chunk? chunk)
    (or (= #x0100 (identifier chunk))
        (= #x0102 (identifier chunk))))

  (define (read-next-file-chunk port)
    (let ((chunk (read-chunk port)))
      (cond
        ((eq? #!eof chunk) #!eof)
        ((file-chunk? chunk) chunk)
        (else (read-next-file-chunk port)))))

  (define (read-file port)
    (let ((chunk (read-next-file-chunk port)))
      (if (eq? #!eof chunk)
        #!eof)
        (assemble-file-chunks chunk port)))

  (define (read-files port)
    (port-fold cons '() (lambda () (read-file port))))
          
  (define-method (open-port (port #t) (archive <uef>))
    (and
      (string=? "UEF File!" (read-string 9 port))
      (= 0 (read-byte port))
      (set! (version archive) (read-version port))
      (set! (files archive) (read-files port))))

  (define-method (open-file (filepath #t) (archive <uef>))
    (and
      (string-ci=? "uef" (pathname-extension filepath))
      (call-with-input-file filepath
        (lambda (port) (open-port port archive))
        #:binary)))
  
  (define-method (iterate (archive <uef>))
    (files archive)))