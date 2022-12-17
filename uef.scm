(module (aat uef)
  (<uef> open-port open-file version files)

  (import
    (aat file)
    bitstring
    (chicken base)
    (chicken bitwise)
    (chicken io)
    (chicken pathname)
    (chicken port)
    coops
    scheme)

  ;; Represents a UEF archive.
  (define-class <uef> ()
    ((version accessor: version)
     (files initform: '() accessor: files)))

  ;; The chunk class represents a UEF chunk. Each chunk consists of a numeric
  ;; identifier, a length, and a payload. The identifier determines how the
  ;; payload should be interpreted.
  (define-class <chunk> ()
    ((identifier accessor: identifier)
     (size accessor: size)
     (data accessor: data)))

  ;; A file is usually stored in multiple blocks on tape. A single block is
  ;; represented by this block class.
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

  ;; The filename in a block is of variable length and null-terminated. It
  ;; starts at index 1, right after the synchronisation byte, and is up to
  ;; ten characters in length. This function returns its length.
  (define (filename-length data #!optional (index 1))
    (cond
      ((= index (min 11 (string-length data))) (- index 1))
      ((eq? #\null (string-ref data index)) (- index 1))
      (else (filename-length data (+ index 1)))))
  
  ;; Reads the version number from a UEF archive.
  (define (read-version port)
    (bitmatch (read-string 2 port)
      (((minor 8 unsigned) (major 8 unsigned)) (cons major minor))))
  
  ;; Reads the next <chunk> from a UEF archive. Possibly returns #!eof.
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

  ;; Transforms a bitstring into a regular string.
  (define (bitstring->string x)
    (list->string (map integer->char (bitstring->list x 8))))

  ;; Parses a single tape block and returns a <block>.
  (define (parse-block chunk-data)
    (let ((n (filename-length chunk-data))
          (block (make <block>)))
      (bitmatch chunk-data
        (((#x2A)
          (Filename (* 8 n) bitstring)
          (#x00)
          (LoadAddr 32 little unsigned)
          (ExecAddr 32 little unsigned)
          (Number 16 little unsigned)
          (Size 16 little unsigned)
          (Flag 8 unsigned)
          (AddressNextFile 32 little unsigned)
          (HeaderCrc 16 little unsigned)
          (Data (* 8 Size) bitstring)
          (DataCrc 16 little unsigned))
         (begin
          (set! (filename block) (bitstring->string Filename))
          (set! (load-addr block) LoadAddr)
          (set! (exec-addr block) ExecAddr)
          (set! (number block) Number)
          (set! (size block) Size)
          (set! (flag block) Flag)
          (set! (header-crc block) HeaderCrc)
          (set! (data block) Data)
          (set! (data-crc block) DataCrc)
          block)))))

  ;; Assembles a single file from sequential chunks. The chunk argument is
  ;; the first chunk of the file. No checks are performed whether subsequent
  ;; chunks are in the right order or even of the same file.
  (define (assemble-file-chunks chunk port)
    (let ((file (make <file>)))
      (define (assemble-blocks block)
        (unless (bit->boolean (flag block) 6)
          (append-data! (data block) file))
        (if (bit->boolean (flag block) 7)
          file
          (assemble-blocks (parse-block (data (read-next-file-chunk port))))))
      (let ((block (parse-block (data chunk))))
        (set-attribute! 'filename (filename block) file)
        (set-attribute! 'load-addr (load-addr block) file)
        (set-attribute! 'exec-addr (exec-addr block) file)
        (set-attribute! 'run-only (bit->boolean (flag block) 0) file)
        (assemble-blocks block))))

  ;; Returns whether a chunk represents a file block.
  (define (file-chunk? chunk)
    (and
      (or (= #x0100 (identifier chunk))
          (= #x0102 (identifier chunk)))
      (eq? #\* (string-ref (data chunk) 0))))

  ;; Locates and returns the next file chunk in the UEF archive, skipping
  ;; any non-file chunks. Possibly returns #!eof.
  (define (read-next-file-chunk port)
    (let ((chunk (read-chunk port)))
      (cond
        ((eq? #!eof chunk) #!eof)
        ((file-chunk? chunk) chunk)
        (else (read-next-file-chunk port)))))

  ;; Reads the next file form a UEF archive. Possibly returns #!eof.
  (define (read-file port)
    (let ((chunk (read-next-file-chunk port)))
      (if (eq? #!eof chunk)
        #!eof
        (assemble-file-chunks chunk port))))

  ;; Reads all files from a UEF archive.
  (define (read-files port)
    (reverse (port-fold cons '() (lambda () (read-file port)))))

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
        #:binary))))
