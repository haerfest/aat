(module (aat dfs)
  (<dfs> open-port open-file title write-cycle-count opt-4 sector-count files)

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
    scheme)

  ;; https://sweh.spuddy.org/Beeb/mmb_utils.html

  (define-class <dfs> ()
    ((title initform: "" accessor: title)
     (write-cycle-count initform: 0 accessor: write-cycle-count)
     (opt-4 initform: 0 accessor: opt-4)
     (sector-count initform: 0 accessor: sector-count)
     (files initform: '() accessor: files)))

  (define (parse-filenames-and-dirs bitstr files)
    (when (pair? files)
      (bitmatch bitstr 
        (((Filename (* 8 7) bitstring)
          (Locked? 1)
          (Directory 7)
          (Remainder bitstring))
         (let ((file (car files)))
          (set-attribute! 'filename (bitstring->string Filename) file)
          (set-attribute! 'directory (integer->char Directory) file)
          (set-attribute! 'locked? (= 1 Locked?) file)
          (parse-filenames-and-dirs Remainder (cdr files)))))))
  
  (define (parse-file-attributes bitstr files)
    (if (pair? files)
      (bitmatch bitstr 
        (((LoadAddr 16 little unsigned)
          (ExecAddr 16 little unsigned)
          (Size 16 little unsigned)
          (ExecAddrHigh 2)
          (SizeHigh 2)
          (LoadAddrHigh 2)
          (StartSector 10 big unsigned)
          (Remainder bitstring))
         (let ((file (car files)))
          (set-attribute! 'load-addr (+ (* 65536 LoadAddrHigh) LoadAddr) file)
          (set-attribute! 'exec-addr (+ (* 65536 ExecAddrHigh) ExecAddr) file)
          (set-attribute! 'size (+ (* 65536 SizeHigh) Size) file)
          (set-attribute! 'start-sector StartSector file)
          (parse-file-attributes Remainder (cdr files)))))))

  (define (parse-content content dfs)
    (bitmatch (substring content 0 (* 2 256))
      (((DiskTitleFirst8 (* 8 8) bitstring)
        (FilenamesAndDirs (* 31 8 (+ 7 1)) bitstring)
        (DiskTitleLast4 (* 4 8) bitstring)
        (WriteCycleCount 8 unsigned)
        (FileCountTimes8 8 unsigned)
        (_ 2)
        (Opt4 2)
        (_ 2)
        (SectorCount 10 big unsigned)
        (FileAttributes (* 31 8 8) bitstring))
       (begin
        (set! (title dfs)
          (string-append
            (bitstring->string DiskTitleFirst8)
            (bitstring->string DiskTitleLast4)))
        (set! (write-cycle-count dfs) WriteCycleCount)
        (set! (opt-4 dfs) Opt4)
        (set! (sector-count dfs) SectorCount)
        (let ((fls (map (lambda (_) (make <file>)) (range (/ FileCountTimes8 8)))))
            (parse-filenames-and-dirs FilenamesAndDirs fls)
            (parse-file-attributes FileAttributes fls)
            (set! (files dfs) fls))))))

  (define-method (open-port (port #t) (archive <dfs>))
    (parse-content (read-string #f port) archive))
  
  (define-method (open-file (filepath #t) (archive <dfs>))
    (and
      (string-ci=? "ssd" (pathname-extension filepath))
      (call-with-input-file filepath
        (lambda (port) (open-port port archive))
        #:binary))))
