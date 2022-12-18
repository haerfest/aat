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
    scheme)

  ;; https://sweh.spuddy.org/Beeb/mmb_utils.html

  (define-class <dfs> ()
    ((title initform: "" accessor: title)
     (write-cycle-count initform: 0 accessor: write-cycle-count)
     (opt-4 initform: 0 accessor: opt-4)
     (sector-count initform: 0 accessor: sector-count)
     (files initform: '() accessor: files)))

  (define (parse-content content dfs)
    (bitmatch (substring content 0 (* 2 256))
      (((DiskTitleFirst8 (* 8 8) bitstring)
        (FilenamesAndDirs (* 31 8 (+ 7 1)) bitstring)
        (DiskTitleLast4 (* 4 8) bitstring)
        (WriteCycleCount 8 unsigned)
        (FileCountTimes8 8 unsigned)
        (Ignored1 2)
        (Opt4 2)
        (Ignored2 2)
        (SectorCountHigh 2)
        (SectorCountLow 8 unsigned)
        (FileAttributes (* 31 8 8) bitstring))
       (begin
        (set! (title dfs)
          (string-append
            (bitstring->string DiskTitleFirst8)
            (bitstring->string DiskTitleLast4)))
        (set! (write-cycle-count dfs) WriteCycleCount)
        (set! (opt-4 dfs) Opt4)
        (set! (sector-count dfs) (+ (* SectorCountHigh 256) SectorCountLow))
        dfs))))

  (define-method (open-port (port #t) (archive <dfs>))
    (parse-content (read-string #f port) archive))
  
  (define-method (open-file (filepath #t) (archive <dfs>))
    (and
      (string-ci=? "ssd" (pathname-extension filepath))
      (call-with-input-file filepath
        (lambda (port) (open-port port archive))
        #:binary))))
