;; -----------------------------------------------------------------------------
;; Represents a DFS (Disc Filing System) disc.
;;
;; See:
;; - https://beebwiki.mdfs.net/Acorn_DFS_disc_format
;; - https://mdfs.net/Docs/Comp/Disk/Format/DFS
;; - https://sweh.spuddy.org/Beeb/mmb_utils.html
;; -----------------------------------------------------------------------------

(module (aat dfs)
  (<dfs>
   mount unmount members
   title write-cycle-count opt-4 sector-count)

  (import
    (aat file)
    (aat fs)
    bitstring
    (chicken base)
    (chicken format)
    (chicken io)
    coops
    coops-primitive-objects
    matchable
    scheme
    srfi-1
    srfi-13)

  (define-class <dfs> (<fs>)
    ((title             initform: "" accessor: title)
     (write-cycle-count initform: 0  accessor: write-cycle-count)
     (opt-4             initform: 0  accessor: opt-4)
     (sector-count      initform: 0  accessor: sector-count)))

  (define-method (mount (disc <dfs>) (port <port>))
    (parse disc (read-string #f port)))

  (define-method (unmount (disc <dfs>))
    #f)

;; -----------------------------------------------------------------------------

  (define (read-file bitstr start-sector size)
    (bitmatch bitstr
      (((_        (* 8 256 start-sector) bitstring)
        (Contents (* 8 size) bitstring)
        (_        bitstring))
       Contents)))

  (define (make-file args bitstr)
    (match args
      (((fn lckd?) (ld-addr ex-addr sz start-sector))
        (let ((file (make <file>)))
          (set! (filename  file) fn)
          (set! (locked?   file) lckd?)
          (set! (load-addr file) ld-addr)
          (set! (exec-addr file) ex-addr)
          (set! (size      file) sz)
          (set! (contents  file) (read-file bitstr start-sector sz))
          (set-meta! file 'dfs.start-sector start-sector)
          file))))

  (define +max-file-count+ 31)

  (define (parse-filenames bitstr file-count #!optional (acc '()))
    (if (zero? file-count)
      (reverse acc)
      (bitmatch bitstr
        (((Filename  (* 8 7) bitstring)
          (Locked?   1)
          (Directory 7)
          (Remainder bitstring))
         (parse-filenames
          Remainder
          (- file-count 1)
          (cons
            (list
              (format #f "~C.~A"
                (integer->char Directory)
                (string-trim-right (bitstring->string Filename)))
              (= 1 Locked?))
            acc))))))

  (define (parse-attributes bitstr file-count #!optional (acc '()))
    (if (zero? file-count)
      (reverse acc)
      (bitmatch bitstr
        (((LoadAddr     16 little unsigned)
          (ExecAddr     16 little unsigned)
          (Size         16 little unsigned)
          (ExecAddrHigh 2)
          (SizeHigh     2)
          (LoadAddrHigh 2)
          (StartSector  10 big unsigned)
          (Remainder    bitstring))
         (parse-attributes
          Remainder
          (- file-count 1)
          (cons
            (list
              (+ (* 65536 LoadAddrHigh) LoadAddr)
              (+ (* 65536 ExecAddrHigh) ExecAddr)
              (+ (* 65536 SizeHigh)     Size)
              StartSector)
            acc))))))

  (define (parse disc bitstr)
    (bitmatch bitstr
      ((
        ; first sector of 256 bytes
        (DiskTitleFirst8  (* 8 8) bitstring)
        (FilenamesAndDirs (* +max-file-count+ 8 (+ 7 1)) bitstring)
        ; second sector of 256 bytes
        (DiskTitleLast4   (* 4 8) bitstring)
        (WriteCycleCount  8 unsigned)
        (FileCountTimes8  8 unsigned)
        (_ 2) (Opt4 2) (_ 2)
        (SectorCount      10 big unsigned)
        (FilesAttributes  (* +max-file-count+ 8 8) bitstring)
        ; remaining sectors
        (_ bitstring))
       (begin
        (set! (title disc)
              (string-trim-right
                (string-append (bitstring->string DiskTitleFirst8)
                               (bitstring->string DiskTitleLast4))))
        (set! (write-cycle-count disc) WriteCycleCount)
        (set! (opt-4 disc) Opt4)
        (set! (sector-count disc) SectorCount)
        (let* ((file-count (/ FileCountTimes8 8))
               (filenames  (parse-filenames FilenamesAndDirs file-count))
               (attributes (parse-attributes FilesAttributes file-count)))
          (set! (members disc)
            (map
              (lambda (args)
                (make-file args bitstr))
              (zip filenames attributes)))))))))
