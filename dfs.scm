;; -----------------------------------------------------------------------------
;; Represents a DFS (Disc Filing System) archive.
;;
;; See:
;; - https://beebwiki.mdfs.net/Acorn_DFS_disc_format
;; - https://sweh.spuddy.org/Beeb/mmb_utils.html
;; -----------------------------------------------------------------------------

(module (aat dfs)
  (<dfs> members read-port)

  (import
    (aat archive)
    (aat file)
    bitstring
    (chicken base)
    (chicken bitwise)
    (chicken io)
    coops
    list-comprehensions
    scheme)

  (define-class <dfs> (<archive>))

  (define-method (read-port (disc <dfs>) (port #t))
    (set! (contents disc) (read-string #f port))
    (parse-contents disc))

;; -----------------------------------------------------------------------------

  (define (parse-filenames-and-dirs bitstr files)
    (when (not (null? files))
      (let ((file (car files)))
        (bitmatch bitstr
          (((Filename (* 8 7) bitstring)
            (Locked? 1)
            (Directory 7)
            (Remainder bitstring))
          (begin
            (set-meta! file 'filename (bitstring->string Filename))
            (set-meta! file 'directory (integer->char Directory))
            (set-meta! file 'locked? (= 1 Locked?))
            (parse-filenames-and-dirs Remainder (cdr files))))))))

  (define (parse-files-attributes bitstr files)
    (if (not (null? files))
      (let ((file (car files)))
        (bitmatch bitstr
          (((LoadAddr 16 little unsigned)
            (ExecAddr 16 little unsigned)
            (Size 16 little unsigned)
            (ExecAddrHigh 2)
            (SizeHigh 2)
            (LoadAddrHigh 2)
            (StartSector 10 big unsigned)
            (Remainder bitstring))
          (begin
            (set-meta! file 'load-addr (+ (* 65536 LoadAddrHigh) LoadAddr))
            (set-meta! file 'exec-addr (+ (* 65536 ExecAddrHigh) ExecAddr))
            (set-meta! file 'size (+ (* 65536 SizeHigh) Size))
            (set-meta! file 'start-sector StartSector)
            (parse-files-attributes Remainder (cdr files))))))))

  (define-method (parse-contents (disc <dfs>))
    (bitmatch (contents disc)
      ((
        ; first sector of 256 bytes
        (DiskTitleFirst8 (* 8 8) bitstring)
        (FilenamesAndDirs (* 31 8 (+ 7 1)) bitstring)
        ; second sector of 256 bytes
        (DiskTitleLast4 (* 4 8) bitstring)
        (WriteCycleCount 8 unsigned)
        (FileCountTimes8 8 unsigned)
        (_ 2) (Opt4 2) (_ 2)
        (SectorCount 10 big unsigned)
        (FilesAttributes (* 31 8 8) bitstring)
        ; remaining sectors
        (_ bitstring))
       (begin
        (set-meta! disc 'title
          (string-append
            (bitstring->string DiskTitleFirst8)
            (bitstring->string DiskTitleLast4)))
        (set-meta! disc 'write-cycle-count WriteCycleCount)
        (set-meta! disc 'opt-4 Opt4)
        (set-meta! disc 'sector-count SectorCount)
        (let ((files (map (lambda (_) (make <file>))
                          (range (/ FileCountTimes8 8)))))
            (parse-filenames-and-dirs FilenamesAndDirs files)
            (parse-files-attributes FilesAttributes files)
            ; the contents of each file are retrieved on-demand
            (for-each
              (lambda (file)
                (set!
                  (contents file)
                  (lambda ()
                    (let ((offset (* 256 (get-meta file 'start-sector)))
                          (size   (get-meta file 'size)))
                      (substring (contents disc) offset (+ offset size))))))
              files)
            (set! (members disc) (lambda () files))))))))
