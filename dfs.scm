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
   to-string
   fs-mount fs-unmount fs-members)

  (import
    (aat directory)
    (aat file)
    (aat fs)
    (aat storage)
    bitstring
    (chicken base)
    (chicken format)
    coops
    scheme
    srfi-13
    srfi-151)

  (define-class <dfs-file> (<file>)
    ((directory    accessor: f-directory)
     (start-sector accessor: f-start-sector)))

  (define-method (to-string (file <dfs-file>))
    (format #f "~A dir=~C sector=~A"
      (call-next-method)
      (f-directory file)
      (f-start-sector file)))

  (define-method (last-sector (file <dfs-file>))
    (+ (f-start-sector file)
       (needed-sectors-count (f-size file))
       -1))

  (define-class <dfs> (<fs>)
    ((storage       initform: #f  accessor: storage)
     (sectors-count initform: 0   accessor: sectors-count)
     (files         initform: '() accessor: files  )))

  (define-method (fs-mount (fs <dfs>))
    (st-open (storage fs) 'rd)
    (read-catalog fs))

  (define-method (fs-unmount (fs <dfs>))
    (st-close (storage fs))
    (set! (storage fs) #f))

  (define-method (fs-members (fs <dfs>))
    (files fs))

  (define (parse-filepath filepath)
    (values #\$ filepath))

  (define-method (fs-add (fs <dfs>) file filepath)
    (if (or (< (length (files fs)) 31)
            (<= (needed-sectors-count (f-size file)) (available-sectors-count fs)))
      (error 'no-room)
      (let-values (((directory filename) (parse-filepath filepath)))
        (let* ((start-sector (first-available-sector fs))
               (new-file     (make <dfs-file>
                              'directory    directory
                              'start-sector start-sector
                              'id           (format #f "~C.~A" directory filename)
                              'filename     filename
                              'load-addr    (f-load-addr file)
                              'exec-addr    (f-exec-addr file)
                              'size         (f-size file)
                              'locked?      (f-locked? file)
                              'readable?    (f-readable? file)
                              'writable?    (f-writable? file)
                              'contents     (lambda ()
                                                (sector fs start-sector)
                                                (st-read (storage fs) (f-size file))))))
          (sector fs start-sector)
          (st-write (storage fs) ((f-contents file)))
          (set! (files fs) (cons new-file (files fs)))
          (write-catalog fs)))))

  (define (needed-sectors-count size)
    (+ (quotient size 256)
       (zero? (remainder size 256) 0 1)))

  (define-method (first-available-sector (fs <dfs>))
    (apply max
           (cons 2
                 (map (lambda (file) (+ (last-sector file) 1))
                      (files fs)))))

  (define-method (available-sectors-count (fs <dfs>))
    (- (sectors-count fs) (first-available-sector fs)))

  (define-method (sector (fs <dfs>) n)
    (st-seek (storage fs) (* 256 n)))

  (define-method (offset (fs <dfs>) n)
    (st-seek (storage fs) (+ (st-tell (storage fs)) n)))

  (define-method (read-catalog (fs <dfs>))
    (sector fs 1) (offset fs 5)
    (bitmatch (st-read (storage fs) 3)
      (((FileCount 5)
        (_ 3)
        (_ 6)
        (SectorCount 10 big))
       (begin
        (set! (files fs) (catalog-files fs FileCount))
        (set! (sectors-count fs) SectorCount)))))

  (define (18-bits hi 16-bits)
    (+ (* hi #x10000) 16-bits))

  (define-method (catalog-file (fs <dfs>) index)
    (sector fs 0) (offset fs (+ 8 (* index 8)))
    (bitmatch (st-read (storage fs) 8)
      (((FileNamePadded (* 7 8) bitstring)
        (Locked         1)
        (Directory      7))
       (begin
        (sector fs 1) (offset fs (+ 8 (* index 8)))
        (bitmatch (st-read (storage fs) 8)
          (((LoadAddress   16 little)
            (ExecAddress   16 little)
            (FileLength    16 little)
            (ExecAddressHi  2)
            (FileLengthHi   2)
            (LoadAddressHi  2)
            (StartSector   10 big))
           (let ((directory (integer->char Directory))
                 (filename  (string-trim-right
                              (bitstring->string FileNamePadded)))
                 (size      (18-bits FileLengthHi FileLength)))
            (make <dfs-file>
              'directory    directory
              'start-sector StartSector
              'id           (format #f "~C.~A" directory filename)
              'filename     filename
              'load-addr    (18-bits LoadAddressHi LoadAddress)
              'exec-addr    (18-bits ExecAddressHi ExecAddress)
              'size         size
              'locked?      (= Locked 1)
              'readable?    #t
              'writable?    #t
              'contents     (lambda ()
                                (sector fs StartSector)
                                (st-read (storage fs) size))))))))))

  (define-method (catalog-files (fs <dfs>) file-count #!optional (files '()))
    (if (= (length files) file-count)
      files
      (catalog-files fs file-count
                     (cons (catalog-file fs (length files)) files))))

  (define (pad str n)
    (if (= n (string-length str))
      str
      (pad (string-append str " ") (- n 1))))

  (define-method (write-catalog* (fs <dfs>) files index)
    (when (not (null? files))
      (let ((file (car files)))
        (sector fs 0) (offset fs (* 8 index))
        (st-write (storage fs) (pad (f-filename file) 7))
        (st-write (storage fs) (f-directory file))
        (sector fs 1) (offset fs (* 8 index))
        (st-write
            (storage fs)
            (bitconstruct
              ((bitwise-and (f-load-addr file) #xFFFF) 16 little)
              ((bitwise-and (f-exec-addr file) #xFFFF) 16 little)
              ((bitwise-and (f-size      file) #xFFFF) 16 little)
              ((arithmetic-shift (f-exec-addr file) -16) 2)
              ((arithmetic-shift (f-size      file) -16) 2)
              ((arithmetic-shift (f-load-addr file) -16) 2)
              ((f-start-sector file) 10 big))))
        (write-catalog* fs (cdr files) (+ index 1))))

  (define-method (write-catalog (fs <dfs>))
    (write-catalog* fs (files fs) 0)))
