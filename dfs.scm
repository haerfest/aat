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
   fs-mount fs-unmount fs-members fs-add fs-remove)

  (import
    (aat file)
    (aat fs)
    (aat storage)
    bitstring
    (chicken base)
    (chicken format)
    (chicken irregex)
    coops
    scheme
    srfi-1
    srfi-13
    srfi-151)

  ;; --------------------------------------------------------------------------

  ;; A DFS has an underlying STORAGE mechanism, a SECTORS-COUNT to indicate the
  ;; capacity of the disk (a sector is 256 bytes), and a CATALOG of files.
  ;; Note that the catalog is in reverse order: the first file in the catalog
  ;; is the last file on disk, i.e., the file with the highest start sector.
  (define-class <dfs> (<fs>)
    ((storage       accessor: storage)
     (sectors-count accessor: sectors-count)
     (catalog       accessor: catalog)))

  (define-method (fs-mount (fs <dfs>))
    (st-open (storage fs) 'rd)
    (read-catalog fs))

  (define-method (fs-unmount (fs <dfs>))
    (st-close (storage fs)))

  (define-method (fs-members (fs <dfs>))
    (catalog fs))

  (define-method (fs-add (fs <dfs>) file filespec)
    (if (or (= (length (catalog fs)) 31)
            (> (needed-sectors-count (f-size file))
               (available-sectors-count fs)))
      (error 'no-room)
      (let-values (((directory filename) (parse-filespec filespec)))
        (let* ((start-sector (first-available-sector fs))
               (new-file (make <dfs-file>
                               'directory    directory
                               'start-sector start-sector
                               'id           (format #f "~C.~A" directory filename)
                               'filename     filename
                               'load-addr    (f-load-addr file)
                               'exec-addr    (f-exec-addr file)
                               'size         (f-size file)
                               'locked?      (f-locked? file)
                               'readable?    (f-readable? file)
                               'writable?    (f-writable? file))))
          (sector fs start-sector)
          (st-write (storage fs) ((f-contents file)))
          (set! (catalog fs) (cons new-file (catalog fs)))
          (write-catalog fs)
          (set! (f-contents new-file) (make-reader fs new-file))))))

  (define-method (fs-remove (fs <dfs>) filespec)
    (let-values (((directory filename) (parse-filespec filespec)))
      (set! (catalog fs)
            (remove (lambda (file)
                      (and
                        (string-ci=? (f-directory file) directory)
                        (string-ci=? (f-filename  file) filename)))
                    (catalog fs)))
      (write-catalog fs)))

  ;; --------------------------------------------------------------------------

  ;; A specialisation of a <file>, now also tracking its DIRECTORY and
  ;; START-SECTOR on disc.
  (define-class <dfs-file> (<file>)
    ((directory    accessor: f-directory)
     (start-sector accessor: f-start-sector)))

  (define-method (to-string (file <dfs-file>))
    (format #f "~A dir=~C sector=~A"
      (call-next-method)
      (f-directory file)
      (f-start-sector file)))

  ;; Returns the last sector on disc occupied by a FILE.
  (define (last-sector file)
    (+ (f-start-sector file)
       (needed-sectors-count (f-size file))
       -1))

  ;; --------------------------------------------------------------------------

  ;; Split a "FILENAM" or "D.FILENAM" filespec into a directory
  ;; (defaults to $) and filename.
  (define (parse-filespec filespec)
    (let ((match (irregex-match "(?<directory>.)\.(?<filename>.+)" filespec)))
      (if match
        (values (string-ref (irregex-match-substring match "directory") 0)
                (irregex-match-substring match "filename"))
        (values #\$ filespec))))

  (define (needed-sectors-count size)
    (+ (quotient size 256)
       (zero? (remainder size 256) 0 1)))

  (define (first-available-sector fs)
    (apply max (cons 2 (map (lambda (file)
                              (+ (last-sector file) 1))
                            (catalog fs)))))

  (define (available-sectors-count fs)
    (- (sectors-count fs) (first-available-sector fs)))

  (define (sector fs n #!optional (offset 0))
    (st-seek (storage fs) (+ (* 256 n) offset)))

  (define (read-catalog fs)
    (define (inner file-count files)
      (if (zero? file-count)
        files
        (inner (sub1 file-count)
               (cons (catalog-file fs (sub1 file-count))
                     files))))
    (sector fs 1 5)
    (bitmatch (st-read (storage fs) 3)
      (((FileCount 5) (_ 3)
        (_ 6) (SectorCount 10 big))
       (begin
        (set! (catalog fs) (inner FileCount '()))
        (set! (sectors-count fs) SectorCount)))))

  (define (18-bits hi 16-bits)
    (bitwise-ior (arithmetic-shift hi 16) 16-bits))

  (define (make-reader fs file)
    (lambda ()
      (sector fs (f-start-sector file))
      (st-read (storage fs) (f-size file))))

  (define (catalog-file fs file-index)
    (sector fs 0 (+ 8 (* file-index 8)))
    (bitmatch (st-read (storage fs) 8)
      (((FileNamePadded (* 7 8) bitstring)
        (Locked 1) (Directory 7))
       (begin
        (sector fs 1 (+ 8 (* file-index 8)))
        (bitmatch (st-read (storage fs) 8)
          (((LoadAddress   16 little)
            (ExecAddress   16 little)
            (FileLength    16 little)
            (ExecAddressHi 2) (FileLengthHi 2) (LoadAddressHi 2)
            (StartSector   10 big))
           (let* ((directory (integer->char Directory))
                  (filename  (string-trim-right
                               (bitstring->string FileNamePadded)))
                  (size      (18-bits FileLengthHi FileLength))
                  (file      (make <dfs-file>
                                   'directory    directory
                                   'start-sector StartSector
                                   'id           (format #f "~C.~A" directory filename)
                                   'filename     filename
                                   'load-addr    (18-bits LoadAddressHi LoadAddress)
                                   'exec-addr    (18-bits ExecAddressHi ExecAddress)
                                   'size         size
                                   'locked?      (= Locked 1)
                                   'readable?    (= Locked 0)
                                   'writable?    (= Locked 0))))
             (set! (f-contents file) (make-reader fs file))
             file)))))))

  (define (pad str n)
    (if (>= n (string-length str))
      str
      (pad (string-append str " ") (sub1 n))))

  (define (write-catalog fs)
    (define (inner catalog index)
      (when (not (null? catalog))
        (let ((file (car catalog)))
          (sector fs 0 (* 8 index))
          (st-write (storage fs) (pad (f-filename file) 7))
          (st-write (storage fs) (f-directory file))
          (sector fs 1 (* 8 index))
          (st-write
            (storage fs)
            (bitconstruct
              ((bitwise-and      (f-load-addr file) #xFFFF) 16 little)
              ((bitwise-and      (f-exec-addr file) #xFFFF) 16 little)
              ((bitwise-and      (f-size      file) #xFFFF) 16 little)
              ((arithmetic-shift (f-exec-addr file)    -16) 2)
              ((arithmetic-shift (f-size      file)    -16) 2)
              ((arithmetic-shift (f-load-addr file)    -16) 2)
              ((f-start-sector file) 10 big))))
          (inner (cdr catalog) (add1 index))))
    (inner (catalog fs) 0)))
