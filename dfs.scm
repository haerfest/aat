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
    srfi-13)

  (define-class <dfs> (<fs>)
    ((storage initform: #f  accessor: storage)
     (files   initform: '() accessor: files  )))

  (define-method (fs-mount (fs <dfs>))
    (st-open (storage fs) 'rd)
    (read-catalog fs))

  (define-method (fs-unmount (fs <dfs>))
    (st-close (storage fs))
    (set! (storage fs) #f))

  (define-method (fs-members (fs <dfs>))
    (files fs))

  (define-method (sector (fs <dfs>) n)
    (st-seek (storage fs) (* 256 n)))

  (define-method (offset (fs <dfs>) n)
    (st-seek (storage fs) (+ (st-tell (storage fs)) n)))

  (define-method (read-catalog (fs <dfs>))
    (sector fs 1) (offset fs 5)
    (bitmatch (st-read (storage fs) 1)
      (((FileCount 5) (_ 3))
       (set! (files fs) (catalog-files fs FileCount)))))

  (define (18-bits hi 16-bits)
    (+ (* hi #x10000) 16-bits))

  (define-class <dfs-file> (<file>)
    ((directory    accessor: f-directory)
     (start-sector accessor: f-start-sector)))

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
                                (sector StartSector)
                                (st-read (storage fs) size))))))))))

  (define-method (catalog-files (fs <dfs>) file-count #!optional (files '()))
    (if (= (length files) file-count)
      files
      (catalog-files fs file-count
                     (cons (catalog-file fs (length files)) files)))))

