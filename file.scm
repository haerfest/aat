;; -----------------------------------------------------------------------------
;; Represents a file.
;; -----------------------------------------------------------------------------

(module (aat file)
  (<file>
   to-string
   f-id f-filename f-load-addr f-exec-addr f-size
   f-locked? f-readable? f-writable? f-contents)

  (import
    (chicken format)
    coops
    scheme)

  (define-class <file> ()
    ((id        accessor: f-id)
     (filename  accessor: f-filename)
     (load-addr accessor: f-load-addr)
     (exec-addr accessor: f-exec-addr)
     (size      accessor: f-size)
     (locked?   accessor: f-locked?)
     (readable? accessor: f-readable?)
     (writable? accessor: f-writable?)
     (contents  accessor: f-contents)))

  (define-method (to-string (file <file>))
    (format #f "id=~S filename=~S load=&~X exec=&~X size=~A ~C~C~C"
      (f-id        file)
      (f-filename  file)
      (f-load-addr file)
      (f-exec-addr file)
      (f-size      file)
      (if (f-locked?   file) #\L #\.)
      (if (f-readable? file) #\R #\.)
      (if (f-writable? file) #\W #\.))))
