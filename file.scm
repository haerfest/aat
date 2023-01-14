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
    ((id        initform: 0   accessor: f-id)
     (filename  initform: ""  accessor: f-filename)
     (load-addr initform: 0   accessor: f-load-addr)
     (exec-addr initform: 0   accessor: f-exec-addr)
     (size      initform: 0   accessor: f-size)
     (locked?   initform: #f  accessor: f-locked?)
     (readable? initform: #t  accessor: f-readable?)
     (writable? initform: #t  accessor: f-writable?)
     (contents  initform: #f  accessor: f-contents)))

  (define-method (to-string (file <file>))
    (format #f "id=~S filename=~S load=&~X exec=&~X size=~A ~C~C~C"
      (f-id file)
      (f-filename file)
      (f-load-addr file)
      (f-exec-addr file)
      (f-size file)
      (if (f-locked?   file) #\L #\space)
      (if (f-readable? file) #\R #\space)
      (if (f-writable? file) #\W #\space))))