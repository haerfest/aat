;; -----------------------------------------------------------------------------
;; Represents a file.
;; -----------------------------------------------------------------------------

(module (aat file)
  (<file>
   f-id f-filename f-load-addr f-exec-addr f-size
   f-locked? f-readable? f-writable? f-contents)

  (import
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
     (contents  initform: #f  accessor: f-contents))))