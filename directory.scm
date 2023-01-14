(module (aat directory)
  (<directory> d-name d-members)

  (import
    coops
    scheme)

  (define-class <directory> ()
    ((name    initform: #f  accessor: d-name)
     (members initform: '() accessor: d-members))))