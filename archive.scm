;; -----------------------------------------------------------------------------
;; Represents an archive.
;;
;; An archive has members, which are either files or archives themselves.
;; -----------------------------------------------------------------------------

(module (aat archive)
  (<archive> members read-port)

  (import
    (aat file)
    coops
    scheme)

  (define-generic (members archive))

  (define-generic (read-port archive))

  (define-class <archive> (<file>)
    ((members initform: (lambda () '()) accessor: members))))