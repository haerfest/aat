;; -----------------------------------------------------------------------------
;; Represents an abstract archive.
;;
;; An archive has members, which are either files or archives themselves.
;; -----------------------------------------------------------------------------

(module (aat archive)
  (<archive> members read-port)

  (import coops)

  (define-generic (members archive))

  (define-generic (read-port archive))

  (define-class <archive> ()))