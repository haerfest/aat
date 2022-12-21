;; -----------------------------------------------------------------------------
;; Represents a filesystem.
;; -----------------------------------------------------------------------------

(module (aat fs)
  (<fs> mount unmount members add remove)

  (import
    coops
    scheme)

  (define-generic (mount fs))

  (define-generic (unmount fs))

  (define-generic (members fs))

  (define-generic (add fs))

  (define-generic (remove fs))

  (define-class <fs> ()
    ((members initform: '() accessor: members))))
