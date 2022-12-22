;; -----------------------------------------------------------------------------
;; Represents a filesystem.
;; -----------------------------------------------------------------------------

(module (aat fs)
  (<fs>
   id mount unmount items add remove)

  (import
    (aat identifiable)
    coops
    scheme)

  (define-generic (mount fs))

  (define-generic (unmount fs))

  (define-generic (members fs))

  (define-generic (add fs))

  (define-generic (remove fs))

  (define-class <fs> (<identifiable>)
    ((items accessor: items))))
