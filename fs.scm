;; -----------------------------------------------------------------------------
;; Represents a filesystem.
;; -----------------------------------------------------------------------------

(module (aat fs)
  (<fs>
   id source items
   mount unmount add remove)

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
    ((source initform: #f accessor: source)
     (items initform: '() accessor: items))))
