;; -----------------------------------------------------------------------------
;; Represents an identifiable item.
;; -----------------------------------------------------------------------------

(module (aat identifiable)
  (<identifiable> id)

  (import
    coops
    scheme)

  (define-class <identifiable> ()
    ((id initform: #f accessor: id))))
