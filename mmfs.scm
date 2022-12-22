;; -----------------------------------------------------------------------------
;; Represents an MMFS archive.
;;
;; See:
;; - https://github.com/hoglet67/MMFS
;; - https://sweh.spuddy.org/Beeb/mmb_utils.html
;; -----------------------------------------------------------------------------

(module (aat mmfs)
  (<mmfs>
   id mount unmount members)

  (import
    (aat file)
    (aat fs)
    (aat dfs)
    bitstring
    (chicken base)
    (chicken io)
    coops
    scheme
    srfi-1)

  (define-class <mmfs-slot> ())
  (define-class <mmfs> (<fs>)
    ((discs initform: (make-vector 511 #f) accessor: discs)
     (slot0 initform: 0 accessor: slot0)
     (slot1 initform: 0 accessor: slot1)
     (slot2 initform: 0 accessor: slot2)
     (slot3 initform: 0 accessor: slot3)))

  (define-method (mount (archive <mmfs>) (port <port>))
    (parse archive (read-string #f port)))

  (define-method (items (archive <mmfs>))
    (filter (lambda (disc) disc) (vector->list (discs archive)))))

;; -----------------------------------------------------------------------------

  (define +status-readonly+    #x00)
  (define +status-readwrite+   #x0F)
  (define +status-unformatted+ #xF0)
  (define +status-invalid+     #xFF)

  (define (parse archive bitstr)
    (bitmatch bitstr
      (((Slot0Low  8 unsigned)
        (Slot1Low  8 unsigned)
        (Slot2Low  8 unsigned)
        (Slot3Low  8 unsigned)
        (Slot0High 8 unsigned)
        (Slot1High 8 unsigned)
        (Slot2High 8 unsigned)
        (Slot3High 8 unsigned)
        (Catalog (* 8 (- 8192 16)) bitstring)
        (Discs   bitstring))
       (begin
        (set! (slot0 archive) (+ (* 65536 Slot0High) Slot0Low))
        (set! (slot1 archive) (+ (* 65536 Slot1High) Slot1Low))
        (set! (slot2 archive) (+ (* 65536 Slot2High) Slot2Low))
        (set! (slot3 archive) (+ (* 65536 Slot3High) Slot3Low))
        (parse-catalog archive Catalog)))))

  (define (->discname bitstr)
    (let* ((ascii-codes (bitstring->list bitstr 8))
           (count       (list-index zero? ascii-codes)))
      (list->string (map integer->char (take ascii-codes count)))))

  (define (parse-catalog archive bitstr #!optional (slot 0))
    (when (< slot 511)
      (bitmatch bitstr
        (((DiscName (* 12 8) bitstring)
          (#x00)
          (#x00)
          (#x00)
          (Status   8)
          (check (or (= Status +status-readonly+)
                     (= Status +status-readwrite+)
                     (= Status +status-unformatted+)
                     (= Status +status-invalid+)))
          (Remaining bitstring))
         (begin
          (when (not (= Status +status-invalid+))
            (let ((disc (make <dfs>)))
              (set! (id disc) (cons (->discname DiscName) slot))
              (vector-set! (discs archive) slot disc)))
          (parse-catalog Remaining archive (+ slot 1))))))))
