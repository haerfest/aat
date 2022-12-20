;; -----------------------------------------------------------------------------
;; Represents an MMFS archive.
;;
;; See:
;; - https://github.com/hoglet67/MMFS
;; - https://sweh.spuddy.org/Beeb/mmb_utils.html
;; -----------------------------------------------------------------------------

(module (aat mmfs)
  (<mmfs> read-port members)

  (import
    (aat archive)
    (aat file)
    (aat dfs)
    bitstring
    (chicken base)
    (chicken io)
    coops
    scheme
    srfi-1)

  (define-class <mmfs> (<archive>)
    ((discs initform: (make-vector 511 #f) accessor: discs)))

  (define-method (read-port (archive <mmfs>) (port #t))
    (set! (contents archive) (read-string (* 100 1024 1024) port))
    (parse-contents archive))

  (define-method (members (archive <mmfs>))
    (lambda ()
      (filter (lambda (disc) disc) (vector->list (discs archive)))))

;; -----------------------------------------------------------------------------

  (define +status-readonly+    #x00)
  (define +status-readwrite+   #x0F)
  (define +status-unformatted+ #xF0)
  (define +status-invalid+     #xFF)

  (define-method (parse-contents (archive <mmfs>))
    (bitmatch (contents archive)
      (((#x00) (#x01) (#x02) (#x03) (#x00) (#x00) (#x00) (#x00)
        (#x00) (#x00) (#x00) (#x00) (#x00) (#x00) (#x00) (#x00)
        (Catalog (* 8 (- 8192 16)) bitstring)
        (Discs bitstring))
       (parse-catalog Catalog archive))))

  (define (->discname bitstr)
    (let* ((ascii-codes (bitstring->list bitstr 8))
           (count       (list-index zero? ascii-codes)))
      (list->string (map integer->char (take ascii-codes count)))))

  (define (parse-catalog bitstr archive #!optional (slot 0))
    (when (< slot 511)
      (bitmatch bitstr
        (((DiscName (* 12 8) bitstring)
          (#x00) (#x00) (#x00)
          (Status 8)
          (check (or (= Status +status-readonly+)
                     (= Status +status-readwrite+)
                     (= Status +status-unformatted+)
                     (= Status +status-invalid+)))
          (Remaining bitstring))
         (begin
          (when (or (= Status +status-readonly+)
                    (= Status +status-readwrite+))
            (let ((disc (make <dfs>)))
              (set-meta! disc 'slot slot)
              (set-meta! disc 'discname (->discname DiscName))
              (vector-set! (discs archive) slot disc)))
          (parse-catalog Remaining archive (+ slot 1))))))))
