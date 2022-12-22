;; -----------------------------------------------------------------------------
;; Represents an MMFS archive.
;;
;; See:
;; - https://github.com/hoglet67/MMFS
;; - https://sweh.spuddy.org/Beeb/mmb_utils.html
;; -----------------------------------------------------------------------------

(module (aat mmfs)
  (<mmfs>
   id source items on-boot-0 on-boot-1 on-boot-2 on-boot-3
   mount unmount)

  (import
    (aat file)
    (aat fs)
    (aat dfs)
    bitstring
    (chicken base)
    (chicken format)
    (chicken io)
    coops
    coops-primitive-objects
    scheme
    srfi-1)

  (define-class <mmfs> (<fs>)
    ((discs initform: (make-vector 511 #f) accessor: discs)
     (on-boot-0 initform: 0 accessor: on-boot-0)
     (on-boot-1 initform: 0 accessor: on-boot-1)
     (on-boot-2 initform: 0 accessor: on-boot-2)
     (on-boot-3 initform: 0 accessor: on-boot-3)))

  (define-method (mount (archive <mmfs>))
    (parse archive (read-string #f (source archive))))

  (define-method (unmount (archive <mmfs>))
    #f)

  (define-method (items (archive <mmfs>))
    (filter (lambda (disc) disc) (vector->list (discs archive))))

;; -----------------------------------------------------------------------------

  (define +status-readonly+    #x00)
  (define +status-readwrite+   #x0F)
  (define +status-unformatted+ #xF0)
  (define +status-invalid+     #xFF)

  (define (parse archive bitstr)
    (bitmatch bitstr
      (((OnBoot0Low  8 unsigned)
        (OnBoot1Low  8 unsigned)
        (OnBoot2Low  8 unsigned)
        (OnBoot3Low  8 unsigned)
        (OnBoot0High 8 unsigned)
        (OnBoot1High 8 unsigned)
        (OnBoot2High 8 unsigned)
        (OnBoot3High 8 unsigned)
        (_         (* 8 8 ) bitstring)
        (Catalog   (* 8 (- 8192 16)) bitstring)
        (Discs     bitstring))
       (begin
        (set! (on-boot-0 archive) (+ (* 65536 OnBoot0High) OnBoot0Low))
        (set! (on-boot-1 archive) (+ (* 65536 OnBoot1High) OnBoot1Low))
        (set! (on-boot-2 archive) (+ (* 65536 OnBoot2High) OnBoot2Low))
        (set! (on-boot-3 archive) (+ (* 65536 OnBoot3High) OnBoot3Low))
        (parse-discs archive Catalog Discs)))))

  (define (->discname bitstr)
    (let* ((asciis (bitstring->list bitstr 8))
           (count  (list-index zero? asciis)))
      (list->string (map integer->char (take asciis count)))))

  (define (parse-discs archive catalog-bitstr discs-bitstr
                       #!optional (slot 0))
    (when (< slot 511)
      (bitmatch catalog-bitstr
        (((DiscName (* 8 12) bitstring)
          (#x00)
          (#x00)
          (#x00)
          (Status 8)
          (check (or (= Status +status-readonly+)
                     (= Status +status-readwrite+)
                     (= Status +status-unformatted+)
                     (= Status +status-invalid+)))
          (RemainingCatalog bitstring))
         (bitmatch discs-bitstr
          (((Contents       (* 8 200 1024) bitstring)
            (RemainingDiscs bitstring))
           (when (not (= Status +status-invalid+))
            (let ((disc (make <dfs> 'source Contents)))
              (set! (id disc) (format #f "~A.~A" slot (->discname DiscName)))
              (vector-set! (discs archive) slot disc)))
           (parse-discs archive RemainingCatalog RemainingDiscs
                        (+ slot 1)))))))))
