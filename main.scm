(load "file")
(load "dfs")
(load "uef")
(load "mmfs")

(import
  ;(aat dfs)
  (aat file)
  (aat mmfs)
  ;(aat uef)
  (chicken format)
  coops)

(define (main)
  (let ((archive (make <mmfs>)))
    (open-file "media/beeb.mmb" archive)
    (for-each
      (lambda (disk)
        (when disk
          (format #t "~A~%" disk)))
      (vector->list (disks archive)))))

(main)