(load "storage")
(load "file-storage")
(load "memory-storage")
(load "fs")
(load "dfs")

(import
  (aat file-storage)
  (aat memory-storage)
  (aat dfs)
  (chicken format)
  coops
  scheme)

(define (main)
  (let* ((file   (make <file-storage> 'filepath "media/Elite.ssd"))
         (memory (make <memory-storage> 'backend file))
         (dfs    (make <dfs> 'storage memory)))
    (fs-mount dfs)
    (for-each (lambda (member)
                (format #t "~S~%" member))
              (fs-members dfs))
    (fs-unmount dfs)))

(main)