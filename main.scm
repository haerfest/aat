(load "storage")
(load "file-storage")
(load "memory-storage")
(load "fs")
(load "dfs")

(import
  (aat file-storage)
  (aat dfs)
  (chicken format)
  coops
  scheme)

(define (main)
  (let ((elite (make <dfs> 'storage (make <file-storage> 'filepath "media/Elite.ssd"))))
    (fs-mount elite)
    (for-each (lambda (member) (format #t "~S~%" member)) (fs-members elite))
    (fs-unmount elite)))

(main)