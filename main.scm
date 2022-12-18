(load "dfs")
(load "file")
(load "uef")

(import
  (aat dfs)
  (aat file)
  ;(aat uef)
  (chicken format)
  coops)

(define (main)
  (let ((archive (make <dfs>)))
    (open-file "media/Elite.ssd" archive)
    (format #t "Title: ~A~%Sectors: ~A~%" (title archive) (sector-count archive))))

(main)