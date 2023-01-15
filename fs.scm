(module (aat fs)
  (<fs>
   fs-mount fs-unmount fs-members)

  (import coops)

  (define-class <fs> ())

  (define-generic (fs-mount (fs <fs>)))

  (define-generic (fs-unmount (fs <fs>)))

  (define-generic (fs-members (fs <fs>)))

  (define-generic (fs-add (fs <fs>) file filepath)))