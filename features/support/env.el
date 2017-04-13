(require 'f)

(defvar exsqlaim-support-path
  (f-dirname load-file-name))

(defvar exsqlaim-features-path
  (f-parent exsqlaim-support-path))

(defvar exsqlaim-root-path
  (f-parent exsqlaim-features-path))

(add-to-list 'load-path exsqlaim-root-path)

;; Ensure that we don't load old byte-compiled versions
(let ((load-prefer-newer t))
  (require 'exsqlaim)
  (require 'espuds)
  (require 'ert))

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
