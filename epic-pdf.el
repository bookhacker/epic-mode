;;; --------------------------------------------------------
;;;
(defun pdf-create ()
  "Creates .pdf file."
  (interactive)
  (odt-create)
  ;; #bbrinkmann 05.10.2015 auskommentiert
  ;; (setq odt-file-name (concat buffer-file-truename ".odt"))
  (message "Creating .pdf ...")
  ;; #bbrinkmann 05.10.2015
  ;;(org-odt-convert odt-file-name "pdf" nil)
  ;;(org-export-odt-convert odt-file-name "pdf" nil)
  (org-export-odt-convert odt-filename "pdf" nil)
  (message "Finished."))
