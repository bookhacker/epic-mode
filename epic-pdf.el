;;; --------------------------------------------------------
;;;
(defun pdf-create ()
  "Creates .pdf file."
  (interactive)
  (odt-create)
  (setq odt-file-name (concat buffer-file-truename ".odt"))
  (message "Creating .pdf ...")
  (org-odt-convert odt-file-name "pdf" nil)
  (message "Finished."))
