(defconst kindlegen "~/kindlegen/kindlegen")

;;; --------------------------------------------------------
;;;
(defun mobi-create ()
  "Create .mobi-file."
  (interactive)
  (setq result (epub-create))
  (message "Creating .mobi ...")
  (setq kindlegen-command (concat kindlegen " " epub-file-name))
  (setq result-string (shell-command-to-string kindlegen-command))
  (message ".mobi created."))
