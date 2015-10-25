(defconst kindlegen "~/kindlegen/kindlegen")

;;; --------------------------------------------------------
;;;
(defun mobi-create ()
  "Create .mobi-file."
  (interactive)
  (setq isbn isbn-mobi)
  (setq result (epub-create))
  (message "Creating .mobi ...")
  (setq kindlegen-command (concat kindlegen " " epub-file-name))
  (setq result-string (shell-command-to-string kindlegen-command))
  (setq isbn nil)
  (message ".mobi created."))
