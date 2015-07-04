;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;; ---------------------------------------------------------
;;;
(defun epic-insert-text-file (filename)
  "Inserts and formats a text-file in current-buffer."
  (interactive "FFind file: ")
  (switch-to-buffer (find-file-noselect filename))
  (setq lines-iterated 0)
  (save-excursion
    (goto-char 0)
    (while (< (point) (point-max))
      (setq current-line (get-current-line))
      (cond
       ((is-heading-style current-line)
	(setq line (epic-insert-get-clean-heading-style-line current-line))
	(debug)
	)
       ((is-location-style current-line)
	(debug)
	)
       ((is-persona-style current-line)
	(debug)
	)
       )

      ;;
      ;; Prefixe
      ;; "* " - HEADING
      ;; "** " - LOCATION
      ;; "*** " - PERSONA
      ;; "(" - Einschub
      ;; ^[A-Za-zöäüÖÄÜ ]* - Standard
      ;; ^[" "]*[a-zA-ZöäüÖÄÜ]*
      
;;      (setq current-style (get-style current-line))
      ;; (when (or
      ;; 	     (eq current-style heading-style)
      ;; 	     (eq current-style location-style)
      ;; 	     (eq current-style persons-style))
      ;; 	(with-current-buffer (get-buffer-create plot-buffer-name)
      ;; 	  (insert (get-line-without-footnotes current-line))
      ;; 	  (newline)
      ;; 	  (when (eq current-style persons-style)
      ;; 	    (newline)
      ;; 	    (newline)
      ;; 	    (newline))))
      (forward-line)
      (setq lines-iterated (+ lines-iterated 1))
      (message (concat "lines-iterated: " (number-to-string lines-iterated)))))
  )

;;; ---------------------------------------------------------
;;;
(defun epic-insert-get-clean-heading-style-line (line)
  "Returns a cleaned heading-style-line."  
  (interactive)
  (setq line (string-single-spaces line))
  (setq line (string-trim line))
  (setq line (upcase line))
  line)

;;; ---------------------------------------------------------
;;;
(ert-deftest epic-insert-test-get-clean-heading-style-line ()
  "Generic test."
  (should (string-equal (epic-insert-get-clean-heading-style-line "* Full Heading") "* FULL HEADING"))
  (should (string-equal (epic-insert-get-clean-heading-style-line "     * Full Heading       ") "* FULL HEADING"))
  (should (string-equal (epic-insert-get-clean-heading-style-line "   *     Full         HeaDIng    ") "* FULL HEADING"))
  (should (string-equal (epic-insert-get-clean-heading-style-line "   *     Full         HeaDIng       - furTHER text  ") "* FULL HEADING - FURTHER TEXT")))

;;; ---------------------------------------------------------
;;;
(defun string-single-spaces (s)
  "Replaces multiple spaces with single space."
  (setq s (replace-regexp-in-string "\s+" " " s))
  s)

;;
;; [From: http://ergoemacs.org/emacs/modernization_elisp_lib_problem.html]
;;
;;; ---------------------------------------------------------
;;;
(defun string-trim-left (s)
  "Remove whitespace at the beginning of S."
  (if (string-match "\\`[ \t\n\r]+" s)
      (replace-match "" t t s)
    s))

;;; ---------------------------------------------------------
;;;
(defun string-trim-right (s)
  "Remove whitespace at the end of S."
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))

;;; ---------------------------------------------------------
;;;
(defun string-trim (s)
  "Remove whitespace at the beginning and end of S."
  (string-trim-left (string-trim-right s)))
