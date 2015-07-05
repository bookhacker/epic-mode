;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
(defconst edit-buffer-name "epic-edit")

;;; ---------------------------------------------------------
;;;
(defun epic-insert-text-file (filename)
  "Inserts and formats a text-file in current-buffer."
  (when (get-buffer edit-buffer-name)
    (kill-buffer edit-buffer-name))
  (interactive "FFind file: ")
  (switch-to-buffer (find-file-noselect filename))
  ;; ---------------------------------------------------------
  ;; Replace commands like enter, backspace etc.
  ;; ---------------------------------------------------------
  ;; " enter " and at end of line
  (setq search-string-regexp "\\ enter\\ \\| enter$")
  (while (search-in-buffer search-string-regexp)
    (replace-in-buffer search-string-regexp "\n "))
  ;; ---------------------------------------------------------
  ;; " enter("
  (setq search-string-regexp "\\ enter(")
  (while (search-in-buffer search-string-regexp)
    (replace-in-buffer search-string-regexp "\n("))
  ;; ---------------------------------------------------------
  ;; " enter…"
  (setq search-string-regexp "\\ enter…")
  (while (search-in-buffer search-string-regexp)
    (replace-in-buffer search-string-regexp "\n…"))
  ;; ---------------------------------------------------------
  ;; " enter-"
  (setq search-string-regexp "\\ enter-")
  (while (search-in-buffer search-string-regexp)
    (replace-in-buffer search-string-regexp "\n-"))
  ;; ---------------------------------------------------------
  ;; " enter?"
  (setq search-string-regexp "\\ enter?")
  (while (search-in-buffer search-string-regexp)
    (replace-in-buffer search-string-regexp "\n?"))
  ;; ---------------------------------------------------------
  ;; " enter."
  (setq search-string-regexp "\\ enter.")
  (while (search-in-buffer search-string-regexp)
    (replace-in-buffer search-string-regexp "\n."))
  ;; ---------------------------------------------------------
  ;; "Backspace"
  (setq search-string-regexp "Backspace")
  (while (search-in-buffer search-string-regexp)
    (replace-in-buffer search-string-regexp ""))
  ;; ---------------------------------------------------------
  ;; "Location"
  (setq search-string-regexp "Location\\ ")
  (while (search-in-buffer search-string-regexp)
    (replace-in-buffer search-string-regexp "** "))
  ;; ---------------------------------------------------------
  ;; "Einschub"
  (setq search-string-regexp "Einschub[\\ ]*")
  (while (search-in-buffer search-string-regexp)
    (replace-in-buffer search-string-regexp "("))
  ;;
  ;; Clean up
  ;;
  (setq lines-iterated 0)
  (save-excursion
    (setq previous-line "")
    (goto-char (point-min))      
    (while (< (point) (point-max))
      (setq line "")
      (setq current-line (get-current-line))
      (unless (eq (string-trim current-line) "")
	(cond
	 ;; ------------------------------------------------
	 ;; heading-style
	 ((is-heading-style current-line)
	  (setq line (epic-insert-get-clean-heading-style-line current-line)))
	 ;; ------------------------------------------------
	 ;; location-style
	 ((is-location-style current-line)	  
	  (setq line (epic-insert-get-clean-location-style-line current-line))
	  (unless (or (<= (point) 1)
		      (is-heading-style previous-line))
	    (setq line (concat "\n" line))))
	 ;; ------------------------------------------------
	 ;; persona-style
	 ((epic-insert-is-persona-style current-line)
	  (setq line (epic-insert-get-clean-persona-style-line current-line))
	  (setq line (concat line "\n"))
	  (unless (is-location-style previous-line)
	    (setq line (concat "\n" line))))
 	 ;; ------------------------------------------------
	 ;; insertion-style
	 ((epic-insert-is-insertion-style current-line)
	  (setq line (epic-insert-get-clean-insertion-style-line current-line)))
	 ;; ------------------------------------------------	 
	 ;; standard-style
	 ((epic-insert-is-standard-style current-line)
	  (setq line (epic-insert-get-clean-standard-style-line current-line)))
	 ;; ------------------------------------------------	 
	 ;; standard-continued-style
	 ((epic-insert-is-standard-continued-style current-line)
	  (setq line (epic-insert-get-clean-standard-continued-style-line current-line))))
	(unless (string-equal line "")
	  (epic-insert-edit-buffer-insert line)
	  (setq previous-line line)))
      (forward-line)
      (setq lines-iterated (+ lines-iterated 1))
      (message (concat "Finished - lines-iterated: " (number-to-string lines-iterated)))))
  (switch-to-buffer edit-buffer-name))

;;; ---------------------------------------------------------
;;;
(defun epic-insert-edit-buffer-insert (line)
  "Inserts line into edit buffer."
  (with-current-buffer (get-buffer-create edit-buffer-name)
    (insert line)(newline)))

;;; ---------------------------------------------------------
;;;
(defun epic-insert-is-persona-style (line)
  "Returns if line has style of persons."
  (setq temp-line line)
  (setq temp-line (string-trim temp-line))
  (string-prefix-p "*** " temp-line))

;;; ---------------------------------------------------------
;;;
(defun epic-insert-is-insertion-style (line)
  "Returns if line is insertion-line."
  (setq temp-line line)
  (setq temp-line (string-trim temp-line))
  (string-prefix-p "(" temp-line))

;;; ---------------------------------------------------------
;;;
(defun epic-insert-is-standard-style (line)
  "Returns if line is standard-line."
  (eq (string-match "^[a-zA-ZöäüÖÄÜ]" line) 0))

;;; ---------------------------------------------------------
;;;
(defun epic-insert-is-standard-continued-style (line)
  "Returns if line is standard-continued-line."
  (eq (string-match "\s+[a-zA-ZöäüÖÄÜ]" line) 0))

;;; ---------------------------------------------------------
;;;
(defun epic-insert-get-clean-heading-style-line (line)
  "Returns a cleaned heading-style-line."  
  (setq line (string-single-spaces line))
  (setq line (string-trim line))
  (setq line (upcase line))
  line)

;;; ---------------------------------------------------------
;;;
(defun epic-insert-get-clean-location-style-line (line)
  "Returns a cleaned heading-style-line."  
  (setq line (string-single-spaces line))
  (setq line (string-trim line))
  (when (string-match "[a-zA-ZöäüÖÄÜ]\\'" line)
    (setq line (concat line ".")))
  line)

;;; ---------------------------------------------------------
;;;
(defun epic-insert-get-clean-persona-style-line (line)
  "Returns a cleaned heading-style-line."  
  (setq line (string-single-spaces line))
  (setq line (string-trim line))
  (when (string-match "[a-zA-ZöäüÖÄÜ]\\'" line)
    (setq line (concat line ".")))
  line)

;;; ---------------------------------------------------------
;;;
(defun epic-insert-get-clean-insertion-style-line (line)
  "Returns a cleaned insertion-style-line."  
  (setq line (string-single-spaces line))
  (setq line (string-trim line))
  (setq line (concat indentation line))
  (when (string-match "[a-zA-ZöäüÖÄÜ]\\'" line)
    (setq line (concat line ".")))
  (when (string-match "\\.\\'" line)
    (setq line (concat line ")")))
  line)

;;; ---------------------------------------------------------
;;;
(defun epic-insert-get-clean-standard-style-line (line)
  "Returns a cleaned standard-style-line."  
  (setq line (string-single-spaces line))
  (setq line (string-trim line))
  (setq position (string-match "(" line))  
  (if position
      (progn
       (setq index 0)
       (setq new-line "")
       (while (< index (string-width line))
	 (setq current-char (aref line index))      
	 (when (<= index position)
	   (setq current-char (upcase current-char)))
	 (setq new-line (concat new-line (char-to-string current-char)))
	 (setq index (+ index 1)))
       (when (string-match "[a-zA-ZöäüÖÄÜ]\\'" new-line)
	 (setq new-line (concat new-line ")")))
       (when (string-match ")\\'" new-line)
	 (setq new-line (concat new-line ".")))
       new-line)
    (setq position (string-match "\\." line))
    (if position
	(progn
	  (setq index 0)
	  (setq new-line "")
	  (while (< index (string-width line))
	    (setq current-char (aref line index))      
	    (when (<= index position)
	      (setq current-char (upcase current-char)))
	    (setq new-line (concat new-line (char-to-string current-char)))
	    (setq index (+ index 1)))   
	  new-line)
      (message "Error in epic-insert-get-clean-standard-style-line (line): position is nil.")
      (debug))))

;;; ---------------------------------------------------------
;;;
(defun epic-insert-get-clean-standard-continued-style-line (line)
  "Returns a cleaned standard-continuedstyle-line."  
  (setq line (string-single-spaces line))
  (setq line (string-trim line))
  (setq line (concat indentation line)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ---------------------------------------------------------
;;
;;                            Tests
;;
;;; ---------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ---------------------------------------------------------
;;;
(ert-deftest epic-insert-test-is-persona-style ()
  "Tests if line is persona-style."
  (should (eq (epic-insert-is-persona-style "*** Persona") t))
  (should (eq (epic-insert-is-persona-style "       *** Persona") t))
  (should (eq (epic-insert-is-persona-style "**** Persona") nil))
  (should (eq (epic-insert-is-persona-style " ** Persona") nil)))  

;;; ---------------------------------------------------------
;;;
(ert-deftest epic-insert-test-is-insertion-style ()
  "Tests if line is insertion."
  (should (eq (epic-insert-is-insertion-style "(insertion") t))
  (should (eq (epic-insert-is-insertion-style " (insertion") t))
  (should (eq (epic-insert-is-insertion-style "  (insertion") t))
  (should (eq (epic-insert-is-insertion-style "   (insertion") t))
  (should (eq (epic-insert-is-insertion-style "                              (insertion") t))
  (should (eq (epic-insert-is-insertion-style "  insertion") nil))
  (should (eq (epic-insert-is-insertion-style "inser(tion") nil)))

;;; ---------------------------------------------------------
;;;
(ert-deftest epic-insert-test-is-standard-style ()
  "Tests if line is standard-style."
  (should (eq (epic-insert-is-standard-style "asdDFSDFfasdf") t))
  (should (eq (epic-insert-is-standard-style "ASDFsertion") t))
  (should (eq (epic-insert-is-standard-style " ASDFsertion") nil))
  (should (eq (epic-insert-is-standard-style "(ASDFsertion") nil))
  (should (eq (epic-insert-is-standard-style "-ASDFsertion") nil)))

;;; ---------------------------------------------------------
;;;
(ert-deftest epic-insert-test-is-standard-continued-style ()
  "Tests if line is standard-style."
  (should (eq (epic-insert-is-standard-continued-style " asdDFSDFfasdf") t))
  (should (eq (epic-insert-is-standard-continued-style "                ASDFsertion") t))
  (should (eq (epic-insert-is-standard-continued-style "ASDFsertion") nil))
  (should (eq (epic-insert-is-standard-continued-style "  (ASDFsertion") nil))
  (should (eq (epic-insert-is-standard-continued-style " *ASDFsertion") nil)))

;;; ---------------------------------------------------------
;;;
(ert-deftest epic-insert-test-get-clean-heading-style-line ()
  "Tests for clean heading-style line."
  (should (string-equal (epic-insert-get-clean-heading-style-line "* Full Heading") "* FULL HEADING"))
  (should (string-equal (epic-insert-get-clean-heading-style-line "* Full Heading - further text") "* FULL HEADING - FURTHER TEXT"))
  (should (string-equal (epic-insert-get-clean-heading-style-line "     * Full Heading       ") "* FULL HEADING"))
  (should (string-equal (epic-insert-get-clean-heading-style-line "   *     Full         HeaDIng    ") "* FULL HEADING"))
  (should (string-equal (epic-insert-get-clean-heading-style-line "   *     Full         HeaDIng       - furTHER text  ") "* FULL HEADING - FURTHER TEXT")))

;;; ---------------------------------------------------------
;;;
(ert-deftest epic-insert-test-get-clean-location-style-line ()
  "Test for clean location-style line."
  (should (string-equal (epic-insert-get-clean-location-style-line "** Location Location Location.") "** Location Location Location."))
  (should (string-equal (epic-insert-get-clean-location-style-line "** Location          Location. Location") "** Location Location. Location.")))

;;; ---------------------------------------------------------
;;;
(ert-deftest epic-insert-test-get-clean-persona-style-line ()
  "Test for clean persona-style line."
  (should (string-equal (epic-insert-get-clean-persona-style-line "***   PERSONA.      Persona   ") "*** PERSONA. Persona."))
  (should (string-equal (epic-insert-get-clean-persona-style-line "        ***   PERSONA.      Persona   ") "*** PERSONA. Persona.")))

;;; ---------------------------------------------------------
;;;
(ert-deftest epic-insert-test-get-clean-insertion-style-line ()
  "Test for clean insertion-style line."
  (should (string-equal (epic-insert-get-clean-insertion-style-line "          (insertion     insertion.       insertion") (concat indentation "(insertion insertion. insertion.)")))
  (should (string-equal (epic-insert-get-clean-insertion-style-line "(insertion     insertion.       insertion.") (concat indentation "(insertion insertion. insertion.)"))))

;;; ---------------------------------------------------------
;;;
(ert-deftest epic-insert-test-get-clean-standard-style-line ()
  "Test for clean standard-style line."
  (should (string-equal (epic-insert-get-clean-standard-style-line "abc (asdf") "ABC (asdf)."))
  (should (string-equal (epic-insert-get-clean-standard-style-line "abc.  asdf") "ABC. asdf"))
  (should (string-equal (epic-insert-get-clean-standard-style-line "ABC (asdf") "ABC (asdf)."))  
  (should (string-equal (epic-insert-get-clean-standard-style-line "ABC. asdf asdf asdf") "ABC. asdf asdf asdf")))

;;; ---------------------------------------------------------
;;;
(ert-deftest epic-insert-test-get-clean-standard-continued-style-line ()
  "Returns a formatted standard-continued-style line."
  (should (string-equal (epic-insert-get-clean-standard-continued-style-line "                     asdf asdf asdf") "  asdf asdf asdf"))
  (should (string-equal (epic-insert-get-clean-standard-continued-style-line " asdf asdf asdf") "  asdf asdf asdf")))

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

;;
;; [From: http://ergoemacs.org/emacs/elisp_find_replace_text.html]
;;
;;; ---------------------------------------------------------
;;;
(defun search-in-buffer (search-string-regexp)
  "Search in buffer for regexp."
  (let ((case-fold-search t)) ; or nil
    (goto-char (point-min))
    (search-forward-regexp search-string-regexp nil t)))

;;; ---------------------------------------------------------
;;;
(defun replace-in-buffer (search-string-regexp replace-string)
  "Idiom for string replacement in current buffer."
  (let ((case-fold-search t)) ; or nil
    (goto-char (point-min))
    ;; if you need regexp, use search-forward-regexp
    (while (search-forward-regexp search-string-regexp nil t)
      (replace-match replace-string))))
