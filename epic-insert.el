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
      (unless (eq (string-trim current-line) "")
	(cond
	 ((is-heading-style current-line)
	  (setq line (epic-insert-get-clean-heading-style-line current-line)))
	 ((is-location-style current-line)
	  (setq line (epic-insert-get-clean-location-style-line current-line)))
	 ((epic-insert-is-persona-style current-line)
	  (setq line (epic-insert-get-clean-persona-style-line current-line)))
	 ((epic-insert-is-insertion-style current-line)
	  (setq line (epic-insert-get-clean-insertion-style-line current-line)))
	 ((epic-insert-is-standard-style current-line)
	  (setq line (epic-insert-get-clean-standard-style-line current-line)))
	 ((epic-insert-is-standard-continued-style current-line)
	  (setq line (epic-insert-get-clean-standard-continued-style-line current-line)))	
	 ))
      (forward-line)
      (setq lines-iterated (+ lines-iterated 1))
      (message (concat "lines-iterated: " (number-to-string lines-iterated))))))

;;; ---------------------------------------------------------
;;;
(defun epic-insert-is-persona-style (line)
  "Returns if line has style of persons."
  (string-prefix-p "*** " line))

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
(ert-deftest epic-insert-test-is-standard-style_ORIGINAL ()
  "Tests if line is standard-style."
  (should (not (eq (epic-insert-is-standard-style "asdDFSDFfasdf") nil)))
  (should (not (eq (epic-insert-is-standard-style "ASDFsertion") nil)))
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
  (should (string-equal (epic-insert-get-clean-persona-style-line "***   PERSONA.      Persona   ") "*** PERSONA. Persona.")))

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
