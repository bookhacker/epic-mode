;; (defconst odt-template-file-name                   "epos_CreateSpace - 5.06 x 7.81.ott")
(defconst odt-template-file-name                    "epos_BOD - 135 x 215.ott")
(defconst odt-author-paragraph-prefix               "<text:p text:style-name=\"w2e_Autor\">")
(defconst odt-title-paragraph-prefix                "<text:p text:style-name=\"w2e_Titel\">")
(defconst odt-schmutztitel-paragraph-prefix         "<text:p text:style-name=\"w2e_Schmutztitel\">")
(defconst odt-subtitle-paragraph-prefix             "<text:p text:style-name=\"w2e_Untertitel\">")
(defconst odt-episoda-paragraph-prefix              "<text:p text:style-name=\"w2e_Episoda\">")
(defconst odt-schmutztitel-episoda-paragraph-prefix "<text:p text:style-name=\"w2e_Episoda_Schmutztitel\">")
(defconst odt-publisher-paragraph-prefix            "<text:p text:style-name=\"w2e_Verlag\">")
(defconst odt-legal-notice-paragraph-prefix         "<text:p text:style-name=\"w2e_Impressum\">")
(defconst odt-heading-paragraph-prefix              "<text:h text:style-name=\"Heading_20_2\" text:outline-level=\"2\">")
(defconst odt-standard-paragraph-prefix             "<text:p text:style-name=\"w2e_Standard\">")
(defconst odt-normal-paragraph-prefix               "<text:p text:style-name=\"w2e_Normal\">")
(defconst odt-normal-paragraph-first-paragraph-prefix "<text:p text:style-name=\"w2e_Normal_FirstParagraph\">")
(defconst odt-standard-paragraph-continued-prefix   "<text:p text:style-name=\"w2e_Standard_Unterbrochen\">")
(defconst odt-insertion-paragraph-prefix            "<text:p text:style-name=\"w2e_Einschub\">")
(defconst odt-location-paragraph-prefix             "<text:p text:style-name=\"w2e_Schauplatz\">")
(defconst odt-person-paragraph-prefix               "<text:p text:style-name=\"w2e_Personen\">")
(defconst odt-person-standalone-paragraph-prefix    "<text:p text:style-name=\"w2e_Personen_Standalone\">")
(defconst odt-personae-standard-paragraph-prefix    "<text:p text:style-name=\"w2e_Personae\">")
(defconst odt-empty-page-paragraph-prefix           "<text:p text:style-name=\"w2e_EmptyPage\">")
(defconst odt-paragraph-postfix                     "</text:p>\n")
(defconst odt-heading-postfix                       "</text:h>\n")
(defconst odt-line-break                            "<text:line-break/>")
(defconst odt-italics-prefix                        "<text:span text:style-name=\"w2e_Kursiv\">")
(defconst odt-italics-postfix                       "</text:span>")
(defconst odt-bold-prefix                           "<text:span text:style-name=\"w2e_bold\">")
(defconst odt-bold-postfix                          "</text:span>")
(defconst odt-footnote-prefix                       "<text:note text:id=\"ftnX\" text:note-class=\"footnote\"><text:note-citation>X</text:note-citation><text:note-body><text:p text:style-name=\"P95\">")
(defconst odt-footnote-postfix                      "</text:p></text:note-body></text:note>")
(defconst org-buffer-name                           "org-buffer")
(defvar   org-file-name                             "")
(defconst odt-personae-buffer                       "odt-personae-buffer")
(defconst odt-autor-buffer                          "odt-autor-buffer")
(defconst odt-appendix-buffer                       "odt-appendix-buffer")
(defconst odt-whathappenedsofar-buffer              "odt-whathappenedsofar-buffer")
(defconst meta-xml-buffer                           "meta-xml-buffer")
(defconst odt-impressum-buffer                      "odt-impressum-buffer")

;;; ---------------------------------------------------------
;;;
(defun create-odt-header ()
  "Inserts odt-header."
  (insert
   (concat "#+ODT_STYLES_FILE: \"" (file-name-as-directory library-directory) (file-name-as-directory "templates") odt-template-file-name "\"

#+TITLE: 
#+DATE:
#+AUTHOR:
#+OPTIONS: ':nil *:t -:t ::t <:t H:3 \\n:nil ^:t arch:headline
#+OPTIONS: title:nil author:nil c:nil creator:comment d:(not LOGBOOK) date:nil e:t
#+OPTIONS: email:nil f:t inline:t num:t p:nil pri:nil stat:t tags:t
#+OPTIONS: tasks:t tex:t timestamp:nil toc:nil todo:t |:t
#+CREATOR: 
#+DESCRIPTION:
#+EXCLUDE_TAGS: noexport
#+KEYWORDS:
#+LANGUAGE: de
#+SELECT_TAGS: export"))(newline)
(newline))

;;; ---------------------------------------------------------
;;;
(defun begin-odt-document ()
  "Inserts beginning of .odt-document."
  (insert "#+BEGIN_ODT")(newline))

;;; ---------------------------------------------------------
;;;
(defun end-odt-document ()
  "Inserts end of .odt-document."
  (insert "#+END_ODT"))

;;; ---------------------------------------------------------
;;;
(defun odt-create ()
  "Exports text to org-format for further converting."
  (interactive)
  (setq odt-filename (concat buffer-file-truename ".odt"))
  (setq unzip-directory (file-name-directory buffer-file-truename))
  (setq meta-xml-file (concat (file-name-as-directory unzip-directory) "meta.xml"))
  (with-current-buffer (get-buffer-create org-buffer-name)
    (create-odt-header)
    (begin-odt-document))
  (iterate-odt-buffer)
  (setq org-filename (concat buffer-file-truename ".org"))
  (with-current-buffer (get-buffer-create org-buffer-name)
    (end-odt-document)
    (write-file org-filename nil)
    (kill-buffer (current-buffer)))
  (setq org-buffer (find-file org-filename))
  (with-current-buffer (get-buffer-create org-buffer)
    ;; Neu #bbrinkmann 05.10.2015
    ;; original
    (org-odt-export-to-odt)
    ;; test
    ;; (org-export-as-odt 0)
    (kill-buffer (current-buffer)))
  (uncompress-odt-file)
  (edit-meta-xml)
  (compress-odt-file)
  (message "Finished."))

;;; ---------------------------------------------------------
;;;
(defun edit-meta-xml ()
  "Edits uncompressed meta.xml."
  (when (file-exists-p meta-xml-file)
    (with-current-buffer (get-buffer-create meta-xml-buffer)
      (find-file meta-xml-file)
      (setq string-to-replace "<dc:title></dc:title>")
      (setq replace-string (concat "<dc:title>" title " - " subtitle "</dc:title>\n"))
      (if (eq episoda-number "")
	  (setq replace-string (concat replace-string "<meta:user-defined meta:name=\"Untertitel\">" title " - " subtitle "</meta:user-defined>"))
	(setq replace-string (concat replace-string "<meta:user-defined meta:name=\"Untertitel\">Episoda " episoda-number ": " episoda-title "</meta:user-defined>")))	
      ;; idiom for string replacement in current buffer;
      (let ((case-fold-search t)) ; or nil
	(goto-char (point-min))
	(while (search-forward string-to-replace nil t) (replace-match replace-string)))
      (write-file meta-xml-file nil)
      (kill-buffer (current-buffer)))))

;;; ---------------------------------------------------------
;;;
(defun uncompress-odt-file ()
  "Uncompresses the .odt-file."
  (with-current-buffer (get-buffer-create "zip-buffer")
    (setq unzip-command (concat "unzip " odt-filename " -d " unzip-directory))
    (shell-command unzip-command t)
    (kill-buffer (current-buffer))))

;;; ---------------------------------------------------------
;;;
(defun compress-odt-file ()
  "Compresses unzipped files to original .odt-file."
  (delete-file odt-filename)
  (setq zip-command (concat "zip -0Xq " odt-filename " mimetype && zip -Xr9Dq " odt-filename " META-INF/manifest.xml content.xml meta.xml styles.xml"))
  (shell-command zip-command t)
  (delete-file "mimetype")
  (delete-directory "META-INF" t)
  (delete-file "content.xml")
  (delete-file "meta.xml")
  (delete-file "styles.xml"))

;;; ---------------------------------------------------------
;;;
(defun person-template-get-formatted-line (array)
  "Returns a formatted person-template line."
  (setq new-string "")
  (setq last-char-was-uppercase nil)
  (setq second-last-char-was-uppercase nil)
  (setq index 0)
  (setq is-footnote-input nil)
  (while (< index (string-width array))
    (setq current-char (aref array index))
    (when (and (= current-char 91) ; 91 [
	       (eq is-footnote-input nil))
      (setq is-footnote-input t))
    (when (and (= current-char 93) ; 93 ]
	       (eq is-footnote-input t))
      (setq is-footnote-input nil))
    (when (eq is-footnote-input nil)
      ;; italics-prefix
      (when (or (and (= index 1)
		     (is-lowercase-letter current-char)
		     last-char-was-uppercase)
		(and (is-lowercase-letter current-char)
		     last-char-was-uppercase
		     second-last-char-was-uppercase))
	(setq char-before-insertion (aref array (- index 1)))
	(if (= char-before-insertion 32) ; 32 <SPACE>
	    (progn
	      (setq new-string (substring new-string 0))
	      (setq new-string (concat new-string odt-italics-prefix)))
	  (setq new-string (substring new-string 0 (- (string-width new-string) 1)))
	  (setq new-string (concat new-string odt-italics-prefix (char-to-string char-before-insertion)))))
      ;; italics-postfix
      (when (or 
	     (and (> index 1)
		  (is-uppercase-letter current-char)
		  last-char-was-uppercase
		  (eq second-last-char-was-uppercase nil)))
	(setq char-before-insertion (aref array (- index 1)))
	(setq new-string (substring new-string 0 (- (string-width new-string) 2)))
	(setq new-string (concat new-string odt-italics-postfix " " (char-to-string char-before-insertion)))))
    (setq new-string (concat new-string (char-to-string current-char)))
    (when (eq is-footnote-input nil)
      (setq second-last-char-was-uppercase last-char-was-uppercase)
      (when (is-uppercase-letter current-char)
	(setq last-char-was-uppercase t))  
      (when (is-lowercase-letter current-char)
	(setq last-char-was-uppercase nil)))
    (setq index (+ index 1)))
  (when (and (eq (is-uppercase-letter current-char) nil)
	     (eq last-char-was-uppercase nil))
    (setq new-string (concat new-string odt-italics-postfix)))
  new-string)

;;; ---------------------------------------------------------
;;;
(defun apply-bold (line)
  "Replaces * with bold-prefx and bold-postfix."
  (setq index 0)
  (setq new-string "")
  (with-current-buffer (get-buffer-create org-buffer-name)
    (while (< index (string-width line))
      (setq current-char (aref line index))
      (if (= current-char 42) ; 42 *
	  (progn
	    (setq new-string (substring new-string 0))
	    (if (eq bold-input nil)
		(progn
		  (setq new-string (concat new-string odt-bold-prefix))
		  (setq bold-input t))
	      (setq new-string (concat new-string odt-bold-postfix))
	      (setq bold-input nil)))
	(setq new-string (concat new-string (char-to-string current-char))))
      (setq index (+ index 1)))
    (setq line new-string))
  line)

;;; ---------------------------------------------------------
;;;
(defun odt-apply-style (style line last-style)
  "Applies the selected template."
  (with-current-buffer (get-buffer-create org-buffer-name)
    (when (and (eq style standard-style)
	       (or
		(and (eq last-style standard-style)
		     (not (string-prefix-p indentation line)))
		(eq last-style standard-interrupted-style)))
      (insert odt-paragraph-postfix)(newline))
    (when (and (or (eq style persons-style)
		   (eq style persons-standalone-style)
		   (eq style location-style)
		   (eq style heading-style)
		   (eq style insertion-style))
	       (or (eq last-style standard-style)
		   (eq last-style standard-interrupted-style)))
      (insert odt-paragraph-postfix)(newline)))
  (cond
   ;; standard-template
   ((eq style standard-style)
    (if (string-prefix-p indentation line)
	(setq line (concat odt-line-break (substring line 2)))
      ;; everything between ( and ) in italics.
      (setq line-to-insert (replace-regexp-in-string "(" (concat odt-italics-prefix "(") line))
      (setq line-to-insert (replace-regexp-in-string ")" (concat")"  odt-italics-postfix) line-to-insert))
      (setq line (concat odt-standard-paragraph-prefix line-to-insert))))
   ;; standard-template-continued
   ((eq style standard-interrupted-style)
    (if (not (or (eq last-style standard-style)
		 (eq last-style standard-interrupted-style)))
	(setq line (concat odt-standard-paragraph-continued-prefix (substring line 2)))
      (setq line (concat odt-line-break (substring line 2)))))
   ;; insertion-template
   ((eq style insertion-style)
    (setq line (concat odt-insertion-paragraph-prefix (substring line 2) odt-paragraph-postfix "\n")))
   ;; heading template
   ((eq style heading-style)
    ;; #bbrinkmann 05.10.2015
    ;; remove heading-style-prefix
    (setq line (substring line (length heading-style-prefix)))
    (setq line-formatted-temp (replace-regexp-in-string (regexp-quote " - ") "<text:line-break/>" line))
    (setq line (concat odt-heading-paragraph-prefix line-formatted-temp odt-heading-postfix "\n")))
   ;; location-template
   ((eq style location-style)
    ;; #bbrinkmann 05.10.2015
    ;; remove location-style-prefix
    (setq line (substring line (length location-style-prefix)))
    (setq line (concat odt-location-paragraph-prefix line odt-paragraph-postfix "\n")))
   ;; persons-template
   ((or (eq style persons-style)
	(eq style persons-standalone-style))
    ;; #bbrinkmann 05.10.2015
    ;; remove persona-style-prefix
    (setq line (substring line (length persona-style-prefix)))
    (setq person-template-line (person-template-get-formatted-line line))
    (if (eq style persons-style)
	(setq line (concat odt-person-paragraph-prefix person-template-line odt-paragraph-postfix))
      (setq line (concat odt-person-standalone-paragraph-prefix person-template-line odt-paragraph-postfix)))
    (setq line (concat line "\n"))))
  line)

;;; ---------------------------------------------------------
;;;
(defun apply-footnotes (line)
  "Replaces [ and ] with footnotes."
  (setq index 0)
  (setq new-string "")
  (with-current-buffer (get-buffer-create org-buffer-name)
    (while (< index (string-width line))
      (setq current-char (aref line index))
      (cond
       ((= current-char 91) ; 91 [
	(setq new-string (substring new-string 0 (string-width new-string)))
	(setq current-footnote-prefix odt-footnote-prefix)
	(setq current-footnote-prefix (replace-regexp-in-string "X" (number-to-string footnote-index) current-footnote-prefix))
	(setq new-string (concat new-string current-footnote-prefix))
	(setq footnote-index (+ footnote-index 1)))
       ((= current-char 93) ; 93 ]
	(setq new-string (substring new-string 0 (string-width new-string)))
	(setq new-string (concat new-string odt-footnote-postfix)))
       (t
	(setq new-string (concat new-string (char-to-string current-char)))))
      (setq index (+ index 1)))
    (setq line new-string))
  line)

;;; ---------------------------------------------------------
;;;
(defun apply-italics (line)
  "Replaces # with italics-prefx and italics-postfix."
  (setq index 0)
  (setq new-string "")
  (with-current-buffer (get-buffer-create org-buffer-name)
    (while (< index (string-width line))
      (setq current-char (aref line index))
      (if (= current-char 35) ; 35 #
	  (progn
	    (setq new-string (substring new-string 0))
	    (if (eq italics-input nil)
		(progn
		  (setq new-string (concat new-string odt-italics-prefix))
		  (setq italics-input t))
	      (setq new-string (concat new-string odt-italics-postfix))
	      (setq italics-input nil)))
	(setq new-string (concat new-string (char-to-string current-char))))
      (setq index (+ index 1)))
    (setq line new-string))
  line)

;;; ---------------------------------------------------------
;;;
(defun insert-formatted-line (line)
  "Inserts formatted-line into org-buffer."
  (with-current-buffer (get-buffer-create org-buffer-name)
    (insert line)))

;;; ---------------------------------------------------------
;;;
(defun odt-insert-personae ()
  "Inserts content of personae file into html."
  (when (file-exists-p personae-file-name)
    (with-current-buffer (get-buffer-create odt-personae-buffer)
      (find-file personae-file-name)
      (goto-char 0)
      (while (< (point) (point-max))
	(setq current-personae-line (get-current-line))
	;; #bbrinkmann 05.10.2015
	;;(unless (eq (get-style current-personae-line) empty-line-style)
	(if (eq (get-style current-personae-line) heading-style)
	    (progn
	      ;; remove heading-style-prefix
	      (setq current-personae-line (substring current-personae-line (length heading-style-prefix)))
	      (setq formatted-line (concat odt-heading-paragraph-prefix current-personae-line odt-heading-postfix)))
	  (if (eq (get-style current-personae-line) location-style)
	      (progn
		;; remove location-style-prefix
		(setq current-personae-line (substring current-personae-line (length location-style-prefix)))
		(setq formatted-line (concat odt-location-paragraph-prefix current-personae-line odt-paragraph-postfix)))
	    (setq formatted-line (concat odt-standard-paragraph-prefix current-personae-line odt-paragraph-postfix))))
	(with-current-buffer (get-buffer-create org-buffer-name)
	  (insert formatted-line)(newline))
	;;)
	(forward-line))
      (kill-buffer (current-buffer)))))

;;; ---------------------------------------------------------
;;;
(defun odt-insert-schmutztitel ()
  "Creates and inserts a Schmutztitel page."
  (setq line (concat odt-schmutztitel-paragraph-prefix (upcase author) odt-line-break title " - " subtitle))
  (if (eq episoda-number "")
      (setq line (concat line odt-paragraph-postfix "\n"))
    (setq line (concat line odt-line-break "Episoda " episoda-number ": " episoda-title odt-paragraph-postfix "\n")))
  (insert-formatted-line line))

;;; ---------------------------------------------------------
;;;
(defun odt-insert-schmutztitel-ORIGINAL ()
  "Creates and inserts a Schmutztitel page."
  (setq line (concat odt-schmutztitel-paragraph-prefix (upcase author) odt-line-break (upcase title) " - " subtitle))
  (if (eq episoda-number "")
      (setq line (concat line odt-paragraph-postfix "\n"))
    (setq line (concat line odt-line-break "Episoda " episoda-number ": " episoda-title odt-paragraph-postfix "\n")))
  (insert-formatted-line line))

;;; ---------------------------------------------------------
;;;
(defun odt-insert-schmutztitel-rueckseite ()
  "Inserts an empty Schmutztitel Rückseite page."
  (setq line (concat odt-empty-page-paragraph-prefix "" odt-paragraph-postfix))
  (insert-formatted-line (concat line "\n")))

;;; ---------------------------------------------------------
;;;
(defun odt-insert-titel ()
  "Creates and inserts a title page."
  (setq line (concat odt-author-paragraph-prefix author odt-paragraph-postfix "\n"))
  (insert-formatted-line line)
  (setq line (concat odt-title-paragraph-prefix title odt-paragraph-postfix "\n"))
  (insert-formatted-line line)
  (setq line (concat odt-subtitle-paragraph-prefix subtitle odt-paragraph-postfix "\n"))
  (insert-formatted-line line)
  (unless (eq episoda-number "")
    (setq line (concat odt-episoda-paragraph-prefix "Episoda " episoda-number ": " episoda-title odt-paragraph-postfix "\n"))
    (insert-formatted-line line)))

;;; ---------------------------------------------------------
;;;
(defun odt-insert-impressum ()
  "Inserts content of impressum file."
  (when (file-exists-p impressum-file-name)
    (with-current-buffer (get-buffer-create odt-impressum-buffer)
      (find-file impressum-file-name)
      (goto-char 0)
      (insert-formatted-line odt-legal-notice-paragraph-prefix)
      (while (< (point) (point-max))
	(setq current-impressum-line (get-current-line))
	(setq formatted-line (concat current-impressum-line odt-line-break))
	(insert-formatted-line formatted-line)
	(forward-line))
      ;; Letzen line-break entfernen
      (with-current-buffer (get-buffer-create org-buffer-name)
	(delete-backward-char (length odt-line-break)))
      (insert-formatted-line (concat odt-paragraph-postfix "\n"))
      (kill-buffer (current-buffer)))))

;;; ---------------------------------------------------------
;;;
(defun odt-insert-impressum-bod ()
  "Inserts content of impressum file."
  (when (file-exists-p impressum-file-name-bod)
    (with-current-buffer (get-buffer-create odt-impressum-buffer)
      (find-file impressum-file-name-bod)
      (goto-char 0)
      (insert-formatted-line odt-legal-notice-paragraph-prefix)
      (while (< (point) (point-max))
	(setq current-impressum-line (get-current-line))
	(setq formatted-line (concat current-impressum-line odt-line-break))
	(insert-formatted-line formatted-line)
	(forward-line))
      ;; Letzen line-break entfernen
      (with-current-buffer (get-buffer-create org-buffer-name)
	(delete-backward-char (length odt-line-break)))
      (insert-formatted-line (concat odt-paragraph-postfix "\n"))
      (kill-buffer (current-buffer)))))

;;; ---------------------------------------------------------
;;;
(defun odt-insert-appendix ()
  "Inserts content of appendix file into html."
  (when (file-exists-p appendix-file-name)
    (with-current-buffer (get-buffer-create odt-appendix-buffer)
      (find-file appendix-file-name)
      (goto-char 0)
      (while (< (point) (point-max))
	(setq current-appendix-line (get-current-line))
	(if (eq (get-style current-appendix-line) heading-style)
	    (progn
	      ;; remove heading-style-prefix
	      (setq current-appendix-line (substring current-appendix-line (length heading-style-prefix)))
	      (setq formatted-line (concat odt-heading-paragraph-prefix current-appendix-line odt-heading-postfix)))
	  (if (eq (get-style current-appendix-line) location-style)
	      (progn
		;; remove location-style-prefix
		(setq current-appendix-line (substring current-appendix-line (length location-style-prefix)))
		(setq formatted-line (concat odt-location-paragraph-prefix current-appendix-line odt-paragraph-postfix)))
	    (setq formatted-line (apply-bold current-appendix-line))
	    (setq formatted-line (apply-italics formatted-line))
	    (setq formatted-line (concat odt-normal-paragraph-first-paragraph-prefix formatted-line odt-paragraph-postfix))))	
	(with-current-buffer (get-buffer-create org-buffer-name)
	  (insert formatted-line)(newline))
	(forward-line))
      (kill-buffer (current-buffer)))))

;;; ---------------------------------------------------------
;;;
(defun odt-insert-autor ()
  "Inserts content of autor file."
  (when (file-exists-p autor-file-name)
    (with-current-buffer (get-buffer-create odt-autor-buffer)
      (find-file autor-file-name)
      (goto-char 0)
      (insert-formatted-line odt-legal-notice-paragraph-prefix)
      (while (< (point) (point-max))
	(setq current-autor-line (get-current-line))
	(setq formatted-line (concat current-autor-line odt-line-break))
	(insert-formatted-line formatted-line)
	(forward-line))
      ;; Letzen line-break entfernen
      (with-current-buffer (get-buffer-create org-buffer-name)
	(delete-backward-char (length odt-line-break)))
      (insert-formatted-line (concat odt-paragraph-postfix "\n"))
      (kill-buffer (current-buffer)))))

;;; ---------------------------------------------------------
;;;
(defun  odt-insert-whathappenedsofar ()
  "Inserts content of whathappenedsofar file into html."
  (when (file-exists-p whathappenedsofar-file-name)
    (setq is-first-paragraph t)
    (with-current-buffer (get-buffer-create odt-whathappenedsofar-buffer)
      (find-file whathappenedsofar-file-name)
      (goto-char 0)
      (while (< (point) (point-max))
	(setq current-whathappenedsofar-line (get-current-line))
	(if (eq (get-style current-whathappenedsofar-line) heading-style)
	    (progn
	      ;; remove heading-style-prefix
	      (setq current-whathappenedsofar-line (substring current-whathappenedsofar-line (length heading-style-prefix)))
	      (setq formatted-line (concat odt-heading-paragraph-prefix current-whathappenedsofar-line odt-heading-postfix)))
	  (if (eq (get-style current-whathappenedsofar-line) location-style)
	      (progn
		;; remove location-style-prefix
		(setq current-whathappenedsofar-line (substring current-whathappenedsofar-line (length location-style-prefix)))
		(setq formatted-line (concat odt-location-paragraph-prefix current-whathappenedsofar-line odt-paragraph-postfix)))
	    (if is-first-paragraph
		(progn
		  (setq formatted-line (concat odt-normal-paragraph-first-paragraph-prefix current-whathappenedsofar-line odt-paragraph-postfix))
		  (unless (string= current-whathappenedsofar-line "")
		    (setq is-first-paragraph nil)))
	      (setq formatted-line (concat odt-normal-paragraph-prefix current-whathappenedsofar-line odt-paragraph-postfix)))))
	(with-current-buffer (get-buffer-create org-buffer-name)
	  (insert formatted-line)(newline))
	(forward-line))
      (kill-buffer (current-buffer)))))

;;; ---------------------------------------------------------
;;;
(defun odt-insert-empty-page ()
  "Inserts an empty page."
  (setq line (concat odt-empty-page-paragraph-prefix "TEST" odt-paragraph-postfix))
  (insert-formatted-line (concat line "\n")))

;;; ---------------------------------------------------------
;;;
(defun iterate-odt-buffer ()
  "Iterates buffer and creates .odt-code."
  (interactive)
  (setq lines-iterated 0)
  (goto-char 1)
  (setq last-line nil)
  (setq last-style nil)
  (odt-insert-schmutztitel)
  (odt-insert-schmutztitel-rueckseite)
  (odt-insert-titel)
  (odt-insert-impressum-bod)
  (odt-insert-personae)
  (odt-insert-whathappenedsofar)
  (while (< (point)(point-max))
    (setq current-line (get-current-line))
    (setq current-style (get-style current-line))
    (unless (eq current-style empty-line-style)
      (setq current-line (apply-italics current-line))
      (setq current-line (odt-apply-style current-style current-line last-style))
      (setq current-line (apply-footnotes current-line))
      (insert-formatted-line current-line)
      (setq last-style current-style)
      (setq last-line current-line))
    (setq lines-iterated (+ lines-iterated 1))
    (message (format "lines-iterated: %d" lines-iterated))
    (forward-line))
  (with-current-buffer (get-buffer-create org-buffer-name)
    (unless (eq current-style insertion-style)
      (insert odt-paragraph-postfix))
    (newline))
  (odt-insert-appendix)
  (odt-insert-autor))
