(defconst html-author-paragraph-prefix              "<p class=\"w2e_Autor\">")
(defconst html-title-paragraph-prefix               "<p class=\"w2e_Titel\">")
(defconst html-subtitle-paragraph-prefix            "<p class=\"w2e_Untertitel\">")
(defconst html-episoda-paragraph-prefix             "<p class=\"w2e_Episoda\">")
(defconst html-publisher-paragraph-prefix           "<p class=\"w2e_Verlag\">")
(defconst html-legal-notice-paragraph-prefix        "<p class=\"w2e_Impressum\">")
(defconst html-legal-notice-first-paragraph-prefix  "<p class=\"w2e_5f_Impressum_5f_FirstParagraph\">")
(defconst html-heading-paragraph-prefix             "<h2 class=\"centered\">")
(defconst html-standard-paragraph-prefix            "<p class=\"w2e_Standard\">")
(defconst html-standard-paragraph-margin-top-prefix "<p class=\"w2e_Standard_margin_top\">")
(defconst html-standard-paragraph-continued-prefix  "<p class=\"w2e_Standard_Unterbrochen\">")
(defconst html-insertion-paragraph-prefix           "<p class=\"w2e_Einschub\">")
(defconst html-location-paragraph-prefix            "<p class=\"w2e_Schauplatz\">")
(defconst html-person-paragraph-prefix              "<p class=\"w2e_Personen\">")
(defconst html-person-standalone-paragraph-prefix   "<p class=\"w2e_Personen_Standalone\">")
(defconst html-personae-standard-paragraph-prefix   "<p class=\"w2e_Personae\">")
(defconst html-paragraph-postfix                    "</p>")
(defconst html-heading-postfix                      "</h2>")
(defconst html-line-break                           "<br/>")
(defconst html-italics-prefix                       "<i>")
(defconst html-italics-postfix                      "</i>")
(defconst html-footnote-prefix                      "<text:note text:id=\"ftnX\" text:note-class=\"footnote\"><text:note-citation>X</text:note-citation><text:note-body><text:p text:style-name=\"P95\">")
(defconst html-footnote-postfix                     "</text:p></text:note-body></text:note>")
(defconst html-buffer-name                          "html-buffer")
(defconst html-toc-heading                          "<h2>Inhaltsverzeichnis</h2>\n<ul>")
(defconst html-footnotes-buffer-name                "footnotes-html")
(defconst html-footnotes-file-name                  "footnotes.html")
(defvar   html-file-index                           1)
(defvar   last-style                                nil)
(defconst personae-buffer                           "personae-buffer")
(defconst impressum-buffer                          "impressum-buffer")
(defconst fnreturn-prefix                           "l" "Prefix for footnote return anchors.")
(defvar   has-second-level                          nil "Defines if current first level heading has second-level.")
(defvar   is-new-second-level                       nil "Defines, if we are already inside a second level.")
(defvar   html-footnotes-file-path                  "")
(defconst html-hang-indent-first-paragraph-prefix   "<p class=\"no-hang\">")
(defconst html-hang-indent-paragraph-prefix         "<p class=\"hang\">")
(defconst autor-buffer                              "autor-buffer")

;;; --------------------------------------------------------
;;;
(defun get-title ()
  "Returns a combined title from book information."
(setq title-combined "")
(setq title-combined (concat title-combined title))
(unless (eq episoda-number "")
  (setq title-combined (concat title-combined " - Episoda " episoda-number ": " episoda-title)))
(when (eq episoda-number "")
  (setq title-combined (concat title-combined " - " subtitle)))
title-combined)

;;; ---------------------------------------------------------
;;;
(defun create-html-header ()
  "Inserts html-header."
  (insert (concat
   "<?xml version=\"1.0\" ?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
    \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
<head>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"/>
<link type=\"text/css\" href=\"../css/styles.css\" rel=\"Stylesheet\"/>
<title>" (get-title) "</title>
</head>
<body>"))
  (newline))

;;; ---------------------------------------------------------
;;;
(defun create-html-footer ()
  "Inserts html-footer."
  (insert "</body>
</html>"))

;;; ---------------------------------------------------------
;;;
(defun html-apply-italics (line)
  "Replaces # with italics-prefx and italics-postfix."
  (setq index 0)
  (setq new-string "")
  (with-current-buffer (get-buffer-create html-buffer-name)
    (while (< index (string-width line))
      (setq current-char (aref line index))
      (if (= current-char 35) ; 35 #
	  (progn
	    (setq new-string (substring new-string 0))
	    (if (eq italics-input nil)
		(progn
		  (setq new-string (concat new-string html-italics-prefix))
		  (setq italics-input t))
	      (setq new-string (concat new-string html-italics-postfix))
	      (setq italics-input nil)))
	(setq new-string (concat new-string (char-to-string current-char))))
      (setq index (+ index 1)))
    (setq line new-string))
  line)

;;; ---------------------------------------------------------
;;;
(defun html-apply-style (style line last-style)
  "Applies the selected template."
  ;; (with-current-buffer (get-buffer-create html-buffer-name)
  ;;   (when (and (eq style standard-style)
  ;; 	       (or
  ;; 		(and (eq last-style standard-style)
  ;; 		     (not (string-prefix-p indentation line)))
  ;; 		(eq last-style standard-interrupted-style)
  ;; 		(eq last-style insertion-style)))
  ;;     (delete-backward-char (+ (length html-line-break) 1))
  ;;     (insert html-paragraph-postfix)(newline))
  ;;   (when (and (or (eq style persons-style)
  ;; 		   (eq style persons-standalone-style)
  ;; 		   (eq style location-style))
  ;; 	       (or (eq last-style standard-style)
  ;; 		   (eq last-style standard-interrupted-style)
  ;; 		   (eq last-style insertion-style)))
  ;;     (delete-backward-char (+ (length html-line-break) 1))
  ;;     (insert html-paragraph-postfix)(newline)))
  (cond
   ;; standard-template
   ((eq style standard-style)
    (if (string-prefix-p indentation line)
;;	(setq line (concat (substring line 2) html-line-break))
	(setq line (concat html-hang-indent-paragraph-prefix (substring line 2) html-paragraph-postfix))
      ;; everything between ( and ) in italics.
      (setq line-to-insert (replace-regexp-in-string "(" (concat html-italics-prefix "(") line))
      (setq line-to-insert (replace-regexp-in-string ")" (concat")"  html-italics-postfix) line-to-insert))
;;      (setq line (concat html-standard-paragraph-prefix line-to-insert html-line-break))))
      (setq line (concat html-hang-indent-first-paragraph-prefix line-to-insert html-paragraph-postfix))))
   ;; standard-template-continued
   ((eq style standard-interrupted-style)
;;      (setq line (concat (substring line 2) html-line-break)))
      (setq line (concat html-hang-indent-paragraph-prefix (substring line 2) html-paragraph-postfix)))
   ;; insertion-template
   ((eq style insertion-style)
;;    (setq line (concat html-italics-prefix (substring line 2) html-italics-postfix html-line-break)))
    (setq line (concat html-hang-indent-paragraph-prefix html-italics-prefix (substring line 2) html-italics-postfix html-paragraph-postfix)))
   ;; heading template
   ((eq style heading-style)
    (setq line (html-insert-fnreturn line))
    (setq line (concat html-heading-paragraph-prefix line html-heading-postfix)))
   ;; location-template
   ((eq style location-style)
;;neu
    (unless (eq last-style heading-style)
      (setq line (html-insert-fnreturn line)))
    (setq line (concat html-location-paragraph-prefix line html-paragraph-postfix)))
   ;; persons-template
   ((or (eq style persons-style)
	(eq style persons-standalone-style))
    (setq person-template-line (html-person-template-get-formatted-line line))
    (if (eq style persons-style)
	(setq line (concat html-person-paragraph-prefix person-template-line html-paragraph-postfix))
      (setq line (concat html-person-standalone-paragraph-prefix person-template-line html-paragraph-postfix)))))
  line)

;;; ---------------------------------------------------------
;;;
(defun html-insert-line (line)
  "Inserts formatted-line into html-buffer."
  (with-current-buffer (get-buffer-create html-buffer-name)
      (insert line)(newline)))

;;; ---------------------------------------------------------
;;;
(defun html-person-template-get-formatted-line (array)
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
	      (setq new-string (concat new-string html-italics-prefix)))
	  (setq new-string (substring new-string 0 (- (string-width new-string) 1)))
	  (setq new-string (concat new-string html-italics-prefix (char-to-string char-before-insertion)))))
      ;; italics-postfix
      (when (or 
	     (and (> index 1)
		  (is-uppercase-letter current-char)
		  last-char-was-uppercase
		  (eq second-last-char-was-uppercase nil)))
	(setq char-before-insertion (aref array (- index 1)))
	(setq new-string (substring new-string 0 (- (string-width new-string) 2)))
	(setq new-string (concat new-string html-italics-postfix " " (char-to-string char-before-insertion)))))
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
    (setq new-string (concat new-string html-italics-postfix)))
  new-string)

;;; --------------------------------------------------------
;;;
(defun create-toc-html ()
  "Creates a toc.html."
  (setq html-toc-buffer-name "toc.html")
  (with-current-buffer (get-buffer-create html-toc-buffer-name)
    (create-html-header)
    (insert html-toc-heading)(newline)
    ;; PERSONAE
    (setq html-toc-entry (concat "<li><p class=\"toctext\"><a href=\"personae.html\" style=\"text-decoration: none\">PERSONAE</a></p></li>"))
    (insert html-toc-entry)(newline)))

;;; ---------------------------------------------------------
;;;
(defun create-new-file ()
  "Creates a new .html-file."
  (setq html-buffer-name (concat (number-to-string html-file-index) ".html"))
  (with-current-buffer (get-buffer-create html-buffer-name)
    (create-html-header)))

;;; ---------------------------------------------------------
;;;
(defun close-old-file ()
  "Closes the last .html-file."
  (unless (eq (get-buffer html-buffer-name) nil)
    ;; (with-current-buffer (get-buffer-create html-buffer-name)
    ;;   ;; replace last \line with \par}
    ;;   (when (or
    ;; 	     (eq last-style standard-style)
    ;; 	     (eq last-style standard-interrupted-style)
    ;; 	     (eq last-style insertion-style))
    ;; 	(delete-backward-char (+ (length html-line-break) 1))
    ;; 	(insert html-paragraph-postfix)(newline)))
    (setq html-filename (concat (file-name-as-directory html-directory) (number-to-string html-file-index) ".html"))
    (with-current-buffer (get-buffer-create html-buffer-name)
      (create-html-footer)
      (write-file html-filename nil)
      (setq html-file-index (+ html-file-index 1))
      (kill-buffer (current-buffer)))))

;;; ---------------------------------------------------------
;;;
(defun insert-into-toc-html ()
  "Inserts file information into toc.html."
  (unless (string= heading-text "")
    (with-current-buffer (get-buffer-create html-toc-buffer-name)
      (setq html-toc-entry (concat "<li><p class=\"toctext\"><a href=\"" (number-to-string html-file-index) ".html\">" (get-line-without-footnotes heading-text) "</a></p></li>"))
      (insert html-toc-entry)(newline)
      (setq heading-text ""))))

;;; ---------------------------------------------------------
;;;
(defun insert-into-toc-html-first-level ()
  "Inserts file information into toc.html."
  (unless (string= heading-text "")
    (with-current-buffer (get-buffer-create html-toc-buffer-name)
      (when has-second-level
	(insert "</ul>\n</li>\n"))
      (setq html-toc-entry (concat "<li><p class=\"toctext\"><a href=\"" (number-to-string html-file-index) ".html\"  style=\"text-decoration: none\">" (get-line-without-footnotes heading-text) "</a></p></li>"))
      (insert html-toc-entry)(newline)
      (setq is-new-second-level t)
      (setq has-second-level nil)
      (setq heading-text ""))))

;;; ---------------------------------------------------------
;;;
(defun insert-into-toc-html-second-level ()
  "Inserts file information into toc.html as second level."
  (unless (string= heading-text "")
    (with-current-buffer (get-buffer-create html-toc-buffer-name)
      (when is-new-second-level
	(insert "<li>\n<span />\n<ul>\n")
	(setq is-new-second-level nil))
      (setq has-second-level t)
      (if (eq last-style heading-style)
	  (setq html-toc-entry (concat "<li><p class=\"toctext\"><a href=\"" (number-to-string html-file-index) ".html\" style=\"text-decoration: none\">" (get-line-without-footnotes heading-text) "</a></p></li>"))
	(setq html-toc-entry (concat "<li><p class=\"toctext\"><a href=\"" (number-to-string html-file-index) ".html#" fnreturn-prefix (number-to-string lines-iterated) "\"  style=\"text-decoration: none\">" (get-line-without-footnotes heading-text) "</a></p></li>")))
      (insert html-toc-entry)(newline)
      (setq heading-text ""))))

;;; ---------------------------------------------------------
;;;
(defun html-apply-footnotes (line)
  "Replaces [text] with according footnote formatting."
  (setq index 0)
  (setq new-string "")
  (setq is-footnote-text nil)
  (setq has-footnote nil)
    (while (< index (string-width line))
      (setq current-char (aref line index))
      (if (= current-char 91) ; 91 [
	  (progn
	    (when (eq (get-buffer html-footnotes-buffer-name) nil)
	      (with-current-buffer (get-buffer-create html-footnotes-buffer-name)
		(create-html-header)
		(insert (concat html-heading-paragraph-prefix "Fußnoten" html-heading-postfix "\n"))))
	    (setq has-footnote t)
	    (setq is-footnote-text t)
	    (setq new-string (substring new-string 0))
	    (setq new-string (concat new-string "<a href=\"" html-footnotes-file-name "#footnote-" (number-to-string footnote-index) "\"><sup>" (number-to-string footnote-index) "</sup></a>"))
	    (with-current-buffer (get-buffer-create html-footnotes-buffer-name)
	      (insert (concat "<div class=\"footnote\" id=\"footnote-" (number-to-string footnote-index) "\"><p class=\"fnparagraph\">" (number-to-string footnote-index) "&nbsp;"))))
	(if (= current-char 93) ; 93 ]
	    (progn
	      (setq is-footnote-text nil)
	      (with-current-buffer (get-buffer-create html-footnotes-buffer-name)
		(insert (concat "&nbsp;<a href=\"" (concat (number-to-string html-file-index) ".html#" fnreturn-prefix (number-to-string lines-iterated) "\"><strong>&#x21B5;</strong></a></p>\n</div>\n"))))
	      (setq footnote-index (+ footnote-index 1)))
	  (if is-footnote-text
	      (with-current-buffer (get-buffer-create html-footnotes-buffer-name)
		(insert (char-to-string current-char)))
	    (setq new-string (concat new-string (char-to-string current-char))))))
      (setq index (+ index 1)))
    (setq line new-string) 
    (when has-footnote
      (setq line (html-insert-fnreturn line)))
    line)

;;; ---------------------------------------------------------
;;;
(defun create-personae-page ()
  "Inserts content of personae file into html."
  (setq is-first-line nil)
  (when (file-exists-p personae-file-name)
    (with-current-buffer (get-buffer-create html-buffer-name)
      (create-html-header))
    (with-current-buffer (get-buffer-create personae-buffer)
      (find-file personae-file-name)
      (goto-char 0)
      (while (< (point) (point-max))
	(setq current-personae-line (get-current-line))
	(if (eq (get-style current-personae-line) empty-line-style)
	    (setq is-first-line t)
	;;     (with-current-buffer (get-buffer-create html-buffer-name)
	;;       (insert html-line-break)(newline))
	  (if (eq (get-style current-personae-line) heading-style)
	      (progn
		(setq heading-text current-personae-line)
		;; #bbrinkmann 07.10.2015: Remove heading-style-prefix
		(setq current-personae-line (substring current-personae-line (length heading-style-prefix)))
		(setq formatted-line (concat html-heading-paragraph-prefix current-personae-line html-heading-postfix)))
	    (if (eq (get-style current-personae-line) location-style)
		(progn
		  (setq heading-text current-personae-line)
		  ;; #bbrinkmann 07.10.2015: Remove location-style-prefix
		  (setq current-personae-line (substring current-personae-line (length location-style-prefix)))
		  (setq formatted-line (concat html-location-paragraph-prefix current-personae-line html-paragraph-postfix)))
	      (if is-first-line
		  (progn
		    (setq is-first-line nil)
		    (setq formatted-line (concat html-standard-paragraph-margin-top-prefix current-personae-line html-paragraph-postfix)))
		(setq formatted-line (concat html-standard-paragraph-prefix current-personae-line html-paragraph-postfix)))))
	  (with-current-buffer (get-buffer-create html-buffer-name)
	    (insert formatted-line)(newline)))
	(forward-line))
      (with-current-buffer (get-buffer-create html-buffer-name)
	(setq html-filename (concat (file-name-as-directory html-directory) "personae.html"))
	(create-html-footer)
	(write-file html-filename nil))
      (kill-buffer (current-buffer)))))

;;; ---------------------------------------------------------
;;;
(defun create-personae-page_ORIGINAL ()
  "Inserts content of personae file into html."
  (when (file-exists-p personae-file-name)
    (with-current-buffer (get-buffer-create html-buffer-name)
      (create-html-header))
    (with-current-buffer (get-buffer-create personae-buffer)
      (find-file personae-file-name)
      (goto-char 0)
      (while (< (point) (point-max))
	(setq current-personae-line (get-current-line))
	(if (eq (get-style current-personae-line) empty-line-style)
	    (with-current-buffer (get-buffer-create html-buffer-name)
	      (insert html-line-break)(newline))
	  (if (eq (get-style current-personae-line) heading-style)
	      (progn
		(setq heading-text current-personae-line)
		;; #bbrinkmann 07.10.2015: Remove heading-style-prefix
		(setq current-personae-line (substring current-personae-line (length heading-style-prefix)))
		(setq formatted-line (concat html-heading-paragraph-prefix current-personae-line html-heading-postfix)))
	    (if (eq (get-style current-personae-line) location-style)
		(progn
		  (setq heading-text current-personae-line)
		  ;; #bbrinkmann 07.10.2015: Remove location-style-prefix
		  (setq current-personae-line (substring current-personae-line (length location-style-prefix)))
		  (setq formatted-line (concat html-location-paragraph-prefix current-personae-line html-paragraph-postfix)))
	      (setq formatted-line (concat html-standard-paragraph-prefix current-personae-line html-paragraph-postfix))))
	  (with-current-buffer (get-buffer-create html-buffer-name)
	    (insert formatted-line)(newline)))
	(forward-line))
      (with-current-buffer (get-buffer-create html-buffer-name)
	(setq html-filename (concat (file-name-as-directory html-directory) "personae.html"))
	(create-html-footer)
	(write-file html-filename nil))
      (kill-buffer (current-buffer)))))

;;; --------------------------------------------------------
;;;
(defun create-legal-notice-page ()
  "Creates a legal notice page."
  (setq is-first-line t)
  (setq is-first-paragraph t)
  (when (file-exists-p impressum-file-name)
    (with-current-buffer (get-buffer-create html-buffer-name)
      (create-html-header))
    (with-current-buffer (get-buffer-create impressum-buffer)      
      (find-file impressum-file-name)
      (goto-char 0)
      (while (< (point) (point-max))
	(setq formatted-line "")
	(setq current-impressum-line (get-current-line))
	(if (eq (get-style current-impressum-line) empty-line-style)
	    (progn
	      (with-current-buffer (get-buffer-create html-buffer-name)
		(delete-backward-char (+ (length html-line-break) 1)))
	      (setq formatted-line html-paragraph-postfix)
	      (setq is-first-line t))
	  (when is-first-line
	    (if is-first-paragraph
		(progn
		  (setq formatted-line (concat html-legal-notice-first-paragraph-prefix))
		  (setq is-first-paragraph nil))
	      (setq formatted-line (concat html-legal-notice-paragraph-prefix)))
	      (setq is-first-line nil))
	  (setq formatted-line (concat formatted-line current-impressum-line html-line-break)))
	(html-insert-line formatted-line)
	(forward-line))
      (with-current-buffer (get-buffer-create html-buffer-name)
	(delete-backward-char (+ (length html-line-break) 1))
	(html-insert-line html-paragraph-postfix))
      (kill-buffer (current-buffer)))
    (with-current-buffer (get-buffer-create html-buffer-name)
      (create-html-footer)
      (setq html-filename (concat (file-name-as-directory html-directory) "impressum.html"))
      (write-file html-filename nil))))

;;; ---------------------------------------------------------
;;;
(defun create-cover-page ()
  (when (file-exists-p cover-file-name)
      (setq target-filename (concat (file-name-as-directory html-directory) cover-file-name))
  (copy-file cover-file-name target-filename)))

;;; ---------------------------------------------------------
;;;
(defun create-werbung-page ()
  (when (file-exists-p werbung-file-name)
      (setq target-filename (concat (file-name-as-directory html-directory) werbung-file-name))
  (copy-file werbung-file-name target-filename)))

;;; ---------------------------------------------------------
;;;
(defun html-insert-fnreturn (line)
  "Inserts footnote-return-anchor for every line."
  (setq index 0)
  (setq fnreturn-already-inserted nil)
  (catch 'break
    (while (< index (string-width line))	 
      (setq current-char (aref line index))
      (when (= current-char 62) ; 62 >
	(setq fnreturn-already-inserted t)
	(throw 'break nil))
      (setq index (+ index 1))))
  (if (string-prefix-p indentation line)
      (setq line (concat indentation current-fnreturn-anchor (substring line (length indentation))))
    (setq line (concat current-fnreturn-anchor line)))
  line)

;;; ---------------------------------------------------------
;;;
(defun get-location-first-part (location-style-line)
  "Returns the part before '.' of location-style line."
  (setq index 0)
  (setq location-first-part "")
  (catch 'break
    (while (< index (string-width location-style-line))
      (setq current-char (aref location-style-line index))
      (if (= current-char 46) ; 46 .
	  (throw 'break nil)
	(setq location-first-part (concat location-first-part (char-to-string current-char))))
      (setq index (+ index 1))))
  location-first-part)

;;; ---------------------------------------------------------
;;;
(defun html-remove-indentation (line)
  "Removes indentation from line."
  (when string-prefix-p indentation line
	(setq line (substring line (length indentation))))
  line)

;;; ---------------------------------------------------------
;;;
(defun iterate-epos-buffer ()
  "Iterates text buffer."
  (setq lines-iterated 0)
  (setq heading-text "")
  (setq current-style nil)
  (setq current-line nil)
  (setq last-line nil)
  (setq last-style nil)
  (goto-char 1)
  (while (< (point)(point-max))
    (setq current-fnreturn-anchor (concat "<a id=\"" fnreturn-prefix (number-to-string lines-iterated) "\"></a>"))
    (setq current-line (get-current-line))
    (setq current-style (get-style current-line))
    (unless (eq current-style empty-line-style)
      (when (eq current-style heading-style)
	(setq next-line (get-next-line 1))
	(setq current-line (substring current-line (length heading-style-prefix)))
	(setq heading-text current-line)
	(close-old-file)
	(epub-insert-into-toc-ncx-first-level heading-text html-file-index)
	(insert-into-toc-html-first-level)
	(create-new-file))
      (when (eq current-style location-style)
	(setq current-line (substring current-line (length location-style-prefix)))
	(setq heading-text current-line)
	(epub-insert-into-toc-ncx-second-level heading-text html-file-index lines-iterated))
      (when (or (eq current-style persons-style)
		(eq current-style persons-standalone-style))
	(setq current-line (substring current-line (length persona-style-prefix))))
      (setq current-line (html-apply-italics current-line))
      (setq current-line (html-apply-footnotes current-line))
      (setq current-line (html-apply-style current-style current-line last-style))
      (html-insert-line current-line)
      (setq last-style current-style)
      (setq last-line current-line))
    (setq lines-iterated (+ lines-iterated 1))
    (message (format "lines-iterated: %d" lines-iterated))
    (forward-line)))

;;; --------------------------------------------------------
;;;
(defun write-toc-html ()
  "Writes toc.html."
  (with-current-buffer (get-buffer-create html-toc-buffer-name)
    ;; Footnotes
    (when (file-exists-p html-footnotes-file-name)
	(setq html-toc-entry (concat "<li><p class=\"toctext\"><a href=\"footnotes.html\" style=\"text-decoration: none\">Fußnoten</a></p></li>"))
	(insert html-toc-entry)(newline))
    ;; Autor
    (setq html-toc-entry (concat "<li><p class=\"toctext\"><a href=\"autor.html\" style=\"text-decoration: none\">Über den Autor</a></p></li>"))
    (insert html-toc-entry)(newline)
    ;; Impressum
    (setq html-toc-entry (concat "<li><p class=\"toctext\"><a href=\"impressum.html\" style=\"text-decoration: none\">Impressum</a></p></li>"))
    (insert html-toc-entry)(newline)
    (insert "</ul>\n")
    (create-html-footer)
    (setq html-toc-filename (concat (file-name-as-directory html-directory) html-toc-buffer-name))
    (write-file html-toc-filename)
    (kill-buffer (current-buffer))))

;;; ---------------------------------------------------------
;;;
(defun write-footnotes-html ()
  "Writes footnotes.html (if footnotes-html-buffer exists)."
  (when (not (eq (get-buffer html-footnotes-buffer-name) nil))
    (with-current-buffer (get-buffer-create html-footnotes-buffer-name)
      (create-html-footer)
      (setq html-footnotes-file-path (concat (file-name-as-directory html-directory) html-footnotes-file-name))
      (write-file html-footnotes-file-path)
      (kill-buffer (current-buffer)))))

;;; ---------------------------------------------------------
;;;
(defun iterate-html-buffer ()
  "Iterates buffer and creates .html-code."
  (create-toc-html)
  (iterate-epos-buffer)
  (close-old-file)
  ;;
  ;; #bbrinkmann 24.10.2015
  ;;
  ;;(insert-into-toc-html)  
  ;;
  ;;
  ;;
  (create-autor-page)
  (write-footnotes-html)
  (create-legal-notice-page)
  (write-toc-html))

;;; --------------------------------------------------------
;;;
(defun create-autor-page ()
  "Creates an autor page."
  (setq is-first-line t)
  (setq is-first-paragraph t)
  (when (file-exists-p autor-file-name)
    (with-current-buffer (get-buffer-create html-buffer-name)
      (create-html-header))
    (with-current-buffer (get-buffer-create autor-buffer)      
      (find-file autor-file-name)
      (goto-char 0)
      (while (< (point) (point-max))
	(setq formatted-line "")
	(setq current-autor-line (get-current-line))
	(if (eq (get-style current-autor-line) empty-line-style)
	    (progn
	      (with-current-buffer (get-buffer-create html-buffer-name)
		(delete-backward-char (+ (length html-line-break) 1)))
	      (setq formatted-line html-paragraph-postfix)
	      (setq is-first-line t))
	  (when is-first-line
	    (if is-first-paragraph
		(progn
		  (setq formatted-line (concat html-legal-notice-first-paragraph-prefix))
		  (setq is-first-paragraph nil))
	      (setq formatted-line (concat html-legal-notice-paragraph-prefix)))
	      (setq is-first-line nil))
	  (setq formatted-line (concat formatted-line current-autor-line html-line-break)))
	(html-insert-line formatted-line)
	(forward-line))
      (with-current-buffer (get-buffer-create html-buffer-name)
	(delete-backward-char (+ (length html-line-break) 1))
	(html-insert-line html-paragraph-postfix))
      (kill-buffer (current-buffer)))
    (with-current-buffer (get-buffer-create html-buffer-name)
      (create-html-footer)
      (setq html-filename (concat (file-name-as-directory html-directory) "autor.html"))
      (write-file html-filename nil))))

;;; --------------------------------------------------------
;;;
(defun create-title-page ()
  "Creates a title page from title information."
  (with-current-buffer (get-buffer-create html-buffer-name)
    (create-html-header))
  (setq current-line (concat html-author-paragraph-prefix (upcase author) html-paragraph-postfix "\n"))
  (html-insert-line current-line)
  (setq current-line (concat html-title-paragraph-prefix (upcase title)))
  (setq current-line (concat current-line  html-paragraph-postfix "\n"))
  (html-insert-line current-line)
  (setq current-line (concat html-subtitle-paragraph-prefix subtitle html-paragraph-postfix "\n"))
  (html-insert-line current-line)
  (unless (eq episoda-number "")
    (setq current-line (concat html-episoda-paragraph-prefix "Episoda " episoda-number ": " episoda-title html-paragraph-postfix "\n"))
    (html-insert-line current-line))
  (with-current-buffer (get-buffer-create html-buffer-name)
    (create-html-footer)
    (setq html-filename (concat (file-name-as-directory html-directory) "title.html"))
    (write-file html-filename nil)))

;;; ---------------------------------------------------------
;;;
(defun create-html (directory)
  "Exports text to html-format for further converting."
  (interactive)
  (setq html-directory directory)
  (setq html-file-index 1)
  (setq footnote-index 1)
  (unless (file-exists-p html-directory)
    (make-directory html-directory))
  (create-title-page)
  (create-personae-page)
  (iterate-html-buffer))
