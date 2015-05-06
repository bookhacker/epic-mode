(defconst heading-marker                       "<text:h")
(defconst paragraph-marker                     "<text:p")
(defconst text-span-start-marker               "<text:span")
(defconst text-span-end-marker                 "</text:span>")
(defconst style-tag-prefix                     "<style:style ")
(defconst text-list-tag-prefix                 "<text:list ")
(defconst text-list-end-tag                    "</text:list-item>")
(defconst text-h-tag-prefix                    "<text:h ")
(defconst text-p-tag-prefix                    "<text:p ")
(defconst text-span-tag-prefix                 "<text:span ")
(defconst text-span-tag-postfix                "</text:span>")
(defconst footnote-tag-prefix                  "<text:note ")
(defconst footnote-tag-postfix                 "</text:note>")
(defconst footnote-start-marker                "[")
(defconst footnote-end-marker                  "]")
(defconst text-note-body-start-tag             "<text:note-body>")
(defconst text-note-body-end-tag               "</text:note-body>")
(defconst office-text-tag-postfix              "</office:text>")
(defconst new-epos-buffer-name                 "new-epos")
(defconst font-style-tag-prefix                "fo:font-style=\"")
(defconst selected-style                       "")
(defconst heading-style                        "Heading")
(defvar   styles-hash-table                    nil)
(defvar   last-text-span-marker-style-name     "")
(defvar   content-xml-buffer                   "")
(defconst heading-style                        "Heading_20_2")
(defconst episoda-style                        "w2e_5f_Episoda")
(defconst location-style                       "w2e_5f_Schauplatz")
(defconst persons-style                        "w2e_5f_Personen")
(defconst persons-standalone-style             "w2e_5f_Personen_5f_Standalone")
(defconst standard-style                       "w2e_5f_Standard")
(defconst standard-interrupted-style           "w2e_5f_Standard_5f_Unterbrochen")
(defconst standard-last-paragraph-style        "w2e_5f_Standard_5f_LetzterAbsatz")
(defconst insertion-style                      "w2e_5f_Einschub")
(defconst insertion-last-paragraph-style       "w2e_5f_Einschub_5f_Letzter_5f_Absatz")
(defconst footnote-style                       "Footnote")
(defconst italics-style-name                   "italic")
(defconst personae-heading-string              "PERSONAE")
(defconst italics-marker                       "#")
(defvar   is-personae-section                  nil)
(defvar   last-selected-style-name             "")
(defconst indentation                          "  ")
(defconst indented-line-break                  (concat "\n" indentation))
(defvar   style-name                           "")
(defvar   in-footnote                          nil)
(defvar   in-text-note-body                    nil)
(defvar   file-name                            "")
(defconst series-name                          "series-name")
(defconst default-extension                    "epos")
(defconst epic-import-buffer                   "epic-import")

;;; --------------------------------------------------------
;;;
(defun epic-import-odt ()
  "Imports an .odt into a .epos-file."
  (interactive)
  (with-current-buffer (get-buffer-create epic-import-buffer)
    (prepare-document)
    (import-to-new-epos)))

;;; --------------------------------------------------------
;;;
(defun initialize-style (line)
  "Initializes a style and adds it to styles-hash-table."
  (setq key "")
  (setq value "")
  (setq splitted-string (split-string line))
  (setq style-name (nth 1 splitted-string))
  (setq splitted-style-name (split-string style-name "\""))
  (setq key (nth 1 splitted-style-name))
  (cond
   ((string-match-p "^P[0-9]*" key)
    (setq parent-style-name (nth 3 splitted-string))
    (setq splitted-parent-style (split-string parent-style-name "\""))
    (setq value (nth 1 splitted-parent-style)))
   ((string-match-p "^T[0-9]*" key)
    (setq font-style-string (nth 3 splitted-string))
    (when (string-prefix-p font-style-tag-prefix font-style-string)
      (setq font-style-string-splitted (split-string font-style-string "\""))
      (setq value (nth 1 font-style-string-splitted)))))
  (puthash key value styles-hash-table))

;;; --------------------------------------------------------
;;;
(defun initialize-heading (line)
  "Initializes a heading line and writes it to new-epos-buffer."
  (setq index 0)
  (setq heading-string "")
  (setq in-tag nil)
  ;; 60 <
  ;; 62 >
  (while (< index (string-width line))
    (setq current-char (aref line index))
    (cond
     ((= current-char 60)
      (setq in-tag t))
     ((= current-char 62)
      (setq in-tag nil))
     ((eq in-tag nil)
      (setq heading-string (concat heading-string (char-to-string current-char)))))
    (setq index (+ index 1)))
  (setq selected-style heading-style)
  (if (string= heading-string personae-heading-string)
      (setq is-personae-section t)
    (setq is-personae-section nil))
  (unless is-personae-section
    (with-current-buffer (get-buffer-create new-epos-buffer-name)
      (when (> (point-max) 1)
	(newline))
      (insert heading-string)(newline)))
  (setq last-selected-style-name heading-style))

;;; --------------------------------------------------------
;;;
(defun get-style-name (line)
  "Returns the name of the paragraph style."
  (setq line-splitted (split-string line))
  (setq style-name-string (nth 1 line-splitted))
  (setq style-name-string-splitted (split-string style-name-string "\""))
  (setq key (nth 1 style-name-string-splitted))
  (if (and (eq (string-match-p "^P[0-9]*" key) nil)
	   (eq (string-match-p "^T[0-9]*" key) nil))
      key
    (gethash key styles-hash-table)))

;;; --------------------------------------------------------
;;;
(defun get-formatted-paragraph (line)
  "Returns a formatted paragraph."
  (setq index 0)
  (setq tag-text "")
  (setq paragraph-text "")
  (setq in-tag nil)
  (setq last-text-span-italics nil)
  (setq line-breaks 0)
  ;; indent first line of some styles
  (when (or (string= style-name insertion-style)
	    (string= style-name insertion-last-paragraph-style)
	    (string= style-name standard-interrupted-style))
    (setq paragraph-text (concat paragraph-text indentation)))
  (while (< index (string-width line))
    (setq current-char (aref line index))
    (cond
     ((= current-char 60) ; 60 <
      (setq in-tag t)
      (setq tag-text (char-to-string current-char)))
     ((= current-char 62) ; 62 >
      (setq in-tag nil)
      (setq tag-text (concat tag-text (char-to-string current-char)))
      (cond
       ;; replace line breaks
       ((eq (string= odt-line-break tag-text) t)
	(setq line-breaks (+ line-breaks 1))
	(setq paragraph-text (concat paragraph-text indented-line-break)))
       ;; <text:note>
       ((eq (string-prefix-p footnote-tag-prefix tag-text) t)
	(setq in-footnote t)
	(setq paragraph-text (concat paragraph-text footnote-start-marker)))
       ;; </text:note>
       ((eq (string= footnote-tag-postfix tag-text) t)
	(setq in-footnote nil)
	(setq paragraph-text (concat paragraph-text footnote-end-marker)))	
       ;; <text:note-body>
       ((eq (string-prefix-p text-note-body-start-tag tag-text) t)
	(setq in-text-note-body t))
       ;; </text:note-body>
       ((eq (string= text-note-body-end-tag tag-text) t)
	(setq in-text-note-body nil))
       ;; <text:span>
       ((eq (string-prefix-p text-span-tag-prefix tag-text) t)
	(when (or (> line-breaks 0) ; no italic-markers in first line of paragraph
		  (string= style-name standard-interrupted-style)) ; (only if standard-interruted-style)
	  (if (string= (get-style-name tag-text) italics-style-name)
	      (progn
		(setq paragraph-text (concat paragraph-text italics-marker))
		(setq last-text-span-italics t))
	    (setq last-text-span-italics nil))))
       ;; </text:span)
       ((eq (string= text-span-tag-postfix tag-text) t)
	(when (or (> line-breaks 0) ; no italic-markers in first line of paragraph
		  (string= style-name standard-interrupted-style)) ; (only if standard-interruted-style)
	  (when last-text-span-italics
	    (setq paragraph-text (concat paragraph-text italics-marker))
	    (setq last-text-span-italics nil))))))
     ((eq in-tag t)
      (setq tag-text (concat tag-text (char-to-string current-char))))
     ((and (eq in-tag nil)
	   (eq in-footnote in-text-note-body))
      (setq paragraph-text (concat paragraph-text (char-to-string current-char)))))
    (setq index (+ index 1)))
  paragraph-text)

;;; --------------------------------------------------------
;;;
(defun get-episoda-title (line)
  "Returns the title of the episoda."
   (setq index (string-match ": " formatted-paragraph))
   (setq title (substring line 0 index))
   title)

;;; --------------------------------------------------------
;;;
(defun initialize-file-name (line)
  "Initializes the filename."
  (setq title (get-episoda-title line))
  (setq title-splitted (split-string title " "))
  (setq title (concat (nth 0 title-splitted) (nth 1 title-splitted)))
  (setq title (downcase title))
  (setq file-name (concat title "." series-name "." default-extension)))

;;; --------------------------------------------------------
;;;
(defun initialize-paragraph (line)
  "Initializes a paragraph line."
  (setq style-name (get-style-name line))
  (when (string= style-name episoda-style)
    (setq formatted-paragraph (get-formatted-paragraph line))
    (initialize-file-name formatted-paragraph))
  (when (and (not is-personae-section)
	     (or (string= style-name heading-style)
		 (string= style-name location-style)
		 (string= style-name persons-style)
		 (string= style-name persons-standalone-style)
		 (string= style-name standard-style)
		 (string= style-name standard-interrupted-style)
		 (string= style-name standard-last-paragraph-style)
		 (string= style-name insertion-style)
		 (string= style-name insertion-last-paragraph-style)
		 (string= style-name footnote-style)))
    (setq formatted-paragraph (get-formatted-paragraph line))
    (with-current-buffer (get-buffer-create new-epos-buffer-name)
      ;; some styles have newline before paragraph
      (cond
       ((or (string= style-name persons-style)
	    (string= style-name persons-standalone-style))
	(unless (string= last-selected-style-name location-style)
	  (newline)))
       ((string= style-name location-style)
	(unless (string= last-selected-style-name heading-style)
	  (newline))))
      ;; insert paragraph
      (when (string= style-name footnote-style)
	(delete-backward-char 1))
      (insert formatted-paragraph)(newline)
      ;; some styles have newline after paragraph
      (when (or (string= style-name persons-style)
		(string= style-name persons-standalone-style))
	(newline))))
  (setq last-selected-style-name style-name))

;;; --------------------------------------------------------
;;;
(defun interpret-line (line)
  "Interprets a line."
  (cond 
   ;; <style:style>
   ((string-prefix-p style-tag-prefix line)
    (initialize-style line))
   ;; <text:h>
   ((string-prefix-p text-h-tag-prefix line)
    (initialize-heading line))
   ;; <text:p>
   ((string-prefix-p text-p-tag-prefix line)
    (initialize-paragraph line))))

;;; --------------------------------------------------------
;;;
(defun clean-up-new-epos ()
  "Remove unnecessary characters."
  (message "Cleaning up ...")
  (with-current-buffer (get-buffer-create new-epos-buffer-name)
    ;; "##" -> "#"
    (goto-char 0)
    (while (re-search-forward "##" nil t)
      (replace-match "#"))
    ;; "#\n  #" -> "\n  "
    (goto-char 0)
    (while (re-search-forward (concat "#\n" indentation "#") nil t)
      (replace-match (concat "\n" indentation)))))

;;; --------------------------------------------------------
;;;
(defun save-new-epos ()
  "Saves the imported file."
  (message "Saving new epos ...")
  (with-current-buffer (get-buffer-create new-epos-buffer-name)
    (write-file file-name nil)
    (kill-buffer)))

;;; --------------------------------------------------------
;;;
(defun kill-content-xml-buffer ()
  "Kills content-xml-buffer without confirmation."
  (with-current-buffer (get-buffer-create content-xml-buffer)
    (not-modified)
    (kill-buffer)))

;;; --------------------------------------------------------
;;;
(defun kill-new-epos-buffer ()
  "Kills the new-epos-buffer."
  (with-current-buffer (get-buffer-create new-epos-buffer-name)
    (kill-buffer)))

;;; --------------------------------------------------------
;;;
(defun import-to-new-epos ()
  "Imports .odt-content into new .epos-file."
  (message "Importing new epos ....")
  (setq in-footnote nil)
  (setq in-text-note-body nil)
  (setq styles-hash-table (make-hash-table :test 'equal))
  (setq buffer-to-kill (get-buffer new-epos-buffer-name))
  (kill-buffer buffer-to-kill)
  (with-current-buffer (get-buffer-create content-xml-buffer)
    (goto-char 0)
    (setq lines-iterated 0)  
    (while (< (point)(point-max))
     (setq formatted-line (get-current-line))
      (interpret-line formatted-line)
      (forward-line)
      (setq lines-iterated (+ lines-iterated 1))
      (message (format "lines-iterated: %d" lines-iterated)))
    (with-current-buffer (get-buffer-create new-epos-buffer-name)
      (delete-backward-char 1)) ; kill last (newline)
    (delete-file buffer-file-truename))
  (clean-up-new-epos)
  (save-new-epos)
  (kill-new-epos-buffer)
  (kill-content-xml-buffer)
  (find-file file-name)
  (message "Finished."))

;;; ---------------------------------------------------------
;;;
(defun get-current-line ()
  "Returns the current line of cursor position."
  (interactive)
  (buffer-substring-no-properties (line-beginning-position)(line-end-position)))

;;; ---------------------------------------------------------
;;;
(defun get-next-line (step)
  "Returns the step next line."
  (interactive)
  (save-excursion
    (forward-line (+ 0 step))
    (setq next-line (buffer-substring-no-properties (line-beginning-position)(line-end-position))))
  next-line)

;;; --------------------------------------------------------
;;;
(defun search-forward-replace-match (regexp newtext)
  "Replaces regexp with newtext in whole buffer."
  (goto-char 0)
  (while (re-search-forward regexp nil t)
    (replace-match newtext)))

;;; --------------------------------------------------------
;;;
(defun prepare-styles ()
  "Prepares style area."
  (message "Preparing \"<style:style \"-tags.")
  (search-forward-replace-match style-tag-prefix (concat "\n" style-tag-prefix)))

;;; --------------------------------------------------------
;;;
(defun prepare-text-h ()
  "Prepares \"<text:h \"-tags."
  (message "Preparing \"<text:h \"-tags ...")
  (search-forward-replace-match text-h-tag-prefix (concat "\n" text-h-tag-prefix)))

;;; --------------------------------------------------------
;;;
(defun prepare-text-p ()
  "Prepares \"<text:p \"-tags."
  (message "Preparing \"<text:p \"-tags ...")
  (search-forward-replace-match text-p-tag-prefix (concat "\n" text-p-tag-prefix))
  (search-forward-replace-match office-text-tag-postfix (concat "\n" office-text-tag-postfix)))

;;; --------------------------------------------------------
;;;
(defun prepare-text-list ()
  "Prepares \"<text:list \"-tags."
  (message "Preparing \"<text:list \"-tags ...")
  (search-forward-replace-match text-list-tag-prefix (concat "\n" text-list-tag-prefix))
  (message (concat "Preparing \"" text-list-end-tag "\"-tags ..."))
  (search-forward-replace-match text-list-end-tag (concat "\n" text-list-end-tag)))

;;; --------------------------------------------------------
;;;
(defun flush-unnecessary-lines ()
  "Flushes unnecessary lines (like \"<text:list \")."
  (goto-char 0)
  (flush-lines (concat "^" text-list-tag-prefix))
  (goto-char 0)
  (flush-lines (concat "^" text-list-end-tag)))

;;; --------------------------------------------------------
;;;
(defun extract-content-xml (file-to-open)
  "Extracs content.xml from file-to-open."
  (setq unzip-command (concat "unzip -p " file-to-open " content.xml > content.xml"))
  (shell-command unzip-command t)
  "content.xml")

;;; --------------------------------------------------------
;;;
(defun replace-special-characters ()
  "Replaces special .odt-characters (like &apos;)."
  (goto-char 1)
  (replace-regexp "&apos;" "'" nil (point)(point-max)))
;;; --------------------------------------------------------
;;;
(defun prepare-document ()
  (setq file-to-open (read-file-name ".odt-file to import: "))
  (setq extracted-content-xml (extract-content-xml file-to-open))
  (setq content-xml-buffer (find-file-noselect extracted-content-xml))
  (with-current-buffer (get-buffer-create content-xml-buffer)
    (message "Preparing document ...")
    (replace-special-characters)
    (prepare-styles)
    (prepare-text-list)
    (prepare-text-h)
    (prepare-text-p)
    (flush-unnecessary-lines))
  (message "Finished."))

;;; --------------------------------------------------------
;;;
(defun interpret-tag (tag)
  (setq splitted-tag-string (split-string tag))
  (if (string= (nth 0 splitted-tag-string) text-span-end-marker)
      (progn
	(when (string= (gethash last-text-span-marker-style-name styles-hash-table) italics-style-name)
	  italics-marker))
    (unless (string= (nth 0 splitted-tag-string) text-span-end-marker)
      (setq splitted-style-string (split-string (nth 1 splitted-tag-string) "\""))
      (setq style-name (nth 1 splitted-style-string))
      (cond
       ((string= (gethash style-name styles-hash-table) "")
	(setq last-text-span-marker-style-name style-name)
	"")
       ((string= (gethash style-name styles-hash-table) italics-style-name)
	(setq last-text-span-marker-style-name style-name)
	italics-marker)))))

;;; epic-import.el ends here
