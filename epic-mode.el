;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
(defconst library-name                              "epic-mode")
(defvar   epic-configuration-file                   "")
(defvar   library-path                              "")
(defvar   library-directory                         "")
(defconst no-template-selected                      0)
(defconst standard-template                         1)
(defconst heading-template                          2)
(defconst location-template                         3)
(defconst insertion-template                        4)
(defconst person-template                           5)
(defconst header-template                           6)
(defconst legal-notice-template                     7)
(defconst personae-template                         8)
(defconst standard-template-continued               9)
(defconst wiki-template                            10)
(defconst plot-template                            11)
(defvar   lines-iterated                           0)
(defvar   current-hook-function                    nil)
(defvar   uppercase-input                          nil)
(defvar   current-output-function                  'default-output-function)
(defvar   current-on-enter-function                'on-enter-default-function)
(defvar   previous-output-function                 nil)
(defvar   previous-input                           0)
(defvar   italics-input                            nil)
(defvar   last-selected-template                   no-template-selected)
(defvar   footnote-index                           1)
(defvar   selected-style                           nil)
(defconst error-in-style                           "error in style")
(defconst unknown-style                            "unknown style")
(defconst empty-line-style                         "empty line")
(defvar   show-current-output-function             nil)
(defvar   display-selected-style                   t)
(defvar   last-char                                nil)
(defconst background-color                         "black")
(defconst foreground-color                         "cyan")
(defconst plot-buffer-name                         "plot-buffer")
(defconst personae-file-name                       "personae")
(defconst impressum-file-name                      "impressum")
(defconst werbung-file-name                        "werbung.html")

;;; ---------------------------------------------------------
;;;
(defun get-line-without-footnotes (line)
  "Returns a line without footnotes."
  (setq index 0)
  (setq in-footnote nil)
  (setq new-line "")
  (while (< index (string-width line))
    (setq current-char (aref line index))
    (cond
     ((= current-char 91) ; 91 [
      (setq in-footnote t))
     ((= current-char 93) ; 93 ]
      (setq in-footnote nil))
     (t
      (unless in-footnote
	(setq new-line (concat new-line (char-to-string current-char))))))
    (setq index (+ index 1)))
  new-line)

;;; ---------------------------------------------------------
;;;
(defun extract-plot-headings ()
  "Extracts all plot relevant headings (Headings, location, person)."
  (interactive)
  (setq lines-iterated 0)
  (save-excursion
    (goto-char 0)
    (while (< (point) (point-max))
      (setq current-line (get-current-line))
      (setq current-style (get-style current-line))
      (when (or
	     (eq current-style heading-style)
	     (eq current-style location-style)
	     (eq current-style persons-style))
	(with-current-buffer (get-buffer-create plot-buffer-name)
	  (insert (get-line-without-footnotes current-line))
	  (newline)
	  (when (eq current-style persons-style)
	    (newline)
	    (newline)
	    (newline))))
      (forward-line)
      (setq lines-iterated (+ lines-iterated 1))
      (message (concat "lines-iterated: " (number-to-string lines-iterated))))
    (setq plot-file-name (concat (file-name-sans-extension (file-name-nondirectory buffer-file-truename)) ".plot"))
    (with-current-buffer (get-buffer-create plot-buffer-name)
      (write-file plot-file-name nil)
      (kill-buffer (current-buffer)))
    (message "Finished.")))

;;; ---------------------------------------------------------
;;;
(defun epos-check-styles ()
  "Checks if any lines contain styles that couldn't be recognized."
  (interactive)
  (setq lines-iterated 0)
  (goto-char (point-min))
  (while (and (< (point) (point-max))
	      (not (eq (get-style (get-current-line)) nil)))
    (forward-line)
    (setq lines-iterated (+ lines-iterated 1))
    (message (concat "lines-iterated: " (number-to-string lines-iterated)))))

;;; ---------------------------------------------------------
;;;
(defun epos-create ()
  "Creates all file formats."
  (interactive)
  (message "Creating all files ...")
  (mobi-create)
  (pdf-create)
  (message "Finished."))

;;; ---------------------------------------------------------
;;;
(defun default-output-function ()
  "Simple output function."
)

;;; ---------------------------------------------------------
;;;
(defun initialize-selected-template ()
  "Initializes the selected template for current line."
  (cond
   ((and (or (eq last-selected-template standard-template-continued)(eq last-selected-template insertion-template))
	 (unless (string-prefix-p indentation formatted-line)
	   (unless (string-prefix-p ";" formatted-line)
	     (unless (string-prefix-p "(" formatted-line)
	       (unless (string-prefix-p "-" formatted-line)
		 (setq selected-template standard-template)))))))
   ((eq last-selected-template heading-template)
    (unless (string= last-line heading-marker)
      (setq selected-template location-template)))
   ((eq last-selected-template location-template)
    (unless (string= last-line location-template-marker)
      (setq selected-template person-template)))    
   ((eq (string= formatted-line header-marker) t)
    (setq selected-template header-template))
   ((eq (string= formatted-line legal-notice-marker) t)
    (setq selected-template legal-notice-template))
   ((eq (string= formatted-line heading-marker) t)
    (setq selected-template heading-template))
   ((eq (string= formatted-line standard-template-marker) t)
    (setq selected-template standard-template))
   ((eq (string= formatted-line location-template-marker) t)
    (setq selected-template location-template))
   ((eq (string= formatted-line insertion-template-marker) t)
    (setq selected-template insertion-template))
   ((eq (string= formatted-line person-template-marker) t)
    (setq selected-template person-template))
   ((eq (string= formatted-line personae-marker) t)
    (setq selected-template personae-template))
   ((eq (string= formatted-line wiki-start-marker) t)
    (setq selected-template wiki-template))
   ((eq (string= formatted-line plot-marker) t)
    (setq selected-template plot-template))
   ;; standard-template line with indentation at the beginning
   ((and (eq (string-prefix-p indentation formatted-line) t)
	 (not (eq (string-prefix-p "  (" formatted-line) t))
	 (setq selected-template standard-template-continued)))
    ;; insertion-template with "(" at the beginning
   ((or (eq (string-prefix-p "(" formatted-line) t)
	(eq (string-prefix-p "  (" formatted-line) t))
    (unless (eq (string-prefix-p "(c) " formatted-line) t)
      (setq selected-template insertion-template)))))

;;; ---------------------------------------------------------
;;;
(defun heading-chapter-output-function ()
  "Handles heading output."
  (delete-backward-char 1)
  (insert (upcase last-char)))

;;; ---------------------------------------------------------
;;;
(defun heading-output-function ()
  "Handles heading output."
  (delete-backward-char 1)
  (if (eq uppercase-input nil)
      (insert last-char)
    (insert (upcase last-char))
    (setq uppercase-input nil)))

;;; ---------------------------------------------------------
;;;
(defun heading-chapter-on-enter-function ()
  "Handles heading chapter on enter."
  (setq current-output-function   'heading-output-function)
  (setq current-on-enter-function 'heading-on-enter-function)
  (setq uppercase-input t)
  (setq last-char 13)
  (insert " - "))

;;; ---------------------------------------------------------
;;;
(defun heading-on-enter-function ()
  "Handles heading on enter."
  (when (eq last-char 13)
    (delete-backward-char 3))
  (deinitialize-template)
  (insert "\n")
  (set-location-template))

;;; ---------------------------------------------------------
;;;
(defun set-heading-template ()
  "Sets and initializes heading template."
  (setq selected-template heading-template)
  (setq current-output-function 'heading-chapter-output-function)
  (if (<= (current-column) 0)
      (setq current-on-enter-function 'on-enter-default-function)
    (setq position-of-delimiter (string-match-p "-" (get-current-line)))
    (if (eq position-of-delimiter nil)
	(setq current-on-enter-function 'heading-chapter-on-enter-function)
      (if (< (current-column) position-of-delimiter)
	  (setq current-on-enter-function 'heading-chapter-on-enter-function)
	(setq current-on-enter-function 'heading-on-enter-function)))))

;;; ---------------------------------------------------------
;;;
(defun location-output-function ()
  "Handles location output."
  (delete-backward-char 1)
  (if (not (eq last-char ""))
      (if (eq uppercase-input nil)
  	  (insert last-char)
  	(setq uppercase-input nil)
  	(insert (upcase last-char)))))

;;; ---------------------------------------------------------
;;;
(defun location-on-enter-function ()
  "Handles location template on enter."
  (deinitialize-template)
  (insert "\n")
  (set-person-template))

;;; ---------------------------------------------------------
;;;
(defun set-location-template ()
  "Sets and initializes location template."
  (setq selected-template         location-template)
  (setq current-output-function   'location-output-function)
  (setq current-on-enter-function 'location-on-enter-function)
  (setq last-char "")
  (setq uppercase-input t)
  (when (eq show-current-output-function t)
    (display-current-output-function)))

;;; ---------------------------------------------------------
;;;
(defun person-person-output-function ()
  "Handles person template person output."
  (delete-backward-char 1)
  (insert (upcase last-char)))

;;; ---------------------------------------------------------
;;;
(defun person-person-on-enter-function ()
  "Handles person template person-on-enter."
  (insert " ")
  (setq current-output-function   'person-output-function)
  (setq current-on-enter-function 'person-on-enter-function))

;;; ---------------------------------------------------------
;;;
(defun person-output-function ()
  "Handles person template output."
  (if (and
       (eq (is-uppercase-letter last-char)      t)
       (eq (is-uppercase-letter previous-input) t))
      (progn
	(end-of-line)
	(setq previous-input 0)
	(setq current-output-function   'person-person-output-function)
	(setq current-on-enter-function 'person-person-on-enter-function))
    (delete-backward-char 1)
    (if (eq uppercase-input t)
    	(insert (upcase last-char))
      (insert last-char))
    (setq previous-input last-char)))

;;; ---------------------------------------------------------
;;;
(defun person-on-enter-function ()
  "Handles person on enter."
  (when (eq (char-before) 32)
    (delete-backward-char 1))
  (insert "\n\n")
  (set-standard-template))

;;; ---------------------------------------------------------
;;;
(defun set-person-template ()
  "Sets and initializes person template."
  (setq selected-template         person-template)
  (setq current-output-function   'person-output-function)
  (setq current-on-enter-function 'person-on-enter-function)
  (setq uppercase-input            nil))

;;; ---------------------------------------------------------
;;;
(defun standard-text-output-function ()
  "Handles standard template standard text output."
  (delete-backward-char 1)
  (if (eq uppercase-input nil)
      (insert last-char)
    (insert (upcase last-char))
    (setq uppercase-input nil)))

;;; ---------------------------------------------------------
;;;
(defun standard-person-braces-on-enter-function ()
  "Handles <ENTER> after opening brace."
  (insert ").\n  ")
  (setq uppercase-input t)
  (setq current-on-enter-function 'standard-on-enter-function)
  (setq current-output-function   'standard-text-output-function))

;;; ---------------------------------------------------------
;;;
(defun standard-person-output-function ()
  "Handles standard-template-person-output."
  (delete-backward-char 1)
  (if (eq uppercase-input t)
      (insert (upcase last-char))
    (insert last-char))
  (cond
   ((eq last-char 46) ; . 46
    (insert " ")
    (setq uppercase-input t)
    (setq current-on-enter-function 'standard-on-enter-function)
    (setq current-output-function   'standard-text-output-function))))

;;; ---------------------------------------------------------
;;;
(defun standard-on-enter-function ()
  "Handles on-enter when standard template is selected."
  (if (string= indentation (get-current-line))
      (progn
	(delete-backward-char (length indentation))
	(set-standard-template))
    (insert "\n" indentation)))

;;; ---------------------------------------------------------
;;;
(defun standard-person-on-enter-function ()
  "Handles standard person on enter input. If user just presses <ENTER>, \".<SPACE>\" is inserted."
  (if (string= "" (get-current-line))
      (progn
	(newline)
	(deinitialize-template))
    (insert " (")
    (setq uppercase-input            nil)
    (setq current-output-function   'standard-text-output-function)
    (setq current-on-enter-function 'standard-person-braces-on-enter-function)))

;;; ---------------------------------------------------------
;;;
(defun set-standard-on-enter-function ()
  "Sets current-on-enter-function to standard-on-enter-function."
  (setq current-on-enter-function 'standard-on-enter-function))

;;; ---------------------------------------------------------
;;;
(defun set-standard-template ()
  "Sets and initializes standard template."
  (setq selected-template standard-template)
  (unless (string-match-p "\\." (get-current-line))
    (setq current-output-function 'standard-person-output-function))
  (if (and (string-prefix-p indentation (get-current-line))
	   (= (current-column) (length (get-current-line))))
      (set-standard-on-enter-function)
    (if (string-match-p "\\." (get-current-line))
	(set-standard-on-enter-function)
      (if (string-match-p "(" (get-current-line))
	  (setq current-on-enter-function 'standard-person-braces-on-enter-function)
	(setq current-on-enter-function 'standard-person-on-enter-function))))
  (when (= (current-column) 0)
    (setq uppercase-input t)))

;;; ---------------------------------------------------------
;;;
(defun set-standard-template-continued ()
  "Sets and initializes standard template continued."
  (setq         selected-template             standard-template-continued)
  (setq         current-output-function       'standard-text-output-function)
  (setq         current-on-enter-function     'standard-on-enter-function)
  (when (eq show-current-output-function t)
    (display-current-output-function)))

;;; ---------------------------------------------------------
;;;   
(defun insertion-output-function ()
  "Handles insertion-template-input"
  (delete-backward-char 1)
  (if (eq uppercase-input t)
      (insert (upcase last-char))
    (insert last-char))
  (setq uppercase-input nil))

;;; ---------------------------------------------------------
;;;
(defun insertion-on-enter-function ()
  "Handles insertion on enter."
  (deinitialize-template)
  (if (string-prefix-p "  (" (get-current-line))
      (progn
	(insert ")\n  ")
	(setq last-char 13)
	(setq current-on-enter-function 'standard-on-enter-function))
    (insert ")\n")))

;;; ---------------------------------------------------------
;;;
(defun set-insertion-template ()
  "Sets and initializes insertion template."
  (setq selected-template         insertion-template)
  (setq current-output-function   'insertion-output-function)
  (setq uppercase-input nil)
  (if (string-match-p ")" (get-current-line))
      (setq current-on-enter-function 'on-enter-default-function)
    (setq current-on-enter-function 'insertion-on-enter-function)))

;;; ---------------------------------------------------------
;;;
(defun display-selected-template ()
  "Displays current selected template."
  (interactive)
  (message "selected-template: %d" selected-template))

;;; ---------------------------------------------------------
;;;
(defun display-current-line ()
  "Displays current line of cursor position."
  (interactive)
  (message (get-current-line)))

;;; ---------------------------------------------------------
;;;
(defun get-current-line ()
  "Returns the current line of cursor position."
  (interactive)
  (buffer-substring-no-properties (line-beginning-position)(line-end-position)))

;;; ---------------------------------------------------------
;;;
(defun legal-notice-text-output-function ()
  "Handles legal notice template input."
  ;; nothing special to do.
)

;;; ---------------------------------------------------------
;;;
(defun legal-notice-on-enter-function ()
  "Handles legal notice on enter."
  (if (eq previous-input 13)
      (progn
	(setq previous-input 0)
	(set-heading-template))
    (newline)
    (setq previous-input 13)))

;;; ---------------------------------------------------------
;;;
(defun header-output-function ()
  "Handles header input."
  ;; nothing special to do.
)

;;; ---------------------------------------------------------
;;;
(defun header-on-enter-function ()
  "Handles header on enter."
  (if (eq previous-input 13)
      (progn
	(setq previous-input 0)
	(set-heading-template))
    (newline)
    (setq previous-input 13)))

;;; ---------------------------------------------------------
;;;
(defun personae-output-function ()
  "Handles personae input."
  ;; nothing special to do.
)

;;; ---------------------------------------------------------
;;;
(defun personae-on-enter-function ()
  "Handles personae on enter."
  (if (eq previous-input 13)
      (progn
	(setq previous-input 0)
	(set-heading-template))
    (newline)
    (setq previous-input 13)))

;;; ---------------------------------------------------------
;;;
(defun get-current-line-template ()
  "Selects the according template at cursor position."
  (save-excursion
    (setq current-line (get-current-line))
    (cond
     ;; In standard-template in line with indentation at the beginning.
     ((and (eq (string-prefix-p indentation current-line) t)
	   (not (eq (string-prefix-p "  (" current-line) t)))
      (setq selected-template standard-template)
      (setq current-on-enter-function 'standard-on-enter-function)
      (setq current-output-function   'standard-text-output-function)
      (if (and (eq (string= indentation current-line) t)
	       (not (eq (string= "  (" current-line) t)))
	  (setq last-char 13)
	(setq last-char 0)))
     (t
      (cond
       	 ;; insertion-template
	 ((or (eq (string-prefix-p "  (" current-line) t)
	      (and (eq (string-prefix-p "(" current-line) t)
		   (not (eq (string-prefix-p "(c)" current-line) t))))
	  (set-insertion-template))
	 (t
	  (setq selected-template-string      "")
	  (setq last-selected-template-string "")
	  (setq marker-found                  nil)
	  (setq lines-iterated 0)
	  (while (eq marker-found nil)     
	    (cond
	     ;; header marker
	     ((eq (string= current-line header-marker) t)
	      (setq marker-found t)
	      (setq selected-template header-template)
	      (setq current-output-function 'header-output-function)
	      (setq current-on-enter-function 'header-on-enter-function))
	     ;; legal notice marker
	     ((eq (string= current-line legal-notice-marker) t)
	      (setq marker-found t)
	      (setq selected-template legal-notice-template)
	      (setq current-output-function   'legal-notice-text-output-function)
	      (setq current-on-enter-function 'legal-notice-on-enter-function))	      
	     ;; heading-marker
	     ((eq (string= current-line heading-marker) t)
	      (setq marker-found t)
	      (cond
	       ;; first line after heading-marker is heading-template
	       ;; why does this work (see below location-template (= lines-iterated -1) ...
	       ((or (= lines-iterated 0)
		    (= lines-iterated -1))		    
		(setq selected-template         heading-template)
		(setq current-output-function   'heading-chapter-output-function)
		(setq current-on-enter-function 'heading-chapter-on-enter-function)
		(when (eq (string= current-line "") t)
		  (setq uppercase-input t)))
	       ;; second line after heading-marker is location-template
	       ((= lines-iterated -2)
		(setq last-selected-template heading-template)
		(set-location-template))
	       ;; third line after heading-marker is person-template
	       ((= lines-iterated -3)
		(setq last-selected-template location-template)
		(set-person-template))))
	     ;; after template-marker
	     ((eq (string= current-line standard-template-marker) t)
	      (setq marker-found              t)
	      (setq selected-template         standard-template)
	      (setq last-selected-template    standard-template)
	      (setq current-output-function   'standard-output-function)
	      (setq current-on-enter-function 'standard-on-enter-function)
	      (setq last-char                 0))
	     ;; after location-marker
	     ((eq (string= current-line location-template-marker) t)
	      (setq marker-found t)
	      (cond
	       ;; first line after location-marker is location-template
	       ((= lines-iterated -1)
		(setq selected-template         location-template)
		(setq current-output-function   'location-output-function)
		(setq current-on-enter-function 'location-on-enter-function)
		;; ... and this not (see above heading-template (= lines-iterated -1)?
		;;	  (when (eq (string= current-line "") t)
		;;	    (setq uppercase-input t))
		)
	       ((= lines-iterated -2)
		(setq last-selected-template location-template)
		(set-person-template))))	     
	     ;; person template
	     ((eq (string= current-line person-template-marker) t)
	      (setq marker-found t)
	      (setq selected-template         person-template)
	      (setq current-output-function   'person-output-function)
	      (setq current-on-enter-function 'person-on-enter-function))
	     ;; personae marker
	     ((eq (string= current-line personae-marker) t)
	      (setq marker-found t)
	      (setq selected-template         personae-template)
	      (setq current-output-function   'personae-output-function)
	      (setq current-on-enter-function 'personae-on-enter-function))
	     ;; plot marker
	     ((eq (string= current-line plot-marker) t)
	      (setq marker-found t)
	      (setq selected-template plot-template)
	      (setq current-output-function   'plot-output-function)
	      (setq current-on-enter-function 'plot-on-enter-function))
	     ;; wiki marker
	     ((eq (string= current-line wiki-start-marker) t)
	      (setq marker-found t)
	      (setq selected-template wiki-template)
	      (setq current-output-function   'wiki-output-function)
	      (setq current-on-enter-function 'wiki-on-enter-function)))
	    (forward-line -1)
	    (setq lines-iterated (- lines-iterated 1))
	    (setq current-line (get-current-line)))))))))

;;; ---------------------------------------------------------
;;;
(defun query-episoda-number (x)
  "Asks user for episoda-number."
  (interactive "sNumber of episoda: ")
  (setq episoda-number x))

;;; ---------------------------------------------------------
;;;
(defun query-episoda-title (x)
  "Asks user for title of episoda."
  (interactive "sTitle of episoda: ")
  (setq episoda-title x))

;;; ---------------------------------------------------------
;;;
(defun insert-book-guid ()
  "Inserts a unique book guid."
  (interactive)
  (insert (get-guid)))

;;; ---------------------------------------------------------
;;;
(defun create-new-episoda ()
  "Creates header, legal notice etc."
  (call-interactively 'query-episoda-number)
  (call-interactively 'query-episoda-title)
  (insert-book-guid)
  (insert header-marker "\n")
  (insert author "\n")
  (insert title "\n")
  (insert subtitle "\n")
  (insert "Episoda " episoda-number ": " episoda-title "\n")
  (insert publisher "\n")
  (insert legal-notice-marker "\n")
  (insert copyright-notice " " publisher "\n")
  (set-heading-template))

;;; ---------------------------------------------------------
;;;
(defun cursor-is-at-end-of-line ()
  "Checks if cursor is at end of line."
  (if (= (current-column) (length (get-current-line)))
      t
    nil))

;;; ---------------------------------------------------------
;;;
(defun on-enter-default-function ()
  "Default on-enter-function if no template is selected."
  (cond
   ((string= (get-current-line) "")
    (setq previous-style (get-style (get-next-line -1)))
    (cond
     ((or (eq previous-style persons-style)
	  (eq previous-style persons-standalone-style))
      (newline)
      (set-standard-template))
     (t
      (deinitialize-template)
      (newline))))
   ((eq (cursor-is-at-end-of-line) t)
    (cond
     ((eq (get-style (get-current-line)) heading-style)
      (set-heading-template)
      (on-enter))
     (t
      (newline))))
   (t
    (newline))))

;;; ---------------------------------------------------------
;;;
(defun is-valid-select-template-input (ascii-code)
  "Checks if character is valid template select input."
  (when
      (or
       (eq ascii-code 40) ; ( 40
       (eq ascii-code 49) ; 1
       (eq ascii-code 50) ; 2
       (is-lowercase-letter ascii-code)
       (is-uppercase-letter ascii-code))
    t))

;;; ---------------------------------------------------------
;;;
(defun deinitialize-template ()
  "Deinitializes standard template."
  (setq previous-input            0)
  (setq uppercase-input           nil)
  (setq last-selected-template    selected-template)
  (setq selected-template         no-template-selected)
  (setq current-output-function   'default-output-function)
  (setq current-on-enter-function 'on-enter-default-function))

;;; ---------------------------------------------------------
;;;
(defun  display-current-output-function ()
  "Displays current-output-function in minibuffer."
    (message "current-output-function: %s" current-output-function))

;;; ---------------------------------------------------------
;;;
(defun initialize-template ()
  "Initialize selected template depending on user input."
  (cond
   ((eq (get-style (get-current-line)) empty-line-style)
    (deinitialize-template))
   ((eq (get-style (get-current-line)) heading-style)
    (set-heading-template))
   ((eq (get-style (get-current-line)) standard-style)
    (if (string-prefix-p indentation (get-current-line))
	(set-standard-template-continued)
      (set-standard-template)))
   ((and (eq (current-column) 1)
	 (eq current-output-function nil)
	 (eq (is-valid-select-template-input last-char) t))
    (cond
     ((eq (is-lowercase-letter last-char) t)
      (set-standard-template))   
     ((eq (is-uppercase-letter last-char) t)
      (set-person-template))
     ((eq last-char 40) ; 40 (
      (set-insertion-template))
     ((and
       (eq previous-input 13)
       (eq last-char 49)) ; 49 1
      (set-location-template))
     ((and
       (eq previous-input 13)
       (eq last-char 50)) ; 50 2
      (set-wiki-template))))
   ((eq (current-column) 3)
    (when (string-prefix-p indentation (get-current-line))
      (if (eq last-char 40) ; 40 (
	  (set-insertion-template)
	(set-standard-template-continued))))))

;;; ---------------------------------------------------------
;;;
(defun post-self-insert-hook-function()
  "Handles post-self-insert-hook"
  (setq last-char (char-before))
;;  (initialize-template)
  (funcall current-output-function)
  (when display-selected-style
    (display-selected-style)))

;;; ---------------------------------------------------------
;;;
(defun on-enter ()
  "Fires when enter is pressed."
  (funcall current-on-enter-function)
  (when display-selected-style
    (display-selected-style)))

;;; ---------------------------------------------------------
;;;
(defun on-backspace ()
  "Fires when backspace is pressed."
  (setq last-input "\b")
  (delete-backward-char 1)
  (when display-selected-style
    (display-selected-style)))

;;; ---------------------------------------------------------
;;;
(defun initialize ()
  "Performs initialization of epic-mode."
  ;;
  ;; Disable distractions
  ;;
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  (setq truncate-lines nil)
  ;;
  ;; hiding mode-line completely aligns last line to the left (bug?).
  ;; Simply setting back- and foreground to black creates an empty black line on the bottom.
  ;;
  (setq mode-line-format "")
  (set-face-foreground 'mode-line          background-color)
  (set-face-background 'mode-line          background-color)
  (set-face-background 'mode-line-inactive background-color)

  (set-face-background 'default            background-color)
  (set-face-foreground 'default            foreground-color)
  (set-face-foreground 'font-lock-function-name-face "blue")

  ;;
  ;; font-lock
  ;;
  (setq epic-mode-keywords '((";;;[a-zäöüßÖÄÜß A-Z-=0-9.:-]*" . font-lock-string-face)
				  ("#.*?#"           . font-lock-keyword-face)
                                  ("\\[.*?\\]"       . font-lock-function-name-face)
				  ("^(.*?)$"         . font-lock-keyword-face)
				  ("^  (.*?)$"       . font-lock-keyword-face)))

  ;; 
  ;;
  ;; Center text input and 60 charachters per line through window margins
  ;;
  ;;  (add-hook 'window-configuration-change-hook (lambda () (set-window-margins (car (get-buffer-window-list (current-buffer) nil t)) 34 33)))
  ;;
  ;; sudo dpkg-reconfigure console-setup TerminusBold 24x12
  ;;
  (add-hook 'window-configuration-change-hook (lambda () (set-window-margins (car (get-buffer-window-list (current-buffer) nil t)) 12 12)))
  ;;
  ;; sudo dpkg-reconfigure console-setup TerminusBold 32x16
  ;;
  ;;(add-hook 'window-configuration-change-hook (lambda () (set-window-margins (car (get-buffer-window-list (current-buffer) nil t)) 5 0)))


  (setq word-wrap t)

  (setq library-path (locate-library library-name))
  (setq library-directory (file-name-directory library-path))

  (if (= (point-max) 1)
      (create-new-episoda)
    (goto-char (point-max))
    (get-current-line-template)
    (setq last-selected-template selected-template))
  (goto-char (point-max))
  (when display-selected-style
    display-selected-style))

;;; --------------------------------------------------------
;;;
(defun debug-get-style ()
  "Debug function for get-style(line)."
  (interactive)
  (if (eq (get-style (get-current-line)) nil)
      (message "nil")
    (message (get-style (get-current-line)))))

;;; --------------------------------------------------------
;;;
(defun is-heading-style (line)
  "Checks if line has style of heading."
  (if (or (string-match-p "^[A-ZÖÄÜ ]+$" line)
	  (string-match-p "^[A-ZÖÄÜ ]+\\-[A-ZÖÄÜ a-zöäüß]+$" line)
	  (string-match-p "^[A-ZÖÄÜ ]+\\-[A-ZÖÄÜ a-zöäüß]+[\\[[a-zA-Z .,öäüÖÄÜß]+\\]]*$" line))
      t
    nil))

;;; --------------------------------------------------------
;;;
(defun is-location-style (line next-line-var)
  "Checks if line has style of location."
  (if (and (not (string-prefix-p indentation line))
	   (eq (string-match-p "^[A-ZÖÄÜ ]\\{2\\}" line) nil)
	   (not (string= "" next-line-var)))
      t
    nil))

;;; --------------------------------------------------------
;;;
(defun is-persons-style (line previous-line-var next-line-var)
  "Returns if line has style of persons."
  (unless (or (string-prefix-p indentation line)
	      (is-persons-standalone-style line previous-line-var next-line-var))	       
    (setq is-location-style-var (is-location-style previous-line-var line))
    (and (string= "" next-line-var)
	 is-location-style-var)))

;;; --------------------------------------------------------
;;;
(defun is-persons-standalone-style (line previous-line-var next-line-var)
  "Returns if line has style of persons-standalone."
  (if (and (not (string-prefix-p indentation line))
	   (string= "" previous-line-var)
	   (string= "" next-line-var))
      t
    nil))

;;; --------------------------------------------------------
;;;
(defun get-style (line)
  "Returns style of line."
  (save-excursion
    (cond
     ((string= line "")
      empty-line-style)
     ;; heading-style
     ((eq (is-heading-style line) t)
      heading-style)
     ;; location-style
     ((eq (is-location-style line (get-next-line 1)) t)
      location-style)
     ;; persons-style
     ((eq (is-persons-style line (get-next-line -1) (get-next-line 1)) t)
      persons-style)
     ;; persons-standalone-style
     ((eq (is-persons-standalone-style line (get-next-line -1) (get-next-line 1)) t)
      persons-standalone-style)
     ;; insertion-style
     ((or (string-prefix-p "(" line)
	  (string-prefix-p (concat indentation "(") line))
      insertion-style)
     ;; standard- or standard-interrupted-style
     ((string-prefix-p indentation line)
      (while (and (string-prefix-p indentation line)
		  (not (string-prefix-p "(" line))
		  (not (string-prefix-p (concat indentation "(") line)))
	(forward-line -1)
	(setq line (get-current-line)))
      (if (and (not (string-prefix-p indentation line))
	       (not (string-prefix-p "(" line))
	       (not (eq (get-style line) persons-standalone-style))
	       (not (string= "" line)))
	  standard-style
	standard-interrupted-style))
     ;; standard-, heading-, location- or persons-style
     ((not (string-prefix-p indentation line))
      ;; first line of buffer has to be heading
      (if (= (line-beginning-position) 1)
	  heading-style
	(if (and (or (string-match-p "^[A-ZÖÄÜ \\-]*\\." line)
		     (string-match-p "^[A-ZÖÄÜ \\-]*(\\([A-ZÖÄÜ a-zöäüß\\-]*\\))?\\.$" line)))
	    standard-style
	  ;; standard style first line
	  (if (or (string-prefix-p indentation (get-next-line 1))
	    	  (string-prefix-p indentation (get-next-line -1)))
	      standard-style
	    (setq lines-forward 0)
	    (while (and (not (string= "" line))
			(not (string-prefix-p indentation line))
			(not (string-prefix-p "(" line))
			(not (= (point) (point-max))))
	      (forward-line)
	      (setq lines-forward (+ lines-forward 1))
	      (setq line (get-current-line))))))))))

;;; --------------------------------------------------------
;;;
(defun get-style-TEMP (line)
  "Returns style of line."
  (save-excursion
    (cond
     ((string= line "")
      empty-line-style)
     ;; heading-style
     ((eq (is-heading-style line) t)
      heading-style)
     ;; location-style
     ((eq (is-location-style line) t)
      location-style)
     ;; persons-style
     ((not (string-prefix-p indentation line))
	   (setq previous-line (get-next-line -1))
	   ;; persons-standalone-style
	   (if (and (string= "" previous-line)
	   	    (string= "" (get-next-line 1)))
	       persons-standalone-style	     
	     (and (is-location-style previous-line)
	   	  (or (string= "" (get-next-line -2))
	   	      (is-heading-style (get-next-line -2)))
	   	  persons-style)))
     ;; insertion-style
     ((or (string-prefix-p "(" line)
	  (string-prefix-p (concat indentation "(") line))
      insertion-style)
     ;; standard- or standard-interrupted-style
     ((string-prefix-p indentation line)
      (while (and (string-prefix-p indentation line)
		  (not (string-prefix-p "(" line))
		  (not (string-prefix-p (concat indentation "(") line)))
	(forward-line -1)
	(setq line (get-current-line)))
      (if (and (not (string-prefix-p indentation line))
	       (not (string-prefix-p "(" line))
	       (not (eq (get-style line) persons-standalone-style))
	       (not (string= "" line)))
	  standard-style
	standard-interrupted-style))
     ;; standard-, heading-, location- or persons-style
     ((not (string-prefix-p indentation line))
      ;; first line of buffer has to be heading
      (if (= (line-beginning-position) 1)
	  heading-style
	(if (and (or (string-match-p "^[A-ZÖÄÜ \\-]*\\." line)
		     (string-match-p "^[A-ZÖÄÜ \\-]*(\\([A-ZÖÄÜ a-zöäüß\\-]*\\))?\\.$" line)))
	    standard-style
	  ;; standard style first line
	  (if (or (string-prefix-p indentation (get-next-line 1))
	    	  (string-prefix-p indentation (get-next-line -1)))
	      standard-style
	    (setq lines-forward 0)
	    (while (and (not (string= "" line))
			(not (string-prefix-p indentation line))
			(not (string-prefix-p "(" line))
			(not (= (point) (point-max))))
	      (forward-line)
	      (setq lines-forward (+ lines-forward 1))
	      (setq line (get-current-line))))))))))


;;; --------------------------------------------------------
;;;
(defun get-style-ORIGINAL (line)
  "Returns style of line."
  (save-excursion
    (cond
     ((string= line "")
      empty-line-style)
     ;; heading-style
     ((or (not (eq (string-match-p "^[A-ZÖÄÜ ]+$" line) nil))
	  (not (eq (string-match-p "^[A-ZÖÄÜ ]+\\-[A-ZÖÄÜ a-zöäüß]+$" line) nil))
	  (not (eq (string-match-p "^[A-ZÖÄÜ ]+\\-[A-ZÖÄÜ a-zöäüß]+[\\[[a-zA-Z .,öäüÖÄÜß]+\\]]*$" line) nil)))
      heading-style)
     ;; insertion-style
     ((or (string-prefix-p "(" line)
	  (string-prefix-p (concat indentation "(") line))
      insertion-style)
     ;; standard- or standard-interrupted-style
     ((string-prefix-p indentation line)
      (while (and (string-prefix-p indentation line)
		  (not (string-prefix-p "(" line))
		  (not (string-prefix-p (concat indentation "(") line)))
	(forward-line -1)
	(setq line (get-current-line)))
      (if (and (not (string-prefix-p indentation line))
	       (not (string-prefix-p "(" line))
	       (not (eq (get-style line) persons-standalone-style))
	       (not (string= "" line)))
	  standard-style
	standard-interrupted-style))
     ;; standard-, heading-, location- or persons-style
     ((not (string-prefix-p indentation line))
      ;; first line of buffer has to be heading
      (if (= (line-beginning-position) 1)
	  heading-style
	(if (and (or (string-match-p "^[A-ZÖÄÜ \\-]*\\." line)
		     (string-match-p "^[A-ZÖÄÜ \\-]*(\\([A-ZÖÄÜ a-zöäüß\\-]*\\))?\\.$" line))
		 (and (not (string= (get-next-line 1) ""))))
	    standard-style
	  ;; standard style first line
	  (if (or (string-prefix-p indentation (get-next-line 1))
	    	  (string-prefix-p indentation (get-next-line -1)))
	      standard-style
	    (setq lines-forward 0)
	    (while (and (not (string= "" line))
			(not (string-prefix-p indentation line))
			(not (string-prefix-p "(" line))
			(not (= (point) (point-max))))
	      (forward-line)
	      (setq lines-forward (+ lines-forward 1))
	      (setq line (get-current-line)))
	    ;; heading-, location- or persons-style
	    (when (string= "" line)
	      (cond
	       ((= lines-forward 1)
		(if (string= (get-next-line -2) "")
		    persons-standalone-style
		  persons-style))
	       ((= lines-forward 2)
		location-style))))))))))

;;; --------------------------------------------------------
;;;
(defun display-selected-style ()
  "Displays selected style in minibuffer."
  (message (concat "selected style: " (get-style (get-current-line)))))

;;; ---------------------------------------------------------
;;;
(defun post-command-hook-function ()
  "Handles post-command-hook."
  (when
      (or
       (eq this-command 'next-line)
       (eq this-command 'previous-line)
       (eq this-command 'backward-char)
       (eq this-command 'forward-char)
       (eq this-command 'move-end-of-line)
       (eq this-command 'move-beginning-of-line)
       (eq this-command 'beginning-of-buffer)
       (eq this-command 'end-of-buffer)
       (eq this-command 'right-char)
       (eq this-command 'left-char)
       (eq this-command 'scroll-up-command)
       (eq this-command 'scroll-down-command)
       (eq this-command 'undo))
    (initialize-template)
    (when display-selected-style
      (display-selected-style))))

;;; ---------------------------------------------------------
;;;
(defun set-up-windows ()
  "Sets up buffers and windows."
  (with-current-buffer (get-buffer-create (current-buffer))
    (split-window nil 20 'left)
    (switch-to-buffer (get-buffer-create "epic-templates"))
    (other-window 1)
    (split-window nil 40 'right)
    (switch-to-buffer (get-buffer-create "epic-navigator"))
    (other-window 1)))

;;; ---------------------------------------------------------
;;;
(defun get-epos-configuration-file-name ()
  "Returns the name of the configuration file."
  (interactive)
  (concat (file-name-as-directory (file-name-directory buffer-file-truename)) "." (file-name-sans-extension (file-name-nondirectory buffer-file-truename))))

;;; ---------------------------------------------------------
;;;
(defun load-book-information ()
  "Initializes book-information from config-file."
  (setq epic-configuration-file (get-epos-configuration-file-name))
  (when (file-exists-p epic-configuration-file)
    (load-file epic-configuration-file)))

;;; ---------------------------------------------------------
;;;
(define-minor-mode epic-mode
  "Mode for writing epic stories."
  :lighter " epic-mode"
  :keymap '(((kbd [127]) . (lambda () (interactive) (on-backspace)))
	    ((kbd "\r") .  (lambda () (interactive) (on-enter))))
  (initialize)
  (load-book-information)
  (setq font-lock-defaults '(epic-mode-keywords))
  (font-lock-mode 1)
  ;; for text-input
  (add-hook 'post-self-insert-hook 'post-self-insert-hook-function t t)
  ;; for navigate-input
  (add-hook 'post-command-hook 'post-command-hook-function t t))

;;; ---------------------------------------------------------
;;;
(define-minor-mode plot-mode
  "Mode for plot of an episoda."
  ;;
  ;; Disable distractions
  ;;
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  (setq truncate-lines nil)
  ;;
  ;; hiding mode-line completely aligns last line to the left (bug?).
  ;; Simply setting back- and foreground to black creates an empty black line on the bottom.
  ;;
  (setq mode-line-format "")
  (set-face-foreground 'mode-line          background-color)
  (set-face-background 'mode-line          background-color)
  (set-face-background 'mode-line-inactive background-color)
  
  (set-face-background 'default            background-color)
  (set-face-foreground 'default            foreground-color)
  (add-hook 'window-configuration-change-hook (lambda () (set-window-margins (car (get-buffer-window-list (current-buffer) nil t)) 0 0))))


(setq auto-mode-alist (append '(("\.epos$" . epic-mode)
				("\.epic$" . epic-mode)
				("\.plot$" . plot-mode))
	      auto-mode-alist))

(load "epic-epub")
(load "epic-odt")
(load "epic-html")
(load "epic-pdf")
(load "epic-mobi")
(load "epic-general-functions")
(load "epic-import")
(load "epic-createspace.el")

;;;; epic-mode.el ends here
