(defconst content-opf-buffer-name           "content-opf-buffer")
(defconst toc-ncx-buffer-name               "toc-ncx-buffer")
(defconst opf-book-id                       "BookId")
(defconst opf-cover-id                      "cover-image")
(defvar   epub-meta-inf-directory           "")
(defvar   epub-oebps-directory              "")
(defvar   epub-oebps-content-directory      "")
(defvar   epub-oebps-css-directory          "")
(defvar   epub-file-name                    "")
(defvar   epub-delete-files-and-directories t)
(defvar   epub-has-second-level             nil "Defines if current first level heading has second-level.")
(defvar   epub-is-new-second-level          nil "Defines, if we are already inside a second level.")
(defvar   navpoint-index                    1)
(defvar   is-first-call                     t   "Defines, if insert-into-toc-ncx-first-level is called for first time.")

;;; --------------------------------------------------------
;;;
(defun epub-delete-files-and-directories ()
  "Deletes temporay files and directories."
  (delete-directory epub-meta-inf-directory t nil)
  (delete-directory epub-oebps-directory    t nil)
  (delete-file "mimetype"))

;;; --------------------------------------------------------
;;;
(defun epub-create ()
  "Creates an .epub from file."
  (interactive)
  (message "Creating .epub ...")
  (if (eq isbn nil)
      (setq isbn isbn-epub))
  (setq is-first-call t)
  (epub-create-directories)
  (epub-create-toc-ncx)
  (create-html epub-oebps-content-directory)
  (epub-close-toc-ncx)
  (epub-create-files)
  (epub-compress-files)
  (when epub-delete-files-and-directories
    (epub-delete-files-and-directories))
  (epub-validate epub-file-name)
  (setq isbn nil)
  (message ".epub created."))

;;; --------------------------------------------------------
;;;
(defun debug-epub-create ()
  (interactive)
  (epub-create))

;;; --------------------------------------------------------
;;;
(defun epub-validate (file-name)
  "Validates .epub-file."
  (setq validate-result (shell-command-to-string (concat "epubcheck -quiet " file-name)))
  (if (string= validate-result "")
      t
    nil))

;;; --------------------------------------------------------
;;;
(defun epub-create-directories ()
  "Creates epub directories."
  ;; ./META-INF
  (setq epub-meta-inf-directory (file-name-as-directory "META-INF"))
  (unless (file-exists-p epub-meta-inf-directory)
    (make-directory epub-meta-inf-directory))
  ;; ./OEBPS
  (setq epub-oebps-directory (file-name-as-directory "OEBPS"))
  (unless (file-exists-p epub-oebps-directory)
    (make-directory epub-oebps-directory))
  ;; ./OEBPS/content
  (setq epub-oebps-content-directory (concat (file-name-as-directory epub-oebps-directory)(file-name-as-directory "content")))
  (unless (file-exists-p epub-oebps-content-directory)
    (make-directory epub-oebps-content-directory))
  ;;./OEBPS/css
  (setq epub-oebps-css-directory (concat (file-name-as-directory epub-oebps-directory)(file-name-as-directory "css")))
  (unless (file-exists-p epub-oebps-css-directory)
    (make-directory epub-oebps-css-directory)))

;;; --------------------------------------------------------
;;;
(defun epub-create-mimetype-file ()
  "Creates mimetype-file."
  (setq file-name "mimetype")
  (setq source-file (concat (file-name-as-directory library-directory) file-name))
  (setq target-file file-name)
  (copy-file source-file target-file t))

;;; --------------------------------------------------------
;;;
(defun epub-create-container-xml-file ()
  "Creates container.xml in directory META-INF."  
  (setq file-name "container.xml")
  (setq source-file (concat (file-name-as-directory library-directory) (file-name-as-directory "META-INF") file-name))
  (setq target-file (concat (file-name-as-directory epub-meta-inf-directory) file-name))
  (copy-file source-file target-file t))

;;; --------------------------------------------------------
;;;
(defun epub-create-content-opf ()
  "Creates content.opf in directory OEBPS."  
  (setq filename "content.opf")
  (with-current-buffer (get-buffer-create content-opf-buffer-name)
    (insert (concat "<?xml version=\"1.0\" encoding=\"utf-8\" ?>
<package version=\"2.0\" xmlns=\"http://www.idpf.org/2007/opf\" unique-identifier=\"" opf-book-id "\">
  <metadata xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:opf=\"http://www.idpf.org/2007/opf\">
    <dc:title>" (get-title) "</dc:title>
    <dc:language>" opf-language "</dc:language>
    <dc:identifier id=\"" opf-book-id "\" opf:scheme=\"uuid\">urn:uuid:" guid "</dc:identifier>
    <dc:identifier id=\"ISBN\">" isbn "</dc:identifier>
    <dc:creator opf:role=\"aut\">" author "</dc:creator>
    <dc:publisher>" publisher "</dc:publisher>
    <dc:date>" date "</dc:date>
    <meta name=\"cover\" content=\"" opf-cover-id "\" />
    <dc:description>" opf-description "</dc:description>
    <dc:subject>" opf-subject1 "</dc:subject>
    <dc:subject>" opf-subject2 "</dc:subject>
    <dc:subject>" opf-subject3 "</dc:subject>
    <dc:rights>" opf-rights "</dc:rights>
    <dc:type>Text</dc:type>
    <dc:source>" opf-source "</dc:source>
    <dc:relation>" opf-relation "</dc:relation>
    <dc:coverage>Worldwide</dc:coverage>
  </metadata>
  <manifest>
    <item href=\"toc.ncx\" id=\"ncx\" media-type=\"application/x-dtbncx+xml\" />\n"))
    (insert "    <item href=\"content/title.html\" id=\"title.html\" media-type=\"application/xhtml+xml\" />\n")
    (insert "    <item href=\"content/toc.html\" id=\"htmltoc\" media-type=\"application/xhtml+xml\" />\n")
    (insert "    <item href=\"content/personae.html\" id=\"personae.html\" media-type=\"application/xhtml+xml\" />\n")
    ;; insert manifest entries
    (setq number-of-html-chapter-files (- html-file-index 1))
    (setq chapter-index 1)
    (while (<= chapter-index number-of-html-chapter-files)
      (setq file-name (concat (file-name-as-directory "content") (number-to-string chapter-index) ".html"))
      (setq file-id (concat "htmlcontent" (number-to-string chapter-index)))
      (insert (concat "    <item href=\"" file-name "\" id=\"" file-id "\" media-type=\"application/xhtml+xml\" />\n"))
      (setq chapter-index (+ chapter-index 1)))
    ;; #bbrinkmann 07.10.2015: Only if footnotes file exists
    (when (and (not (eq html-footnotes-file-path ""))
	       (file-exists-p html-footnotes-file-path))
      (insert "    <item href=\"content/footnotes.html\" id=\"footnotes.html\" media-type=\"application/xhtml+xml\" />\n"))
    (insert "    <item href=\"content/autor.html\" id=\"autor.html\" media-type=\"application/xhtml+xml\" />\n")
    (insert "    <item href=\"content/impressum.html\" id=\"impressum.html\" media-type=\"application/xhtml+xml\" />\n")
    (when (file-exists-p werbung-file-name)
      (insert "    <item href=\"content/werbung.html\" id=\"werbung.html\" media-type=\"application/xhtml+xml\" />\n"))
    (insert "    <item href=\"css/styles.css\" id=\"cssstylesheet\" media-type=\"text/css\" />
  </manifest>
  <spine toc=\"ncx\">\n")
    (insert (concat "    <itemref idref=\"title.html\"/>\n"))
    (insert (concat "    <itemref idref=\"htmltoc\"/>\n"))
    (insert (concat "    <itemref idref=\"personae.html\"/>\n"))
    ;; insert spine entries
    (setq chapter-index 1)
    (while (<= chapter-index number-of-html-chapter-files)
      (insert (concat "    <itemref idref=\"htmlcontent" (number-to-string chapter-index) "\" />\n"))
      (setq chapter-index (+ chapter-index 1)))
    ;; #bbrinkmann 07.10.2015: Only if footnotes file exists
    (when (and (not (eq html-footnotes-file-path ""))
	       (file-exists-p html-footnotes-file-path))
      (insert (concat "    <itemref idref=\"footnotes.html\"/>\n")))
    (insert (concat "    <itemref idref=\"autor.html\"/>\n"))
    (insert (concat "    <itemref idref=\"impressum.html\"/>\n"))
    (when (file-exists-p werbung-file-name)
      (insert (concat "    <itemref idref=\"werbung.html\"/>\n")))
    (insert "  </spine>
  <guide>
    <!-- For Kindle, the eBook opens to this HTML file when the user first opens the eBook -->
    <reference href=\"content/toc.html\" title=\"Inhaltsverzeichnis\" type=\"toc\"/>
    <reference href=\"content/personae.html\" type=\"text\" title=\"PERSONAE\" />
  </guide>
</package>")
    (setq content-opf-file-name (concat (file-name-as-directory epub-oebps-directory) "content.opf"))
    (write-file content-opf-file-name nil)
    (kill-buffer (current-buffer))))

;;; --------------------------------------------------------
;;;
(defun epub-create-toc-ncx ()
  "Creates toc.ncx."
  (with-current-buffer (get-buffer-create toc-ncx-buffer-name)
    (insert (concat "<?xml version=\"1.0\" encoding=\"utf-8\" ?>
<!DOCTYPE ncx PUBLIC \"-//NISO//DTD ncx 2005-1//EN\" \"http://www.daisy.org/z3986/2005/ncx-2005-1.dtd\">
<ncx xmlns=\"http://www.daisy.org/z3986/2005/ncx/\" version=\"2005-1\" xml:lang=\"en\">
  <head>
    <meta content=\"urn:uuid:" guid "\" name=\"dtb:uid\" /> 
    <meta content=\"1\" name=\"dtb:depth\" />    
    <meta content=\"0\" name=\"dtb:totalPageCount\" /> 
    <meta content=\"0\" name=\"dtb:maxPageNumber\" /> 
  </head>
  <docTitle>
    <text>" (get-title) "</text>
  </docTitle>
  <docAuthor>
    <text>" author "</text>
  </docAuthor>
  <navMap>\n"))
    ;; Titelseite
    (insert (concat "    <navPoint id=\"ncxcontent" (number-to-string navpoint-index) " \" playOrder=\"" (number-to-string navpoint-index) "\">
      <navLabel>
        <text>Titel</text>
      </navLabel>
      <content src=\"content/title.html\" />
    </navPoint>\n"))
    (setq navpoint-index (+ navpoint-index 1))
    ;; toc.html
    (insert (concat "    <navPoint id=\"ncxcontent" (number-to-string navpoint-index) " \" playOrder=\"" (number-to-string navpoint-index) "\">
      <navLabel>
        <text>Inhaltsverzeichnis</text>
      </navLabel>
      <content src=\"content/toc.html\" />
    </navPoint>\n"))
    (setq navpoint-index (+ navpoint-index 1))
    ;; PERSONAE
    (insert (concat "    <navPoint id=\"ncxcontent" (number-to-string navpoint-index) " \" playOrder=\"" (number-to-string navpoint-index) "\">
      <navLabel>
        <text>PERSONAE</text>
      </navLabel>
      <content src=\"content/personae.html\" />
    </navPoint>\n"))
    (setq navpoint-index (+ navpoint-index 1))))

;;; --------------------------------------------------------
;;;
(defun epub-close-toc-ncx ()
  "Closes toc.ncx."
  (with-current-buffer (get-buffer-create toc-ncx-buffer-name)
    ;; close last open navPoint
    (insert "    </navPoint>\n")
    ;; Footnotes
    ;; #bbrinkmann 07.10.2015 footnotes only if footnote-file exists
    (when (and (not (eq html-footnotes-file-path ""))
	       (file-exists-p html-footnotes-file-path))
      (insert (concat "    <navPoint id=\"ncxcontent" (number-to-string navpoint-index) " \" playOrder=\"" (number-to-string navpoint-index) "\">
      <navLabel>
        <text>Fußnoten</text>
      </navLabel>
      <content src=\"content/footnotes.html\" />
    </navPoint>\n"))
      (setq navpoint-index (+ navpoint-index 1)))
    ;; Autor
    (insert (concat "    <navPoint id=\"ncxcontent" (number-to-string navpoint-index) " \" playOrder=\"" (number-to-string navpoint-index) "\">
      <navLabel>
        <text>Autor</text>
      </navLabel>
      <content src=\"content/autor.html\" />
    </navPoint>\n"))
    (setq navpoint-index (+ navpoint-index 1))
    ;; Impressum
    (insert (concat "    <navPoint id=\"ncxcontent" (number-to-string navpoint-index) " \" playOrder=\"" (number-to-string navpoint-index) "\">
      <navLabel>
        <text>Impressum</text>
      </navLabel>
      <content src=\"content/impressum.html\" />
    </navPoint>\n"))
    (setq navpoint-index (+ navpoint-index 1))
    ;; close navpoints
    (insert "  </navMap>
</ncx>")
    (setq toc-ncx-file-name (concat (file-name-as-directory epub-oebps-directory) "toc.ncx"))
    (write-file toc-ncx-file-name nil)
    (kill-buffer (current-buffer))))

;;; ---------------------------------------------------------
;;;
(defun epub-insert-into-toc-ncx-first-level (current-heading-text current-html-file-index)
  "Inserts file information into toc.html."
  (unless (string= current-heading-text "")
    (with-current-buffer (get-buffer-create toc-ncx-buffer-name)
      (unless is-first-call
	(insert "    </navPoint>\n"))
      (setq is-first-call nil)
      (setq new-nav-point (concat "    <navPoint id=\"ncxcontent" (number-to-string navpoint-index) "\" playOrder=\"" (number-to-string navpoint-index) "\">\n      <navLabel>\n        <text>" (get-line-without-footnotes current-heading-text) "</text>\n      </navLabel>\n      <content src=\"content/" (number-to-string  current-html-file-index) ".html\" />\n"))
      (insert new-nav-point) ;; (newline)
      (setq navpoint-index (+ navpoint-index 1))
      (setq epub-is-new-second-level t)
      (setq epub-has-second-level nil))))

;;; ---------------------------------------------------------
;;;
(defun epub-insert-into-toc-ncx-second-level (current-heading-text current-html-file-index current-line-number)
  "Inserts file information into toc.ncx as second level."
  (unless (string= current-heading-text "")
    (with-current-buffer (get-buffer-create toc-ncx-buffer-name)
      (when epub-is-new-second-level
	(setq epub-is-new-second-level nil))
      (setq epub-has-second-level t)
      (if (eq last-style heading-style)
	  (setq new-nav-point (concat "      <navPoint id=\"ncxcontent" (number-to-string navpoint-index) "\" playOrder=\"" (number-to-string navpoint-index) "\">\n        <navLabel>\n          <text>" (get-line-without-footnotes current-heading-text) "</text>\n        </navLabel>\n        <content src=\"content/" (number-to-string current-html-file-index) ".html#" fnreturn-prefix (number-to-string (- current-line-number 1)) "\" />\n      </navPoint>"))
	(setq new-nav-point (concat "      <navPoint id=\"ncxcontent" (number-to-string navpoint-index) "\" playOrder=\"" (number-to-string navpoint-index) "\">\n        <navLabel>\n          <text>" (get-line-without-footnotes current-heading-text) "</text>\n        </navLabel>\n        <content src=\"content/" (number-to-string current-html-file-index) ".html#" fnreturn-prefix (number-to-string current-line-number) "\" />\n      </navPoint>")))
      (insert new-nav-point)(newline)
      (setq navpoint-index (+ navpoint-index 1)))))

;;; --------------------------------------------------------
;;;
(defun epub-create-style-css ()
  "Creates styles.css."
  (setq source-file (concat (file-name-as-directory library-directory) (file-name-as-directory "styles") "styles_mobi.css"))
  (setq target-file (concat (file-name-as-directory epub-oebps-css-directory) "styles.css"))
  (copy-file source-file target-file t))

;;; --------------------------------------------------------
;;;
(defun epub-create-files ()
  "Create epub files."
  ;; mimetype
  (epub-create-mimetype-file)
  ;; META-INF/container.xml
  (epub-create-container-xml-file)
  ;; OEBPS/content.opf
  (epub-create-content-opf)
  ;; OEBPS/css/styles.css
  (epub-create-style-css))

;;; --------------------------------------------------------
;;;
(defun epub-compress-files ()
  "Compresses all files to .epub."
  (setq epub-file-name (concat buffer-file-truename ".epub"))
  (setq zip-command (concat "zip -0Xq " epub-file-name " mimetype"))
  (when (file-exists-p "werbung.jpg")
    (setq zip-command (concat zip-command " werbung.jpg")))
  (setq zip-command (concat zip-command " && zip -Xr9Dq " epub-file-name " META-INF OEBPS"))
  (shell-command zip-command t))
