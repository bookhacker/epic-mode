(defconst createspace-template-file-name                   "epos_CreateSpace - 5.06 x 7.81.ott")

;;; ---------------------------------------------------------
;;;
(defun create-createspace-header ()
  "Inserts odt-header."
  (insert
   (concat "#+ODT_STYLES_FILE: \"" (file-name-as-directory library-directory) (file-name-as-directory "templates") createspace-template-file-name "\"

#+TITLE: 
#+DATE:
#+AUTHOR:
#+OPTIONS: ':nil *:t -:t ::t <:t H:3 \\n:nil ^:t arch:headline
#+OPTIONS: title:nil author:nil c:nil creator:comment d:(not LOGBOOK) date:nil e:t
#+OPTIONS: email:nil f:t inline:t num:t p:nil pri:nil stat:t tags:t
#+OPTIONS: tasks:t tex:t timestamp:t toc:t todo:t |:t
#+CREATOR: 
#+DESCRIPTION:
#+EXCLUDE_TAGS: noexport
#+KEYWORDS:
#+LANGUAGE: de
#+SELECT_TAGS: export"))(newline)
(newline))

;;; ---------------------------------------------------------
;;;
(defun createspace-create ()
  "Exports text to org-format for further converting."
  (interactive)
  (setq odt-filename (concat buffer-file-truename "_CreateSpace.odt"))
  (setq unzip-directory (file-name-directory buffer-file-truename))
  (setq meta-xml-file (concat (file-name-as-directory unzip-directory) "meta.xml"))
  (with-current-buffer (get-buffer-create org-buffer-name)
    (create-createspace-header)
    (begin-odt-document))
  (iterate-odt-buffer)
  (setq org-filename (concat buffer-file-truename "_CreateSpace.org"))
  (with-current-buffer (get-buffer-create org-buffer-name)
    (end-odt-document)
    (write-file org-filename nil)
    (kill-buffer (current-buffer)))
  (setq org-buffer (find-file org-filename))
  (with-current-buffer (get-buffer-create org-buffer)
    (org-odt-export-to-odt)
    (kill-buffer (current-buffer)))
  (uncompress-odt-file)
  (edit-meta-xml)
  (compress-odt-file)  
  (message "Finished."))
