;;; --------------------------------------------------------
;;;
(defun get-guid ()
  "Returns randomly generated GUID."
  (shell-command-to-string "uuidgen"))

;;; ---------------------------------------------------------
;;;
(defun is-lowercase-letter (ascii-code)
  "Checks if character is lowercase."
  ;; a-z 97-122
  ;; ä 228
  ;; ö 246
  ;; ü 252
  ;; ß 223
  (when
      (or
       (and (>= ascii-code 97)
	    (<= ascii-code 122))
       (= ascii-code 223)
       (= ascii-code 228)
       (= ascii-code 246)
       (= ascii-code 252))
    t))

;;; ---------------------------------------------------------
;;;
(defun is-uppercase-letter (ascii-code)
  "Checks if character is uppercase."
  ;; A-Z 65-90
  ;; Ä 196
  ;; Ö 214
  ;; Ü 220
  (when
      (or
       (and (>= ascii-code 65)
	    (<= ascii-code 90))
       (= ascii-code 196)
       (= ascii-code 214)
       (= ascii-code 220))
    t))
