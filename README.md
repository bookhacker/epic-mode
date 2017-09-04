# epic-mode

This is epic-mode. An Emacs mode to write stunning epics in a screenplay like format.

## Installation
* Clone files (for example to ~/.emacs.d/epic-mode):

````
cd ~/.emacs.d

git clone https://github.com/bookhacker/epic-mode.git
````

* To load epic-mode when opening a file ending on `.epos` with side margins of 25 characters (for example in ~/.emacs.d/init.el):

````
;;
;; Epic mode
;;
(add-to-list 'load-path "~/.emacs.d/epic-mode")
(load "epic-mode")
(defvar epic-margin-left 25)
(defvar epic-margin-right 25)
````
