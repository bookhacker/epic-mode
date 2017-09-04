# epic-mode

This is epic-mode. An Emacs mode to write stunning epics in a screenplay like format.

## Installation
* Clone files (for example to ~/.emacs.d/epic-mode):

````
cd ~/.emacs.d

git clone https://github.com/bookhacker/epic-mode.git
````

* To load epic-mode when opening a file ending on `.epos` with side margins of 25 characters, add to your Emacs configuration file (for example in ~/.emacs.d/init.el):

````
;;
;; Epic mode
;;
(add-to-list 'load-path "~/.emacs.d/epic-mode")
(load "epic-mode")
(defvar epic-margin-left 25)
(defvar epic-margin-right 25)
````

## Usage

epic-mode writes in a screenplay like format and does different things to make your life easier (especially when inputing text), so you don't have to care about putting in spaces or Caps where epic-mode knows what you mean.

### What you need to know first

The general format when writing in epic-mode looks kinda like this:

````
* CHAPTER - Optional chapter subtitle
** Location.
*** Personae.

NAME (what he does).
  Text Text Text.
NAME. Text Text.
NAME. Text Text
  Text Text.
NAME (what she does).
  Text.

** Location.
*** Personae.

NAME. Text Text
  Text Text.
NAME (what he does).
  Text.

*** Personae.

NAME (what she does).
  Text Text.

* NEXT CHAPTER
** Location.
*** Personae.

NAME (what he does).
  Text.
````

As you see, some text is written in `CAPS`, there are parenthesis, two spaces at beginning of some lines, headings start with one, two or three asterisks. The good message: You don't have to care about this. epic-mode does the most for you automatically, which means, you have to know how inputing text works:

### Headings

### Paragraphs

### When starting with an empty file

When you start with an empty file, you HAVE to put in a single asterisk followed by a `SPACE`:

````

````
