;;; packages.el --- myxcscope layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: xoyo <xoyo@xoyo-mac>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `myxcscope-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `myxcscope/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `myxcscope/pre-init-PACKAGE' and/or
;;   `myxcscope/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst modes-packages
  '(my-buildin
    my-utils)
  "The list of Lisp packages required by the myxcscope layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun buildin/post-init-my-utils ()
  (use-package my-utils
    (progn
      ;; indent and complete style
      ;(global-set-key [(control tab)] 'my-indent-or-complete)
      ;; goto one line
      (global-set-key [(meta g)] 'goto-line)
      ;; Set up the delete key 
      (global-set-key [delete] 'delete-char)
      (global-set-key [kp-delete] 'delete-char)
      ;; refresh file
      (global-set-key "\C-\M-r" 'revert-buffer-no-confirm)
      (global-set-key "\C-c\C-r" 'revert-all-buffers)
      (global-set-key "\C-\M-g" 'compile)
      ;; kill current buffer
      (global-set-key "\C-xc" 'kill-this-buffer)
      ;; Mark regions
      (global-set-key (kbd "C-M-2") 'set-mark-command)
      ;(global-set-key "\C-2" 'set-mark)
      
      ;; find file at cursor point
      (global-set-key "\C-x\C-g" 'find-file-at-point)
      (global-set-key "\C-xg" 'find-function)
      
      ;; prefer re version
      (global-set-key "\M-%" 'query-replace-regexp)
      
      ;; comment or uncomment region
      (global-set-key (kbd "C-x /") 'comment-or-uncomment-region)
      
      ;; easy keys to split window. Key based on ErgoEmacs keybinding
      (global-set-key (kbd "M-2") 'split-window-vertically) ; split pane top/bottom
      (global-set-key (kbd "M-3") 'split-window-horizontally ) ; split pane side by side
      (global-set-key (kbd "M-s") 'other-window) ; cursor to other pane
      
      ;; for os whose alt-x is stoken
      (global-set-key [(meta n)] 'execute-extended-command)
      
      ;; use ibuffer over buffer
      (defalias 'list-buffers 'ibuffer)
      
      ;; quick buffer switch
      (global-set-key (kbd "<C-S-tab>") 'wg-previous-buffer)
      (global-set-key (kbd "<C-tab>") 'wg-next-buffer)
          
      ;; fix confliction with the shortcut of input mothod switch
      (global-set-key (kbd "M-SPC") 'set-mark-command)
       
      ;; jump backward/forward by a symbol instead of word
      ;; like foo-bar fooBar
      (global-set-key (kbd "M-b") 'backward-symbol)
      (global-set-key (kbd "M-f") 'forward-symbol) 
      
      ;; rectangle/vertical insert
      (global-set-key "\C-xri" 'string-insert-rectangle)
      
      ;; bind buffer to window 
      (global-set-key "\C-c\C-b" 'toggle-window-dedicated)
      
      ;; fix enter key in -nw mode
      ;(global-set-key (kbd "RET") [return])
      
      ;; open the coresponding include or src file
      (global-set-key "\C-c\C-f" 'open-include-or-src-file)
)))

