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
  '(google-c-style)
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

(defun modes/post-init-google-c-style ()
  (use-package google-c-style
    (progn
      (defun my-c-mode-common-hook()
        (setq tab-width 4 indent-tabs-mode nil)
        (setq c-basic-offset 4)
        (c-set-offset 'case-label '0) ; case label
        (c-set-offset 'inextern-lang '0) ; extern block
      ;  (c-set-offset 'inline-open 0)
      ;  (c-set-offset 'friend '-)
      ;  (c-set-offset 'substatement-open 0)
        
        ;; hungry-delete and auto-newline
        (c-toggle-hungry-state 1)
        (hs-minor-mode)
        ;; show trial whitespace
        (setq show-trailing-whitespace t)
        ;; comment multi-line start with *
        (setq comment-multi-line t)

        ;; auto load which funtion mode
        (which-func-mode t)
      )
      (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
      (add-hook 'c-mode-common-hook 'google-set-c-style)
      (add-hook 'c-mode-common-hook 'google-make-newline-indent)
      
      (add-to-list 'auto-mode-alist '("\\.c\\'" . c-mode))
      (add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))

      (defun modes//my-c++-mode-hook()
        (c-set-offset 'access-label '-) ; public/private label
        (setq tab-width 2 indent-tabs-mode nil)
        (setq c-basic-offset 2)
        (c-set-offset 'inextern-lang '2) ; extern block
        ;(c-set-style "bsd")
        ;; (define-key c++-mode-map [f3] 'replace-regexp)
      
        ;; auto load which funtion mode
        (which-func-mode t)
      )
      (add-hook 'c++-mode-hook 'modes//my-c++-mode-hook)
      
      (add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
      (add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
      (add-to-list 'auto-mode-alist '("\\.H\\'" . c++-mode))
      (add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
      ; for RT project
      (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

)))

