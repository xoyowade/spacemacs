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

(defconst myxcscope-packages
  '(evil-jumper
    helm-cscope
    xcscope)
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

(defun myxcscope/post-init-evil-jump ()
  (defadvice helm-cscope-find-this-symbol (before myxcscope/goto activate)
    (evil-jumper--push)))

(defun myxcscope/init-xcscope ()
  (use-package xcscope
    :commands (cscope-index-files myxcscope/run-pycscope)
    :init
    (progn
      (cscope-setup)
      ;; for python projects, we don't want xcscope to rebuild the databse,
      ;; because it uses cscope instead of pycscope
      ;(setq cscope-option-do-not-update-database t
      ;      cscope-display-cscope-buffer nil)

      (defun myxcscope//safe-project-root ()
        "Return project's root, or nil if not in a project."
        (and (fboundp 'projectile-project-root)
             (projectile-project-p)
             (projectile-project-root)))

      (defun myxcscope/run-pycscope (directory)
        (interactive (list (file-name-as-directory
                            (read-directory-name "Run pycscope in directory: "
                                                 (myxcscope//safe-project-root)))))
        (let ((default-directory directory))
          (shell-command
           (format "pycscope -R -f '%s'"
                   (expand-file-name "cscope.out" directory))))))))

(defun myxcscope/post-init-xcscope ()
  (progn
    (define-key cscope-list-entry-keymap (kbd "RET") 'cscope-select-entry-other-window)
  )
)
;(defun myxcscope/post-init-xcscope ()
;  (progn
;    (defvar cscope:map nil
;      "The cscope keymap.")
;    (if cscope:map
;        nil
;      (setq cscope:map (make-sparse-keymap))
;      ;; The following line corresponds to be beginning of the "Cscope" menu.
;      (define-key cscope:map "\C-css" 'cscope-find-this-symbol)
;      (define-key cscope:map "\C-csd" 'cscope-find-global-definition)
;      (define-key cscope:map "\C-csg" 'cscope-find-global-definition)
;      (define-key cscope:map "\C-csG" 'cscope-find-global-definition-no-prompting)
;      (define-key cscope:map "\C-csc" 'cscope-find-functions-calling-this-function)
;      (define-key cscope:map "\C-csC" 'cscope-find-called-functions)
;      (define-key cscope:map "\C-cst" 'cscope-find-this-text-string)
;      (define-key cscope:map "\C-cse" 'cscope-find-egrep-pattern)
;      (define-key cscope:map "\C-csf" 'cscope-find-this-file)
;      (define-key cscope:map "\C-csi" 'cscope-find-files-including-file)
;      ;; --- (The '---' indicates that this line corresponds to a menu separator.)
;      (define-key cscope:map "\C-csb" 'cscope-display-buffer)
;      (define-key cscope:map "\C-csB" 'cscope-display-buffer-toggle)
;      (define-key cscope:map "\C-csn" 'cscope-next-symbol)
;      (define-key cscope:map "\C-csN" 'cscope-next-file)
;      (define-key cscope:map "\C-csp" 'cscope-prev-symbol)
;      (define-key cscope:map "\C-csP" 'cscope-prev-file)
;      (define-key cscope:map "\C-csu" 'cscope-pop-mark)
;      ;; ---
;      (define-key cscope:map "\C-csa" 'cscope-set-initial-directory)
;      (define-key cscope:map "\C-csA" 'cscope-unset-initial-directory)
;      ;; ---
;      (define-key cscope:map "\C-csL" 'cscope-create-list-of-files-to-index)
;      (define-key cscope:map "\C-csI" 'cscope-index-files)
;      (define-key cscope:map "\C-csE" 'cscope-edit-list-of-files-to-index)
;      (define-key cscope:map "\C-csW" 'cscope-tell-user-about-directory)
;      (define-key cscope:map "\C-csS" 'cscope-tell-user-about-directory)
;      (define-key cscope:map "\C-csT" 'cscope-tell-user-about-directory)
;      (define-key cscope:map "\C-csD" 'cscope-dired-directory))
;    ;; The previous line corresponds to be end of the "Cscope" menu.
;  )
;)

;(defun myxcscope/init-helm-cscope ()
;  (use-package helm-cscope
;    :defer t
;    :init
;    (defun spacemacs/setup-helm-cscope (mode)
;      "Setup `helm-cscope' for MODE"
;      (spacemacs/set-leader-keys-for-major-mode mode
;        "gc" 'helm-cscope-find-called-function
;        "gC" 'helm-cscope-find-calling-this-funtcion
;        "gd" 'helm-cscope-find-global-definition
;        "ge" 'helm-cscope-find-egrep-pattern
;        "gf" 'helm-cscope-find-this-file
;        "gi" 'helm-cscope-find-files-including-file
;        "gs" 'helm-cscope-find-this-symbol
;        "gt" 'helm-cscope-find-this-text-string))))


;;; packages.el ends here
