;;; packages.el --- ein-devel layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author:  <millejoh@mac.com>
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
;; added to `ein-devel-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `ein-devel/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `ein-devel/pre-init-PACKAGE' and/or
;;   `ein-devel/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst ein-devel-packages
  '((emacs-ipython-notebook :location local))
  "The list of Lisp packages required by the ein-devel layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

;; (defun ein-devel/post-init-company ()
;;   (spacemacs|add-company-backends
;;     :backends ein:company-backend
;;     :modes ein:notebook-mode))

(defun ein-devel/pre-init-emacs-ipython-notebook ()
  (add-to-list 'load-path "~/.emacs.d/private/ein-devel/local/emacs-ipython-notebook/lisp"))

(defun ein-devel/init-emacs-ipython-notebook ()
  (use-package ein
    :defer t
    :commands (ein:notebooklist-open ein:notebooklist-login ein:jupyter-server-start ein:jupyter-server-stop)
    :init
    (progn
      (require 'ein-jupyter)
      (spacemacs/set-leader-keys
        "ayl" 'ein:notebooklist-login
        "ayo" 'ein:notebooklist-open
        "ays" 'ein:jupyter-server-start
        "ayx" 'ein:jupyter-server-stop)
      (with-eval-after-load 'ein-notebooklist
        (evilified-state-evilify-map ein:notebooklist-mode-map
          :mode ein:notebooklist-mode
          :bindings
          (kbd "o") 'spacemacs/ace-buffer-links)
        (define-key ein:notebooklist-mode-map "o" 'spacemacs/ace-buffer-links)))
    :config
    (progn
      (defun spacemacs/ein:worksheet-merge-cell-next ()
        (interactive)
        (ein:worksheet-merge-cell (ein:worksheet--get-ws-or-error) (ein:worksheet-get-current-cell) t t))

      (defun spacemacs//concat-leader (key)
        (if dotspacemacs-major-mode-leader-key
            (concat dotspacemacs-major-mode-leader-key key)
          (concat "," key)))

      (spacemacs/set-leader-keys-for-major-mode 'ein:notebook-multilang-mode
        "y" 'ein:worksheet-copy-cell
        "p" 'ein:worksheet-yank-cell
        "d" 'ein:worksheet-kill-cell
        "h" 'ein:notebook-worksheet-open-prev-or-last
        "i" 'ein:worksheet-insert-cell-below
        "I" 'ein:worksheet-insert-cell-above
        "j" 'ein:worksheet-goto-next-input
        "k" 'ein:worksheet-goto-prev-input
        "l" 'ein:notebook-worksheet-open-next-or-first
        "H" 'ein:notebook-worksheet-move-prev
        "J" 'ein:worksheet-move-cell-down
        "K" 'ein:worksheet-move-cell-up
        "L" 'ein:notebook-worksheet-move-next
        "t" 'ein:worksheet-toggle-output
        "R" 'ein:worksheet-rename-sheet
        "RET" 'ein:worksheet-execute-cell-and-goto-next
        ;; Output
        "C-l" 'ein:worksheet-clear-output
        "C-S-l" 'ein:worksheet-clear-all-output
        ;;Console
        "C-o" 'ein:console-open
        ;; Merge cells
        "C-k" 'ein:worksheet-merge-cell
        "C-j" 'spacemacs/ein:worksheet-merge-cell-next
        "s" 'ein:worksheet-split-cell-at-point
        ;; Notebook
        "C-s" 'ein:notebook-save-notebook-command
        "C-r" 'ein:notebook-rename-command
        "1" 'ein:notebook-worksheet-open-1th
        "2" 'ein:notebook-worksheet-open-2th
        "3" 'ein:notebook-worksheet-open-3th
        "4" 'ein:notebook-worksheet-open-4th
        "5" 'ein:notebook-worksheet-open-5th
        "6" 'ein:notebook-worksheet-open-6th
        "7" 'ein:notebook-worksheet-open-7th
        "8" 'ein:notebook-worksheet-open-8th
        "9" 'ein:notebook-worksheet-open-last
        "+" 'ein:notebook-worksheet-insert-next
        "-" 'ein:notebook-worksheet-delete
        "x" 'ein:notebook-close
        "u" 'ein:worksheet-change-cell-type
        "fs" 'ein:notebook-save-notebook-command)

      ;; keybindings for ipython notebook traceback mode
      (spacemacs/set-leader-keys-for-major-mode 'ein:traceback-mode
        "RET" 'ein:tb-jump-to-source-at-point-command
        "n" 'ein:tb-next-item
        "p" 'ein:tb-prev-item
        "q" 'bury-buffer)

      ;; keybindings mirror ipython web interface behavior
      (evil-define-key 'insert ein:notebook-multilang-mode-map
        (kbd "<C-return>") 'ein:worksheet-execute-cell
        (kbd "<S-return>") 'ein:worksheet-execute-cell-and-goto-next)

      ;; keybindings mirror ipython web interface behavior
      (evil-define-key 'hybrid ein:notebook-multilang-mode-map
        (kbd "<C-return>") 'ein:worksheet-execute-cell
        (kbd "<S-return>") 'ein:worksheet-execute-cell-and-goto-next)

      (evil-define-key 'normal ein:notebook-multilang-mode-map
        ;; keybindings mirror ipython web interface behavior
        (kbd "<C-return>") 'ein:worksheet-execute-cell
        (kbd "<S-return>") 'ein:worksheet-execute-cell-and-goto-next
        "gj" 'ein:worksheet-goto-next-input
        "gk" 'ein:worksheet-goto-prev-input)

      ;; if this is not required then the following keygindings fail
      (require 'ein-multilang)
      (define-key ein:notebook-multilang-mode-map (kbd "M-j") 'ein:worksheet-move-cell-down)
      (define-key ein:notebook-multilang-mode-map (kbd "M-k") 'ein:worksheet-move-cell-up)

      (spacemacs|define-transient-state ein-devel
        :title "iPython Notebook Transient State"
        :doc "
 Operations on Cells^^^^^^            On Worksheets^^^^              Other
 ----------------------------^^^^^^   ------------------------^^^^   ----------------------------------^^^^
 [_k_/_j_]^^     select prev/next     [_h_/_l_]   select prev/next   [_t_]^^         toggle output
 [_K_/_J_]^^     move up/down         [_H_/_L_]   move left/right    [_C-l_/_C-S-l_] clear/clear all output
 [_C-k_/_C-j_]^^ merge above/below    [_1_.._9_]  open [1st..last]   [_C-o_]^^       open console
 [_O_/_o_]^^     insert above/below   [_+_/_-_]   create/delete      [_C-s_/_C-r_]   save/rename notebook
 [_y_/_p_/_d_]   copy/paste           ^^^^                           [_x_]^^         close notebook
 [_u_]^^^^       change type          ^^^^                           [_q_]^^         quit transient-state
 [_RET_]^^^^     execute"
        :evil-leader-for-mode (ein:notebook-multilang-mode . ".")
        :bindings
        ("q" nil :exit t)
        ("?" spacemacs//ein-devel-ms-toggle-doc)
        ("h" ein:notebook-worksheet-open-prev-or-last)
        ("j" ein:worksheet-goto-next-input)
        ("k" ein:worksheet-goto-prev-input)
        ("l" ein:notebook-worksheet-open-next-or-first)
        ("H" ein:notebook-worksheet-move-prev)
        ("J" ein:worksheet-move-cell-down)
        ("K" ein:worksheet-move-cell-up)
        ("L" ein:notebook-worksheet-move-next)
        ("t" ein:worksheet-toggle-output)
        ("d" ein:worksheet-kill-cell)
        ("R" ein:worksheet-rename-sheet)
        ("y" ein:worksheet-copy-cell)
        ("p" ein:worksheet-yank-cell)
        ("o" ein:worksheet-insert-cell-below)
        ("O" ein:worksheet-insert-cell-above)
        ("u" ein:worksheet-change-cell-type)
        ("RET" ein:worksheet-execute-cell-and-goto-next)
        ;; Output
        ("C-l" ein:worksheet-clear-output)
        ("C-S-l" ein:worksheet-clear-all-output)
        ;;Console
        ("C-o" ein:console-open)
        ;; Merge and split cells
        ("C-k" ein:worksheet-merge-cell)
        ("C-j" spacemacs/ein:worksheet-merge-cell-next)
        ("s" ein:worksheet-split-cell-at-point)
        ;; Notebook
        ("C-s" ein:notebook-save-notebook-command)
        ("C-r" ein:notebook-rename-command)
        ("1" ein:notebook-worksheet-open-1th)
        ("2" ein:notebook-worksheet-open-2th)
        ("3" ein:notebook-worksheet-open-3th)
        ("4" ein:notebook-worksheet-open-4th)
        ("5" ein:notebook-worksheet-open-5th)
        ("6" ein:notebook-worksheet-open-6th)
        ("7" ein:notebook-worksheet-open-7th)
        ("8" ein:notebook-worksheet-open-8th)
        ("9" ein:notebook-worksheet-open-last)
        ("+" ein:notebook-worksheet-insert-next)
        ("-" ein:notebook-worksheet-delete)
        ("x" ein:notebook-close))
      (spacemacs/set-leader-keys "ein" 'spacemacs/ein-devel-transient-state/body))))


;;; packages.el ends here
