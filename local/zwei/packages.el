;;; packages.el --- ipython Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: John Miller
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq zwei-packages '(ein))

(defun zwei/init-ein ()
  (use-package ein
    :init
    (spacemacs/set-leader-keys "ayl" 'ein:login
      "ayr" 'ein:run
      "ays" 'ein:stop)
    (spacemacs/declare-prefix "ay" "ipython notebook")
    :config
    (mapc
     (lambda (mode)
       (evil-define-minor-mode-key
	       mode 'ein:notebook-mode
	       (kbd "<C-return>") 'ein:worksheet-execute-cell-km
	       (kbd "<S-return>") 'ein:worksheet-execute-cell-and-goto-next-km))
     '(insert hybrid normal))
    (with-eval-after-load 'ein-notebook
      (evil-define-key nil ein:notebooklist-mode-map "o" 'spacemacs/ace-buffer-links)
      (let ((bindings '(("j" ein:worksheet-goto-next-input-km)
                        ("k" ein:worksheet-goto-prev-input-km)
                        ("J" ein:worksheet-move-cell-down-km)
                        ("K" ein:worksheet-move-cell-up-km)
                        ("e" ein:worksheet-toggle-output-km)
                        ("d" ein:worksheet-kill-cell-km)
                        ("y" ein:worksheet-copy-cell-km)
                        ("p" ein:worksheet-yank-cell-km)
                        ("m" ein:worksheet-merge-cell-km)
                        ("s" ein:worksheet-split-cell-at-point-km)
                        ("o" ein:worksheet-insert-cell-below-km)
                        ("O" ein:worksheet-insert-cell-above-km)
                        ("t" ein:worksheet-toggle-cell-type-km)
                        ("C-m" ein:worksheet-execute-cell-km)
                        ("l" ein:worksheet-clear-output-km)
                        ("L" ein:worksheet-clear-all-output-km)
                        ("fs" ein:notebook-save-notebook-command-km)
                        ("fc" ein:notebook-reconnect-session-command-km)
                        ("fr" ein:notebook-restart-session-command-km)
                        ("C-r" ein:notebook-rename-command-km)
                        ("x" ein:notebook-close-km)
                        ("gg" ein:kernel-utils-jump-to-source)
                        ("hh" ein:kernel-utils-request-tooltip-or-help)
                        ("z" ein:notebook-kernel-interrupt-command-km))))
        (apply #'spacemacs/set-leader-keys-for-minor-mode
               (quote ein:notebook-mode)
               (cl-mapcan
                (lambda (bind)
                  (if (fboundp (cl-second bind))
                      bind
                    (prog1 nil
                      (display-warning
                       'warn (format "ipython-notebook/init-ein: undefined %s")))))
                (copy-tree bindings)))
        (eval (append '(spacemacs|define-transient-state
			                     ipython-notebook
			                   :title "iPython Notebook Transient State"
			                   :bindings
			                   ("q" nil :exit t))
                      bindings))))))

(defun zwei/post-init-ein ()
  (use-package ein-kernel-utils
    :load-path "c:/Users/E341194/Documents/github/ein-kernel-utils/lisp/"
    :config
    (require 'ein-kernel-completion)
    (require 'ein-buffer-connect)
    (add-hook 'ein:notebook-mode-hook #'(lambda () (add-to-list 'company-backends 'ein:company-backend)))
    (add-hook 'ein:notebook-mode-hook
              #'(lambda ()
                  (when (featurep 'eldoc)
                        (add-function :before-until (local 'eldoc-documentation-function)
                                      #'ein:completer--get-eldoc-signature)
                        (eldoc-mode))))
    (add-hook 'ein:on-kernel-connect-functions #'(lambda (kernel)
                                                   (ein:kernel-utils-load-safely kernel)))
    (add-hook 'org-src-mode-hook #'ein:on-edit-source-block)
    (add-hook 'hy-mode-hook
              (lambda ()
                (define-key python-mode-map "\C-c." 'ein:kernel-utils-jump-to-source)
                (define-key python-mode-map "\C-c\C-h" 'ein:kernel-utils-request-tooltip-or-help)
                (define-key python-mode-map "\C-c\C-c" 'ein:connect-run-or-eval-buffer)
                (define-key python-mode-map "\C-c\C-l" 'ein:connect-reload-buffer)
                (define-key python-mode-map "\C-c\C-r" 'ein:connect-eval-region)
                (define-key python-mode-map (kbd "C-:") 'ein:shared-output-eval-string)
                (define-key python-mode-map "\C-c\C-z" 'ein:connect-pop-to-notebook)
                (define-key python-mode-map "\C-c\C-x" 'ein:tb-show)
                (define-key python-mode-map (kbd "C-c C-/") 'ein:notebook-scratchsheet-open)))
    (add-hook 'python-mode-hook
              (lambda ()
                (define-key python-mode-map "\C-c." 'ein:kernel-utils-jump-to-source)
                (define-key python-mode-map "\C-c\C-h" 'ein:kernel-utils-request-tooltip-or-help)
                (define-key python-mode-map "\C-c\C-c" 'ein:connect-run-or-eval-buffer)
                (define-key python-mode-map "\C-c\C-l" 'ein:connect-reload-buffer)
                (define-key python-mode-map "\C-c\C-r" 'ein:connect-eval-region)
                (define-key python-mode-map (kbd "C-:") 'ein:shared-output-eval-string)
                (define-key python-mode-map "\C-c\C-z" 'ein:connect-pop-to-notebook)
                (define-key python-mode-map "\C-c\C-x" 'ein:tb-show)
                (define-key python-mode-map (kbd "C-c C-/") 'ein:notebook-scratchsheet-open)))))
