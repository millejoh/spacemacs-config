(defconst common-lisp-sly-packages
  '((common-lisp-snippets :requires yasnippet)
    xterm-color
    popwin
    helm
    evil
    evil-cleverparens
    parinfer
    smartparens
    company
    (sly :requires smartparens)
    (sly-mrepl :requires sly :location built-in)
    (sly-macrostep :requires (sly macrostep))
    (sly-repl-ansi-color :requires sly)))

(defun common-lisp-sly/pre-init-xterm-color ()
  (when (configuration-layer/package-usedp 'sly)
    (add-hook 'sly-mrepl-mode-hook (lambda () (setq xterm-color-preserve-properties t)))))

(defun common-lisp-sly/pre-init-popwin ()
  (spacemacs|use-package-add-hook sly
    :post-config
    (push '("*sly-description*" :width 0.5 :position right)
          popwin:special-display-config)
    (push '("*sly-macroexpansion*" :width 0.5 :position right)
          popwin:special-display-config)))

(defun common-lisp-sly/init-common-lisp-snippets ())

(defun common-lisp-sly/pre-init-helm ()
  (spacemacs|use-package-add-hook sly
    :post-init
    (spacemacs/set-leader-keys-for-major-mode 'lisp-mode
      "sI" 'spacemacs/helm-sly)))

(defun common-lisp-sly/pre-init-evil-cleverparens ()
  (add-to-list 'evil-lisp-safe-structural-editing-modes 'lisp-mode)
  (add-to-list 'evil-lisp-safe-structural-editing-modes 'common-lisp-mode)
  (spacemacs|use-package-add-hook evil-cleverparens
    :post-init
    (spacemacs/toggle-evil-safe-lisp-structural-editing-on-register-hooks)
    :post-config
    (setq evil-move-beyond-eol t
          evil-cleverparens-use-additional-movement-keys nil
          evil-cleverparens-use-additional-bindings nil)))

(defun common-lisp-sly/post-init-parinfer ()
  (add-hook 'lisp-mode-hook 'parinfer-mode))

(defun common-lisp-sly/pre-init-evil ()
  (with-eval-after-load 'evil
    (when (configuration-layer/package-used-p 'sly)
      (evil-set-initial-state 'sly-mrepl-mode 'insert)
      (evil-set-initial-state 'sly-inspector-mode 'emacs)
      (evil-set-initial-state 'sly-db-mode 'emacs)
      (evil-set-initial-state 'sly-xref-mode 'emacs)
      (evil-set-initial-state 'sly-stickers--replay-mode 'emacs))))

(defun common-lisp-sly/pre-init-smartparens ()
  (with-eval-after-load 'smartparens
    (when (configuration-layer/package-used-p 'sly)
      (sp-local-pair '(sly-mrepl-mode) "'" "'" :actions nil)
      (sp-local-pair '(sly-mrepl-mode) "`" "`" :actions nil))))

(defun common-lisp-sly/init-sly ()
  (use-package sly
    :defer t
    :init
    ;; (require 'sly)
    (spacemacs/register-repl 'sly 'sly)
    (setq sly-autodoc-use-multiline t
          ;; sly-complete-symbol*-fancy t
          sly-kill-without-query-p t
          sly-repl-history-remove-duplicates t
          sly-repl-history-trim-whitespaces t
          sly-net-coding-system 'utf-8-unix)
    ;; (sly-setup '(sly-fancy))
    (spacemacs/set-leader-keys-for-major-mode 'lisp-mode
      "'" 'sly
      "ha" 'sly-apropos
      "hb" 'sly-who-binds
      "hd" 'sly-disassemble-symbol
      "hh" 'sly-describe-symbol
      "hH" 'sly-hyperspec-lookup
      "hm" 'sly-who-macroexpands
      "hp" 'sly-apropos-package
      "hr" 'sly-who-references
      "hs" 'sly-who-specializes
      "hS" 'sly-who-sets
      "h<" 'sly-who-calls
      "h>" 'sly-calls-who
      "cc" 'sly-compile-file
      "cC" 'sly-compile-and-load-file
      "cf" 'sly-compile-defun
      "cl" 'sly-load-file
      "cn" 'sly-remove-notes
      "cr" 'sly-compile-region
      "eb" 'sly-eval-buffer
      "ee" 'sly-eval-last-expression
      "eE" 'sly-eval-print-last-expression
      "ef" 'sly-eval-defun
      "eF" 'slime-undefine-function
      "er" 'sly-eval-region
      "g" 'spacemacs/common-lisp-navigation-transient-state/body
      "me" 'sly-macroexpand-1
      "mE" 'sly-macroexpand-all
      "sc" 'sly-mrepl-clear-repl
      "si" 'sly
      "sq" 'sly-quit-lisp
      "sr" 'sly-restart-inferior-lisp
      "ss" 'sly-mrepl-sync
      "Sb" 'sly-stickers-toggle-break-on-stickers
      "Sc" 'sly-stickers-clear-defun-stickers
      "SC" 'sly-stickers-clear-buffer-stickers
      "Sf" 'sly-stickers-fetch
      "Sr" 'sly-stickers-replay
      "Ss" 'sly-stickers-dwim
      "tt" 'sly-toggle-trace-fdefinition
      "tT" 'sly-toggle-fancy-trace
      "tu" 'sly-untrace-all)
    (mapc (lambda (x)
            (spacemacs/declare-prefix-for-mode 'lisp-mode (car x) (cdr x)))
          '(("mc" . "compile")
            ("me" . "evaluate")
            ("mg" . "navigation")
            ("mh" . "help")
            ("mm" . "macro")
            ("ms" . "repl")
            ("mS" . "stickers")
            ("mt" . "trace")))))

(defun common-lisp-sly/post-init-company()
  (spacemacs|add-company-backends
    :backends company-capf
    :modes sly-mode sly-mrepl-mode))

(defun common-lisp-sly/init-sly-mrepl ()
  (use-package sly-mrepl
    :after sly
    :bind
    (:map sly-mrepl-mode-map
          ("<up>" . sly-mrepl-previous-input-or-button)
          ("<down>" . sly-mrepl-next-input-or-button)
          ("<C-up>" . sly-mrepl-previous-input-or-button)
          ("<C-down>" . sly-mrepl-next-input-or-button))))


(defun common-lisp-sly/init-sly-macrostep ()
  (use-package sly-macrostep
    :after sly
    :config
    (when (configuration-layer/layer-usedp 'emacs-lisp)
      (spacemacs/set-leader-keys-for-major-mode 'lisp-mode
        "ms" 'spacemacs/macrostep-transient-state/body))))

(defun common-lisp-sly/init-sly-repl-ansi-color ()
  (use-package sly-repl-ansi-color
    :demand t
    :config (push 'sly-repl-ansi-color sly-contribs)))
