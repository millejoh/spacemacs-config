(defun spacemacs//sly-helm-source (&optional table)
  (or table (setq table sly-lisp-implementations))
  `((name . "Sly")
    (candidates . ,(mapcar #'car table))
    (action . (lambda (candidate)
                (car (helm-marked-candidates))))))

(defun spacemacs/helm-sly ()
  (interactive)
  (let ((command (helm :sources (spacemacs//sly-helm-source))))
    (and command (sly (intern command)))))

(when (configuration-layer/package-usedp 'sly)
  (spacemacs|define-transient-state common-lisp-navigation
    :title "Common Lisp Navigation Transient State"
    :doc "

^^Definitions                           ^^Compiler Notes             ^^Stickers
^^^^^^─────────────────────────────────────────────────────────────────────────────────────
[_g_] Jump to definition                [_n_] Next compiler note     [_s_] Next sticker
[_G_] Jump to definition (other window) [_N_] Previous compiler note [_S_] Previous sticker
[_b_] Pop from definition

[_q_] Exit
"
  :foreign-keys run
  :bindings
  ("g" sly-edit-definition)
  ("G" sly-edit-definition-other-window)
  ("b" sly-pop-find-definition-stack)
  ("n" sly-next-note)
  ("N" sly-previous-note)
  ("s" sly-stickers-next-sticker)
  ("S" sly-stickers-prev-sticker)
  ("q" nil :exit t)))
