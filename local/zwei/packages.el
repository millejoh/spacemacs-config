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

(defconst zwei-packages
  '(
    ;; ;; A local package
    ;; (some-package :location local)

    ;; A local package to be built with Quelpa
    ;; (ein-kernel-utils :location (recipe :fetcher github
    ;;                                     :repo "millejoh/ein-kernel-utils"))
    ))

(defun zwei/pre-init-ein-kernel-utils ()
)

(defun zwei/init-ein-kernel-utils ()
  (use-package ein-kernel-utils
    :config
    (add-to-list 'ein:on-kernel-connect-functions #'ein:enable-company-kernel-completion)))
