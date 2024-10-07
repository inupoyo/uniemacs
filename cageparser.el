;;; cageparser.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 OPASO
;;
;; Author: OPASO <ryu@OPASO>
;; Maintainer: OPASO <ryu@OPASO>
;; Created: 10月 07, 2024
;; Modified: 10月 07, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/ryu/cageparser
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


;;; parser.el -*- lexical-binding: t; -*-

;; TOML
(require 'cl)
(require 'f)

(defun load-server-setting (sname)
 (toml2sexp
  (f-read (format "~/.doom.d/servers/%s" sname))))


(defun clsr (acm)
  (lambda (x)
    (setf acm (+ x acm))
    acm))

(let ((a (clsr 1)))
  (funcall a 3)
  (funcall a 3)
  )


(defun remove-space (str)
  (replace-regexp-in-string " " "" str))

(defun remove-wquote (str)
  (replace-regexp-in-string "\"" "" str))

(defun remove-rect-bracket (str)
  (replace-regexp-in-string "\\[\\|\\]$" "" str))

;; TOMLをplistにする。
(defun toml2sexp (str)
  (let ((line-lst (remove "" (split-string str "\n"))))
    (cl-loop for l in line-lst
             with head = nil
             with cn = '()
             when (string-match-p "^\\[.*\\]" l) do
             (progn
               (setf head (intern
                           (format ":%s"
                                   (remove-rect-bracket l))))
                  (push '() cn) (push head cn))
             else do (push (split-string (-> l
                                             remove-space
                                             remove-wquote)
                                         "=")
                           (cl-getf cn head))
             finally (return cn))))


;; AWK

(provide 'cageparser)
;;; cageparser.el ends here
