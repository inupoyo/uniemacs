;;; nakama.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 OPASO
;;
;; Author: OPASO <ryu@OPASO>
;; Maintainer: OPASO <ryu@OPASO>
;; Created: 10月 07, 2024
;; Modified: 10月 07, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/ryu/nakama
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


(defun nakama-calsed ()
  "編集中のCGIやAJAXに対する仲間HTMLの中で ###はめられる文字###を探して、CALSEDをする。"
  (interactive)
  (let ((hamaru-list (remove "" (nakama-hamaru-list))))
    (u-popup-buffer
     (string-join
      (cl-loop for s in
               (-zip-pair
                hamaru-list
                (mapcar (lambda (s)
                          (when (string-match "###\\(.*\\)###" s)
                            (downcase (match-string 1 s)))
                          )
                        hamaru-list))
               collect (format "calsed '%s' $%s |\n"
                               (car s) (cdr s))))
     "*nakama-calsed*")))

(defun nakama-buffers ()
    (interactive)
    (let* ((conf (load-server-setting (check-current-host)))
           (appdir (cadr (assoc "appdir"
                                (cl-getf conf :database))))
           (remote-path (replace-regexp-in-string
                         "/$"
                         ""
                         (cadr (assoc "front"
                                (cl-getf conf :doorway)))))
           (nakama-buffer-list (mapcar
                                (lambda (ext)
                                  (list
                                   (concat
                                    (file-name-sans-extension (buffer-name))
                                    "."
                                    ext)
                                   ext))
                                '("AJAX" "CGI" "HTML"))))
      (ivy-read "nakama buffer > " nakama-buffer-list
                :action (lambda (choice)
                          (let ((parent (cadr choice))
                                (fname (car choice)))
                            (if (and
                                 (string-match-p "os" (check-current-host))
                                 (equal "HTML" parent))
                                (find-file-other-window ({f} "{remote-path}{appdir}/{parent}/JP/{fname}"))
                                (find-file-other-window ({f} "{remote-path}{appdir}/{parent}/{fname}")))
                            ))
                )))

(defun nakama-buffer-text (&optional all)
    (let* ((conf (load-server-setting (check-current-host)))
           (appdir (cadr (assoc "appdir"
                                (cl-getf conf :database))))
           (remote-path (replace-regexp-in-string
                         "/$"
                         ""
                         (cadr (assoc "front"
                                (cl-getf conf :doorway)))))
           (nakama-buffer-list (mapcar
                                (lambda (ext)
                                  (list
                                   (concat
                                    (file-name-sans-extension (buffer-name))
                                    "."
                                    ext)
                                   ext))
                                '("AJAX" "CGI" "HTML")))
           result)
      (ivy-read "info from > " nakama-buffer-list
                :action (lambda (choice)
                          (let ((parent (cadr choice))
                                (fname (car choice)))
                            (setq result
                                  (f-read ({f} "{remote-path}{appdir}/{parent}/{fname}")))))
                )
      result))

(cl-defmacro with-nakama-text (&body body)
  "仲間ファイルの選択、バッファ内容の取得のアナフォラ"
  `(let ((nakama-buffer-text (nakama-buffer-text)))
        ,@body))

(defun strip-hameru-mark (str)
  (replace-regexp-in-string
   "#" "" str))

(defun nakama-hamaru-list ()
  (with-nakama-text
      (grepped-list
              nakama-buffer-text
              "###[^#]*###")))

(defun u-popup-buffer (content bufname)
  "文字列を新しいバッファにいれて出す"
  (let ((buf (get-buffer-create bufname)))
    (with-current-buffer buf
      (erase-buffer)
      (insert content))
    (display-buffer buf)))

(defun nakama-variable ()
  (interactive)
  (with-nakama-text
   (let* ((name-raw (remove ""
                            (mapcar (lambda (s)
                              (shell-command-to-string
                               ({f} "echo {s} | sed 's/name=//g' | tr -d '\n'")))
                            (grepped-list
                             nakama-buffer-text
                             (format "name=\"[^[:space:]]*\"")))))
          (name-down (mapcar #'downcase name-raw)))
     (u-popup-buffer
      (string-join
      (cl-loop for name-pair in (-zip-pair name-raw name-down)
              collect (format "%s=$(nameread %s $tmp-name)"
                              (cdr name-pair)
                              (car name-pair)))
      "\n")
      "*nameread-vars*"))))

(defun switch-to-buffer-other-frame-by-name (buffer-name)
  (select-window
   (car
    (remove-if #'null
               (cl-loop for w in (window-list)
                        collect (when (string-match-p
                                       buffer-name
                                       (buffer-name (window-buffer w)))
                                  w))))))

(defun nakama-profile ()
  (interactive)
  (u-popup-buffer
  "* test
inu
* test2
inunu"
  "profile.org"
   )
  (switch-to-buffer-other-frame-by-name "profile.org")
  (org-mode))

;; リンクとか PATH(兄弟)をあつめるやつ。
(defun nakama-kyodai ()
    (interactive)
    (print "foo"))

(defun nakama-stylesheet ()
    (interactive)
    (find-file
     (concat
      (server-conf-get-value :doorway "front")
      (--> (load-server-setting (check-current-host))
           (cl-getf it :database)
           (assoc "stylesheet" it)
           (second it)
           ))))

(defun go-to-userver-conf ()
    (interactive)
    (find-file "~/.doom.d/servers"))

(defun insert-layout ()
  (let ((layoutdir (server-conf-get-value :database "layout")))
    (list-app-on-server)
    ))

(defun swipe-midashi ()
    (interactive)
    (swiper-isearch "#[ ]?-+\\n#.*\\\n#"))

(provide 'nakama)
;;; nakama.el ends here
