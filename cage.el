;;; cage.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 OPASO
;;
;; Author: OPASO <ryu@OPASO>
;; Maintainer: OPASO <ryu@OPASO>
;; Created: 10月 07, 2024
;; Modified: 10月 07, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/ryu/cage
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(load-file "~/uniemacs/cageparser.el")
(load-file "~/uniemacs/nakama.el")
(load-file "~/uniemacs/chromium.el")

;;-------------------------------------------------------------
;; 文字列操作
;;-------------------------------------------------------------
(defun collapse-space (str)
  "Returns string reducing the multiple spaces inside into one"
  (replace-regexp-in-string " +"  " " str))

(defun no-extra-wrap (str)
  "Returns string without last newline char"
  (declare (pure t) (side-effect-free t))
  (replace-regexp-in-string "\n$" "" str))

(defun no-newline (str)
  "Returns string without any newline char"
  (declare (pure t) (side-effect-free t))
  (replace-regexp-in-string "\n" "" str))

(defun no-space (str)
  "Returns string without any space"
  (declare (pure t) (side-effect-free t))
  (replace-regexp-in-string " " "" str))

(defun line-split (str) (split-string str "\n"))

;; least length of the each pipe lines.
(defun least-length-line (line)
  (collapse-space
   (replace-regexp-in-string " +|$" " |" line)))

;; 文字列の PRED
(defun piped-p (line) (string-match-p "[^|]+| ?$" line))
(defun backslashed-p (line) (string-match-p "[^\\]+[\\] ?$" line))
(defun redirected-p (line) (cl-search ">" line :test #'=))
(defun to-be-aligned-p (str) (or (piped-p str) (redirected-p str)))
(defun empty-line-p (line) (equal line "\n"))
(defun comment-p (line) (string-match "^#" line))
(defun porc-p () nil)
(defun slash-notation-p (notation) (string-match-p "[0-9]+/\(\([0-9]+\)|NF\)" notation ))
(defun layout-p (cmt) (string-match-p "^# ?[1-9]+:.*" cmt))
;; --------------------------------------------------------------
;; サーバ系
;; --------------------------------------------------------------
(defun check-current-server ()
  (intern (format ":%s"
                  (no-newline (shell-command-to-string "whoami")))))

(defun check-current-host ()
  (format "%s.toml"
          (no-newline (shell-command-to-string "hostname"))))

(defun server-conf-get-value (pkey akey)
  (second (assoc akey
                 (cl-getf
                  (load-server-setting (check-current-host))
                  pkey))))

(defun ssh-command-plain-silent (command)
  "Run a remote ssh command."
  (with-temp-buffer
    (let ((default-directory (second (assoc "front"
                (cl-getf (load-server-setting (check-current-host)) :doorway)))))
      (shell-command command nil nil))))

(defun ssh-command-plain-to-string (command)
  "Run a remote ssh command."
  (with-temp-buffer
    (let ((default-directory (second (assoc "front"
                (cl-getf (load-server-setting (check-current-host)) :doorway)))))
      (shell-command-to-string command))))


(defun ssh-command-plain (command &optional bname)
  "Run a remote ssh command."
  (with-temp-buffer
    (let ((default-directory (second (assoc "front"
                (cl-getf (load-server-setting (check-current-host)) :doorway)))))
      (shell-command command (current-buffer))
      (--> (buffer-string)
      (progn
        (switch-to-buffer
         (if bname
             bname
             "***temp-file***")
         (if (gnus-buffer-exists-p bname)
             bname
             (get-buffer-create bname)))
        (erase-buffer)
        (insert it)
        ;; just to get back immedietely
        (evil-normal-state)
        (re-search-backward "plus")
        ;; to prettify the table
        )))))

(defun look-at-lovely-log-scale ()
  (interactive)
  (ssh-command-plain (look-at-log-command) "**LOG**"))

(defun look-at-log-command ()
  (interactive)
  (format "ls  -art ~ %s|
                               grep %s                |
                               tail -1                |
                               xargs -I{} cat %s/{} |
                               awk '{if(NF>20){print \"=====\"}else{print $0}}'"
          (second (assoc "logdir"
                         (cl-getf (load-server-setting (check-current-host)) :database)))
          (buffer-name)
          (second (assoc "logdir"
                         (cl-getf (load-server-setting (check-current-host)) :database)))
          ))

(defun look-at-lovely-log-scale ()
  (interactive)
  (ssh-command-plain (look-at-log-command) "**LOG**"))

(defun this-log-file-name-bash ()
  (interactive)
   (print
    (ssh-command-plain-to-string
    (format "ls -art ~ %s               |
                               grep %s  |
                               tail -1 "
          (second (assoc "logdir"
                         (cl-getf (load-server-setting (check-current-host)) :database)))
          (buffer-name)
          ))))

(defun bash-debug-server ()
  (interactive)
  (ediff-files
   (buffer-name)
   (replace-regexp-in-string
    "\n" ""
    (format "%s/%s"
           (second (assoc "logdirfullpath"
                  (cl-getf (load-server-setting (check-current-host)) :database)))
           (this-log-file-name-bash)))
   ))


(defun exec-this-shell ()
  (interactive)
  (ssh-command-plain-silent
   (format "%s"
           (replace-regexp-in-string
            (server-conf-get-value :doorway "front")
            ""
            (buffer-file-name)))))


(defun list-app-on-server ()
  (mapcar #'helm-basename (split-string-without-empty
   (ssh-command-plain-to-string
    (format "echo %s/* | xargs -n1"
            (server-conf-get-value :database "approot")))
   "\n")))

(defun split-string-without-empty (str sp)
  (remove "" (split-string str sp)))

;; キーバインド
(map! :leader
      (:prefix ("U". "unicage")
               (:prefix ("n" . "仲間系")
                :desc "仲間ファイルをさがす"                   "f" #'nakama-buffers
                :desc "仲間ファイルから CALSED を生成する"        "c" #'nakama-calsed
                :desc "仲間ファイルからCSSをさがす"             "s" #'nakama-stylesheet
                :desc "仲間ファイルから変数を生成する"           "v" #'nakama-variable
                :desc "仲間ファイル内側で参照しているリンクの編集" "k" #'nakama-kyodai
                :desc "仲間ファイル内のプロファイルをみる"        "p" #'nakama-profile
                :desc "現在のファイルを仲間ファイルのエントリに追加"    "r" #'register-link-entry
                :desc "仲間ファイルのエントリと現在ファイルを結びつける" "a" #'attach-to-link
                :desc "仲間ファイルのエントリと現在ファイルを結びつける" "i" #'nakama-navigate-from-here
                )
               (:prefix ("c" . "変換系")
                :desc "tcom"                   "c" #'tcom
                :desc "tcom"                   "s" #'self-awk-lcalc
                )
               (:prefix ("s" . "シェル系")
                :desc "カーソル上のプロセスのみを実行"   "t" #'exec-tiny-proc
                :desc "man2 をみる"                   "m" #'man2
                :desc "データを自動整形"               "s" #'self-awk-lcalc
                :desc "tmpd にいれるデータをさっとつくる" "d" #'ins-DFC
                :desc "検証ファイルを作成、編集する"     "k" #'kensho-shellscript-interactive
                :desc "現在バッファの SHELL を実行"       "e" #'exec-this-shell
                :desc "デバッグ用にログとEDIFF"       "i" #'bash-debug-server
                :desc "編集中のログをみる"              "l" #'look-at-lovely-log-scale)
               (:prefix ("d" . "データ系")
                :desc "lv3" "3" #'lv3
                :desc "lv3" "c" #'go-to-userver-conf
                )
               (:prefix ("w" . "データ系")
                :desc "web 画面" "o" #'open-webgamen-as-in-curebuf
                :desc "devtool" "d" #'unicage-devtool
                :desc "ブラウザ初期化" "i" #'start-webgamen
                )
               (:prefix ("f" . "マッチ系")
                :desc "見出しを搜す" "m" #'swipe-midashi)
               )
      )



(provide 'cage)
;;; cage.el ends here
