;;; chromium.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 OPASO
;;
;; Author: OPASO <ryu@OPASO>
;; Maintainer: OPASO <ryu@OPASO>
;; Created: 10月 07, 2024
;; Modified: 10月 07, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/ryu/chromium
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

;;--------------------------------------------------------------
;; CHROME DEVTOOL PROTOCOL 系
;;--------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WEB AUTOMATION
(cl-defun start-webgamen (&optional (thunk (lambda () nil)))
  (interactive)
  (setq async-shell-command-display-buffer nil)
  (setq async-shell-command-buffer nil)
  ;; (shell-command)では、PATHがリモートになってしまうので、
  ;; ここだけ、(call-process-shell-command)でよぶ。
  (call-process-shell-command "chromium --remote-debugging-port=9222 &")
  (funcall thunk))

(defun launch-chromium-with-debuggin-port ()
  (async-shell-command "chromium --remote-debugging-port=9222" nil nil))

(load-file "~/emacs-websocket/websocket.el")

(defvar *ws-chrome* "ws://localhost:9222/")
(defvar *baseurl* "http://localhost:9222")
(defvar *wsdbgurl* "webSocketDebuggerUrl")

(defun get-response (end)
  "WS の LINK をとるためにリクエストをとる"
  (let*
      ((url-request-method "PUT")
       (buffer (url-retrieve-synchronously (format "%s/%s" *baseurl* end))))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (goto-line 6)
      (concat (buffer-substring-no-properties (point) (point-max))) )))

(defun chrome-wslink (end)
  (json-parse-string
 (get-response end)))

(defun search-wslink-by-title (title)
  (let ((json-hash (chrome-wslink "json/list")))
  (remove nil
          (cl-loop for i from 0 to (1- (length json-hash))
                   collect (when (string-match-p
                                  title
                                  (gethash "title" (aref json-hash i))
                                  )
                             (gethash *wsdbgurl* (aref json-hash i)))))))

(defun search-wslink-by-url (url)
  (let ((json-hash (chrome-wslink "json/list")))
  (remove nil
          (cl-loop for i from 0 to (1- (length json-hash))
                   collect (when (string-match-p
                                  url
                                  (gethash "url" (aref json-hash i))
                                  )
                             (gethash *wsdbgurl* (aref json-hash i)))))))


(defun chrome-hot-reload ()
  (interactive)
  (let ((nakamatic (car (split-string (buffer-name) "\\."))))
    (when
        ;; TODO 拡張性を意識するとnakamaticではきびしいかも
        (condition-case err
            (search-wslink-by-url nakamatic)
          (file-error
           (message "chrome を通信用にひらく")
           (start-webgamen)))
      (chrome-socket-do
       (car (search-wslink-by-url nakamatic))
       (lambda
         (ws)
         (message "reload")
         (websocket-send-text ws
                              (make-cdp-req-json 1
                                                 "Page.reload"
                                                 '())))))))

(cl-defun chrome-socket-do
    (wslink on-open &optional
            (on-message (lambda (ws frame)
                          (message "Receive: %s" (websocket-frame-text frame))
                          (websocket-close ws))
                        )
            (on-close (lambda (ws) (message "Closed"))))
  (websocket-open
   wslink
   :on-open on-open
   :on-message on-message
   :on-close on-close
   ))

(cl-defun chrome-just-login (url &optional (additional (lambda () 'nil)))
  (setq lexical-binding t)
  (let* ((hash (chrome-wslink "json/new")))
    ;; ログインページまでとぶ
    (chrome-socket-do
     (gethash *wsdbgurl* hash)
     (let ((url url))
       (lambda
         (ws)
         (message "タブ作成")
         (websocket-send-text ws
                              (make-cdp-req-json 1
                                                 "Page.navigate"
                                                 `(:url ,url)))))
     ;; WSのレスポンス次第で処理
     (lambda (ws frame)
       (let ((lim (--> (websocket-frame-text frame))))
         (if (string-match-p "frameId" lim)
           ;; resにframeIdという文字があるやつだけとる。
           ;; login用のプログラムを送信する用途。
           (funcall additional ws nil)
           (funcall additional ws t)
           ))
       ))))

(defvar *devtool-ws* nil)

(cl-defun chrome-just-devtool-ground ()
  (interactive)
  (setq lexical-binding t)
   (let* ((hash (chrome-wslink "json/new")))
    (chrome-socket-do
     (gethash *wsdbgurl* hash)
       (lambda
         (ws)
         (message "タブ作成")
         (websocket-send-text ws
                              (make-cdp-req-json 1
                                                 "Page.navigate"
                                                 `(:url "https://example.com")))))))


;; TODO FORM を保持できる LOGIN 前後の WS をうまく扱えていない
(defun open-webgamen-as-in-curebuf ()
  (interactive)
  ;; クロージャ用
  (let ((cnt 0))
    ;; ログインしたあとに遷移する。
    (chrome-just-login
     "http://ops.realord.co.jp/IROKU/CGI/IRK_LOGIN.CGI"

     (lambda (ws next)
       (websocket-send-text ws
                            (make-cdp-req-json 1
                                               "Runtime.evaluate"
                                               `(:expression
                                                 ,(login-program (format "%s/%s"
                                                                         (second (split-string (buffer-name) "\\."))
                                                                         (buffer-name))
                                                                 next))))

       (when (and next (< cnt 10)) ;; よくわかんねえ
         (setq cnt (1+ cnt)))    ;;同じ WS をもったままうまく身動きとれないの謎
       (when (> cnt 7)
         (websocket-close ws))))))

(defun login-program (cgi-file comment)
  "TODO プログラムを呼んで目的のCGIをブラウザでひらく。サーバ毎の対応をする必要あり"
  (format
   "postURL('../%s')
    document.getElementById('username').value ='000001'
    document.getElementById('password').value='iroku1234'
    document.getElementById('langSetting').value='JP'
    login() "
   cgi-file))

;; Runtime
(defun with-runtime-enabled (tab-title then cb)
  (chrome-socket-do
   (car (search-wslink-by-title tab-title))
   (lambda (ws)
     (message "Connected")
     (websocket-send-text ws
                          (make-cdp-req-json 1
                                             "Runtime.enable"
                                             `())))
   (lambda (ws frame)
     (funcall then ws)
     (when
         (string-match-p
          "description"
          (websocket-frame-text
           frame))
       (let ((msg (--> (json-parse-string (websocket-frame-text frame))
                       (gethash "result" it)
                       (gethash "result" it)
                       (gethash "description" it))))

         (if msg
             (funcall cb msg)
           (let ((img (create-image (nth (random (length
                                                  gif-candidates))
                                         gif-candidates))))

             (funcall cb (propertize "foo" 'display img))
             (image-animate img)

             )
           )
         (unwind-protect(websocket-close ws))))

     (progn
       (print (--> (json-parse-string (websocket-frame-text frame))
                   (gethash "params" it)
                   (gethash "args" it)
                   ))
       (unwind-protect (websocket-close wb)))

     (let ((msg-type (--> (json-parse-string (websocket-frame-text frame))
                          (gethash "result" it)
                          (gethash "result" it)
                          (gethash "type" it))))
       (cond ((string= msg-type "undefined")
              (progn (funcall cb (websocket-frame-text frame)) (unwind-protect
                                                                   (websocket-close ws))))
             ((string= msg-type "string")
              (funcall cb (websocket-frame-text frame)) (unwind-protect
                                                            (websocket-close ws)))))

     ))
  )

(defun send-js-to-tab (tab-title program cb)
  (chrome-socket-do
   (car (search-wslink-by-url tab-title))
   (lambda (ws)
     (message "Connected")
     (websocket-send-text ws
                          (make-cdp-req-json 1
                                             "Runtime.evaluate"
                                             `(:expression
                                               ,program))))
   (lambda (ws frame)
     ;;(funcall cb)
     (let ((msg (--> (json-parse-string (websocket-frame-text frame))
                     (gethash "result" it)
                     (gethash "result" it)
                     (gethash "description" it))))

       (if msg
           (funcall cb msg)
         (let ((img (create-image (nth (random (length
                                                gif-candidates))
                                       gif-candidates))))

           (funcall cb (propertize "foo" 'display img))
           (image-animate img)

           )
         )
       (unwind-protect(websocket-close ws))))))


(defun cdp-add-bp (tab-title)
  (chrome-socket-do
   (car (search-wslink-by-title tab-title))
   (lambda (ws)
     (message "Connected")
     (websocket-send-text ws
                          (make-cdp-req-json 1
                                             "Debugger.enable"
                                             '())))
  (lambda (ws frame)
    ;; スクリプトのテキストの到着をまつ
    (let* ((params (--> (json-parse-string (websocket-frame-text frame))
                   (gethash "params" it)))
           (url (gethash "url" params))
           (id (gethash "scriptId" params)))
      (if (not (string-match-p "chrome-extension" url)) ;;chrome-extension はみたくない。
          (websocket-send-text ws
                               (make-cdp-req-json 1
                                                  "Debugger.setBreakpoint"
                                                  `(:location (:scriptId ,id
                                                               :lineNumber 0))))))
    (webscoket-close ws))))

;;(cdp-add-bp "Login")

(defun cdp-nit-runtime (tab-title)
  (chrome-socket-do
   (car (search-wslink-by-title tab-title))
   (lambda (ws)
     (message "Connected")
     (websocket-send-text ws
                          (make-cdp-req-json 1
                                             "Debugger.enable"
                                             '())))
  (lambda (ws frame)
    ;; スクリプトのテキストの到着をまつ
    (when (string-match-p
           "scriptSource"
           (websocket-frame-text frame))
      (let ((src (--> (json-parse-string (websocket-frame-text frame))
                      )))
        (print src))
      )
    (let* ((params (--> (json-parse-string (websocket-frame-text frame))
                   (gethash "params" it)))
           (url (gethash "url" params))
           (id (gethash "scriptId" params)))
      (if (not (string-match-p "chrome-extension" url)) ;;chrome-extension はみたくない。
          (websocket-send-text ws
                           (make-cdp-req-json 1
                                              "Debugger.getScriptSource"
                                              `(:scriptId ,id)))
                                              ))
    (webscoket-close ws))))

;; DEBUGGER
(defun cdp-init-debugger (tab-title)
  (chrome-socket-do
   (car (search-wslink-by-title tab-title))
   (lambda (ws)
     (message "Connected")
     (websocket-send-text ws
                          (make-cdp-req-json 1
                                             "Debugger.enable"
                                             '())))
   (lambda (ws frame)
         (let ((id (--> (json-parse-string (websocket-frame-text frame))
                        (gethash "params" it)
                        (gethash "scriptId" it))))
           (cdp-loc-debugger ws id 1)
           )
         (webscoket-close ws)
       )))


;; LOG
(defun cdp-init-log (tab-title)
  (chrome-socket-do
   (car (search-wslink-by-title tab-title))
   (lambda (ws)
     (message "Connected")
     (websocket-send-text ws
                          (make-cdp-req-json 1
                                             "Log.enable"
                                             '())))
   (lambda (ws frame)
     )))

(defun cdp-loc-debugger (ws scriptId linum)
  (websocket-send-text ws
                       (make-cdp-req-json 1
                                          "Debugger.getPossibleBreakpoints"
                                          `(:start
                                                    (:scriptId ,scriptId
                                                     :lineNumber 1)))
                       ))

;; Network

;; CSS
(defun add-css (tab-title)
  (chrome-socket-do
   (car (search-wslink-by-title tab-title))
   (lambda (ws)
     (message "Connected")
     (websocket-send-text ws
                          (make-cdp-req-json 1
                                             "CSS.styleSheetAdded"
                                             '(:styleSheetId: 1
                                               :ruleText: "example"
                                               ))))))


; DOM
(defun manipulate-DOM (tab-title)
  (chrome-socket-do
   (car (search-wslink-by-title tab-title))
   (lambda (ws)
       (websocket-send-text ws
                            (make-cdp-req-json 1
                                               "DOM.getDocument"
                                               '())
                            ))
   (lambda (ws frame)

     (websocket-close ws)
       )))

(defun make-cdp-req-json (id method params)
  (json-encode `(:id ,id
                 :method ,method
                 :params ,params)))


(defface unicage-devtool-prompt-face
  '((t :foreground "green"))
  "Face for the prompt in `custom-shell-mode`.")

;;
(define-derived-mode unicage-devtool-mode js2-mode "unicage-devtool"
  "A custom shell mode.")

(defun unicage-devtool-readonly-prompt (msg)
  "Set [ʎ]: as readonly prompt."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert "\n")
    (print msg)
    (insert msg)
    (insert "\n")
    (unwind-protect
        (insert (propertize "[ʎ]: " 'face 'unicage-devtool-prompt-face 'read-only t 'rear-nonsticky t 'front-sticky t)))))
;; ステータスがいいかんじの時にかえってくる。
(defvar gif-candidates
 '("dalshim.gif"))

(defun switch-devtool-target2this ()
  (interactive)
   (ivy-read "type" '("default" "this")
             :action (lambda (choice)
                       (if (string= choice "default")
                           (setq unicage-devtool-target-dir-regexp "example")
                         (setq unicage-devtool-target-dir-regexp
                               (car (split-string (buffer-name) "\\.")))))))

(defvar unicage-devtool-history '())

(defun buffer-name-nakamatified ()
  (car (split-string (buffer-name) "\\.")))

(defun search-shell-vars ()
  (interactive)
  (let ((str (buffer-string)))
    (print (s-match-strings-all ".*=" str))
    ))


(cl-defun unicage-devtool-process-input (&optional (loaded ""))
  "Process input given to udevtool shell."
  (interactive)
  (if (string= loaded "")
      (re-search-backward "[ʎ]")
    ;; 最近のプロンプトまでいって、スクリプトをとばす。
    (unless (string-equal (buffer-substring-no-properties (line-beginning-position) (line-end-position))
                          "[ʎ]: ")
      (let* ((inhibit-read-only t)
             (input-start (+ 5 (line-beginning-position)))  ; 5 characters: "[ʎ]: "
             (input (buffer-substring-no-properties input-start (point-max))))

        ;; キャッシュ
        (add-to-list 'unicage-devtool-history input)
        ;; INPUT の値をつかうところ。
        (send-js-to-tab unicage-devtool-target-dir-regexp
                        input
                        (lambda (message)
                          (unicage-devtool-readonly-prompt message)))
        ))
    (send-js-to-tab unicage-devtool-target-dir-regexp
                    loaded
                    (lambda (message)
                      (unicage-devtool-readonly-prompt message)))
    )
  )

(defun unicage-devtool-newline ()
  "Create a new line in custom shell."
  (interactive)
  (insert "\n     "))  ; create new line with spaces to align with prompt

(defun unicage-devtool-comleting-read ()
  (interactive)
  (insert (completing-read "history> " unicage-devtool-history)))


(defun unicage-devtool-load-js-file ()
  (interactive)
  (ivy-read "load file > " (helm-list-directory "~/tools/js")
            :action (lambda (choice)
                      (unicage-devtool-process-input (f-read-text choice)))))

(defun unicage-devtool-edit-js ()
  (interactive)
  (ivy-read "load file > " (helm-list-directory "~/tools/js")
            :action (lambda (choice)
                      (find-file choice))))

(define-key unicage-devtool-mode-map (kbd "RET") 'unicage-devtool-process-input)
(define-key unicage-devtool-mode-map (kbd "<S-return>") 'unicage-devtool-shell-newline)
(define-key unicage-devtool-mode-map (kbd "M-n") 'unicage-devtool-comleting-read)
(define-key unicage-devtool-mode-map (kbd "M-l") 'unicage-devtool-load-js-file)
(define-key unicage-devtool-mode-map (kbd "M-e") 'unicage-devtool-edit-js)

(defun unicage-devtool-buffer (buffer)
  (switch-to-buffer (format "unicage-devtool"))
  (display-buffer-in-side-window buffer '((side . bottom))))

;;------------------------------------------------------------
;; DEVTOOL
;;------------------------------------------------------------
(defun unicage-devtool ()
  "DEVTOOLをEMACSからひらく"
  (interactive)
  (unicage-devtool-attach-to-page)
  (unicage-devtool-buffer (format "unicage-devtool"))
  (unicage-devtool-mode)
  (unicage-devtool-readonly-prompt ""))

(defun unicage-devtool-attach-to-page ()
  "unicage-devtoolがスクリプトをおくるタブをきめる"
  (interactive)
  (ivy-read "which to attach?> " (helm-buffer-list)
            :action (lambda (choice)
                      (setq unicage-devtool-target-dir-regexp
                            choice))))



(provide 'chromium)
;;; chromium.el ends here
