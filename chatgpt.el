;;; ;; -- lexical-binding: t --
;;; chatgpt.el --- ChatGPT in Emacs

;; Copyright (C) 2011 Free Software Foundation, Inc.

;; Author: Jungmin "Josh" Cho <joshchonpc@gmail.com>
;; Version: 0.1
;; Package-Requires: ((epc "0.1.1") (deferred "0.5.1"))
;; Keywords: ai
;; URL: https://github.com/joshcho/ChatGPT.el

;;; Commentary:
;; This package provides an interactive interface with ChatGPT API.

(require 'epc)
(require 'deferred)
(require 'python)
(require 'cl)
(require 'org-id)


;;; Code:

(defgroup chatgpt nil
  "Configuration for chatgpt."
  :prefix "chatgpt-"
  :group 'ai)

(defcustom chatgpt-query-format-string-map
  '(("doc" . "Please write the documentation for the following function.\n\n%s")
    ("bug" . "There is a bug in the following function, please help me fix it.\n\n%s")
    ("understand" . "What is the following?\n\n%s")
    ("improve" . "Please improve the following.\n\n%s"))
  "An association list that maps query types to their corresponding format strings."
  :type '(alist :key-type (string :tag "Query Type")
          :value-type (string :tag "Format String"))
  :group 'chatgpt)

(defcustom chatgpt-enable-loading-ellipsis t
  "Whether the ellipsis animation is displayed in *ChatGPT*."
  :type 'boolean
  :group 'chatgpt)

(defcustom chatgpt-display-on-query t
  "Whether *ChatGPT* is displayed when a query is sent."
  :type 'boolean
  :group 'chatgpt)

(defcustom chatgpt-display-on-response t
  "Whether *ChatGPT* is displayed when a response is received."
  :type 'boolean
  :group 'chatgpt)

(defcustom chatgpt-repo-path nil
  "The path of ChatGPT.el repository."
  :type 'string
  :group 'chatgpt)

(defvar chatgpt-process nil
  "The ChatGPT process.")

(defcustom chatgpt-buffer-name "*ChatGPT*"
  "Chatgpt buffer name"
  :type 'string
  :group 'chatgpt)

(defcustom chatgpt-default-model "ellis"
  "The model to use for ChatGPT."
  :type 'string
  :group 'chatgpt)

;;;###autoload
(defun chatgpt-login ()
  "Log in to ChatGPT."
  (interactive)
  (shell-command "chatgpt install &"))

;; TODO: save chatgpt conversation to file
(defcustom chatgpt-record-path nil
  "The path of ChatGPT.el repository."
  :type 'string
  :group 'chatgpt)

(defun chatgpt-save-file-name ()
  "Return the current workspace name."
  (interactive)
  (let* ((now (current-time))
         (year (format-time-string "%Y" now))
         (week (format-time-string "%U" now)))
    (concat year "_week" week (if persp-mode
                                  (safe-persp-name (get-current-persp))
                                "default"))))

(defun chatgpt-get-output-buffer-name ()
  (or (and chatgpt-record-path (buffer-name
                                (find-file-noselect (format "%s/gptchat_record_%s.txt"
                                                            chatgpt-record-path
                                                            (chatgpt-save-file-name)))))
      (with-current-buffer (get-buffer-create chatgpt-buffer-name)
        (setq-local truncate-lines nil))
      (get-buffer-create chatgpt-buffer-name)))

(defvar chatgpt-finish-response-hook nil)

;;;###autoload
(defun chatgpt-init ()
  "Initialize the ChatGPT server.

This function creates the ChatGPT process and starts it. It also
initializes the ChatGPT buffer, enabling visual line mode in it. A
message is displayed to indicate that the initialization was
successful.

If ChatGPT server is not initialized, 'chatgpt-query' calls this
function."
  (interactive)
  (let ((default-directory chatgpt-repo-path))
    (when (equal (shell-command-to-string "pip list | grep revChatGPT") "")
      (shell-command "pip install revChatGPT")
      (message "revChatGPT installed through pip.")
      (chatgpt-login))
    (when (null chatgpt-repo-path)
      (error "chatgpt-repo-path is nil. Please set chatgpt-repo-path as specified in joshcho/ChatGPT.el"))
    (setq chatgpt-process (epc:start-epc python-interpreter (list (expand-file-name
                                                                   (format "%schatgpt.py"
                                                                           chatgpt-repo-path))
                                                                  (auth-source-pick-first-password
                                                                   :host "openai.com"
                                                                   :user "chatgpt"))))
    (with-current-buffer (chatgpt-get-output-buffer-name)
      (visual-line-mode 1)
      (markdown-mode))
    (message "ChatGPT initialized.")))

(defvar chatgpt-wait-timers (make-hash-table)
  "Timers to update the waiting message in the ChatGPT buffer.")

;;;###autoload
(defun chatgpt-stop ()
  "Stops the ChatGPT server."
  (interactive)
  (dolist (id (hash-table-keys chatgpt-wait-timers))
    (chatgpt--stop-wait id))
  (epc:stop-epc chatgpt-process)
  (setq chatgpt-process nil)
  (message "Stop ChatGPT process."))

;;;###autoload
(defun chatgpt-reset ()
  "Reset the ChatGPT server. The same session is maintained."
  (interactive)
  (chatgpt-stop)
  (chatgpt-init))

;;;###autoload
(defun chatgpt-display (&optional output-buffer)
  "Displays *ChatGPT*."
  (interactive)

  (unless output-buffer
    (setq output-buffer (chatgpt-get-output-buffer-name))
    (display-buffer output-buffer)) ; 创建或获取缓冲区
                                        ; 显示缓冲区
  (when-let ((saved-win (get-buffer-window (current-buffer)))
             (win (get-buffer-window output-buffer)))
    (unless (equal (current-buffer) output-buffer)
      (select-window win)
      (if (not (eq major-mode 'markdown-mode))
          (markdown-mode))
      (goto-char (point-max))
      (unless (pos-visible-in-window-p (point-max) win)
        (goto-char (point-max))
        (recenter -1))
      (select-window saved-win)))
  (get-buffer output-buffer))

(defun chatgpt--clear-line ()
  "Clear line in *ChatGPT*."
  (cl-assert (equal (current-buffer) (get-buffer (chatgpt-get-output-buffer-name))))
  (delete-region (save-excursion (beginning-of-line)
                                 (point))
                 (save-excursion (end-of-line)
                                 (point))))

(defun chatgpt--identifier-string (id)
  "Identifier string corresponding to ID."
  (format "cg?[%s]" id))

(defun chatgpt--regex-string (id)
  "Regex corresponding to ID."
  (format "cg\\?\\[%s\\]" id))

(defun chatgpt--goto-identifier (id &optional output-buffer)
  "Go to response of ID."
  (unless output-buffer
    (setq output-buffer (get-buffer (chatgpt-get-output-buffer-name))))
  (cl-assert (equal (current-buffer) output-buffer))
  (goto-char (point-max))
  (re-search-backward (chatgpt--regex-string id))
  (forward-line))

;; (defun chatgpt--goto-identifier (id)
;;   "Go to response of ID."
;;   (cl-assert (equal (current-buffer) (get-buffer (chatgpt-get-output-buffer-name))))
;;   (goto-char (point-max))
;;   (let ((regex (chatgpt--regex-string id)))
;;     (message "Searching for regex: %s" regex)
;;     (if (re-search-backward regex nil t) ; 添加 't' 参数，避免在没有匹配到时抛出错误
;;         (progn
;;           (message "Found identifier for ID: %d" id)
;;           (forward-line))
;;       (message "Identifier not found for ID: %d" id))))

(defun chatgpt-get-buffer-width-by-char (char)
  "Return the width of the currently focused window in terms of the number of CHAR characters that can fit in the window."
  (let* ((c-width (if (eq char ?=)
                      2
                    1))
         (window (selected-window))
         (line-num-width (if (and (boundp 'display-line-numbers)
                                  (not (eq display-line-numbers nil)))
                             (length (format-mode-line "%l")) 0)))
    (- (/ (- (window-text-width window) (* line-num-width 2)) 1) (+ c-width 2))))

(defun chatgpt--insert-query (query id model-name &optional output-buffer)
  "Insert QUERY with ID into *ChatGPT*."
  (unless output-buffer
    (setq output-buffer (chatgpt-get-output-buffer-name))) ; 创建或获取缓冲区
  (with-current-buffer output-buffer
    (save-excursion
      (goto-char (point-max))
      (with-selected-window (get-buffer-window output-buffer)
        (recenter 0))
      (let ((inhibit-read-only t))
        (insert (format "%s %s >>> %s\n%s\n%s\n%s"
                        (if (= (point-min) (point))
                            "\n"
                          "\n\n")
                        model-name
                        (propertize query 'face 'bold)
                        (make-string (chatgpt-get-buffer-width-by-char ?-) ?-)
                        (propertize
                         (chatgpt--identifier-string id)
                         'invisible t)
                        (if chatgpt-enable-loading-ellipsis
                            ""
                          (concat "Waiting for ChatGPT..."))))))))

(defun chatgpt--insert-response (response id)
  "Insert RESPONSE into *ChatGPT* for ID."
  (with-current-buffer (chatgpt-get-output-buffer-name)
    (save-excursion
      (chatgpt--goto-identifier id)
      (chatgpt--clear-line)
      (insert response)
      (progn
        (insert (format "\n\n%s"
                        (make-string (chatgpt-get-buffer-width-by-char ?=) ?=)))))))

(defun chatgpt--insert-error (error-msg id)
  "Insert ERROR-MSG into *ChatGPT* for ID."
  (with-current-buffer (chatgpt-get-output-buffer-name)
    (save-excursion
      (chatgpt--goto-identifier id)
      (chatgpt--clear-line)
      (insert (format "ERROR: %s" error-msg)))))

(defun chatgpt--add-timer (id)
  "Add timer for ID to 'chatgpt-wait-timers'."
  (cl-assert (null (gethash id chatgpt-wait-timers)))
  (puthash id
           (run-with-timer 0.5 0.5
                           (eval
                            `(lambda ()
                               (with-current-buffer (chatgpt-get-output-buffer-name)
                                 (save-excursion
                                   (chatgpt--goto-identifier ,id)
                                   (let ((line (thing-at-point 'line)))
                                     (when (>= (+ (length line)
                                                  (if (eq (substring line -1) "\n")
                                                      0
                                                    1))
                                               4)
                                       (chatgpt--clear-line))
                                     (insert ".")))))))
           chatgpt-wait-timers))

(defun chatgpt--stop-wait (id)
  "Stop waiting for a response from ID."
  (when-let (timer (gethash id chatgpt-wait-timers))
    (cancel-timer timer)
    (remhash id chatgpt-wait-timers)))

(defvar chatgpt-id 0
  "Tracks responses in the background.")

(defvar chatgpt-last-query nil)

(defvar chatgpt-last-response (make-marker) "Global variable to store the last response marker")

(defvar chatgpt-last-use-buffer nil)

(defvar chatgpt-last-use-model nil)

(defun chatgpt--query (query use-model)
  "Send QUERY to the ChatGPT process.

The query is inserted into the *ChatGPT* buffer with bold text,
and the response from the ChatGPT process is appended to the
buffer. If `chatgpt-enable-loading-ellipsis' is non-nil, a loading
ellipsis is displayed in the buffer while waiting for the
response.

This function is intended to be called internally by the
`chatgpt-query' function, and should not be called directly by
users."
  (unless chatgpt-process
    (chatgpt-init))
  (let ((saved-id (cl-incf chatgpt-id)))
    (when chatgpt-enable-loading-ellipsis
      (chatgpt--add-timer saved-id))
    (when chatgpt-display-on-query
      (chatgpt-display))
    (chatgpt--insert-query query saved-id use_model)
    (print saved-id)
    (deferred:$
     (deferred:$
      (epc:call-deferred chatgpt-process 'query (list query use-model))
      (eval `(deferred:nextc it
              (lambda (response)
                (chatgpt--stop-wait ,saved-id)
                (chatgpt--insert-response response ,saved-id)
                (when chatgpt-display-on-response
                  (chatgpt-display)
                  (and chatgpt-finish-response-hook (run-hooks 'chatgpt-finish-response-hook)))))))
     (eval
      `(deferred:error it
        (lambda (err)
          (chatgpt--stop-wait ,saved-id)
          (string-match "\"Error('\\(.*\\)')\"" (error-message-string err))
          (let ((error-str (match-string 1 (error-message-string err))))
            (chatgpt--insert-error error-str
                                   ,saved-id)
            (when (yes-or-no-p (format "Error encountered. Reset chatgpt (If reset doesn't work, try \"\"pkill ms-playwright/firefox\"\" in the shell then reset)?" error-str))
              (chatgpt-reset)))))))))

(defun chatgpt--query-by-type (query query-type use-model)
  "Query ChatGPT with a given QUERY and QUERY-TYPE.

QUERY is the text to be passed to ChatGPT.

QUERY-TYPE is the type of query, which determines the format string
used to generate the final query sent to ChatGPT. The format string
is looked up in 'chatgpt-query-format-string-map', which is an
alist of QUERY-TYPE and format string pairs. If no matching
format string is found, an error is raised.

The format string is expected to contain a %s placeholder, which
will be replaced with QUERY. The resulting string is then passed
to ChatGPT.

For example, if QUERY is \"(defun square (x) (* x x))\" and
QUERY-TYPE is \"doc\", the final query sent to ChatGPT would be
\"Please write the documentation for the following function.
\(defun square (x) (* x x))\""
  (if (equal query-type "custom")
      (chatgpt--query
       (format "%s\n\n%s" (read-from-minibuffer "ChatGPT Custom Prompt: ") query) use-model)
    (if-let (format-string (cdr (assoc query-type chatgpt-query-format-string-map)))
        (chatgpt--query
         (format format-string query) use-model)
      (error "No format string associated with 'query-type' %s. Please customize 'chatgpt-query-format-string-map'" query-type))))


;;;###autoload
(defun chatgpt-reask ()
  ;; switch chatgpt-use-model between gpt35 and gpt4
  ;; but with save conversation history
  (interactive)
  (message chatgpt-last-use-model)

  (progn
    (with-current-buffer chatgpt-last-use-buffer
      (save-excursion
        (goto-char chatgpt-last-response)
        (message "loaded chatgpt-last-response: %s" chatgpt-last-response)
        (delete-region (point) (point-max)))))

  (if (string= chatgpt-last-use-model "ellis")
      (chatgpt--query-stream chatgpt-last-query "rogers" nil chatgpt-last-use-buffer t)
    (chatgpt--query-stream chatgpt-last-query "ellis" nil chatgpt-last-use-buffer t)))


(defun chatgpt--query-stream (query use-model &optional recursive use-buffer-name reuse)
  (unless chatgpt-process
    (chatgpt-init))
  (if recursive
      (chatgpt-display use-buffer-name)
    (setq use-buffer-name (chatgpt-display))
    (setq chatgpt-last-query query)
    (setq chatgpt-last-use-buffer use-buffer-name)
    (setq chatgpt-last-use-model use-model))
  (lexical-let ((saved-id (if recursive
                              chatgpt-id
                            (cl-incf chatgpt-id)))
                (query (if recursive
                           (string-join (nthcdr 5 (split-string query "-")) "-")
                         query))
                (query_with_id (if recursive
                                   query
                                 (format "%s-%s" (org-id-uuid) query)))
                (recursive-model use-model)
                (use-buffer-name use-buffer-name)
                (reuse (if recursive
                           nil
                         reuse)))

    (if recursive
        (setq next-recursive recursive)
      (progn
        (setq next-recursive nil)
        (set-marker chatgpt-last-response (point))
        (chatgpt--insert-query query saved-id use-model use-buffer-name)))

    (deferred:$
     (deferred:$
      ;; (epc:call-deferred chatgpt-process 'querystream (list query_with_id recursive-model reuse "default"))

      (epc:call-deferred chatgpt-process 'querystream (list query_with_id recursive-model reuse (buffer-name use-buffer-name)))

      (deferred:nextc it
                      #'(lambda (response)
                          (with-current-buffer use-buffer-name
                            (save-excursion
                              (if (numberp next-recursive)
                                  (goto-char next-recursive)
                                (chatgpt--goto-identifier chatgpt-id use-buffer-name))
                              (if (and (stringp response))
                                  (progn
                                    (insert response)
                                    (goto-char (point-max))
                                    (setq next-recursive (point)))
                                (progn
                                  (insert (format "\n\n%s"
                                                  (make-string (chatgpt-get-buffer-width-by-char ?=) ?=)))
                                  (goto-char (point-max))
                                  (let ((output-window (get-buffer-window (current-buffer))))
                                    (when output-window
                                      (with-selected-window output-window
                                        (goto-char (point-max))
                                        (unless (>= (window-end output-window) (point-max))
                                          (recenter -1)))))
                                  (setq next-recursive nil)
                                  (save-buffer)))))
                          (if next-recursive
                              (chatgpt--query-stream query_with_id recursive-model next-recursive use-buffer-name)))))
     (deferred:error it
                     `(lambda (err)
                        (message "err is:%s" err))))))


(defun chatgpt--query-by-type-stream (query query-type use-model)
  (if (equal query-type "custom")
      (chatgpt--query-stream
       (format "%s\n\n%s" (read-from-minibuffer "ChatGPT Custom Prompt: ") query) use-model)
    (if-let (format-string (cdr (assoc query-type chatgpt-query-format-string-map)))
        (chatgpt--query-stream
         (format format-string query) use-model)
      (error "No format string associated with 'query-type' %s. Please customize 'chatgpt-query-format-string-map'" query-type))))


;;;###autoload
(defun chatgpt-query-by-type (query use-model)
  "Query ChatGPT with from QUERY and interactively chosen 'query-type'.

The function uses the 'completing-read' function to prompt the
user to select the type of query to use. The selected query type
is passed to the 'chatgpt--query-by-type' function along with the
'query' argument, which sends the query to the ChatGPT model and
returns the response."
                                        ; TODO
  (interactive (list (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (read-from-minibuffer "ChatGPT Query: "))
                     (read-string "GPT model: " chatgpt-default-model)))
  (let* ((query-type (completing-read "Type of Query: " (cons "custom" (mapcar #'car chatgpt-query-format-string-map)))))
    (if (or (assoc query-type chatgpt-query-format-string-map)
            (equal query-type "custom"))
        (chatgpt--query-by-type query query-type use-model)
      (chatgpt--query (format "%s\n```%s```" query-type query) use-model))))

;;;###autoload
(defun chatgpt-query (query use-model)
  "Query ChatGPT with QUERY.

The user will be prompted to enter a query if none is provided. If
there is an active region, the user will be prompted to select the
type of query to perform.

Supported query types are:

* doc: Ask for documentation in query
* bug: Find bug in query
* improve: Suggestions for improving code
* understand: Query for understanding code or behavior"
                                        ;TODO
  (interactive (list (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (read-from-minibuffer "ChatGPT Query: "))
                     (read-string "GPT model: " chatgpt-default-model)))
  ;; (if chatgpt-waiting-dot-timer
  ;;     (message "Already waiting on a ChatGPT query. If there was an error with your previous query, try M-x chatgpt-reset")
  ;;   (if (region-active-p)
  ;;       (chatgpt-query-by-type query)
  ;;     (chatgpt--query query)))
  (if (region-active-p)
      (chatgpt-query-by-type query use-model)
    (chatgpt--query query use-model)))

;;;###autoload
(defun chatgpt-query-by-type-stream (query use-model)
  (interactive (list (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (read-from-minibuffer "ChatGPT Stream Query: "))
                     (read-string "GPT model: " chatgpt-default-model)))
  (let* ((query-type (completing-read "Type of Stream Query: " (cons "custom" (mapcar #'car chatgpt-query-format-string-map)))))
    (if (or (assoc query-type chatgpt-query-format-string-map)
            (equal query-type "custom"))
        (chatgpt--query-by-type-stream query query-type (or use-model chatgpt-default-model))
      (chatgpt--query-stream (format "%s\n\n%s" query-type query) (or use-model chatgpt-default-model)))))

;;;###autoload
(defun chatgpt-query-stream (query use-model)
  (interactive (list (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (read-from-minibuffer "ChatGPT Stream Query: "))
                     (unless use-model (read-string "GPT model: " chatgpt-default-model))))
  (if (region-active-p)
      (chatgpt-query-by-type-stream query use-model)
    (chatgpt--query-stream query use-model)))


;;;###autoload
(defun chatgpt-query-stream-gpt4 (query)
  (interactive (list (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (read-from-minibuffer "ChatGPT Stream Query: "))))
  (chatgpt-query-stream query "rogers"))

;;;###autoload
(defun chatgpt-query-stream-gpt35 (query)
  (interactive (list (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (read-from-minibuffer "ChatGPT Stream Query: "))))
  (chatgpt-query-stream query "ellis"))

(provide 'chatgpt)
;;; chatgpt.el ends here
