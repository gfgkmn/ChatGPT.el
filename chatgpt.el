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

(defun chatgpt-get-current-date-string ()
  "Return the current date string in the format year_month_weekofyear_day."
  (let* ((now (current-time))
         (year (format-time-string "%Y" now))
         (week (format-time-string "%U" now)))
    (concat year "_week" week)))

(defun chatgpt-get-output-buffer-name ()
  (or (and chatgpt-record-path (buffer-name
                                (find-file-noselect (format "%s/gptchat_record_%s.txt"
                                                            chatgpt-record-path
                                                            (chatgpt-get-current-date-string)))))
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
    (visual-line-mode 1))
  (message "ChatGPT initialized."))

(defvar chatgpt-wait-timers (make-hash-table)
  "Timers to update the waiting message in the ChatGPT buffer.")


(defun chatgpt-converstaion-log-path ()
  (let* ((monthly-folder-name (format "%s/%s" chatgpt-record-path
                                      (format-time-string "%Y-%m"
                                                          (time-subtract (current-time) (seconds-to-time (* 3 60 60)))))))
    (unless (file-directory-p monthly-folder-name)
      (make-directory monthly-folder-name t))
    (format "%s/chatgpt_record_%s.txt" monthly-folder-name (chatgpt-get-current-date-string))))


(defun chatgpt-write-string-to-file (filename string)
  "Write STRING to FILENAME, creating the file if it doesn't exist, and appending to it if it does."
  (with-temp-buffer
    (insert string)
    (when (file-exists-p filename)
      (append-to-file (point-min) (point-max) filename))
    (unless (file-exists-p filename)
      (write-region (point-min) (point-max) filename))))


(defun chatgpt-append-gptchat-record (recordstr &optional record_path)
  (and chatgpt-record-path (chatgpt-write-string-to-file
                            (format "%s/gptchat_record_%s.txt"
                                    (or record_path chatgpt-record-path )
                                    (chatgpt-get-current-date-string))
                            (concat "\n\n" (make-string 80 ?-) "\n\n"  recordstr))))

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
(defun chatgpt-display ()
  "Displays *ChatGPT*."
  (interactive)
  (let ((output-buffer (chatgpt-get-output-buffer-name))) ; 创建或获取缓冲区
    (display-buffer output-buffer) ; 显示缓冲区
    (when-let ((saved-win (get-buffer-window (current-buffer)))
               (win (get-buffer-window output-buffer)))
      (unless (equal (current-buffer) output-buffer)
        (select-window win)
        (goto-char (point-max))
        (unless (pos-visible-in-window-p (point-max) win)
          (goto-char (point-max))
          (recenter -1))
        (select-window saved-win)))))

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

(defun chatgpt--goto-identifier (id)
  "Go to response of ID."
  (cl-assert (equal (current-buffer) (get-buffer (chatgpt-get-output-buffer-name))))
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

(defun chatgpt-get-buffer-width-by-dash ()
  "Return the width of the currently focused window in terms of the number of DASH-WIDTH - characters that can fit in the window."
  (let ((dash-width 1)) ; adjust this value if necessary for different font sizes
    (- (/ (window-width) dash-width) 1)))

(defun chatgpt--insert-query (query id)
  "Insert QUERY with ID into *ChatGPT*."
  (with-current-buffer (chatgpt-get-output-buffer-name)
    (save-excursion
      (goto-char (point-max))
      (insert (format "\n%s >>> %s\n%s\n%s\n%s"
                      (if (= (point-min) (point))
                          ""
                        "\n\n")
                      (propertize query 'face 'bold)
                      (make-string (chatgpt-get-buffer-width-by-dash) ?-)
                      (propertize
                       (chatgpt--identifier-string id)
                       'invisible t)
                      (if chatgpt-enable-loading-ellipsis
                          ""
                        (concat "Waiting for ChatGPT...")))))))

(defun chatgpt--insert-response (response id)
  "Insert RESPONSE into *ChatGPT* for ID."
    (with-current-buffer (chatgpt-get-output-buffer-name)
    (save-excursion
      (chatgpt--goto-identifier id)
      (chatgpt--clear-line)
      (insert response))))

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

(defun chatgpt--query (query)
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
    (chatgpt--insert-query query saved-id)
    (when chatgpt-enable-loading-ellipsis
      (chatgpt--add-timer saved-id))
    (when chatgpt-display-on-query
      (chatgpt-display))
    (print saved-id)
    (deferred:$
      (deferred:$
        (epc:call-deferred chatgpt-process 'query (list query))
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

(defun chatgpt--query-by-type (query query-type)
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
       (format "%s\n\n%s" (read-from-minibuffer "ChatGPT Custom Prompt: ") query))
    (if-let (format-string (cdr (assoc query-type chatgpt-query-format-string-map)))
        (chatgpt--query
         (format format-string query))
      (error "No format string associated with 'query-type' %s. Please customize 'chatgpt-query-format-string-map'" query-type))))

;;;###autoload
(defun chatgpt-query-by-type (query)
  "Query ChatGPT with from QUERY and interactively chosen 'query-type'.

The function uses the 'completing-read' function to prompt the
user to select the type of query to use. The selected query type
is passed to the 'chatgpt--query-by-type' function along with the
'query' argument, which sends the query to the ChatGPT model and
returns the response."
  (interactive (list (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (read-from-minibuffer "ChatGPT Query: "))))
  (let* ((query-type (completing-read "Type of Query: " (cons "custom" (mapcar #'car chatgpt-query-format-string-map)))))
    (if (or (assoc query-type chatgpt-query-format-string-map)
            (equal query-type "custom"))
        (chatgpt--query-by-type query query-type)
      (chatgpt--query (format "%s\n```%s```" query-type query)))))

;;;###autoload
(defun chatgpt-query (query)
  "Query ChatGPT with QUERY.

The user will be prompted to enter a query if none is provided. If
there is an active region, the user will be prompted to select the
type of query to perform.

Supported query types are:

* doc: Ask for documentation in query
* bug: Find bug in query
* improve: Suggestions for improving code
* understand: Query for understanding code or behavior"
  (interactive (list (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (read-from-minibuffer "ChatGPT Query: "))))
  ;; (if chatgpt-waiting-dot-timer
  ;;     (message "Already waiting on a ChatGPT query. If there was an error with your previous query, try M-x chatgpt-reset")
  ;;   (if (region-active-p)
  ;;       (chatgpt-query-by-type query)
  ;;     (chatgpt--query query)))
  (if (region-active-p)
      (chatgpt-query-by-type query)
    (chatgpt--query query)))


(defun chatgpt--query-stream (query &optional recursive)
  (unless chatgpt-process
    (chatgpt-init))
  (chatgpt-display)
  (lexical-let ((saved-id (if recursive
                              chatgpt-id
                            (cl-incf chatgpt-id)))
                (query (if recursive
                           (string-join (nthcdr 1 (split-string query "-")) "-")
                         query))
                (query_with_id (if recursive
                                   query
                                 (format "%s-%s" (org-id-uuid) query))))

    (if recursive
        (setq next-recursive recursive)
      (progn
        (setq next-recursive nil)
        (chatgpt--insert-query query saved-id)))

    (deferred:$
      (deferred:$
        (epc:call-deferred chatgpt-process 'querystream (list query_with_id))
        (deferred:nextc it
          #'(lambda (response)
              (with-current-buffer (chatgpt-get-output-buffer-name)
                (save-excursion
                  (if (numberp next-recursive)
                      (goto-char next-recursive)
                    (chatgpt--goto-identifier chatgpt-id))
                  (if (and (stringp response))
                      (progn
                        (insert response)
                        (chatgpt--query-stream query_with_id (point)))
                    (progn
                      (insert (format "\n\n%s"
                                      (make-string (chatgpt-get-buffer-width-by-dash) ?=))))))
                (let ((output-window (get-buffer-window (current-buffer))))
                  (when output-window
                    (with-selected-window output-window
                      (goto-char (point-max))
                      (recenter -1))))))))
      (deferred:error it
        `(lambda (err)
           (message "err is:%s" err))))))

;;;###autoload
(defun chatgpt--query-by-type-stream (query query-type)
  (if (equal query-type "custom")
      (chatgpt--query
       (format "%s\n\n%s" (read-from-minibuffer "ChatGPT Custom Prompt: ") query))
    (if-let (format-string (cdr (assoc query-type chatgpt-query-format-string-map)))
        (chatgpt--query-stream
         (format format-string query))
      (error "No format string associated with 'query-type' %s. Please customize 'chatgpt-query-format-string-map'" query-type))))

(defun chatgpt-query-by-type-stream (query)
  (interactive (list (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (read-from-minibuffer "ChatGPT Stream Query: "))))
  (let* ((query-type (completing-read "Type of Stream Query: " (cons "custom" (mapcar #'car chatgpt-query-format-string-map)))))
    (if (or (assoc query-type chatgpt-query-format-string-map)
            (equal query-type "custom"))
        (chatgpt--query-by-type-stream query query-type)
      (chatgpt--query-stream (format "%s\n\n%s" query-type query)))))

;;;###autoload
(defun chatgpt-query-stream (query)
  (interactive (list (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (read-from-minibuffer "ChatGPT Stream Query: "))))
  (if (region-active-p)
      (chatgpt-query-by-type-stream query)
    (chatgpt--query-stream query)))

(provide 'chatgpt)
;;; chatgpt.el ends here
