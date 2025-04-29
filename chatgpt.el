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


(defcustom chatgpt-ai-choices
  '("perplexity" "gpt4o" "lispgpt" "pythongpt" "qwen25" "claude" "dsr1")
  "List of AI choices available for selection."
  :type '(repeat string)
  :group 'chatgpt)


(defcustom chatgpt-second-preferest-model "perplexity"
  "Chatgpt buffer name"
  :type 'string
  :group 'chatgpt)

(defvar chatgpt--in-query-minibuffer nil
  "Flag to indicate if we're currently in a ChatGPT query minibuffer.")

(defvar chatgpt-process nil
  "The ChatGPT process.")

(defcustom chatgpt-buffer-name "*ChatGPT*"
  "Chatgpt buffer name"
  :type 'string
  :group 'chatgpt)

(defcustom chatgpt-default-model "claude"
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

(defvar chatgpt-check-running-flag nil "The flag is utilized to denote whether we are within the interval of running the check.")

(defvar chatgpt-running-flag nil "Flag to indicate whether chatgpt is running.")

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
                                                                           chatgpt-repo-path)))))
    (with-current-buffer (chatgpt-get-output-buffer-name)
      (visual-line-mode 1)
      (markdown-mode))
    (message "ChatGPT initialized.")))

(defun chatgpt-query-epc-port ()
  "Query the EPC server port for the ChatGPT process.
This function retrieves and displays the port number of the running EPC server."
  (interactive)
  (if (and chatgpt-process (epc:manager-p chatgpt-process))
      (let ((port (epc:manager-port chatgpt-process)))
        (message "ChatGPT EPC server is running on port: %d" port)
        port)
    (error "ChatGPT EPC server is not running.")))

(defvar chatgpt-wait-timers (make-hash-table)
  "Timers to update the waiting message in the ChatGPT buffer.")

;;;###autoload
(defun chatgpt-stop ()
  "Stops the ChatGPT server."
  (interactive)
  (dolist (id (hash-table-keys chatgpt-wait-timers))
    (chatgpt--stop-wait id))
  (when chatgpt-process
    (epc:call-deferred chatgpt-process 'clear_streams (list))
    (epc:stop-epc chatgpt-process)
    (setq chatgpt-process nil))

  (setq chatgpt-check-running-flag t)
  ;; wait until chatgpt-check-running-flag is nil
  (if chatgpt-running-flag
      (progn
        (message "ChatGPT is running. Please wait until it finishes.")
        (while chatgpt-check-running-flag
          (sleep-for 0.1))))
  (setq chatgpt-check-running-flag nil)
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
        (setq chatgpt-last-response (point))
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

(defun chatgpt--insert-response (response id &optional output-buffer)
  "Insert RESPONSE into *ChatGPT* for ID."
  (unless output-buffer
    (setq output-buffer (chatgpt-get-output-buffer-name))) ; 创建或获取缓冲区
  (with-current-buffer output-buffer
    (save-excursion
      (chatgpt--goto-identifier id)
      (chatgpt--clear-line)
      (insert response)
      (progn
        (insert (format "\n\n%s"
                        (make-string (chatgpt-get-buffer-width-by-char ?=) ?=)))))))

(defun chatgpt--insert-error (error-msg id &optional output-buffer)
  "Insert ERROR-MSG into *ChatGPT* for ID."
  (unless output-buffer
    (setq output-buffer (chatgpt-get-output-buffer-name))) ; 创建或获取缓冲区
  (with-current-buffer output-buffer
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
  ;; support invisible invoke


    ;; but this would report void variable use-buffer-name
  (let ((use-buffer-name (chatgpt-display)))
        ;; (chatgpt-last-use-buffer use-buffer-name)
        ;; (saved-id (cl-incf chatgpt-id)))

    (setq chatgpt-last-use-model use-model)
    (setq chatgpt-last-use-buffer use-buffer-name)
    (setq chatgpt-last-query query)
    (setq saved-id (cl-incf chatgpt-id))

    (chatgpt--insert-query query saved-id use-model use-buffer-name)
    (when chatgpt-enable-loading-ellipsis
      (chatgpt--add-timer saved-id))
    (deferred:$
     (deferred:$
      (epc:call-deferred chatgpt-process 'query (list query use-model (buffer-name chatgpt-last-use-buffer)))
      (eval `(deferred:nextc it
              (lambda (response)
                (chatgpt--stop-wait ,saved-id)
                (chatgpt--insert-response response ,saved-id chatgpt-last-use-buffer)
                (when chatgpt-display-on-response
                  (chatgpt-display chatgpt-last-use-buffer)
                  (and chatgpt-finish-response-hook (run-hooks 'chatgpt-finish-response-hook)))
                (with-current-buffer chatgpt-last-use-buffer
                  (goto-char (point-max))
                  (let ((output-window (get-buffer-window (current-buffer))))
                    (when output-window
                      (with-selected-window output-window
                        (goto-char (point-max))
                        (unless (>= (window-end output-window) (point-max))
                          (recenter -1)
                          (save-buffer))))))))))
     (eval
      `(deferred:error it
        (lambda (err)
          (chatgpt--stop-wait ,saved-id)
          (string-match "\"Error('\\(.*\\)')\"" (error-message-string err))
          (let ((error-str (match-string 1 (error-message-string err))))
            (chatgpt--insert-error error-str
                                   ,saved-id
                                   chatgpt-last-use-buffer)
            ;; (when (yes-or-no-p (format "Error encountered. Reset chatgpt (If reset doesn't work, try \"\"pkill ms-playwright/firefox\"\" in the shell then reset)?" error-str))
            ;;   (chatgpt-reset))
            (with-current-buffer chatgpt-last-use-buffer
              (goto-char (point-max))
              (let ((output-window (get-buffer-window (current-buffer))))
                (when output-window
                  (with-selected-window output-window
                    (goto-char (point-max))
                    (unless (>= (window-end output-window) (point-max))
                      (recenter -1)
                      (save-buffer)))))))))))))

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
       (format "%s\n\n%s" (chatgpt-read-query "ChatGPT Custom Prompt: ") query) use-model)
    (if-let (format-string (cdr (assoc query-type chatgpt-query-format-string-map)))
        (chatgpt--query
         (format format-string query) use-model)
      (error "No format string associated with 'query-type' %s. Please customize 'chatgpt-query-format-string-map'" query-type))))


;;;###autoload
(defun chatgpt-reask ()
  ;; switch chatgpt-use-model between different model
  ;; but with save conversation history
  (interactive)
  (setq chatgpt-check-running-flag t)
  (message chatgpt-last-use-model)
  (message (buffer-name chatgpt-last-use-buffer))
  ;; wait until chatgpt-check-running-flag is nil
  (if chatgpt-running-flag
      (progn
        (message "ChatGPT is running. Please wait until it finishes.")
        (while chatgpt-check-running-flag
          (sleep-for 0.1))))

  (setq chatgpt-check-running-flag nil)

  (let* ((excluded-models '(chatgpt-last-use-model))
         (filtered-choices (cl-set-difference chatgpt-ai-choices
                                              excluded-models
                                              :test #'string=))
         (model (ivy-read "Choose model: " filtered-choices)))
    (progn
      (with-current-buffer chatgpt-last-use-buffer
        (save-excursion
          (goto-char chatgpt-last-response)
          (message "loaded chatgpt-last-response: %s" chatgpt-last-response)
          ;; (delete-region chatgpt-last-response (point-max)))))
          (delete-region (point) (point-max))))
      (chatgpt--query-stream chatgpt-last-query model nil chatgpt-last-use-buffer t))))


;;;###autoload
(defun chatgpt-again ()
  ;; switch chatgpt-use-model between gpt35 and gpt4
  ;; but with save conversation history
  (interactive)
  (setq chatgpt-check-running-flag t)
  (message chatgpt-last-use-model)
  (message (buffer-name chatgpt-last-use-buffer))
  ;; wait until chatgpt-check-running-flag is nil
  (if chatgpt-running-flag
      (progn
        (message "ChatGPT is running. Please wait until it finishes.")
        (while chatgpt-check-running-flag
          (sleep-for 0.1))))
  (setq chatgpt-check-running-flag nil)

  (progn
    (with-current-buffer chatgpt-last-use-buffer
      (save-excursion
        (goto-char chatgpt-last-response)
        (message "loaded chatgpt-last-response: %s" chatgpt-last-response)
        ;; (delete-region chatgpt-last-response (point-max)))))
        (delete-region (point) (point-max)))))

  (chatgpt--query-stream chatgpt-last-query chatgpt-last-use-model nil chatgpt-last-use-buffer t))


(defun chatgpt--query-stream (query use-model &optional recursive use-buffer-name reuse)
  (unless chatgpt-process
    (chatgpt-init))

  (when (and chatgpt-running-flag (not recursive))
    (message "ChatGPT is running. Please try again after it completes or use chatgpt-stop first.")
    (return-from chatgpt--query-stream nil))

  ;; (message "DEBUG: query=%s, use-model=%s, recursive=%s, use-buffer-name=%s, reuse=%s"
  ;;          query use-model recursive use-buffer-name reuse)
  (if recursive
      (progn
        (chatgpt-display use-buffer-name)
        (setq chatgpt-running-flag t))

    (progn
      (setq use-buffer-name (chatgpt-display))
      (setq chatgpt-last-query query)
      (setq chatgpt-last-use-buffer use-buffer-name)
      (setq chatgpt-last-use-model use-model)))
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
        (chatgpt--insert-query query saved-id use-model use-buffer-name)))


    ;; (message "DEBUG: Calling EPC with query_with_id=%s, recursive-model=%s, reuse=%s, buffer-name=%s"
    ;;          query_with_id recursive-model reuse (buffer-name use-buffer-name))

    (deferred:$
     (deferred:$
      (epc:call-deferred chatgpt-process 'querystream (list query_with_id recursive-model reuse (buffer-name use-buffer-name)))

      (deferred:nextc it
                      #'(lambda (response)
                          (with-current-buffer use-buffer-name
                            (save-excursion
                              (if (numberp next-recursive)
                                  (goto-char next-recursive)
                                (chatgpt--goto-identifier chatgpt-id use-buffer-name))
                              (if chatgpt-check-running-flag
                                  (progn
                                    (setq next-recursive nil)
                                    (save-buffer))
                                (if (= (plist-get response :type) 0)  ; Normal response
                                    (if (plist-get response :message) ; Check if message exists
                                        (progn
                                          (insert (plist-get response :message))
                                          (goto-char (point-max))
                                          (setq next-recursive (point)))
                                      (progn  ; End of stream
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
                                        (save-buffer)))
                                  (progn ; Error response
                                    (insert (format "ERROR: %s" (plist-get response :message)))
                                    (setq next-recursive nil)
                                    (save-buffer))))))
                          (if next-recursive
                              (chatgpt--query-stream query_with_id recursive-model next-recursive use-buffer-name)
                            (progn
                              (setq chatgpt-check-running-flag nil)
                              (setq chatgpt-running-flag nil))))))
     (deferred:error it
                     `(lambda (err)
                        (message "err is:%s" err)
                        (with-current-buffer ,use-buffer-name
                          (save-excursion
                            (if (numberp next-recursive)
                                (goto-char next-recursive)
                              (chatgpt--goto-identifier ,saved-id ,use-buffer-name))
                            (chatgpt--clear-line)
                            (insert (format "ERROR: %s" (error-message-string err)))
                            (save-buffer)))
                        (setq chatgpt-check-running-flag nil)
                        (setq chatgpt-running-flag nil))))))


(defun chatgpt--query-by-type-stream (query query-type use-model)
  (if (equal query-type "custom")
      (chatgpt--query-stream
       (format "%s\n\n%s" (chatgpt-read-query "ChatGPT Custom Prompt: ") query) use-model)
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
                       (chatgpt-read-query "ChatGPT Query: "))
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
                       (chatgpt-read-query "ChatGPT Query: "))
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
                       (chatgpt-read-query "ChatGPT Stream Query: "))
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
                       (chatgpt-read-query "ChatGPT Stream Query: "))
                     (unless use-model (read-string "GPT model: " chatgpt-default-model))))
  (if (region-active-p)
      (chatgpt-query-by-type-stream query use-model)
    (chatgpt--query-stream query use-model)))


;;;###autoload
(defun chatgpt-query-stream-default (query)
  (interactive (list (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (chatgpt-read-query "ChatGPT Stream Query: "))))
  (chatgpt-query-stream query
                       (if (null chatgpt-last-use-model)
                           chatgpt-default-model
                         chatgpt-last-use-model)))

;;;###autoload
(defun chatgpt-query-stream-second-preferest (query)
  (interactive (list (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (chatgpt-read-query "ChatGPT Stream Query: "))))
  (chatgpt-query-stream query chatgpt-second-preferest-model))


;;;###autoload
(defun chatgpt-query-stream-choose (query choice)
  (interactive
   (list
    (if (region-active-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (chatgpt-read-query "ChatGPT Stream Query: "))
    nil))  ; Default choice to nil when called interactively
  (let ((model (or choice
                   (let ((choices chatgpt-ai-choices))
                     (ivy-read "Choose one: " choices
                               :initial-input chatgpt-last-use-model)))))
    (chatgpt-query-stream query model)))


;;;###autoload
(defun chatgpt-query-default (query)
  (interactive (list (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (chatgpt-read-query "ChatGPT Stream Query: "))))
  (chatgpt-query query chatgpt-default-model))

(defun chatgpt-apply-region-diff-to-buffer (start end)
  "Apply a unified diff from the selected region to another buffer in the same window.
Follows strict line ordering, tracks first change position, and ensures undo correctness.
Additionally, ignores the (wrong) hunk-header line numbers, instead searching the next remove/context/add line in the target buffer to anchor changes."
  (interactive "r")
  (let ((diff-buffer (current-buffer))
        (target-buffer nil)
        (lines nil)
        (first-change-pos nil)
        (success t)
        ;; We'll store a line-number-like variable but won't use it
        ;; because the hunk header info is said to be wrong:
        (current-line nil))  ;; Tracks "wanted" line number from hunk-header (not actually used).

    ;; Find the target buffer in the same window
    (dolist (win (window-list))
      (let ((buf (window-buffer win)))
        (unless (eq buf diff-buffer)
          (setq target-buffer buf))))

    (unless target-buffer
      (error "No other buffer found in the same window to apply diff"))

    ;; Parse the diff content
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          (cond
           ;; Ignore file metadata lines (--- and +++)
           ((or (string-prefix-p "+++" line)
                (string-prefix-p "---" line))
            nil)

           ;; Extract @@ hunk header information
           ((string-match "^@@ -\\([0-9]+\\),?\\([0-9]*\\) \\+\\([0-9]+\\),?\\([0-9]*\\) @@" line)
            ;; We'll store the new-file line number, but won't rely on it for movement:
            (setq current-line (string-to-number (match-string 3 line)))
            (push (cons 'hunk-header current-line) lines))

           ;; Actual diff content lines
           (t
            (push (cons (cond ((string-prefix-p "-" line) 'remove)
                              ((string-prefix-p "+" line) 'add)
                              (t 'context))
                        (substring line 1))
                  lines))))
        (forward-line 1)))

    ;; Reverse to preserve the original top-to-bottom order
    (setq lines (reverse lines))

    ;; Apply changes to the target buffer
    (with-current-buffer target-buffer
      (undo-boundary)
      (save-excursion
        (dolist (hunk lines)
          (cond
           ((eq (car hunk) 'hunk-header)
            ;; The cdr is the new-file line number, but we won't use it.
            ;; Instead, we look ahead in the diff lines for the next
            ;; remove/context/add line to anchor our position.
            (let* ((lines-after-this (cdr (memq hunk lines))) ;; everything after THIS hunk in `lines`
                   (anchor
                    (cl-find-if
                     (lambda (l)
                       (memq (car l) '(remove context add)))
                     lines-after-this)))
              (when anchor
                (goto-char (point-min))
                (let ((anchor-text (cdr anchor)))
                  ;; If the anchor line is not found, we error out.
                  (unless (search-forward anchor-text nil t)
                    (error "Cannot find anchor text for hunk: %S" anchor-text))
                  (beginning-of-line)))))

           ((eq (car hunk) 'remove)
            (let ((expected-line (cdr hunk))
                  (actual-line (buffer-substring-no-properties
                                (line-beginning-position)
                                (line-end-position))))
              (if (string= expected-line actual-line)
                  (progn
                    (unless first-change-pos (setq first-change-pos (point)))
                    (delete-region (line-beginning-position) (line-end-position))
                    (when (not (eobp))
                      (delete-char 1))) ;; remove the newline if not end-of-buffer
                (error "Error: Expected line for removal not found.\nExpected: %S\nFound:    %S"
                       expected-line actual-line))))

           ((eq (car hunk) 'add)
            (unless first-change-pos (setq first-change-pos (point)))
            (insert (cdr hunk) "\n"))

           ((eq (car hunk) 'context)
            (let ((expected-line (cdr hunk))
                  (actual-line (buffer-substring-no-properties
                                (line-beginning-position)
                                (line-end-position))))
              (unless (string= expected-line actual-line)
                ;; Log mismatch for debugging
                (with-current-buffer (get-buffer-create "*Diff Mismatch Log*")
                  (goto-char (point-max))
                  (insert (format "Context mismatch!\nExpected: %s\nFound:    %s\n\n"
                                  expected-line actual-line)))
                (error "Context mismatch.\nExpected: %S\nFound:    %S"
                       expected-line actual-line))
              (forward-line 1)))))
      ;; After applying all hunks:

      ;; Move cursor to the first modified position
      (when first-change-pos
        (goto-char first-change-pos))

      (undo-boundary)

      ;; If you use Evil mode, refresh cursor
      (when (bound-and-true-p evil-mode)
        (evil-normal-state)
        (evil-refresh-cursor)))

    (if success
        (message "Diff applied successfully to %s (cursor moved to first change)"
                 (buffer-name target-buffer))
      (message "Diff application aborted due to errors.")))))

(defun chatgpt-format-file-name (file)
  "Format FILE name with proper face and properties."
  (if (file-directory-p file)
      (propertize file
                  'face 'diredfl-dir-name  ; using dired's directory face
                  'full-path file)
    file))

(defun chatgpt-get-git-root ()
  "Return the root directory of the current Git repository, or nil if not in a Git repo."
  (when-let ((git-root (locate-dominating-file default-directory ".git")))
    (expand-file-name git-root)))

(defun chatgpt-list-files (directory)
  "List all files and folders in DIRECTORY, including hidden files."
  (directory-files directory t ".*" t)) ;; Include hidden files

(defun chatgpt-select-file-or-folder (type)
  "Select a file or folder interactively.
- @ expects a file.
- # expects a folder.
- $ inserts @project."
  (let* ((base-dir (cond
                    ((equal type "@") default-directory)
                    ((equal type "#") default-directory)
                    ((equal type "$") (or (chatgpt-get-git-root)
                                        (user-error "Not inside a Git repository")))))
         (files (chatgpt-list-files base-dir))
         (formatted-files (mapcar #'chatgpt-format-file-name files))
         (selected (completing-read "Choose a file or folder: "
                                  (lambda (string pred action)
                                    (if (eq action 'metadata)
                                        '(metadata (category . chatgpt-files))
                                      (complete-with-action
                                       action formatted-files string pred)))
                                  nil t)))

    (cond
     ((equal type "$") "@project")  ;; Ensure $ is replaced with @project
     ((and (equal type "@") (file-regular-p selected))
      (format "@[[%s]]" selected)) ;; Return formatted file path
     ((and (equal type "#") (file-directory-p selected))
      (format "#[[%s]]" selected)) ;; Return formatted folder path
     (t
      (minibuffer-message "⚠️ Invalid selection. Expected %s." (if (equal type "@") "a file" "a folder"))
      nil)))) ;; Return nil if invalid selection

(defvar chatgpt-last-special-char-time 0
  "Timestamp of the last special character (@, #, $) insertion.")

 (defun chatgpt-insert-path ()
   "Detect @, #, or $ in the minibuffer and trigger file/folder/Git root selection,
ensuring correct format and preventing duplicate insertion.
Rapidly typing the same character twice (within 0.3 seconds) will insert it literally."
   (when (minibufferp)  ;; Only run in minibuffer
     (let ((key (this-command-keys)))
      ;; Get current time
      (let ((current-time (float-time)))
        ;; Check if the character was typed very quickly after the last one (double-tap)
        (if (member key '("@" "#" "$"))
            (if (< (- current-time chatgpt-last-special-char-time) 0.3)
                (progn
                  (setq chatgpt-last-special-char-time 0) ;; Reset timer
                  (cond
                   ((equal key "@") (delete-char -2) (when-let ((result (chatgpt-select-file-or-folder "@"))) (insert result)))
                   ((equal key "#") (delete-char -2) (when-let ((result (chatgpt-select-file-or-folder "#"))) (insert result)))
                   ((equal key "$") (delete-char -2) (insert "@project"))))
              (setq chatgpt-last-special-char-time current-time)))))))

(defun chatgpt-read-query (prompt)
  "Read a ChatGPT query with path insertion support."
  (let ((chatgpt--in-query-minibuffer t))
    (read-from-minibuffer prompt)))

(defun chatgpt-setup-path-insertion ()
  "Setup path insertion only for ChatGPT query minibuffers."
  (when chatgpt--in-query-minibuffer
    (add-hook 'post-self-insert-hook #'chatgpt-insert-path nil t)))

(defun chatgpt-cleanup-path-insertion ()
  "Clean up path insertion hook after minibuffer use."
  (when chatgpt--in-query-minibuffer
    (remove-hook 'post-self-insert-hook #'chatgpt-insert-path t)))

;; Add both setup and cleanup hooks
(add-hook 'minibuffer-setup-hook #'chatgpt-setup-path-insertion)
(add-hook 'minibuffer-exit-hook #'chatgpt-cleanup-path-insertion)

(provide 'chatgpt)
;;; chatgpt.el ends here
