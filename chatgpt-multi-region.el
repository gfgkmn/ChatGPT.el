;;; chatgpt-multi-region.el --- Multi-region support for ChatGPT.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Assistant
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: ai, regions
;; URL: https://github.com/joshcho/ChatGPT.el

;;; Commentary:
;; This package provides multi-region collection and querying functionality
;; for ChatGPT.el. It allows users to collect multiple regions from different
;; buffers and query them together.

;;; Code:

(require 'cl-lib)

;;; Customization

(defgroup chatgpt-multi-region nil
  "Multi-region support for ChatGPT."
  :prefix "chatgpt-mr-"
  :group 'chatgpt)

(defcustom chatgpt-mr-auto-label t
  "Whether to automatically generate labels for regions."
  :type 'boolean
  :group 'chatgpt-multi-region)

(defcustom chatgpt-mr-show-line-numbers t
  "Whether to show line numbers in region labels."
  :type 'boolean
  :group 'chatgpt-multi-region)

(defcustom chatgpt-mr-max-regions 10
  "Maximum number of regions to collect."
  :type 'integer
  :group 'chatgpt-multi-region)

;;; Variables

(defvar chatgpt-mr-regions nil
  "List of collected regions.
Each element is a plist with keys:
:buffer - buffer object
:start - start position
:end - end position
:start-line - start line number
:end-line - end line number
:content - region content
:label - user or auto-generated label
:file - file path (if buffer has file)")

(defvar chatgpt-mr-counter 0
  "Counter for auto-generating region labels.")

(defvar chatgpt-mr-mode nil
  "Whether multi-region mode is currently active.")

(defvar chatgpt-mr--original-yank-function nil
  "Store the original yank function when multi-region mode is enabled.")

;;; Core Functions

(defun chatgpt-mr--generate-label (buffer start-line end-line)
  "Generate automatic label for region in BUFFER from START-LINE to END-LINE."
  (cl-incf chatgpt-mr-counter)
  (let ((file-name (if (buffer-file-name buffer)
                       (file-name-nondirectory (buffer-file-name buffer))
                     (buffer-name buffer))))
    (if chatgpt-mr-show-line-numbers
        (format "Region %d (%s:%d-%d)" chatgpt-mr-counter file-name start-line end-line)
      (format "Region %d (%s)" chatgpt-mr-counter file-name))))

(defun chatgpt-mr--get-language-for-buffer (buffer)
  "Determine programming language for BUFFER based on major mode."
  (with-current-buffer buffer
    (let ((mode-name (symbol-name major-mode)))
      (cond
       ((string-match "python" mode-name) "python")
       ((string-match "javascript\\|js" mode-name) "javascript")
       ((string-match "typescript\\|ts" mode-name) "typescript")
       ((string-match "java" mode-name) "java")
       ((string-match "c\\+\\+" mode-name) "cpp")
       ((string-match "c-mode" mode-name) "c")
       ((string-match "emacs-lisp\\|lisp" mode-name) "elisp")
       ((string-match "shell\\|bash" mode-name) "bash")
       ((string-match "html" mode-name) "html")
       ((string-match "css" mode-name) "css")
       ((string-match "json" mode-name) "json")
       ((string-match "yaml" mode-name) "yaml")
       ((string-match "xml" mode-name) "xml")
       ((string-match "markdown" mode-name) "markdown")
       (t "text")))))

;;; Multi-Region Mode

;;;###autoload
(defun chatgpt-mr-mode-on ()
  "Turn on multi-region mode.
Clears existing regions and hooks into yank to auto-add regions."
  (interactive)
  (when chatgpt-mr-mode
    (message "Multi-region mode already active")
    (cl-return))

  ;; Clear existing regions when turning on
  (setq chatgpt-mr-regions nil
        chatgpt-mr-counter 0)

  ;; Enable multi-region mode
  (setq chatgpt-mr-mode t)

  ;; Hook into Evil's yank if available
  (when (and (boundp 'evil-mode) evil-mode)
    (advice-add 'evil-yank :after #'chatgpt-mr--evil-yank-advice))

  ;; Hook into regular yank as fallback
  (advice-add 'yank :after #'chatgpt-mr--yank-advice)

  (message "Multi-region mode ON - yank (y) will now add regions automatically"))

;;;###autoload
(defun chatgpt-mr-mode-off ()
  "Turn off multi-region mode and remove yank hooks."
  (interactive)
  (unless chatgpt-mr-mode
    (message "Multi-region mode already inactive")
    (cl-return))

  ;; Disable multi-region mode
  (setq chatgpt-mr-mode nil)

  ;; Remove hooks
  (advice-remove 'evil-yank #'chatgpt-mr--evil-yank-advice)
  (advice-remove 'yank #'chatgpt-mr--yank-advice)

  (message "Multi-region mode OFF"))

;;;###autoload
(defun chatgpt-mr-toggle ()
  "Toggle multi-region mode on/off."
  (interactive)
  (if chatgpt-mr-mode
      (chatgpt-mr-mode-off)
    (chatgpt-mr-mode-on)))

(defun chatgpt-mr--evil-yank-advice (&rest args)
  "Advice function to add region after evil yank."
  (when (and chatgpt-mr-mode
             (evil-visual-state-p)
             (< (length chatgpt-mr-regions) chatgpt-mr-max-regions))
    (let ((start (region-beginning))
          (end (region-end)))
      (chatgpt-mr--add-region-internal start end))))

(defun chatgpt-mr--yank-advice (&rest args)
  "Advice function to add region after regular yank when region was active."
  (when (and chatgpt-mr-mode
             (region-active-p)
             (< (length chatgpt-mr-regions) chatgpt-mr-max-regions))
    (chatgpt-mr--add-region-internal (region-beginning) (region-end))))

(defun chatgpt-mr--add-region-internal (start end)
  "Internal function to add region from START to END."
  (let* ((buffer (current-buffer))
         (start-line (line-number-at-pos start))
         (end-line (line-number-at-pos end))
         (content (buffer-substring-no-properties start end))
         (label (chatgpt-mr--generate-label buffer start-line end-line))
         (file-path (buffer-file-name buffer))
         (region-plist (list :buffer buffer
                            :start start
                            :end end
                            :start-line start-line
                            :end-line end-line
                            :content content
                            :label label
                            :file file-path)))
    (push region-plist chatgpt-mr-regions)
    (message "Added %s (Total: %d)" label (length chatgpt-mr-regions))))

;;;###autoload
(defun chatgpt-mr-add-region (&optional label)
  "Add current region to collection with optional LABEL."
  (interactive)
  (unless (region-active-p)
    (user-error "No active region"))

  (when (>= (length chatgpt-mr-regions) chatgpt-mr-max-regions)
    (user-error "Maximum number of regions (%d) reached" chatgpt-mr-max-regions))

  (chatgpt-mr--add-region-internal (region-beginning) (region-end))
  (deactivate-mark))

;;;###autoload
(defun chatgpt-mr-clear-regions ()
  "Clear all collected regions."
  (interactive)
  (when (or (null chatgpt-mr-regions)
            (yes-or-no-p (format "Clear all %d collected regions? " (length chatgpt-mr-regions))))
    (setq chatgpt-mr-regions nil
          chatgpt-mr-counter 0)
    (message "All regions cleared")))

;;;###autoload
(defun chatgpt-mr-list-regions ()
  "List all collected regions in a buffer."
  (interactive)
  (if (null chatgpt-mr-regions)
      (message "No regions collected%s"
               (if chatgpt-mr-mode " (multi-region mode is ON)" ""))
    (let ((buffer (get-buffer-create "*ChatGPT Multi-Regions*")))
      (with-current-buffer buffer
        (erase-buffer)
        (insert (format "Collected Regions (%d)%s:\n\n"
                        (length chatgpt-mr-regions)
                        (if chatgpt-mr-mode " - Mode: ON" " - Mode: OFF")))
        (cl-loop for region in (reverse chatgpt-mr-regions)
                 for i from 1
                 do (insert (format "%d. %s\n" i (plist-get region :label)))
                 do (insert (format "   Buffer: %s\n" (buffer-name (plist-get region :buffer))))
                 do (when (plist-get region :file)
                      (insert (format "   File: %s\n" (plist-get region :file))))
                 do (insert (format "   Lines: %d-%d\n"
                                   (plist-get region :start-line)
                                   (plist-get region :end-line)))
                 do (insert (format "   Content preview: %s...\n\n"
                                   (substring (plist-get region :content) 0
                                             (min 50 (length (plist-get region :content)))))))
        (goto-char (point-min))
        (view-mode 1))
      (display-buffer buffer))))

;;;###autoload
(defun chatgpt-mr-remove-region ()
  "Remove a region from collection interactively."
  (interactive)
  (if (null chatgpt-mr-regions)
      (message "No regions to remove")
    (let* ((choices (cl-loop for region in chatgpt-mr-regions
                            collect (plist-get region :label)))
           (selected (completing-read "Remove region: " choices nil t)))
      (setq chatgpt-mr-regions
            (cl-remove-if (lambda (region)
                           (string= (plist-get region :label) selected))
                         chatgpt-mr-regions))
      (message "Removed region: %s" selected))))

(defun chatgpt-mr--format-regions-for-query (prompt)
  "Format collected regions with PROMPT for ChatGPT query."
  (if (null chatgpt-mr-regions)
      (user-error "No regions collected. Use `chatgpt-mr-add-region` first")
    (let ((formatted-content (list prompt "\n")))
      (cl-loop for region in (reverse chatgpt-mr-regions)
               for i from 1
               do (let ((language (chatgpt-mr--get-language-for-buffer (plist-get region :buffer)))
                       (label (plist-get region :label))
                       (content (plist-get region :content)))
                    (push (format "\n%s:\n```%s\n%s\n```\n" label language content) formatted-content)))
      (string-join (reverse formatted-content) ""))))

;;; Integration with ChatGPT.el

;;;###autoload
(defun chatgpt-mr-query-stream (prompt &optional model)
  "Query ChatGPT with collected regions using stream interface.
PROMPT will be applied to all collected regions.
MODEL defaults to the last used model or chatgpt-default-model."
  (interactive (list (read-string "Prompt for all regions: ")
                     nil))
  (let ((formatted-query (chatgpt-mr--format-regions-for-query prompt))
        (use-model (or model
                      (bound-and-true-p chatgpt-last-use-model)
                      (bound-and-true-p chatgpt-default-model)
                      "claude")))
    (if (fboundp 'chatgpt--query-stream)
        (chatgpt--query-stream formatted-query use-model)
      (user-error "ChatGPT.el not properly loaded or chatgpt--query-stream not available"))))

;;; Status Display

(defun chatgpt-mr--mode-line-status ()
  "Return mode line status string for multi-region mode and collected regions."
  (when chatgpt-mr-mode
    (format " [MR:%s%d]"
            (if chatgpt-mr-mode "ON:" "OFF:")
            (length chatgpt-mr-regions))))

;; Add to mode line (optional, user can enable if desired)
;;;###autoload
(defun chatgpt-mr-enable-mode-line ()
  "Enable mode line display of multi-region mode status and count."
  (interactive)
  (add-to-list 'global-mode-string '(:eval (chatgpt-mr--mode-line-status))))

;;;###autoload
(defun chatgpt-mr-disable-mode-line ()
  "Disable mode line display of multi-region mode status."
  (interactive)
  (setq global-mode-string
        (remove '(:eval (chatgpt-mr--mode-line-status)) global-mode-string)))

;;; Key Bindings Helper

;; ;;;###autoload
;; (defun chatgpt-mr-setup-keys ()
;;   "Setup key bindings for simplified multi-region functionality."
;;   (interactive)
;;   (global-set-key (kbd "C-c g t") 'chatgpt-mr-toggle)      ; Toggle multi-region mode
;;   (global-set-key (kbd "C-c g a") 'chatgpt-mr-add-region)
;;   (global-set-key (kbd "C-c g l") 'chatgpt-mr-list-regions)
;;   (global-set-key (kbd "C-c g c") 'chatgpt-mr-clear-regions)
;;   (global-set-key (kbd "C-c g q") 'chatgpt-mr-query-stream-choose)
;;   (message "ChatGPT multi-region keys bound under C-c g prefix"))

(provide 'chatgpt-multi-region)
;;; chatgpt-multi-region.el ends here
