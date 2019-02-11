;;; y-misc.el --- Misc Routines

;; Copyright (C) 2018 yanyg<yygcode@gmail.com>

;; Author: yanyg<yygcode@gmail.com>
;; Maintainer: yanyg<yygcode@gmail.com>
;; Keyword: Emacs Initialization Customization Configuration
;; URL: https://ycode.org; http://ycode.org

;;; Commentary:

;; Common Routines

;;; Code:

(defun y/comment-or-uncomment-region-or-line()
  "Call `comment-or-uncomment-region'.  Use current line if no active region."
  (interactive)
  ;; (interactive "*r\nP")
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position)
                                 (line-end-position))))


;; add-to-list-multi

(defun y/add-to-list-multi(list-var element &rest others-element)
  "Prepend multiple elements ELEMENT and OTHERS-ELEMENT to list LIST-VAR."
  (interactive)
  (dolist (e (reverse others-element))
    (add-to-list list-var e))
  (add-to-list list-var element)
  (symbol-value list-var))
(defun y/add-to-list-multi-append(list-var element &rest others-element)
  "Append multiple elements ELEMENT and OTHERS-ELEMENT to list LIST-VAR."
  (interactive)
  (add-to-list list-var element t)
  (dolist (e others-element)
    (add-to-list list-var e t))
  (symbol-value list-var))

(defun y/match-string-prefix-list(string list-var)
  "Return first element in string LIST-VAR that matche STRING prefix."
  (let (result)
    (dolist (e list-var result)
      (when (string-match "yasnippet-" e)
        (setq result e)))
    result))

;; quelpa self update
(defun y/upgrade-quelpa()
  "Upgrade quelpa package."
  (interactive)
  (unless (package-installed-p 'quelpa)
    (user-error "Package `quelpa' does not installed"))
  (require 'quelpa)
  (quelpa-self-upgrade)
  (quelpa-upgrade)
  (quelpa-checkout-melpa)
  (with-temp-buffer
    (url-insert-file-contents
     "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

(defun y/date(&optional insert)
  "Show Today Date in echo area.  Insert to current buffer if INSERT."
  (interactive "P")
  (message (format-time-string "%Y-%m-%d"))
  (and insert
       (if buffer-read-only
           (error "Could not insert to read-only buffer")
         (insert (format-time-string "%Y-%m-%d")))))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;; source: https://emacs.stackexchange.com/a/24461
(defun revert-all-file-buffers()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in emacs will not be reverted. They
will be reverted though if they were modified outside emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
                 (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            ;; If the file exists and is readable, revert the buffer.
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; Otherwise, kill the buffer.
          (let (kill-buffer-query-functions) ; No query done when killing buffer
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))
(global-set-key (kbd "C-c C-x r") #'revert-all-file-buffers)

(defun y/view-lossage-timer()
  "Show view-lossage periodically."
  (interactive)
  (defvar y/--view-lossage-timer nil)
  (if y/--view-lossage-timer
      (progn
        (cancel-timer y/--view-lossage-timer)
        (setq y/--view-lossage-timer nil))
    (setq y/--view-lossage-timer (run-at-time nil .5 #'view-lossage))))
(global-set-key (kbd "C-c C-x v") #'y/view-lossage-timer)

(provide 'y-misc)

;;; y-misc.el ends here
