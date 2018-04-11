;;; helm-eww.el --- Helm interface for eww buffers.

;; Copyright (C) 2018 whitypig

;; Author: whitypig <whitypig@gmail.com>
;; URL: https://github.com/whitypig/helm-eww
;; Version: 0.01
;; Package-Requires: ((helm "") (cl-lib "") (eww "") (emacs ""))
;; Keywords: eww helm

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; (define-key eww-mode-map (kbd "S") #'helm-eww-buffers-list-buffers)
;; (define-key eww-mode-map (kbd "H") #'helm-eww-history)
;; (define-key eww-mode-map (kbd "C-c C-n") #'eww-next-buffer)
;; (define-key eww-mode-map (kbd "C-c C-p") #'eww-previous-buffer)

;;; Code

(require 'helm)
(require 'cl-lib)
(require 'eww)

(defvar eww-data)
(defun eww-current-title ()
  (if (boundp 'eww-current-title)
      ;; emacs24.4
      eww-current-title
    (plist-get eww-data :title)))

(unless (fboundp 'eww-current-url)
  (defun eww-current-url ()
    (if (boundp 'eww-current-url)
        ;; emacs24.4
        eww-current-url
      (plist-get eww-data :url))))

(defun eww-new (url)
  "Open a new page in a new eww buffer."
  (interactive (list (read-from-minibuffer "Enter URL or keywords: ")))
  (switch-to-buffer (generate-new-buffer "eww"))
  (eww-mode)
  (eww url))

;; eww buffers management
(defvar helm-eww-buffers-map
  (let ((map (copy-keymap helm-map)))
    (define-key map (kbd "C-n") #'helm-eww-buffers-next-line)
    (define-key map (kbd "C-p") #'helm-eww-buffers-previous-line)
    ;; need to override other moving keybinds such as C-v, M-v?
    map))

(defvar helm-eww-buffers--buffer-window nil)

(defun helm-eww-buffers-next-line (&optional arg)
  (interactive "p")
  (let ((helm-move-to-line-cycle-in-source t))
    (helm-next-line arg)
    (helm-eww-buffers--move-line-action)))

(defun helm-eww-buffers-previous-line (&optional arg)
  (interactive "p")
  (let ((helm-move-to-line-cycle-in-source t))
    (helm-previous-line arg)
    (helm-eww-buffers--move-line-action)))

(defun helm-eww-buffers--move-line-action ()
  (with-helm-window
    (let ((buffer (helm-get-selection)))
        (helm-eww-buffers--display-buffer buffer))))

(defun helm-eww-buffers--display-buffer (buffer)
  (cond
   ((window-live-p (get-buffer-window helm-current-buffer))
    (setq helm-eww-buffers--buffer-window (get-buffer-window helm-current-buffer)))
   ((window-live-p helm-eww-buffers--buffer-window)
    nil)
   ((eq major-mode 'eww-mode)
    ;; If we are in eww-mode buffer
    (setq helm-eww-buffers--buffer-window (selected-window)))
   (t
    ;; todo: split window
    (error "We have to split window!")))
  (set-window-buffer helm-eww-buffers--buffer-window buffer))

(defvar helm-source-eww-buffers
  (helm-build-sync-source "eww buffers"
    :candidates #'helm-eww-buffers-candidates
    :action #'helm-eww-buffers-select-buffer
    :migemo t
    :keymap helm-eww-buffers-map))

(defun helm-eww-buffers-select-buffer (buffer)
  (unless (eq (current-buffer) buffer)
    (set-window-buffer (selected-window) buffer)))

(defun helm-eww-buffers-candidates ()
  (cl-loop for buffer in (buffer-list)
           when (eq 'eww-mode (buffer-local-value 'major-mode buffer))
           collect (cons (with-current-buffer buffer
                           ;; need to put more information?
                           (format "%s: %s" (buffer-name buffer) (eww-current-title)))
                         buffer)
           into ret
           finally return (sort ret
                                ;; sort by buffer name
                                (lambda (a b)
                                  (string< (buffer-name (cdr a))
                                           (buffer-name (cdr b)))))))

(defun helm-eww-buffers-get-preselection ()
  (cond
   ((eq major-mode 'eww-mode)
    (format "%s: %s" (buffer-name (current-buffer)) (eww-current-title)))
   (t
    nil)))

;;;###autoload
(defun helm-eww-buffers-list-buffers ()
  (interactive)
  (helm :sources helm-source-eww-buffers
        :preselect (helm-eww-buffers-get-preselection)))

(defun eww--get-buffer-list ()
  (sort (cl-remove-if-not (lambda (b) (eq 'eww-mode
                                          (buffer-local-value 'major-mode b)))
                          (buffer-list))
        (lambda (a b)
          (string< (buffer-name a) (buffer-name b)))))

(defun eww-next-buffer ()
  (interactive)
  (when (eq major-mode 'eww-mode)
    (eww--move-buffer 1)))

(defun eww-previous-buffer ()
  (interactive)
  (when (eq major-mode 'eww-mode)
    (eww--move-buffer -1)))

(defun eww--move-buffer (delta)
  (let* ((lst (eww--get-buffer-list))
         (len (length lst))
         (pos (cl-position (current-buffer) lst))
         (next-pos (and pos (% (+ delta pos len) len)))
         (next-buf (nth next-pos lst)))
    (helm-eww-buffers--display-buffer next-buf)))

;; Thanks to eww history management by rubikitch at
;; http://rubikitch.com/f/helm-eww.el.
(defun helm-eww-history-candidates ()
  (cl-loop with hash = (make-hash-table :test 'equal)
           for b in (buffer-list)
           when (eq (buffer-local-value 'major-mode b) 'eww-mode)
           append (with-current-buffer b
                    (clrhash hash)
                    (puthash (eww-current-url) t hash)
                    (cons
                     (cons (format "%s (%s) <%s>" (eww-current-title) (eww-current-url) b) b)
                     (cl-loop for pl in eww-history
                              unless (gethash (plist-get pl :url) hash)
                              collect
                              (prog1 (cons (format "%s (%s) <%s>"
                                                   (plist-get pl :title)
                                                   (plist-get pl :url) b)
                                           (cons b pl))
                                (puthash (plist-get pl :url) t hash)))))))

(defun helm-eww-history-browse (buf-hist)
  (if (bufferp buf-hist)
      (switch-to-buffer buf-hist)
    (switch-to-buffer (car buf-hist))
    (eww-save-history)
    (eww-restore-history (cdr buf-hist))))

(defvar helm-source-eww-history
  (helm-build-sync-source "eww history"
    :candidates #'helm-eww-history-candidates
    :migemo t
    :action  #'helm-eww-history-browse))

;;;###autoload
(defun helm-eww-history ()
  (interactive)
  (helm :sources 'helm-source-eww-history))

(provide 'helm-eww)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;; helm-eww.el ends here
