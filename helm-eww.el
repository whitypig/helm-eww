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
;;

;;; Todo:
;; * Suport deletion of a history in helm-eww-history buffer.

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


;; eww buffer management

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
    (switch-to-buffer buffer)))

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
    (format "^%s: %s$"
            (regexp-quote (buffer-name (current-buffer)))
            (regexp-quote (eww-current-title))))
   (t
    nil)))

(defun helm-eww--get-buffer-list ()
  (sort (cl-remove-if-not (lambda (b) (eq 'eww-mode
                                          (buffer-local-value 'major-mode b)))
                          (buffer-list))
        (lambda (a b)
          (string< (buffer-name a) (buffer-name b)))))

;;;###autoload
(defun helm-eww-buffers-list-buffers ()
  (interactive)
  (helm :sources helm-source-eww-buffers
        :preselect (helm-eww-buffers-get-preselection)))

(defun eww-next-buffer ()
  (interactive)
  (when (eq major-mode 'eww-mode)
    (eww--move-buffer 1)))

(defun eww-previous-buffer ()
  (interactive)
  (when (eq major-mode 'eww-mode)
    (eww--move-buffer -1)))

(defun eww--move-buffer (delta)
  (let* ((lst (helm-eww--get-buffer-list))
         (len (length lst))
         (pos (cl-position (current-buffer) lst))
         (next-pos (and pos (% (+ delta pos len) len)))
         (next-buf (nth next-pos lst)))
    (helm-eww-buffers--display-buffer next-buf)))


;; history management

(defvar eww-current-buffer)

(defun helm-eww-history-candidates ()
  ;; code from `eww-list-histories' in eww.el
  (let ((domain-length 0)
        (title-length 0)
        (histories (buffer-local-value 'eww-history helm-current-buffer))
        url title format-string start)
    (with-helm-current-buffer
      (setq-local eww-current-buffer (current-buffer)))
    (dolist (history histories)
      (setq domain-length (max domain-length (length (plist-get history :url))))
      (setq title-length (max title-length (length (plist-get history :title)))))
    (setq format-string (format "%%-%ds %%-%ds" title-length domain-length))
    (cl-loop for history in histories
             for ix from 0
             for title = (plist-get history :title)
             for url = (plist-get history :url)
             when (and title url)
             collect (cons (format format-string title url)
                           ;; Let each candidate have its id so that
                           ;; we can later use it to delete history.
                           ;; value is cons cell (ix . history)
                           (cons ix history)))))

(defun helm-eww-history-next-line (&optional arg)
  (interactive "p")
  (let ((helm-move-to-line-cycle-in-source t))
    (helm-next-line arg)
    (helm-eww-history--move-line-action)))

(defun helm-eww-history-previous-line (&optional arg)
  (interactive "p")
  (let ((helm-move-to-line-cycle-in-source t))
    (helm-previous-line arg)
    (helm-eww-history--move-line-action)))

(defun helm-eww-history--move-line-action ()
  "Temporarily display history under point in helm-window."
  (with-helm-window
    (let ((history (cdr (helm-get-selection)))
          (win (get-buffer-window helm-current-buffer)))
      (when (window-live-p win)
        (with-selected-window win
          (helm-eww-history--display-history history))))))

(defun helm-eww-history--display-history (history)
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t)
        (text (plist-get history :text))
        (pos (plist-get history :point)))
    (cond
     (text
      (erase-buffer)
      (insert text))
     (t
      ;; Note: desktop.el may restore the previous state, so all the
      ;; pages in history can be empty. In that case, should we reload
      ;; the page? For now, we have decided to do nothing.
      ;; (eww-reload)
      nil
      ))
    (and (integerp pos) (goto-char pos))))

(defun helm-eww-history-delete-history (_history)
  ;; _history is a history under the point and is also considered as
  ;; a marked candidate.
  ;; We cannot simply delete a history by comparing its value because
  ;; there can be the same history entries in eww-history.
  (let* ((delete-indices
          ;; collect id of history to be deleted.
          (cl-loop for candidate in (helm-marked-candidates)
                   ;; #'helm-marked-candidates ALWAYS returns at least
                   ;; one candidate.
                   collect (car candidate)))
         ;; collect not-to-be-deleted histories.
         (new-histories
          (cl-loop for ix from 0
                   for history in eww-history
                   with lst = nil
                   unless (memq ix delete-indices)
                   collect history)))
    (setq-local eww-history new-histories)
    ;; then update helm buffer with new eww-history
    (helm-force-update)))

(defun helm-eww-history-run-delete-history-persistent ()
  "Delete history without quitting helm."
  (interactive)
  (with-helm-alive-p
    (helm-attrset 'kill-action '(helm-eww-history-delete-history . never-split))
    (helm-execute-persistent-action 'kill-action)))

(setq helm-eww-history-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-n") #'helm-eww-history-next-line)
    (define-key map (kbd "C-p") #'helm-eww-history-previous-line)
    (define-key map (kbd "C-c d") #'helm-eww-history-run-delete-history-persistent)
    map))

(defvar helm-source-eww-history
  (helm-build-sync-source "eww history"
    :candidates #'helm-eww-history-candidates
    :migemo t
    :keymap helm-eww-history-map))

;;;###autoload
(defun helm-eww-history ()
  (interactive)
  (if (or (not (eq major-mode 'eww-mode))
          (null eww-history))
      (error "No eww-histories are defined")
    (let ((current-text (buffer-string))
          (current-pos (point))
          (prev-text (plist-get (car eww-history) :text))
          (prev-pos (plist-get (car eww-history) :point))
          (history nil)
          (inhibit-read-only t)
          (inhibit-modification-hooks t))
      ;; Our strategy is that we first save curent text and position
      ;; in variables text and pos. Then render the most recent
      ;; history because, when calling helm, the first element in
      ;; eww-history is selected.
      ;; When we get back from helm, either by
      ;; canceling or choosing a history, the content of the buffer
      ;; must be changed, so we restore the buffer content with
      ;; saved text and pos.
      ;; If a user has selected some history, then save the current
      ;; page into history by calling `eww-save-history' and restore
      ;; the chosen history by calling `eww-restore-history.'
      ;;
      ;; insert the content of the most recent history
      (erase-buffer)
      (when prev-text
        (insert prev-text)
        (and (integerp prev-pos) (goto-char prev-pos)))
      ;; call helm
      (setq history (cdr (helm :sources 'helm-source-eww-history)))
      ;; restore the original content
      (erase-buffer)
      (insert current-text)
      (goto-char current-pos)
      (when history
        ;; restore the selected history
        (eww-save-history)
        (eww-restore-history history)))))

(provide 'helm-eww)

;; Local Variables:
;; coding: utf-8-unix
;; indent-tabs-mode: nil
;; End:

;; helm-eww.el ends here
