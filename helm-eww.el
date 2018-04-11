;;; helm-eww.el --- Helm interface for eww buffers.

;; Copyright (C) 2018 whitypig

;; Author: whitypig <whitypig@gmail.com>
;; URL:
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

;;; Code

(require 'helm)
(require 'cl-lib)

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
   ((get-buffer-window helm-current-buffer)
    (setq helm-eww-buffers--buffer-window (get-buffer-window helm-current-buffer)))
   ((window-live-p helm-eww-buffers--buffer-window)
    nil)
   (t
    ;; todo: split window
    (error "We have to split window!")))
  (set-window-buffer helm-eww-buffers--buffer-window buffer))

(defvar helm-source-eww-buffers
  (helm-build-sync-source "helm-eww-buffers"
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
           ;; todo: need to sort eww buffers in some order?
           collect (cons (with-current-buffer buffer
                           (format "%s" (plist-get eww-data :title)))
                         buffer)))

(defun helm-eww-buffers-get-preselection ()
  (cond
   ((derived-mode-p 'eww-mode)
    (plist-get eww-data :title))
   (t
    nil)))

;;;###autoload
(defun helm-eww-buffers-list-buffers ()
  (interactive)
  (helm :sources helm-source-eww-buffers
        ;; We will need to preselect a candidate if eww buffers are
        ;; sorted in some order.
        :preselect (helm-eww-buffers-get-preselection)))

(provide 'helm-eww)
