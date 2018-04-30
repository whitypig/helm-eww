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

(defvar helm-eww-history-map
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

;; bookmark management

(defcustom helm-eww-bookmark-bookmarks-filename "helm-eww-bookmarks"
  ""
  :type 'string
  :group 'helm-eww)

;; (
;; (section1 . ((url1 title1) (url2 title2)))
;; (section2 . ((url3 title3) (url4 title4)))
;; )
(defvar helm-eww-bookmark-bookmarks nil
  "Alist of bookmarks managed by helm-eww, whose key is a section name
  and its value is list of bookmarks. Each bookmark is represented by
  (url title).")

(defun helm-eww-bookmark-bookmark-current-url ()
  "Bookmark current page."
  (interactive)
  (let* ((url (plist-get eww-data :url))
         (section (and url (helm-eww-bookmark--get-section)))
         (title (and section
                     (> (length section) 0)
                     (helm-eww-bookmark--read-title-from-minibuffer
                      (replace-regexp-in-string "[\n\t\r]" " "
                                                (plist-get eww-data :title))))))
    (if (not (stringp url))
        (error "No url found.")
      (when (and (stringp section) (stringp title) (stringp url))
        (helm-eww-bookmark--add-bookmark section url title)
        (helm-eww-bookmark--write-bookmarks-to-file)
        (message "%s added in %s." title section)))))

(defun helm-eww-bookmark--add-bookmark (section url title)
  "Add url URL into section SECTION with title being TITLE."
  (let ((lst (assoc section helm-eww-bookmark-bookmarks)))
    (cond
     ((null helm-eww-bookmark-bookmarks)
      (setq helm-eww-bookmark-bookmarks
            `((,section . ((,url ,title))))))
     (lst
      (setcdr (last lst) `((,url ,title))))
     (t
      ;; Create and append SECTION.
      (add-to-list 'helm-eww-bookmark-bookmarks `(,section . ((,url ,title))) t)))))

(defun helm-eww-bookmark--get-section-names ()
  (mapcar #'car helm-eww-bookmark-bookmarks))

(defvar helm-source-eww-bookmark-sections
      (helm-build-sync-source "Helm-eww bookmark sections"
        :candidates #'helm-eww-bookmark--get-section-names
        :migemo t))

(defvar helm-source-eww-bookmark-sections-not-found
  (helm-build-dummy-source "Create new section"
    :action (helm-make-actions
             "Create new section"
             (lambda (candidate)
               ;; Todo: find out how to inhibit candidate from
               ;; being the source name.
               candidate))))

(defun helm-eww-bookmark--get-section ()
  "Let user choose section or input a new section name."
  (helm :sources '(helm-source-eww-bookmark-sections
                   helm-source-eww-bookmark-sections-not-found)))

(defun helm-eww-bookmark--read-title-from-minibuffer (title)
  ;; We intentionally use INITIAL-CONTENS to #'read-from-minibuffer
  ;; because it would be convenient to edit and modify the original
  ;; title of the actual page rather than to input from scratch.
  (read-from-minibuffer "Title: " title))

(defun helm-eww-bookmark--get-bookmark-filepath ()
  "Use `eww-bookmarks-directory' defined in eww.el."
  (expand-file-name helm-eww-bookmark-bookmarks-filename eww-bookmarks-directory))

(defun helm-eww-bookmark--write-bookmarks-to-file ()
  "Write `helm-eww-bookmark-bookmarks' to a file."
  (with-temp-file (helm-eww-bookmark--get-bookmark-filepath)
    (insert ";; Auto-generated file. Don't edit this file!!\n")
    (insert (pp helm-eww-bookmark-bookmarks))))

(defun helm-eww-bookmark--read-bookmarks-from-file ()
  "Read in bookmarks from file and set `helm-eww-bookmark-bookmarks'."
  (when (file-readable-p (helm-eww-bookmark--get-bookmark-filepath))
    (setq helm-eww-bookmark-bookmarks
          (read (with-temp-buffer
                  (insert-file-contents (helm-eww-bookmark--get-bookmark-filepath))
                  (buffer-string))))))

(defun helm-eww-bookmark--get-bookmarks-in-section (section)
  "Return list of bookmarks in section SECTION."
  (assoc-default section helm-eww-bookmark-bookmarks))

(defun helm-eww-bookmark--get-all-bookmark-candidates ()
  (cl-mapcan (lambda (lst)
               (mapcar (lambda (bookmark)
                         ;; bookmark is (url title)
                         (cons (elt bookmark 1) (elt bookmark 0)))
                       (cdr lst)))
             helm-eww-bookmark-bookmarks))

(defun helm-eww-bookmark--build-section-candidates ()
  (helm-eww-bookmark--get-section-names))

(setq helm-eww-bookmark--actions-in-section
  (helm-make-actions
   "Default" (lambda (candidate) (cons t candidate))
   "Back to top" #'helm-eww-bookmark--go-back-to-section
   "Open bookmark in new buffer" #'helm-eww-bookmark-open-bookmark-in-new-buffer-action
   "Delete bookmark" #'helm-eww-bookmark-delete-bookmark-action
   "Edit title" #'helm-eww-bookmark-edit-bookmark-title-action
   "Copy to other section" #'helm-eww-bookmark-copy-bookmark-to-other-section-action
   "Move to other section" #'helm-eww-bookmark-move-bookmark-to-other-section-action))

(defun helm-eww-bookmark--go-back-to-section (candidate)
  (cons 'back candidate))

(defun helm-eww-bookmark--build-in-section-source (section)
  (helm-build-sync-source (format "Bookmarks in %s" section)
    :candidates (helm-eww-bookmark--get-candidates-in-section section)
    :action 'helm-eww-bookmark--actions-in-section
    :migemo t))

(defun helm-eww-bookmark--get-candidates-in-section (section)
  (cl-loop for bookmark in (assoc-default section helm-eww-bookmark-bookmarks)
           for url = (elt bookmark 0)
           for title = (elt bookmark 1)
           ;; Display is title, and real is (url . section)
           collect (cons title (cons url section))))

(defvar helm-source-eww-bookmarks
  (helm-build-sync-source "Helm eww bookmark sections"
    :candidates #'helm-eww-bookmark--build-section-candidates
    :migemo t))

(defvar helm-source-eww-all-bookmarks
  (helm-build-sync-source "Helm eww all bookmarks"
    :candidates #'helm-eww-bookmark--get-all-bookmark-candidates
    :action #'eww
    :migemo t))

(defun helm-eww-bookmark-open-bookmark-in-new-buffer-action (candidate)
  (cons 'new candidate))

(defun helm-eww-bookmark-delete-bookmark-action (candidate)
  (cons 'kill candidate))

(defun helm-eww-bookmark-edit-bookmark-title-action (candidate)
  (cons 'edit candidate))

(defun helm-eww-bookmark-copy-bookmark-to-other-section-action (candidate)
  (cons 'copy candidate))

(defun helm-eww-bookmark-move-bookmark-to-other-section-action (candidate)
  (cons 'move candidate))

(defun helm-eww-bookmark--select-bookmark-in-section (section)
  "Do helm with candidates being bookmarks in section SECTION."
  (interactive)
  (helm :sources (helm-eww-bookmark--build-in-section-source section)))

(defun helm-eww-bookmark-display-bookmarks (&optional prev-section)
  ""
  (let ((section (helm :sources '(helm-source-eww-bookmarks
                                  helm-source-eww-all-bookmarks)
                       :preselect prev-section)))
    (and (stringp section)
         (helm-eww-bookmark--select-bookmark-in-section section))))

(defun helm-eww-bookmark--get-url-from-candidate (candidate)
  "Return url in CANDIDIDATE."
  (car candidate))

(defun helm-eww-bookmark--get-section-from-candidate (candidate)
  "Return section in CANDIDATE."
  (and (consp candidate) (cdr candidate)))

(defun helm-eww-bookmark--get-title (section url)
  (car (assoc-default
        url
        (assoc-default section helm-eww-bookmark-bookmarks))))

(defun helm-eww-bookmark--delete-bookmark (section url title)
  "Delete bookmark in SECTION."
  ;; We assume that there is only one entry in SECTION whose url is
  ;; equal to URL.
  (when (y-or-n-p (format "Delete %s in %s? " title section))
    (setf (elt helm-eww-bookmark-bookmarks
               (cl-position section helm-eww-bookmark-bookmarks
                            :test #'equal
                            :key #'car))
          (cl-remove `(,url ,title)
                     (assoc section helm-eww-bookmark-bookmarks)
                     :test #'equal))
    (helm-eww-bookmark--write-bookmarks-to-file)
    (message "Deleted bookmark %s in %s" title section)))

(defun helm-eww-bookmark-bookmarks ()
  (interactive)
  (let ((val nil)
        (url nil)
        (go 'go))
    ;; Value returned from #'helm-eww-bookmark-display-bookmarks is
    ;; one the following:
    ;; nil: when pressed C-g or something like that.
    ;; (t candidate): user has selected some candidate, i.e default action.
    ;; (back candidate): user wants to go back to a list of sections.
    ;; (new candidate): user wants to open bookmark in a new buffer.
    (while (eq 'back
               (car (setq val
                          (helm-eww-bookmark-display-bookmarks
                           (helm-eww-bookmark--get-section-from-candidate (cdr val))))))
      ;; pass
      )
    (setq url (helm-eww-bookmark--get-url-from-candidate (cdr val)))
    (setq section (helm-eww-bookmark--get-section-from-candidate (cdr val)))
    (setq title (helm-eww-bookmark--get-title section url))
    (cl-case (car val)
      ((t)
       ;; Visit url.
       (and (stringp url) (eww url)))
      ('new
       ;; Visit url in new buffer.
       (and (stringp url) (eww-new url)))
      ('delete
       ;; Delete this bookmark.
       (helm-eww-bookmark--delete-bookmark section url title)
       )
      ('edit
       ;; Edit title of this bookmark.
       (message "Editing title, bookmark=%s" (cons url title))
       )
      ('copy
       ;; Copy this bookmark to other section.
       (message "Copying bookmark=%s" (cons url title))
       )
      ('move
       ;; Move this bookmark to other section.
       (message "Moving bookmark=%s" (cons url title))
       )
      (t
       ;; Otherwise, do nothing.
       nil))))

(provide 'helm-eww)

;; Local Variables:
;; coding: utf-8-unix
;; indent-tabs-mode: nil
;; End:

;; helm-eww.el ends here
