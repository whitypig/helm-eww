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
;; (define-key eww-mode-map (kbd "B") #'helm-eww-bookmark-bookmarks)
;; (define-key eww-mode-map (kbd "a") #'helm-eww-bookmark-bookmark-current-url)
;; (define-key eww-mode-map (kbd "C-c C-n") #'eww-next-buffer)
;; (define-key eww-mode-map (kbd "C-c C-p") #'eww-previous-buffer)
;;

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
    (define-key helm-map (kbd "C-c d") #'helm-eww-buffers--run-persistent-kill-buffers)
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

(defun helm-eww-buffers--run-persistent-kill-buffers ()
  (interactive)
  (with-helm-alive-p
    (helm-attrset 'kill '(helm-eww-buffers--kill-buffers . never-split))
    (helm-execute-persistent-action 'kill)
    (helm-update)))

(defun helm-eww-buffers--kill-buffers (_candidate)
  (let ((buffers (helm-marked-candidates)))
    (when (y-or-n-p (format "Kill the following buffer(s)?: %s"
                            (mapconcat (lambda (b)
                                         (format "\"%s\"" (buffer-name b)))
                                       buffers
                                       ", ")))
      (mapc #'kill-buffer buffers))))

(defvar helm-source-eww-buffers
  (helm-build-sync-source "eww buffers"
    :candidates #'helm-eww-buffers-candidates
    :action #'helm-eww-buffers-select-buffer
    :volatile t
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
    (with-current-buffer helm-current-buffer
      (setq-local eww-history new-histories))))

(defun helm-eww-history-run-delete-history-persistent ()
  "Delete history without quitting helm."
  (interactive)
  (with-helm-alive-p
    (helm-attrset 'kill-action '(helm-eww-history-delete-history . never-split))
    (helm-execute-persistent-action 'kill-action)
    (helm-force-update)))

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
    :volatile t
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

;; session management

(defcustom helm-eww-session-session-file
  (concat (expand-file-name user-emacs-directory) "helm-eww-sessions")
  "Absolute file name for storing eww session."
  :group 'helm-eww)

(defun helm-eww-session--collect-sessions ()
  "Collect eww session list, each element of which is in the form of
(buffer-name url)."
  (cl-loop for buffer in (cl-remove-if-not (lambda (buffer)
                                          (eq 'eww-mode
                                              (buffer-local-value 'major-mode
                                                                  buffer)))
                                        (buffer-list))
           collect (with-current-buffer buffer
                     (list
                      ;; buffer name
                      (buffer-name)
                      ;; URL
                      (eww-current-url)
                      ;; How about history? Saving history takes very long.
                      ;; Maybe we should store only title and url in eww-history.
                   ))
           into ret
           finally return (sort ret (lambda (a b)
                                      ;; Sort by buffer name
                                      (string< (nth 0 a) (nth 0 b))))))

(defun helm-eww-session-save-sessions ()
  (interactive)
  (with-temp-file helm-eww-session-session-file
    (insert (pp (helm-eww-session--collect-sessions))))
  (message "Saved sessions in %s" helm-eww-session-session-file))

(defun helm-eww-session--load-sessions ()
  (when (file-readable-p helm-eww-session-session-file)
    (read (with-temp-buffer
            (insert-file-contents-literally helm-eww-session-session-file)
            (buffer-string)))))

(defun helm-eww-session-restore-sessions ()
  (interactive)
  (cl-loop for session in (helm-eww-session--load-sessions)
           for name = (nth 0 session)
           for url = (nth 1 session)
           with lst = nil
           ;; Open all urls in sessions.
           do (progn (eww-new url)
                     ;; Give it a temporary unique name.
                     (rename-buffer (make-temp-name "helm-eww-"))
                     (push (cons name (current-buffer)) lst))
           ;; Then, rename each buffer.
           finally (cl-loop for cell in lst
                            for n = (car cell)
                            for b = (cdr cell)
                            do (with-current-buffer b
                                 (rename-buffer n))))
  (message "Restored eww session."))

;; bookmark management

(defcustom helm-eww-bookmark-bookmarks-filename "helm-eww-bookmarks"
  ""
  :type 'string
  :group 'helm-eww)

(defvar helm-eww-bookmark-bookmarks nil
  "List of sections of type `heww-bookmark-section'. Each section has
its own list of bookmarks of type `heww-bookmark'.")

(defclass heww-bookmark ()
  ((url :initarg :url
        :initform nil)
   (title :initarg :title
          :initform nil)
   (date :initform (format-time-string "%Y%m%d%H%M")))
  "Represents a bookmark.")

(defmethod helm-eww-bookmark--bookmark-equal ((bm1 heww-bookmark) bm2)
  (string= (slot-value bm1 :url) (slot-value bm2 :url)))

(defclass heww-bookmark-section ()
  ((name :initarg :name
         :initform nil)
   (bookmarks :initarg :bookmarks
              :initform nil
              :type list
              :documentation "Bookmarks in this section.")
   (date :initform (format-time-string "%Y%m%d%H%M")))
  "Represents a section which holds multiple bookmarks of type
`heww-bookmark'.")

(defmethod helm-eww-bookmark--heww-add-bookmark ((section-obj heww-bookmark-section)
                                                 bookmark)
  (cond
   ((null (slot-value section-obj :bookmarks))
    (push bookmark (slot-value section-obj :bookmarks)))
   ((cl-find-if (lambda (bm) (helm-eww-bookmark--bookmark-equal bm bookmark))
                (slot-value section-obj :bookmarks))
    (message "You already have bookmarked %s." (slot-value bookmark :url)))
   (t
    ;; Append bookmark
    (object-add-to-list section-obj :bookmarks bookmark t))))

(defun helm-eww-bookmark-bookmark-current-url ()
  "Bookmark current page."
  (interactive)
  (helm-eww-bookmark--restore-bookmarks-maybe)
  (let* ((url (plist-get eww-data :url))
         (section-obj (and url (helm-eww-bookmark--get-section)))
         (title (and section-obj
                     (> (length (slot-value section-obj :name)) 0)
                     (helm-eww-bookmark--read-from-minibuffer
                      "Title: "
                      (replace-regexp-in-string "[\n\t\r]" " "
                                                (plist-get eww-data :title))))))
    (cond
     ((not (stringp url))
      (error "No url found."))
     ((and (stringp title) (stringp url))
      (helm-eww-bookmark--heww-add-bookmark section-obj
                                            (heww-bookmark :url url :title title))
      (helm-eww-bookmark--write-bookmarks-to-file)
      (message "%s added in %s." title (slot-value section-obj :name))))))

(defun helm-eww-bookmark--add-bookmark (section url title)
  "Create and add bookmark whose url is URL and title TITLE into
section SECTION-OBJ."
  (let ((section-obj (or (cl-find-if (lambda (obj)
                                       (string= (slot-value obj :name) section))
                                     helm-eww-bookmark-bookmarks)
                         (progn
                           (add-to-list 'helm-eww-bookmark-bookmarks
                                        (heww-bookmark-section :name section)
                                        t)
                           (car (last helm-eww-bookmark-bookmarks))))))
    (helm-eww-bookmark--heww-add-bookmark section-obj
                                          (heww-bookmark :url url :title title))))

(defvar helm-eww-bookmark-sections-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-e") #'helm-eww-bookmark--persistent-edit-section)
    (define-key map (kbd "C-c C-d") #'helm-eww-bookmark--persistent-delete-section)
    (define-key map (kbd "C-c C-a") #'helm-eww-bookmark--persistent-add-section)
    map)
  "Keymap used in `helm-source-eww-bookmark-sections'.")

(defvar helm-eww-bookmark-in-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-l") #'helm-eww-bookmark--run-go-back-to-section-action)
    (define-key map (kbd "C-c C-e") #'helm-eww-bookmark--persistent-edit)
    (define-key map (kbd "C-c C-d") #'helm-eww-bookmark--persistent-delete)
    (define-key map (kbd "C-c C-f") #'helm-eww-bookmark--run-open-in-new-buffer)
    map)
  "Keymap used in source returned by `helm-eww-bookmark--build-in-section-source'.")

(defvar helm-eww-bookmark--no-bookmarks-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-b") #'helm-eww-bookmark--run-go-back-to-section-action)
    (define-key map (kbd "C-l") #'helm-eww-bookmark--run-go-back-to-section-action)
    map)
  "Keymap used when there is no bookmarks in a section.")

(defvar helm-eww-bookmark-all-bookmarks-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-f") #'helm-eww-bookmark--run-open-in-new-buffer)
    map)
  "Keymap used in `helm-source-eww-all-bookmarks'.")

(defvar helm-source-eww-bookmark-sections
  (helm-build-sync-source "Helm eww bookmark sections"
    :candidates #'helm-eww-bookmark--build-section-candidates
    :volatile t
    :keymap helm-eww-bookmark-sections-map
    :migemo t)
  "A helm source for selecting a section.")

(defvar helm-source-eww-bookmark-sections-not-found
  (helm-build-dummy-source "Create new section"
    :action (helm-make-actions
             "Create new section"
             (lambda (candidate)
               ;; Todo: find out how to inhibit candidate from
               ;; being the source name.
               ;; Create and add new section whose name is candidate.
               (add-to-list 'helm-eww-bookmark-bookmarks
                            (heww-bookmark-section :name candidate)
                            t)
               (car (last helm-eww-bookmark-bookmarks)))))
  "A helm source for creating a new section.")

(defvar helm-source-eww-all-bookmarks
  (helm-build-sync-source "Helm eww all bookmarks"
    :candidates #'helm-eww-bookmark--get-all-bookmark-candidates
    :action (lambda (candidate) (cons t candidate))
    :keymap helm-eww-bookmark-all-bookmarks-map
    :migemo t)
  "A helm source listing all of bookmarks in `helm-eww-bookmark-bookmarks'.")

(defun helm-eww-bookmark--persistent-edit-section ()
  "Edit the name of section."
  (interactive)
  (with-helm-alive-p
    (helm-attrset 'edit-section
                  '(helm-eww-bookmark--run-persistent-edit-section . never-split))
    (helm-execute-persistent-action 'edit-section)
    (helm-update)))

(defun helm-eww-bookmark--run-persistent-edit-section (candidate)
  (helm-eww-bookmark--edit-section candidate))

(defun helm-eww-bookmark--read-from-minibuffer (prompt initial)
  (let ((ret nil)
        (ok nil))
    (while (not ok)
      (if (zerop (length (setq ret (read-from-minibuffer prompt initial))))
          ;; We intentionally use INITIAL-CONTENS to
          ;; #'read-from-minibuffer because it would be much easier to
          ;; edit the current one rather than to input from scratch.
          (and (message "Empty string is not allowed.")
               (sit-for 1))
        (setq ok t)))
    ret))

(defun helm-eww-bookmark--edit-section (section-obj)
  (let* ((name (slot-value section-obj :name))
         (new-name (helm-eww-bookmark--read-from-minibuffer "Section name: " name)))
    (if (string= name new-name)
        (message "No need to change.")
      (progn
        (setf (slot-value section-obj :name) new-name)
        (message "Section name has changed to %s" new-name)))))

(defun helm-eww-bookmark--persistent-delete-section ()
  "Delete this section."
  (interactive)
  (with-helm-alive-p
    (helm-attrset 'delete-section
                  '(helm-eww-bookmark--run-persistent-delete-section . never-split))
    (helm-execute-persistent-action 'delete-section)
    (helm-update)))

(defun helm-eww-bookmark--run-persistent-delete-section (candidate)
  ;; CANDIDATE is a section object.
  (helm-eww-bookmark--delete-section candidate))

(defun helm-eww-bookmark--delete-section (section-obj)
  "Delete section represented by SECTION-OBJ from
`helm-eww-bookmark-bookmarks'."
  (let ((name (slot-value section-obj :name)))
    (when (and (yes-or-no-p (format "Are you sure to delete section \"%s\"? " name))
               (if (slot-value section-obj :bookmarks)
                   ;; If this section is not empty, ask user once again.
                   (yes-or-no-p "Warning: This section is not empty. Proceed? ")
                 t))
      (setq helm-eww-bookmark-bookmarks
            (delete section-obj helm-eww-bookmark-bookmarks))
      (message "Section %s has been deleted." name))))

(defun helm-eww-bookmark--persistent-add-section ()
  "Add a new section."
  (interactive)
  (with-helm-alive-p
    (helm-attrset 'add-section
                  '(helm-eww-bookmark--run-persistent-add-section . never-split))
    (helm-execute-persistent-action 'add-section)
    (helm-update)))

(defun helm-eww-bookmark--run-persistent-add-section (_ignore)
  (helm-eww-bookmark--add-section))

(defun helm-eww-bookmark--add-section ()
  (let ((section-names (mapcar (lambda (section-obj)
                                 (slot-value section-obj :name))
                               helm-eww-bookmark-bookmarks))
        (name nil))
    (while (member
            (setq name
                  (helm-eww-bookmark--read-from-minibuffer "New section name: " ""))
            section-names)
      (message "Section name %s is already in use. Try another one." name))
    (add-to-list 'helm-eww-bookmark-bookmarks
                 (heww-bookmark-section :name name)
                 t)))

(defun helm-eww-bookmark--get-section ()
  "Let user choose section or input a new section name."
  (helm :sources '(helm-source-eww-bookmark-sections
                   helm-source-eww-bookmark-sections-not-found)))

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

(defun helm-eww-bookmark--get-all-bookmark-candidates ()
  "Return candidates of all bookmarks in
`helm-eww-bookmark-bookmarks'. Display value is title and real value
is of the form (heww-bookmark . nil)."
  (mapcar (lambda (bm-obj)
            ;; Display is title and real is (bookmark object . nil).
            (cons (slot-value bm-obj :title)
                  (cons bm-obj nil)))
          (cl-mapcan (lambda (section-obj)
                       (copy-sequence (slot-value section-obj :bookmarks)))
                     helm-eww-bookmark-bookmarks)))

(defun helm-eww-bookmark--build-section-candidates ()
  "Return section candidates. Display value is name of section and
real value is heww-bookmark-section object."
  (mapcar (lambda (obj)
            ;; Display is section name and real is section object.
            (cons (slot-value obj :name) obj))
          helm-eww-bookmark-bookmarks))

(defvar helm-eww-bookmark--actions-in-section
  (helm-make-actions
   "Default" (lambda (candidate) (cons t (list candidate)))
   "Back to top" #'helm-eww-bookmark--go-back-to-section
   "Open bookmark in new buffer" #'helm-eww-bookmark-open-bookmark-in-new-buffer-action
   "Delete bookmark" #'helm-eww-bookmark-delete-bookmark-action
   "Edit bookmark" #'helm-eww-bookmark-edit-bookmark-action
   "Copy to other section" #'helm-eww-bookmark-copy-bookmark-to-other-section-action
   "Move to other section" #'helm-eww-bookmark-move-bookmark-to-other-section-action)
  "Actions used in a source returned by
`helm-eww-bookmark--build-in-section-source'.")

(defun helm-eww-bookmark--go-back-to-section (candidate)
  "An action to go back to the list of sections."
  (cons 'back (list candidate)))

(defun helm-eww-bookmark--run-go-back-to-section-action ()
  "Go back to the list of sections."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-eww-bookmark--go-back-to-section)))

(defun helm-eww-bookmark--run-open-in-new-buffer ()
  "Open bookmark in a new eww buffer."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-eww-bookmark-open-bookmark-in-new-buffer-action)))

(defun helm-eww-bookmark--persistent-edit ()
  "Edit currently-selected bookmark."
  (interactive)
  (with-helm-alive-p
    (helm-attrset 'edit '(helm-eww-bookmark--run-persistent-edit . never-split))
    (helm-execute-persistent-action 'edit)))

(defun helm-eww-bookmark--run-persistent-edit (candidate)
  ;; candidate is (bookmark-obj . section-obj)
  (helm-eww-bookmark--edit-bookmark (car candidate) (cdr candidate))
  (helm-update))

(defun helm-eww-bookmark--persistent-delete ()
  "Delete marked bookmarks."
  (interactive)
  (with-helm-alive-p
    (helm-attrset 'delete '(helm-eww-bookmark--run-persistent-delete . never-split))
    (helm-execute-persistent-action 'delete)))

(defun helm-eww-bookmark--run-persistent-delete (_candidate)
  (cl-loop for candidate in (helm-marked-candidates)
           for bm-obj = (car candidate)
           ;; Note: section-obj stays same across this loop because delete
           ;; action is called for bookmarks in a section.
           for section-obj = (cdr candidate)
           do (helm-eww-bookmark--delete-bookmark bm-obj section-obj)
           finally (helm-update)))

(defun helm-eww-bookmark--build-in-section-source (section-obj)
  "Build helm source with bookmarks in section SECTION-OBJ."
  (helm-build-sync-source (format "Bookmarks in %s (C-l: Go back to sections)"
                                  (slot-value section-obj :name))
    :candidates (lambda ()
                  (helm-eww-bookmark--get-candidates-in-section section-obj))
    :action 'helm-eww-bookmark--actions-in-section
    :volatile t
    :keymap helm-eww-bookmark-in-section-map
    :migemo t))

(defun helm-eww-bookmark--get-candidates-in-section (section-obj)
  "Return candidates for bookmarks in section SECTION-OBJ. Display
value is bookmark title and real value is (`heww-bookmark'
. `heww-bookmark-section')."
  (cl-loop for bookmark in (slot-value section-obj :bookmarks)
           ;; Display is title, and real is (bookmark-obj . section-obj)
           collect (cons (slot-value bookmark :title)
                         (cons bookmark section-obj))))

(defun helm-eww-bookmark-open-bookmark-in-new-buffer-action (_candidate)
  (cons 'new (helm-marked-candidates)))

(defun helm-eww-bookmark-delete-bookmark-action (candidate)
  (cons 'delete (helm-marked-candidates)))

(defun helm-eww-bookmark-edit-bookmark-action (candidate)
  (cons 'edit (list candidate)))

(defun helm-eww-bookmark-copy-bookmark-to-other-section-action (candidate)
  (cons 'copy (list candidate)))

(defun helm-eww-bookmark-move-bookmark-to-other-section-action (candidate)
  (cons 'move (list candidate)))

(defun helm-eww-bookmark--do-helm-in-section (section-obj)
  "Do helm with candidates for bookmarks in section SECTION-OBJ."
  (interactive)
  (helm :sources `(,(helm-eww-bookmark--build-in-section-source section-obj)
                   ;; We need another source created by
                   ;; #'helm-eww-bookmark--build-no-bookmarks-source
                   ;; so that we can go back to the list of sections
                   ;; when there is no bookmarks in a section.
                   ,(helm-eww-bookmark--build-no-bookmarks-source section-obj))))

(defun helm-eww-bookmark--build-no-bookmarks-source (section-obj)
  ;; We cannot get back to the list of sections if we use dummy
  ;; source.
  (helm-build-sync-source "No bookmarks here. (C-l: Go back to sections)"
    :keymap 'helm-eww-bookmark--no-bookmarks-map
    :candidates `(,(cons "Go back to sections" (cons 'back
                                                     `(,(cons nil
                                                              section-obj)))))))

(defun helm-eww-bookmark-do-helm (&optional prev-section)
  (let ((val (helm :sources '(helm-source-eww-bookmark-sections
                              helm-source-eww-all-bookmarks)
                   :preselect prev-section)))
    (cond
     ((heww-bookmark-section-p val)
      (helm-eww-bookmark--do-helm-in-section val))
     (t
      val))))

(defun helm-eww-bookmark--get-url-from-candidate (candidate)
  "Return url in CANDIDIDATE."
  (car candidate))

(defun helm-eww-bookmark--get-section-from-candidate (candidate)
  "Return section in CANDIDATE, which is of type `heww-bookmark-section'."
  (and (heww-bookmark-section-p candidate)
       (slot-value candidate :name)))

(defun helm-eww-new (bm-obj section-obj)
  (eww-new (slot-value bm-obj :url)))

(defun helm-eww-bookmark--delete-bookmark (bm-obj section-obj)
  "Delete bookmark BM-OBJ in section SECTION-OBJ."
  ;; We assume that there is only one entry in SECTION whose url is
  ;; equal to URL.
  (let ((title (slot-value bm-obj :title))
        (section (slot-value section-obj :name)))
    (when (y-or-n-p (format "Delete %s in %s? " title section))
      (setf (slot-value section-obj :bookmarks)
            (delete bm-obj (slot-value section-obj :bookmarks)))
      (helm-eww-bookmark--write-bookmarks-to-file)
      (message "Deleted bookmark %s in %s" title section))))

(defun helm-eww-bookmark--edit-bookmark (bm-obj _section-obj)
  "Edit bookmark."
  (let* ((title (slot-value bm-obj :title))
         (new-title (helm-eww-bookmark--read-from-minibuffer "Title: " title))
         (url (slot-value bm-obj :url))
         (new-url (helm-eww-bookmark--read-from-minibuffer "URL: " url)))
    (if (and (string= new-title title) (string= url new-url))
        (message "No need to change title!")
      (progn
        (setf (slot-value bm-obj :title) new-title)
        (setf (slot-value bm-obj :url) new-url)
        (helm-eww-bookmark--write-bookmarks-to-file)
        (message "Changes have been saved.")))))

(defun helm-eww-bookmark--restore-bookmarks-maybe ()
  "Restore `helm-eww-bookmark-bookmarks' if not set yet."
  (unless helm-eww-bookmark-bookmarks
    (helm-eww-bookmark--read-bookmarks-from-file)))

(defun helm-eww-bookmark--do-action-on-list (lst func)
  "Call function FUNC passing each element in list LST as
arguments.Each element is of the form (heww-bookmark
. heww-bookmark-section)."
  (cl-loop for cell in lst
           for bm-obj = (car cell)
           for section-obj = (cdr cell)
           do (funcall func bm-obj section-obj)))

;;;###autoload
(defun helm-eww-bookmark-bookmarks ()
  "Display helm eww bookmarks with the help of `helm'."
  (interactive)
  (helm-eww-bookmark--restore-bookmarks-maybe)
  (let ((val nil)
        (lst nil)
        (bm-obj nil)
        (section-obj))
    ;; Value returned from #'helm-eww-bookmark-do-helm is
    ;; (action-sign . candidates) and candidates is a list of the
    ;; form ((bm-obj1 . section-obj1) (bm-obj2 . section-obj2)) or nil
    ;; when returning from dummy source.
    (while (eq
            'back
            (car
             (setq val
                   (helm-eww-bookmark-do-helm
                    (helm-eww-bookmark--get-section-from-candidate
                     ;; val is ('kind . ((bm-obj . section-obj) (bm-obj . section-obj))).
                     ;; (cdr val) is ((bm-obj . section-obj) (bm-obj . section-obj)).
                     ;; (cadr val) is (bm-obj . section-obj).
                     ;; (cdadr val) is section-obj.
                     (cdadr val))))))
      ;; pass
      )
    (setq lst (cdr val))
    (setq bm-obj (caar lst))
    (setq section-obj (cdar lst))
    (when (heww-bookmark-p bm-obj)
      (cl-case (car val)
        ((t)
         ;; Visit url.
         ;; Even if lst contains more than one element, visiting
         ;; multiple URLs in one buffer doesn't make any sense, at
         ;; least to me. So we assume that the first element in lst
         ;; is the bookmark that the user wants to visit.
         (eww (slot-value bm-obj :url)))
        ('new
         ;; Visit url in new buffer.
         (helm-eww-bookmark--do-action-on-list lst #'helm-eww-new))
        ('delete
         ;; Delete this bookmark.
         (helm-eww-bookmark--do-action-on-list lst #'helm-eww-bookmark--delete-bookmark)
         ;; (helm-eww-bookmark--delete-bookmark section-obj bm-obj)
         )
        ('edit
         ;; Edit this bookmark.
         (helm-eww-bookmark--do-action-on-list lst #'helm-eww-bookmark--edit-bookmark))
        ('copy
         ;; Copy this bookmark to other section.
         ;; Not implemented.
         ;; (message "Copying bookmark=%s" (slot-value bm-obj :title))
         )
        ('move
         ;; Move this bookmark to other section.
         ;; Not implemented.
         ;; (message "Moving bookmark=%s" (slot-value bm-obj :title))
         )
        (t
         ;; Otherwise, do nothing.
         nil)))))

(defun helm-eww-bookmark--setup-dev ()
  "Debugging purpose."
  (interactive)
  (shell-command (format "cp %s %s.orig"
                         (helm-eww-bookmark--get-bookmark-filepath)
                         (helm-eww-bookmark--get-bookmark-filepath))))

(provide 'helm-eww)

;; Local Variables:
;; coding: utf-8-unix
;; indent-tabs-mode: nil
;; End:

;; helm-eww.el ends here
