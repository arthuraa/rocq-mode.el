;;; rocq-mode.el ---- Rocq mode using coq-lsp  -*- lexical-binding: t -*-

;; Author: Josselin Poiret <dev@jpoiret.xyz>
;; Version: 0.1
;; Package-Requires: ((eglot "1.12") (magit-section "3.0"))
;; Keywords: coq, rocq
;; URL: https://codeberg.org/jpoiret/rocq-mode.el

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'eglot)
(require 'magit-section)


;; LSP implementation

;;;###autoload
(defclass rocq--lsp-server (eglot-lsp-server)
  ((workspace
    :documentation "List of workspace folders."
    :accessor rocq--workspace
    :initform '())
   (last-goal-request-state
    :documentation "Last goal request state."
    :accessor rocq--last-goal-request-state
    :initform nil)
   (timing-data
    :documentation "Whether to get timing data."
    :accessor rocq--timing-data
    :initform nil)))

(defun rocq-workspace-folder--repr (folder)
  (list :uri (eglot--path-to-uri folder)
        :name (abbreviate-file-name folder)))

(cl-defmethod eglot-workspace-folders ((server rocq--lsp-server))
  (vconcat
   (mapcar #'rocq-workspace-folder--repr
           (rocq--workspace server))))

(cl-defmethod eglot-initialization-options ((server rocq--lsp-server))
  (let ((starting (make-hash-table :size 1)))
    (puthash "show_coq_info_messages" :json-false starting)
    (puthash "pp_type" 2 starting)
    (puthash "send_perf_data" (if (rocq--timing-data server) t :json-false) starting)
    starting))

(defun rocq-add-folder-to-workspace (folder)
  ""
  (interactive "DSelect folder to add: ")
  (let ((server (eglot--current-server-or-lose)))
    (if (member folder (rocq--workspace server))
        (message "%s is already in the Rocq workspace folders." folder)
      (setf (rocq--workspace server) (cons folder (rocq--workspace server)))
      (jsonrpc-notify
       server :workspace/didChangeWorkspaceFolders
       `(:event (:added [,(rocq-workspace-folder--repr folder)]
                        :removed []))))))


;; Goal display

(defvar rocq-goals-buffer-name
  "*Rocq Goals*"
  "Name of the goals buffer.")

(defun rocq--goals-buffer ()
  "Return the goals buffer.  If it doesn't exist, create it."
  (let ((existing (get-buffer rocq-goals-buffer-name)))
    (if existing
        existing
      (let ((new (get-buffer-create rocq-goals-buffer-name)))
        (display-buffer new)
        (with-current-buffer new (magit-section-mode))
        new))))

(defun rocq--goal-request-state ()
  "Builds a goal request state."
  (list (current-buffer) (buffer-modified-tick) (point)))

(defun rocq--insert-goal (goal &optional hide)
  "Insert a single goal into the buffer."
  (eglot--dbind (hyps ty) goal
    (magit-insert-section (magit-section goal hide)
      (magit-insert-heading
        (format "%s\n" ty))
      (mapc (eglot--lambda (names def ty)
              (mapc (lambda (name) (insert (propertize name 'face 'font-lock-variable-name-face) " "))
                    names)
              (when def
                (insert ":= " def "\n"))
              (let ((pt (point)))
                (insert ": " ty "\n")
                (indent-region pt (point) 2)))
            hyps))))

;;;###autoload
(defun rocq-goals ()
  "Update the goal display."
  (interactive)
  (let ((serv (eglot--current-server-or-lose))
        (state (rocq--goal-request-state)))
    (unless (equal state
                   (rocq--last-goal-request-state serv))
      (setf (rocq--last-goal-request-state serv) state)
      (jsonrpc-async-request
       serv
       :proof/goals
       `(:textDocument ,(eglot--TextDocumentIdentifier)
                       :position ,(eglot--pos-to-lsp-position (point)))
       :success-fn
       (eglot--lambda (goals messages)
         (with-current-buffer (rocq--goals-buffer)
           (let ((inhibit-read-only t))
             (erase-buffer)
             (magit-insert-section (magit-section)
               (eglot--dbind (goals shelf) goals
                 (magit-insert-section (magit-section)
                   (magit-insert-heading
                     (format "Focused goals (%d)\n" (length goals)))
                   (let ((hide nil))
                     (mapc
                      (lambda (goal)
                        (rocq--insert-goal goal hide)
                        (setq hide t))
                      goals)))
                 (newline)
                 (magit-insert-section (magit-section shelf t)
                   (magit-insert-heading
                     (format "Shelf (%d)\n" (length shelf)))
                   (mapc #'rocq--insert-goal shelf)))
               (newline)
               (magit-insert-section (magit-section)
                 (magit-insert-heading
                   (format "Messages (%d)\n" (length messages)))
                 (mapc (eglot--lambda (text)
                         (insert text "\n"))
                       messages))))))))))


;; Processing overlay

(defvar-local rocq-mode--processing-overlays
    nil
  "")

(defface rocq-mode-processing-face
  `((t :background "light gray"))
  "")

(defvar rocq-mode--idle-timer
  nil)

(defcustom rocq-mode-idle-goals-delay
  0.5
  "Delay used for automatic goal refreshing."
  :type '(number))

(defun rocq-mode--setup-timer ()
  (or rocq-mode--idle-timer
      (setq
       rocq-mode--idle-timer
       (run-with-idle-timer
        rocq-mode-idle-goals-delay nil
        (lambda ()
          (setq rocq-mode--idle-timer nil)
          (when (eq major-mode 'rocq-mode)
            (rocq-goals)))))))

(cl-defmethod eglot-handle-notification
  ((server rocq--lsp-server) (_method (eql $/coq/fileProgress)) &key textDocument processing)
  (eglot--dbind (uri) textDocument
    (if-let* ((path (expand-file-name (eglot--uri-to-path uri)))
              (buffer (find-buffer-visiting path)))
        (with-current-buffer buffer
          (when (equal (eglot--VersionedTextDocumentIdentifier) textDocument)
            (mapc #'delete-overlay rocq-mode--processing-overlays)
            (mapc (eglot--lambda (range)
                    (eglot--dbind (start end) range
                      (let ((overlay
                             (make-overlay
                              (eglot--lsp-position-to-point start)
                              (eglot--lsp-position-to-point end))))
                      (progn
                        (add-to-list 'rocq-mode--processing-overlays
                                     overlay)
                        (overlay-put overlay 'face 'rocq-mode-processing-face)))))
                  processing))))))


;; Timing data display

(defvar-local rocq-mode--timing-overlays
    nil
  "")

(defcustom rocq-mode-too-slow
  0.3
  "Time (in seconds) such that any command that takes longer to execute is
considered slow."
  :type '(number))

(defface rocq-mode-slow-face
  `((t :background "firebrick4"
       :foreground "white"))
  "Face used for slow commands.")

(cl-defmethod eglot-handle-notification
  ((server rocq--lsp-server) (_method (eql $/coq/filePerfData)) &key textDocument summary timings)
  (eglot--dbind (uri) textDocument
    (if-let* ((path (expand-file-name (eglot--uri-to-path uri)))
              (buffer (find-buffer-visiting path)))
        (with-current-buffer buffer
          (when (equal (eglot--VersionedTextDocumentIdentifier) textDocument)
            (mapc #'delete-overlay rocq-mode--timing-overlays)
            (mapc (eglot--lambda (range info)
                    (eglot--dbind (time memory cache_hit time_hash) info
                      (when (>= time rocq-mode-too-slow)
                        (eglot--dbind (start end) range
                          (let ((overlay
                                 (make-overlay
                                  (eglot--lsp-position-to-point start)
                                  (eglot--lsp-position-to-point end))))
                            (progn
                              (add-to-list 'rocq-mode--timing-overlays
                                           overlay)
                              (overlay-put overlay 'face 'rocq-mode-slow-face)))))))
                  timings))))))


(defun rocq-toggle-timing-data ()
  "Toggle timing data display.

Which commands are considered slow and thus highlighted is governed by the
customizable variable `rocq-mode-too-slow'."
  (interactive)
  (let* ((server (eglot--current-server-or-lose))
         (prev-val (rocq--timing-data server)))
    (setf (rocq--timing-data server) (not prev-val))
    (jsonrpc-notify
     server :workspace/didChangeConfiguration
     `(:settings ,(eglot-initialization-options server)))
    (when prev-val
      (mapc #'delete-overlay rocq-mode--timing-overlays)
      (setq rocq-mode--timing-overlays '()))))


;; Misc commands

(defun rocq-save-vo ()
  "Save the .vo file corresponding to the current buffer."
  (interactive)
  (let ((server (eglot--current-server-or-lose)))
    (jsonrpc-async-request
     server :coq/saveVo
     `(:textDocument ,(eglot--VersionedTextDocumentIdentifier))
     :success-fn
     (lambda (_) (message "%s"
                          (propertize "Successfully saved .vo file"
                                      'face 'success)))
     :error-fn
     (lambda (_) (message "%s"
                          (propertize "Failed saving .vo file"
                                      'face 'error))))))

(defun rocq-trim ()
  "Ask coq-lsp to free memory."
  (interactive)
  (let ((server (eglot--current-server-or-lose)))
    (jsonrpc-async-request
     server :coq/saveVo
     `(:textDocument ,(eglot--VersionedTextDocumentIdentifier))
     :success-fn
     (lambda (_) (message "%s"
                          (propertize "Successfully trimmed memory"
                                      'face 'success)))
     :error-fn
     (lambda (_) (message "%s"
                          (propertize "Failed to trim memory"
                                      'face 'error))))))


;; General mode setup

(defvar rocq-mode-syntax-table
  (let ((st (make-syntax-table))) ; define-derived-mode will add the proper parent
    (modify-syntax-entry ?\( "()1" st)
    (modify-syntax-entry ?\* ". 23n" st)
    (modify-syntax-entry ?\) ")(4" st)
    st))

;;;###autoload
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs (cons 'rocq-mode (list 'rocq--lsp-server "coq-lsp"))))

;;;###autoload
(define-derived-mode rocq-mode prog-mode "Rocq"
  (when-let ((server (eglot-current-server))
             (rocq-proj-dir (locate-dominating-file (buffer-file-name) "_CoqProject"))
             ((not (member rocq-proj-dir (rocq--workspace server)))))
    (rocq-add-folder-to-workspace rocq-proj-dir))
  (eglot-ensure)
  (add-hook 'post-command-hook #'rocq-mode--setup-timer)
  (setq-local comment-start "(*"
              comment-end "*)"
              comment-style 'multi-line))

(provide 'rocq-mode)
