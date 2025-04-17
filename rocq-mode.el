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

(require 'rocq-syntax)


;; LSP implementation

;;;###autoload
(defclass rocq--lsp-server (eglot-lsp-server)
  ((workspace-folders
    :documentation "List of workspace folders."
    :reader rocq-workspace-folders
    :initform '())
   (timing-data
    :documentation "Whether to get timing data."
    :reader rocq-timing-data
    :initform nil)
   (pending-file-progress
    :documentation "Alist mapping LSP URIs to the last pending file progress notification."
    :accessor rocq--pending-file-progress
    :initform '())
   (pending-file-progress-timer
    :documentation "Timer for last pending file progress debouncing."
    :accessor rocq--pending-file-progress-timer
    :initform nil)
   (check-on-request
    :documentation "Whether to only check on request."
    :reader rocq-check-on-request
    :initform t)
   (goal-after-tactic
    :documentation "Whether to show the goals after the sentence under point."
    :reader rocq-goal-after-tactic
    :initform t)))


;; XXX: This should really be a generic in eglot.
;; We don't want eglot to use its configuration system because it relies solely on dir
;; local variables.  We're a major mode, we can do this ourselves.
(define-advice eglot--workspace-configuration-plist
    (:around (oldfun server) rocq-configuration-plist)
  (if (rocq--lsp-server-child-p server)
      (eglot-initialization-options server)
    (oldfun server)))

(cl-defmethod eglot-initialization-options ((server rocq--lsp-server))
  (list
   :goal_after_tactic (if (rocq-goal-after-tactic server) t :json-false)
   :show_coq_info_messages :json-false
   :pp_type 0
   :send_perf_data (if (rocq-timing-data server) t :json-false)
   :check_only_on_request (if (rocq-check-on-request server) t :json-false)))

(cl-defgeneric (setf rocq-check-on-request) (value server)
  ""
  (:method
   (value (server rocq--lsp-server))
   (setf (slot-value server 'check-on-request) value)
   (eglot-signal-didChangeConfiguration server)))

(cl-defgeneric (setf rocq-timing-data) (value server)
  ""
  (:method
   (value (server rocq--lsp-server))
   (setf (slot-value server 'timing-data) value)
   (eglot-signal-didChangeConfiguration server)
   (unless value
     (mapc
      (lambda (buffer)
        (mapc #'delete-overlay rocq-mode--timing-overlays)
        (setq rocq-mode--timing-overlays '()))
      (eglot--managed-buffers server)))))

(cl-defgeneric (setf rocq-goal-after-tactic) (value server)
  ""
  (:method
   (value (server rocq--lsp-server))
   (setf (slot-value server 'goal-after-tactic) value)
   (eglot-signal-didChangeConfiguration server)))

(defun rocq--workspace-folder-repr (folder)
  (list :uri (eglot--path-to-uri folder)
        :name (abbreviate-file-name folder)))

(cl-defgeneric (setf rocq-workspace-folders) (value server)
  ""
  (:method
   (value (server rocq--lsp-server))
   (let ((prev-val (slot-value server 'workspace-folders)))
     (jsonrpc-notify
      server :workspace/didChangeWorkspaceFolders
      (list
       :event
       (list
        :added (seq-into
                (mapcar #'rocq--workspace-folder-repr (cl-set-difference value prev-val :test #'equal))
                'vector)
        :removed (seq-into
                  (mapcar #'rocq--workspace-folder-repr (cl-set-difference prev-val value :test #'equal))
                  'vector)))))
   (setf (slot-value server 'workspace-folders) value)))

(cl-defmethod eglot-workspace-folders
  ((server rocq--lsp-server))
  (let ((project (eglot--project server)))
    (seq-into
     (mapcar #'rocq--workspace-folder-repr
             (rocq-workspace-folders server))
     'vector)))

(defun rocq-add-workspace-folder (folder)
  "Add a folder to the workspace."
  (interactive "DSelect folder to add: ")
  (let ((server (eglot--current-server-or-lose)))
    (if (member folder (rocq-workspace-folders server))
        (message "%s is already in the Rocq workspace folders." folder)
      (push folder (rocq-workspace-folders server)))))

(defun rocq-remove-workspace-folder (folder)
  "Remove a folder to the workspace."
  (interactive
   (let ((server (eglot--current-server-or-lose)))
     (list
      (completing-read
       "Select folder to remove: "
       (rocq-workspace-folders server)
       nil t))))
  (setf (rocq-workspace-folders server) (delete folder (rocq-workspace-folders server) :test #'equal)))

(defun rocq-toggle-check-on-request ()
  "Toggle checking on request."
  (interactive)
  (let* ((server (eglot--current-server-or-lose)))
    (setf (rocq-check-on-request server) (not (rocq-check-on-request server)))))

(defun rocq-toggle-timing-data ()
  "Toggle timing data display.

Which commands are considered slow and thus highlighted is governed by the
customizable variable `rocq-mode-too-slow'."
  (interactive)
  (let* ((server (eglot--current-server-or-lose)))
    (setf (rocq-timing-data server) (not (rocq-timing-data server)))))

(defun rocq-toggle-goal-after-tactic ()
  ""
  (interactive)
  (let* ((server (eglot--current-server-or-lose)))
    (setf (rocq-goal-after-tactic server) (not (rocq-goal-after-tactic server)))))


;; Goal display

(define-derived-mode rocq-goals-mode magit-section-mode "Goals"
  "Rocq Goals")

(defvar-local rocq--last-goal-request-state nil)

(defun rocq--goal-request-state ()
  "Builds a goal request state."
  (list (buffer-modified-tick) (point)))

(defface rocq-goal-face
  `()
  "")

(defun rocq--insert-goal (goal &optional num)
  "Insert a single goal into the buffer."
  (eglot--dbind (hyps ty) goal
    (magit-insert-section (magit-section goal (not (and num (eql num 1))))
      (magit-insert-heading
        (format "%s%s\n"
                (if num (propertize (format "%d: " num) 'face 'bold) "")
                (propertize ty 'face 'rocq-goal-face)))
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
        (state (rocq--goal-request-state))
        (bufname (format "*Goals %s*" (buffer-name))))
    (unless (equal state
                   rocq--last-goal-request-state)
      (setq rocq--last-goal-request-state state)
      (jsonrpc-async-request
       serv
       :proof/goals
       (list
        :textDocument (eglot--TextDocumentIdentifier)
        :position (eglot--pos-to-lsp-position (point)))
       :success-fn
       (eglot--lambda (goals messages)
         (with-current-buffer (get-buffer-create bufname)
           (when (eq major-mode 'fundamental-mode)
             (rocq-goals-mode))
           (let ((inhibit-read-only t))
             (erase-buffer)
             (magit-insert-section (magit-section)
               (eglot--dbind (goals shelf) goals
                 (magit-insert-section (magit-section)
                   (magit-insert-heading
                     (format "Focused goals (%d)\n" (length goals)))
                   (cl-loop for i from 0 to (- (length goals) 1)
                            do (rocq--insert-goal (aref goals i) (+ i 1))))
                 (newline)
                 (magit-insert-section (magit-section shelf t)
                   (magit-insert-heading
                     (format "Shelf (%d)\n" (length shelf)))
                   (cl-loop for i from 0 to (- (length shelf) 1)
                            do (rocq--insert-goal (aref shelf i)))))
               (newline)
               (magit-insert-section (magit-section)
                 (magit-insert-heading
                   (format "Messages (%d)\n" (length messages)))
                 (mapc (eglot--lambda (text)
                         (insert text "\n"))
                       messages))))
           (display-buffer
            (current-buffer)
            `(display-buffer-reuse-mode-window . ((inhibit-same-window . ,t))))))))))

(defvar rocq--idle-goals-timer
  nil)

(defcustom rocq-mode-idle-goals-delay
  0.5
  "Delay used for automatic goal refreshing."
  :type '(number))

(defun rocq--setup-goals-timer ()
  (or rocq--idle-goals-timer
      (setq
       rocq--idle-goals-timer
       (run-with-idle-timer
        rocq-mode-idle-goals-delay nil
        (lambda ()
          (setq rocq--idle-goals-timer nil)
          (when (eq major-mode 'rocq-mode)
            (rocq-goals)))))))


;; Processing overlay

(defvar-local rocq-mode--processing-overlays
    nil
  "")

(defface rocq-mode-processing-face
  `((t :background "light gray"))
  "")

(defcustom rocq-mode-file-progress-debounce-time
  0.2
  "Debounce time for file progress update handling."
  :type '(number))

(cl-defun rocq--update-file-progress (uri &key textDocument processing)
  (if-let* ((path (expand-file-name (eglot--uri-to-path uri)))
            (buffer (find-buffer-visiting path)))
      (with-current-buffer buffer
        (when (equal (eglot--VersionedTextDocumentIdentifier) textDocument)
          (mapc #'delete-overlay rocq-mode--processing-overlays)
          (setq rocq-mode--processing-overlays '())
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
                processing)))))

(defun rocq--debounced-file-progress-handling (server)
  (mapc (lambda (l) (apply #'rocq--update-file-progress l))
        (rocq--pending-file-progress server))
  (setf (rocq--pending-file-progress server) '())
  (setf (rocq--pending-file-progress-timer server) nil))

(cl-defmethod eglot-handle-notification
  ((server rocq--lsp-server) (_method (eql $/coq/fileProgress)) &key textDocument processing)
  (eglot--dbind (uri) textDocument
    (unless (timerp (rocq--pending-file-progress-timer server))
      (run-with-timer rocq-mode-file-progress-debounce-time
                      nil #'rocq--debounced-file-progress-handling server))
    (setf (alist-get uri (rocq--pending-file-progress server) nil nil #'equal)
          (list :textDocument textDocument :processing processing))))


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
            (setq rocq-mode--timing-overlays '())
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


;; Misc commands

(defun rocq-save-vo ()
  "Save the .vo file corresponding to the current buffer."
  (interactive)
  (let ((server (eglot--current-server-or-lose)))
    (jsonrpc-async-request
     server :coq/saveVo
     (list
      :textDocument (eglot--VersionedTextDocumentIdentifier))
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
     (list
      :textDocument (eglot--VersionedTextDocumentIdentifier))
     :success-fn
     (lambda (_) (message "%s"
                          (propertize "Successfully trimmed memory"
                                      'face 'success)))
     :error-fn
     (lambda (_) (message "%s"
                          (propertize "Failed to trim memory"
                                      'face 'error))))))


;; Update view

(defcustom rocq-mode-scroll-delay
  0.5
  "Debounce time before scroll notification is sent to coq-lsp."
  :type '(number))

(defvar rocq-mode--scroll-timer nil)

(defun rocq-mode--update-view (dstart dend buffer)
  (setq rocq-mode--scroll-timer nil)
  (with-current-buffer buffer
    (let ((server (eglot--current-server-or-lose)))
      (jsonrpc-notify
       server :coq/viewRange
       (list
        :textDocument (eglot--VersionedTextDocumentIdentifier)
        :range (list
                :start (eglot--pos-to-lsp-position dstart)
                :end (eglot--pos-to-lsp-position dend)))))))

(defun rocq-mode--scroll-function (window _)
  (when (timerp rocq-mode--scroll-timer)
    (cancel-timer rocq-mode--scroll-timer))
  (setq rocq-mode--scroll-timer
        (run-at-time rocq-mode-scroll-delay nil #'rocq-mode--update-view
                     (window-group-start window)
                     (window-group-end window t)
                     (window-buffer window))))


;; General mode setup

(defvar rocq-mode-syntax-table
  (let ((st (make-syntax-table))) ; define-derived-mode will add the proper parent
    (modify-syntax-entry ?\( "()1" st)
    (modify-syntax-entry ?\* ". 23n" st)
    (modify-syntax-entry ?\) ")(4" st)
    st))

(defvar-keymap rocq-mode-map
  :doc "Keymap for Rocq interaction."
  "C-c C-," #'rocq-goals)

;;;###autoload
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs (cons 'rocq-mode (list 'rocq--lsp-server "coq-lsp"))))

;;;###autoload
(define-derived-mode rocq-mode prog-mode "Rocq"
  "Major mode for Rocq files, using coq-lsp.

Key bindings:
\\{rocq-mode-map}"
  (when-let ((server (eglot-current-server))
             (rocq-proj-dir (locate-dominating-file (buffer-file-name) "_CoqProject"))
             ((not (member rocq-proj-dir (rocq-workspace-folders server)))))
    (rocq-add-workspace-folder rocq-proj-dir))
  (eglot-ensure)
  (setq-local comment-start "(*"
              comment-end "*)"
              comment-style 'multi-line)
  (setq font-lock-defaults
        `(((,(regexp-opt rocq-vernac-commands 'symbols) . 'rocq-vernac-commands)
           (,(regexp-opt rocq-gallina-keywords 'symbols) . 'rocq-gallina-keywords)
           (,(regexp-opt rocq-sorts 'symbols) . 'rocq-sorts)
           (,(regexp-opt rocq-tactics 'symbols) . 'rocq-tactics)
           (,(regexp-opt rocq-terminators 'symbols) . 'rocq-terminators)
           (,(regexp-opt rocq-control 'symbols) . 'rocq-control)))))

(define-minor-mode rocq-follow-viewport-mode
  "Send notifications of the viewport position to coq-lsp."
  :init-value nil
  :global nil
  (if rocq-follow-viewport-mode
      (progn
        (add-hook 'window-scroll-functions #'rocq-mode--scroll-function 0 t)
        (rocq-mode--update-view (window-group-start) (window-group-end) (window-buffer)))
    (remove-hook 'window-scroll-functions #'rocq-mode--scroll-function)))

(define-minor-mode rocq-auto-goals-at-point-mode
  "Automatically request goals at point."
  :init-value nil
  :global nil
  (if rocq-auto-goals-at-point-mode
      (progn
        (add-hook 'post-command-hook #'rocq--setup-goals-timer 0 t))
    (remove-hook 'post-command-hook #'rocq--setup-goals-timer t)))

(provide 'rocq-mode)
