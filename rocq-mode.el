;;; rocq-mode.el ---- Rocq mode using coq-lsp  -*- lexical-binding: t -*-

;; Author: Josselin Poiret <dev@jpoiret.xyz>
;; Version: 0.2
;; Package-Requires: ((eglot "1.12"))
;; Keywords: coq, rocq
;; URL: https://codeberg.org/jpoiret/rocq-mode.el

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'eglot)

(require 'rocq-syntax)

(defgroup rocq nil
  "Rocq mode using coq-lsp."
  :group 'languages
  :prefix "rocq-")


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
    :documentation
    "Alist mapping LSP URIs to the last pending file progress notification."
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
;; We don't want eglot to use its configuration system because it relies
;; solely on dir local variables.  We're a major mode, we can do this
;; ourselves.
(define-advice eglot--workspace-configuration-plist
    (:around (oldfun server) rocq-configuration-plist)
  (if (cl-typep server 'rocq--lsp-server)
      (eglot-initialization-options server)
    (funcall oldfun server)))

(cl-defmethod eglot-initialization-options ((server rocq--lsp-server))
  (list
   :goal_after_tactic (if (rocq-goal-after-tactic server) t :json-false)
   :show_coq_info_messages :json-false
   :pp_type 0
   :send_perf_data (if (rocq-timing-data server) t :json-false)
   :check_only_on_request (if (rocq-check-on-request server) t :json-false)
   :messages_follow_goal t))

(cl-defgeneric (setf rocq-check-on-request) (value server)
  ""
  (:method
   (value (server rocq--lsp-server))
   (setf (slot-value server 'check-on-request) value)
   (eglot-signal-didChangeConfiguration server)))

(defvar rocq-mode--timing-overlays)
(defvar rocq-mode--processing-overlays)
(defvar rocq--last-request-overlay)

(cl-defgeneric (setf rocq-timing-data) (value server)
  ""
  (:method
   (value (server rocq--lsp-server))
   (setf (slot-value server 'timing-data) value)
   (eglot-signal-didChangeConfiguration server)
   (unless value
     (mapc
      (lambda (buffer)
        (with-current-buffer buffer
          (mapc #'delete-overlay rocq-mode--timing-overlays)
          (setq rocq-mode--timing-overlays '())))
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
                (mapcar #'rocq--workspace-folder-repr
                        (cl-set-difference value prev-val :test #'equal))
                'vector)
        :removed (seq-into
                  (mapcar #'rocq--workspace-folder-repr
                          (cl-set-difference prev-val value :test #'equal))
                  'vector)))))
   (setf (slot-value server 'workspace-folders) value)))

(cl-defmethod eglot-workspace-folders
  ((server rocq--lsp-server))
  "Eglot's default folders (the project roots) plus the Rocq ones."
  (vconcat
   (mapcar #'rocq--workspace-folder-repr (rocq-workspace-folders server))
   (cl-call-next-method)))

(defun rocq--workspace-folder-p (server folder)
  "Return non-nil if FOLDER is already a workspace folder of SERVER."
  (let ((uri (plist-get (rocq--workspace-folder-repr folder) :uri)))
    (seq-some (lambda (repr) (equal (plist-get repr :uri) uri))
              (eglot-workspace-folders server))))

(defun rocq-add-workspace-folder (folder)
  "Add a folder to the workspace."
  (interactive "DSelect folder to add: ")
  (let ((server (eglot--current-server-or-lose)))
    (if (rocq--workspace-folder-p server folder)
        (message "%s is already in the Rocq workspace folders." folder)
      (push folder (rocq-workspace-folders server)))))

(defcustom rocq-project-files '("_RocqProject" "_CoqProject")
  "File names marking the root of a Rocq project.
The nearest directory above a Rocq file containing one of these files
is registered as a workspace folder, so that coq-lsp reads the project
file there.  See `rocq-project-root-function'."
  :type '(repeat string))

(defcustom rocq-project-root-function #'rocq-default-project-root
  "Function returning the root of the Rocq project of the current buffer.
It is called with no arguments in a buffer visiting a Rocq file, and
should return a directory name, or nil if the file belongs to no
project.  It is not consulted when `rocq-project-root' is set."
  :type 'function)

(defvar-local rocq-project-root nil
  "Root of the Rocq project of the current buffer, or nil to detect it.
Relative names are expanded against the buffer's `default-directory'.
Intended to be set from `.dir-locals.el' for projects whose layout
`rocq-project-root-function' does not detect.")
;;;###autoload
(put 'rocq-project-root 'safe-local-variable #'stringp)

(defun rocq-default-project-root ()
  "Return the nearest directory above the current file with a project file.
Project files are the ones listed in `rocq-project-files'."
  (and buffer-file-name
       (locate-dominating-file
        buffer-file-name
        (lambda (dir)
          (seq-some (lambda (file) (file-exists-p (expand-file-name file dir)))
                    rocq-project-files)))))

(defun rocq--project-root ()
  "Return the root of the Rocq project of the current buffer, or nil."
  (if rocq-project-root
      (file-name-as-directory (expand-file-name rocq-project-root))
    (funcall rocq-project-root-function)))

(defun rocq--register-project-root ()
  "Register the Rocq project of the current buffer as a workspace folder.

coq-lsp looks for project files only at the root of its workspace
folders, and picks the workspace of a file when the file is opened.
This therefore has to run before `textDocument/didOpen' is sent, which
is why it goes on `eglot--managed-mode-hook' rather than on the public
`eglot-managed-mode-hook', which Eglot runs after that notification."
  (when-let* (((eglot-managed-p))
              (server (eglot-current-server))
              ((cl-typep server 'rocq--lsp-server))
              (root (rocq--project-root))
              ((not (rocq--workspace-folder-p server root))))
    (push root (rocq-workspace-folders server))))

(defun rocq--clear-overlays ()
  "Remove the processing, timing and last request overlays of the buffer."
  (mapc #'delete-overlay rocq-mode--processing-overlays)
  (setq rocq-mode--processing-overlays '())
  (mapc #'delete-overlay rocq-mode--timing-overlays)
  (setq rocq-mode--timing-overlays '())
  (when rocq--last-request-overlay
    (delete-overlay rocq--last-request-overlay)))

(defun rocq--on-managed-mode-change ()
  "React to Eglot starting or stopping to manage the current buffer.
When it stops, after `eglot-shutdown' for instance, remove the overlays
that reflect server state, as they would otherwise linger."
  (if (eglot-managed-p)
      (rocq--register-project-root)
    (rocq--clear-overlays)))

(defun rocq-remove-workspace-folder (folder)
  "Remove a folder to the workspace."
  (interactive
   (let ((server (eglot--current-server-or-lose)))
     (list
      (completing-read
       "Select folder to remove: "
       (rocq-workspace-folders server)
       nil t))))
  (let ((server (eglot--current-server-or-lose)))
    (setf (rocq-workspace-folders server)
          (remove folder (rocq-workspace-folders server)))))

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
  "Toggle whether the sentence under point is run when requesting state."
  (interactive)
  (let* ((server (eglot--current-server-or-lose)))
    (setf (rocq-goal-after-tactic server)
          (not (rocq-goal-after-tactic server)))))


;; Goal display

(defvar-local rocq--last-request-state nil
  "State of the buffer when goals were last requested, or nil.
See `rocq--goal-request-state'.  Used to skip repeated requests for the
same state and to ignore responses that are no longer current.")

(defvar-local rocq--last-request-overlay nil
  "Overlay highlighting the sentence of the last goal request, or nil.")

;; The following variables are local to the status buffer.

(defvar-local rocq--goals nil
  "Goals from the last `proof/goals' response shown in the status buffer.
This is the `goals' object of the response, with the `goals', `stack'
and `shelf' keys.")

(defvar-local rocq--messages nil
  "Messages from the last `proof/goals' response.")

(defvar-local rocq--selected-goal nil
  "Goal currently shown in the goal buffer, or nil to show the messages.
One of `(goal . INDEX)', `(stack . (LEVEL SIDE . INDEX))' or
`(shelf . INDEX)', indexing into `rocq--goals'.")

(defcustom rocq-top-goal
  t
  "Whether to show the goal at the top or bottom."
  :type '(choice (const :tag "Display goal at the top" t)
                 (const :tag "Display goal at the bottom" nil)))

(defcustom rocq-show-stack
  nil
  "Whether to show the full goal stack."
  :type '(choice (const :tag "Show the stack" t)
                 (const :tag "Don't show the stack" nil))
  :local t)

(defun rocq-toggle-show-stack ()
  ""
  (interactive)
  (setq rocq-show-stack (not rocq-show-stack))
  (rocq-refresh-status))

(defcustom rocq-show-shelf
  nil
  "Whether to show the shelf."
  :type '(choice (const :tag "Show the shelf" t)
                 (const :tag "Don't show the shelf" nil))
  :local t)

(defun rocq-toggle-show-shelf ()
  ""
  (interactive)
  (setq rocq-show-shelf (not rocq-show-shelf))
  (rocq-refresh-status))

(defun rocq-show-messages ()
  ""
  (interactive)
  (setq rocq--selected-goal nil)
  (rocq-refresh-status))

(defvar-keymap rocq-status-mode-map
  :doc "Keymap for Rocq interaction."
  "RET" #'rocq-select-goal
  "s f" #'rocq-toggle-show-stack
  "s s" #'rocq-toggle-show-shelf
  "m" #'rocq-show-messages)

(define-derived-mode rocq-status-mode special-mode "Rocq Status"
  "Rocq Status"
  (rocq--setup-font-lock))

(define-derived-mode rocq-goal-mode special-mode "Rocq Goal"
  "Rocq Goal"
  (rocq--setup-font-lock))

(defun rocq--status-buffer (buf)
  ""
  (unless (with-current-buffer buf
            (derived-mode-p 'rocq-mode))
    (error "Buffer is not a Rocq buffer."))
  (get-buffer-create
   (format "*Rocq Status %s*" (buffer-name buf))))

(defun rocq-switch-to-status ()
  "Switch to the status buffer for the current Rocq buffer.
Display the buffer if needed."
  (interactive)
  (when-let
      ((action-alist
        `((,#'display-buffer-reuse-mode-window
           ,#'display-buffer-below-selected)
          (mode . rocq-status-mode)))
       (window
        (display-buffer
         (rocq--status-buffer (current-buffer))
         action-alist)))
    (select-window window)))

(defun rocq--goal-request-state ()
  "Builds a goal request state."
  (list (buffer-modified-tick) (point)))

(defface rocq-goal-face
  `()
  "Face for Rocq goals.")

(defface rocq-mode-last-request
  `((t :background "DarkSeaGreen1"))
  "Face for sentence of last goal request.")

(defun rocq-go-to-last-request ()
  (interactive)
  (if-let* ((overlay rocq--last-request-overlay)
            (pos (overlay-start overlay)))
      (goto-char pos)
    (message "%s"
             (propertize "No last goal request to go back to."
                         'face 'error))))

(defun rocq--insert-separator ()
  "Insert a separator between hypotheses and goal."
  (insert
   (make-string 6 ?─))
  (newline))

(defun rocq--insert-goal (goal &optional _num)
  "Insert a single goal into the buffer."
  (eglot--dbind (hyps ty) goal
    (let ((ty (propertize ty 'font-lock-face 'rocq-goal-face)))
      (when rocq-top-goal
        (insert ty)
        (newline)
        (rocq--insert-separator))
      (mapc (eglot--lambda (names def ty)
              (mapc
               (lambda (name)
                 (insert (propertize name 'font-lock-face
                                     'font-lock-variable-name-face)
                         " "))
               names)
              (let ((pt (point)))
                (when def
                  (insert ":= " def "\n"))
                (insert ": " ty "\n")
                (indent-region pt (point) 2)))
            hyps)
      (unless rocq-top-goal
        (rocq--insert-separator)
        (insert ty)
        (newline)))))

(defun rocq-request ()
  "Request current Rocq status."
  (interactive)
  (let ((serv (eglot--current-server-or-lose))
        (state (rocq--goal-request-state))
        (buf (current-buffer)))
    (unless (equal state
                   rocq--last-request-state)
      (setq rocq--last-request-state state)
      (jsonrpc-async-request
       serv
       :proof/goals
       (list
        :textDocument (eglot--TextDocumentIdentifier)
        :position (eglot--pos-to-lsp-position (point)))
       :success-fn
       (eglot--lambda (textDocument range goals messages)
         (with-current-buffer buf
           (when (equal state rocq--last-request-state)
             (unless rocq--last-request-overlay
               (setq rocq--last-request-overlay (make-overlay 1 1))
               (overlay-put rocq--last-request-overlay
                            'face 'rocq-mode-last-request))
             (if range
                 (when (equal (eglot--VersionedTextDocumentIdentifier)
                              textDocument)
                   (eglot--dbind (start end) range
                     (move-overlay rocq--last-request-overlay
                                   (eglot--lsp-position-to-point start)
                                   (eglot--lsp-position-to-point end))))
               (delete-overlay rocq--last-request-overlay))
             (with-current-buffer (rocq--status-buffer buf)
               (when (eq major-mode 'fundamental-mode)
                 (rocq-status-mode))
               (setq rocq--goals goals)
               (setq rocq--messages messages)
               (setq rocq--selected-goal
                     (eglot--dbind (goals stack shelf) goals
                       (ignore stack shelf)
                       (if (eql (length goals) 0)
                           nil
                         `(goal . 0))))
               (rocq-refresh-status)))))))))

(defun rocq--get-goal (spec)
  "Get goal corresponding to SPEC."
  (eglot--dbind (goals stack shelf) rocq--goals
    (pcase spec
      (`(goal . ,i) (aref goals i))
      (`(stack . (,height ,position . ,num))
       (let* ((slice (aref stack height))
              (slice (elt slice position)))
         (aref slice num)))
      (`(shelf . ,i) (aref shelf i))
      (_ nil))))

(defun rocq-select-goal ()
  "Select goal under point."
  (interactive)
  (if-let (goal (get-text-property (point) 'rocq--goal))
      (progn
        (setq rocq--selected-goal goal)
        (rocq-refresh-status))
    (message "No goal under point.")))

(defun rocq--insert-goal-line (spec with-stack)
  "Insert a line for goal given by SPEC at point.
If WITH-STACK is non-nil, indent the line according to its depth in
the goal stack."
  (let* ((stack-len (eglot--dbind (stack) rocq--goals (length stack)))
         (indent
          (if with-stack
              (pcase spec
                (`(goal . ,_) stack-len)
                (`(stack . (,height ,_ . ,_)) (- stack-len height 1))
                (`(shelf . ,_) 0)
                (_ nil))
            0)))
    (eglot--dbind (info ty) (rocq--get-goal spec)
      (let ((p (point))
            (name (when-let ((name-array (plist-get info :name)))
                    (or
                     (when-let* ((dirpath (plist-get name-array :dirpath))
                                 (name-array (plist-get name-array :basename)))
                       (mapconcat (lambda (x) (elt x 1))
                                  (nreverse (vconcat (vector name-array)
                                                     (elt dirpath 1)))
                                  "."))
                     (elt name-array 1))))
            (num (pcase spec (`(goal . ,i) (+ i 1)) (_ nil))))
        (when (or name num)
          (when num (insert (format "%d" num)))
          (when name (insert "(?" name ")"))
          (insert ": "))
        (insert (propertize "G"
                            'display
                            `(left-fringe ,(if (equal spec rocq--selected-goal)
                                               'filled-square
                                             'hollow-square))))
        (insert ty)
        (indent-rigidly p (point) indent)
        (insert ?\n)
        (add-text-properties
         p (point)
         `(rocq--goal ,spec))))))

(defun rocq-refresh-status ()
  ""
  (interactive)
  (when-let ((inhibit-read-only t))
    (let ((p (point))
          (tot-goals 0))
      (erase-buffer)
      (eglot--dbind (goals stack shelf) rocq--goals
        (cl-loop for i from (- (length stack) 1) downto 0
                 do (cl-loop for j below (length (elt (aref stack i) 0))
                             do (setq tot-goals (+ tot-goals 1))
                             if rocq-show-stack
                             do (rocq--insert-goal-line
                                 `(stack . (,i 0 . ,j)) t))
                 for pt = (point)
                 do (cl-loop for j below (length (elt (aref stack i) 1))
                             do (setq tot-goals (+ tot-goals 1))
                             if rocq-show-stack
                             do (rocq--insert-goal-line
                                 `(stack . (,i 1 . ,j)) t))
                 do (goto-char pt))
        (cl-loop for i below (length goals)
                 do (setq tot-goals (+ tot-goals 1))
                 do (rocq--insert-goal-line `(goal . ,i) rocq-show-stack))
        (goto-char (point-max))
        (newline)
        (when rocq-show-shelf
          (insert "Shelf")
          (newline)
          (cl-loop for i below (length shelf)
                   do (rocq--insert-goal-line `(shelf . ,i) nil))
          (newline))
        (insert (format "%d/%d Goals  %d Shelf  %d Messages\n"
                        (length goals)
                        tot-goals
                        (length shelf)
                        (length rocq--messages))))
      (let ((goal (rocq--get-goal rocq--selected-goal))
            (messages rocq--messages))
        (with-current-buffer (get-buffer-create "*Rocq Goal*")
          (when (eq major-mode 'fundamental-mode)
            (rocq-goal-mode))
          (erase-buffer)
          (if goal
              (rocq--insert-goal goal)
            (mapc (eglot--lambda (text)
                    (insert text "\n"))
                  messages))
          (display-buffer (current-buffer))))
      (goto-char p))))

(defvar rocq--idle-goals-timer nil
  "Idle timer of the pending automatic goal request, or nil.")

(defcustom rocq-mode-idle-goals-delay
  (default-value 'eglot-send-changes-idle-time)
  "Delay used for automatic goal refreshing.

Do not bring this below `eglot-send-changes-idle-time', as otherwise
the server will not be aware of the changes required to serve the
request."
  :type '(number))

(defun rocq--setup-goals-timer ()
  "Schedule a goal request for when Emacs goes idle.
Nothing is scheduled while no server manages the current buffer, for
instance after `eglot-shutdown'."
  (when (and (eglot-managed-p) (not rocq--idle-goals-timer))
    (setq
     rocq--idle-goals-timer
     (run-with-idle-timer
      rocq-mode-idle-goals-delay nil
      (lambda ()
        (setq rocq--idle-goals-timer nil)
        (when (and (eq major-mode 'rocq-mode) (eglot-managed-p))
          (rocq-request)))))))


;; Processing overlay

(defvar-local rocq-mode--processing-overlays nil
  "Overlays over the parts of the buffer coq-lsp has yet to check.
Updated from `$/coq/fileProgress' notifications.")

(defface rocq-mode-processing-face
  `((t :background "light gray"))
  "Face highlighting unprocessed Rocq code.")

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
                        (overlay-put overlay
                                     'face 'rocq-mode-processing-face)))))
                processing)))))

(defun rocq--debounced-file-progress-handling (server)
  (mapc (lambda (l) (apply #'rocq--update-file-progress l))
        (rocq--pending-file-progress server))
  (setf (rocq--pending-file-progress server) '())
  (setf (rocq--pending-file-progress-timer server) nil))

(cl-defmethod eglot-handle-notification
  ((server rocq--lsp-server) (_method (eql $/coq/fileProgress))
   &key textDocument processing)
  (eglot--dbind (uri) textDocument
    (unless (timerp (rocq--pending-file-progress-timer server))
      (run-with-timer rocq-mode-file-progress-debounce-time
                      nil #'rocq--debounced-file-progress-handling server))
    (setf (alist-get uri (rocq--pending-file-progress server) nil nil #'equal)
          (list :textDocument textDocument :processing processing))))


;; Timing data display

(defvar-local rocq-mode--timing-overlays nil
  "Overlays over the commands coq-lsp reported as slow.
Updated from `$/coq/filePerfData' notifications; see `rocq-mode-too-slow'.")

(defcustom rocq-mode-too-slow
  0.3
  "Time in seconds above which a command is considered slow."
  :type '(number))

(defface rocq-mode-slow-face
  `((t :background "firebrick4"
       :foreground "white"))
  "Face used for slow commands.")

(cl-defmethod eglot-handle-notification
  ((_server rocq--lsp-server) (_method (eql $/coq/filePerfData))
   &key textDocument summary timings)
  (ignore summary)
  (eglot--dbind (uri) textDocument
    (if-let* ((path (expand-file-name (eglot--uri-to-path uri)))
              (buffer (find-buffer-visiting path)))
        (with-current-buffer buffer
          (when (equal (eglot--VersionedTextDocumentIdentifier) textDocument)
            (mapc #'delete-overlay rocq-mode--timing-overlays)
            (setq rocq-mode--timing-overlays '())
            (mapc (eglot--lambda (range info)
                    (eglot--dbind (time memory cache_hit time_hash) info
                      (ignore memory cache_hit time_hash)
                      (when (>= time rocq-mode-too-slow)
                        (eglot--dbind (start end) range
                          (let ((overlay
                                 (make-overlay
                                  (eglot--lsp-position-to-point start)
                                  (eglot--lsp-position-to-point end))))
                            (progn
                              (add-to-list 'rocq-mode--timing-overlays
                                           overlay)
                              (overlay-put overlay
                                           'face 'rocq-mode-slow-face)))))))
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
     (lambda (_)
       (message "%s\n%s"
                (propertize "Failed saving .vo file" 'face 'error)
                "Maybe you haven't finished checking the whole file?")))))

(defun rocq-reload-vos ()
  "Ask coq-lsp to reload currently loaded vo files."
  (interactive)
  (let ((server (eglot--current-server-or-lose)))
    (jsonrpc-notify
     server :coq/workspace_update
     (list
      :textDocument (eglot--VersionedTextDocumentIdentifier)))))

(defun rocq-trim ()
  "Ask coq-lsp to free memory."
  (interactive)
  (let ((server (eglot--current-server-or-lose)))
    (jsonrpc-notify
     server :coq/trimCaches
     (list
      :textDocument (eglot--VersionedTextDocumentIdentifier)))))


;; Update view

(defcustom rocq-mode-scroll-delay
  0.5
  "Debounce time before scroll notification is sent to coq-lsp."
  :type '(number))

(defvar rocq-mode--scroll-timer nil
  "Timer debouncing viewport notifications to coq-lsp, or nil.")

(defun rocq-mode--update-view (dstart dend buffer)
  (setq rocq-mode--scroll-timer nil)
  (with-current-buffer buffer
    (when-let ((server (eglot-current-server)))
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


;; Filling comments

(defun rocq--comment-start ()
  "Return the start of the comment at point, or nil.
Point counts as in a comment also when it is just before the opener
or just after the closer."
  (or (nth 8 (syntax-ppss))
      (save-excursion
        (skip-chars-forward " \t")
        (and (looking-at "(\\*") (point)))
      (save-excursion
        (skip-chars-backward " \t")
        (and (>= (- (point) 2) (point-min))
             (string= "*)"
                      (buffer-substring-no-properties (- (point) 2) (point)))
             (nth 8 (syntax-ppss (1- (point))))))))

(defun rocq--comment-bounds ()
  "Return (START . END) of the comment around point, or nil if not in one."
  (when-let ((start (rocq--comment-start)))
    (cons start
          (save-excursion
            (goto-char start)
            (if (forward-comment 1) (point) (point-max))))))

(defun rocq-fill-paragraph (&optional justify)
  "Fill the paragraph of the comment at point.
Only the text of the comment is touched: surrounding code is never
filled, and blank lines inside the comment separate paragraphs.
Outside comments, do nothing."
  (when-let ((bounds (rocq--comment-bounds)))
    (let* ((start (car bounds))
           (end (cdr bounds))
           ;; Where the comment text begins: on the next line if the
           ;; opener sits alone on its line.
           (text-beg (save-excursion
                       (goto-char start)
                       (skip-chars-forward "(*")
                       (if (looking-at "[ \t]*$")
                           (progn (forward-line 1) (point))
                         start)))
           ;; Where it ends: on the previous line if the closer sits
           ;; alone on its line.
           (text-end (save-excursion
                       (goto-char end)
                       (skip-chars-backward "*)")
                       (skip-chars-backward " \t")
                       (if (bolp) (point) end))))
      (when (< text-beg text-end)
        (save-excursion
          (goto-char (max text-beg (min (point) (1- text-end))))
          ;; On a blank line, fill the following paragraph.
          (when (save-excursion (beginning-of-line) (looking-at "[ \t]*$"))
            (skip-chars-forward " \t\n" text-end))
          (when (< (point) text-end)
            (let* ((pt (point))
                   (pbeg (if (re-search-backward "^[ \t]*$" text-beg t)
                             (progn (forward-line 1) (point))
                           text-beg))
                   (pend (progn
                           (goto-char pt)
                           (if (re-search-forward "^[ \t]*$" text-end t)
                               (match-beginning 0)
                             text-end)))
                   (fill-prefix
                    (progn
                      (goto-char pbeg)
                      (cond
                       ;; Several lines: indent like the second one.
                       ((< (save-excursion (forward-line 1) (point)) pend)
                        (forward-line 1)
                        (skip-chars-forward " \t")
                        (buffer-substring-no-properties
                         (line-beginning-position) (point)))
                       ;; One line starting with the opener: align
                       ;; continuation lines with the text after it.
                       ((= pbeg start)
                        (skip-chars-forward "(*")
                        (skip-chars-forward " \t")
                        (make-string (current-column) ?\s))
                       ;; One line of text: keep its indentation.
                       (t
                        (skip-chars-forward " \t")
                        (buffer-substring-no-properties
                         (line-beginning-position) (point)))))))
              (fill-region-as-paragraph pbeg (copy-marker pend) justify)))))))
  ;; Non-nil tells `fill-paragraph' we are done, also outside comments,
  ;; so that code is left alone.
  t)


;; General mode setup

(defvar rocq-mode-syntax-table
  ;; `define-derived-mode' will add the proper parent.
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\( "()1" st)
    (modify-syntax-entry ?\* ". 23n" st)
    (modify-syntax-entry ?\) ")(4" st)
    st)
  "Syntax table for `rocq-mode', with nestable `(* *)' comments.")

(defvar-keymap rocq-mode-map
  :doc "Keymap for Rocq interaction."
  "C-c C-," #'rocq-request
  "C-c C-M-," #'rocq-go-to-last-request
  "C-c C-c" #'rocq-switch-to-status
  "C-c C-s" #'rocq-save-vo
  "C-c C-r" #'rocq-reload-vos)

;;;###autoload
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               (cons 'rocq-mode (list 'rocq--lsp-server "coq-lsp"))))

(defun rocq--setup-font-lock ()
    ""
  (setq font-lock-defaults
        `(((,(regexp-opt rocq-vernac-commands 'symbols) . 'rocq-vernac-commands)
           (,(regexp-opt rocq-gallina-keywords 'symbols)
            . 'rocq-gallina-keywords)
           (,(regexp-opt rocq-sorts 'symbols) . 'rocq-sorts)
           (,(regexp-opt rocq-tactics 'symbols) . 'rocq-tactics)
           (,(regexp-opt rocq-terminators 'symbols) . 'rocq-terminators)
           (,(regexp-opt rocq-control 'symbols) . 'rocq-control)))))

(defcustom rocq-mode-start-server t
  "Whether to start coq-lsp automatically when entering `rocq-mode'.
When nil, start it on demand with \[eglot].  Either way, a buffer whose
project already has a running server is managed by it right away."
  :type 'boolean)

;;;###autoload
(define-derived-mode rocq-mode prog-mode "Rocq"
  "Major mode for Rocq files, using coq-lsp.

Key bindings:
\\{rocq-mode-map}"
  ;; See `rocq--register-project-root' for why the obsolete hook is used.
  ;; Unlike `eglot-managed-mode-hook', it also runs when Eglot stops
  ;; managing the buffer.
  (with-suppressed-warnings ((obsolete eglot--managed-mode-hook))
    (add-hook 'eglot--managed-mode-hook #'rocq--on-managed-mode-change
              nil t))
  (when rocq-mode-start-server
    (eglot-ensure))
  (setq-local comment-start "(*"
              comment-end "*)"
              comment-start-skip "(\\*+[ \t]*"
              comment-end-skip "[ \t]*\\*+)"
              comment-style 'multi-line
              fill-paragraph-function #'rocq-fill-paragraph)
  (rocq--setup-font-lock))

(define-minor-mode rocq-follow-viewport-mode
  "Send notifications of the viewport position to coq-lsp."
  :init-value nil
  :global nil
  (if rocq-follow-viewport-mode
      (progn
        (add-hook 'window-scroll-functions #'rocq-mode--scroll-function 0 t)
        (rocq-mode--update-view (window-group-start) (window-group-end)
                                (window-buffer)))
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
