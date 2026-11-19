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
(require 'rocq-indent)

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
(defvar rocq--last-request-state)
(defvar forward-sentence-function)

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

;; Eglot 1.16 made its URI conversions public; keep working with
;; older versions, which only have the internal names.
(defalias 'rocq--path-to-uri
  (if (fboundp 'eglot-path-to-uri)
      #'eglot-path-to-uri
    (with-suppressed-warnings ((obsolete eglot--path-to-uri))
      #'eglot--path-to-uri))
  "Return the LSP URI of a file name, see `eglot-path-to-uri'.")

(defalias 'rocq--uri-to-path
  (if (fboundp 'eglot-uri-to-path)
      #'eglot-uri-to-path
    (with-suppressed-warnings ((obsolete eglot--uri-to-path))
      #'eglot--uri-to-path))
  "Return the file name of an LSP URI, see `eglot-uri-to-path'.")

(defun rocq--workspace-folder-repr (folder)
  (list :uri (rocq--path-to-uri folder)
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
that reflect server state, as they would otherwise linger, and forget
the last request so that the next one is sent to the next server."
  (if (eglot-managed-p)
      (rocq--register-project-root)
    (rocq--clear-overlays)
    (setq rocq--last-request-state nil)))

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

(defvar-local rocq--display-pending nil
  "Whether a display of the goal buffer was asked for and is still due.
Set by `rocq-request' when called with DISPLAY and cleared once goals
are shown, so that the request that starts the server with
\\[rocq-request] still brings the goal buffer up when an automatic
request supersedes it before its response arrives.")

;; The following variables are local to the goal buffer.

(defvar-local rocq--goals nil
  "Goals from the last `proof/goals' response, or nil outside a proof.
This is the `goals' object of the response, with the `goals', `stack',
`shelf' and `given_up' keys.")

(defvar-local rocq--messages nil
  "Messages from the last `proof/goals' response.
Each is a string or a plist with `:level' and `:text'.")

(defvar-local rocq--error nil
  "Error of the sentence of the last `proof/goals' response, or nil.")

(defvar-local rocq--source nil
  "The Rocq buffer whose goals the goal buffer shows.")

(defvar-local rocq--selected-goal nil
  "Goal shown by the focused view of the goal buffer, or nil if none.
One of `(goal . INDEX)', `(stack . (LEVEL SIDE . INDEX))',
`(shelf . INDEX)' or `(given-up . INDEX)', indexing into `rocq--goals'.")

(defvar-local rocq--selected-evar nil
  "Existential variable of the selected goal, if known.
It identifies the goal across responses, so that the selection follows
it while it exists.")

(defvar-local rocq--view 'goal
  "What the goal buffer shows.
With `goal', the selected goal in full; with `summary', one line per
goal, to pick one.")

(defcustom rocq-display-goals 'explicit
  "When to display the goal buffer in a window.
With `explicit', only commands you invoke, such as `rocq-request' or
`rocq-switch-to-goals', display it; automatic refreshes update it only
if it is already visible.  With `always', every refresh displays it."
  :type '(choice (const :tag "On explicit commands only" explicit)
                 (const :tag "On every refresh" always)))

(defcustom rocq-top-goal
  t
  "Whether to show the goal at the top or bottom."
  :type '(choice (const :tag "Display goal at the top" t)
                 (const :tag "Display goal at the bottom" nil)))

(defcustom rocq-show-stack
  nil
  "Whether the goal summary lists unfocused goals, around the focused ones.
When nil, it only counts them.  Toggle it with \\<rocq-goal-mode-map>\
\\[rocq-toggle-show-stack] in the goal buffer."
  :type '(choice (const :tag "Show the stack" t)
                 (const :tag "Don't show the stack" nil))
  :local t)

(defcustom rocq-show-shelf
  nil
  "Whether the goal summary lists shelved and given up goals.
When nil, it only counts them.  Toggle it with \\<rocq-goal-mode-map>\
\\[rocq-toggle-show-shelf] in the goal buffer."
  :type '(choice (const :tag "Show the shelf" t)
                 (const :tag "Don't show the shelf" nil))
  :local t)

(defconst rocq--goal-buffer-name "*Rocq Goal*"
  "Name of the goal buffer.")

(defvar-keymap rocq-goal-mode-map
  :doc "Keymap of the goal buffer."
  "n" #'rocq-next-goal
  "p" #'rocq-previous-goal
  "TAB" #'rocq-toggle-goals-view
  "RET" #'rocq-select-goal
  "q" #'rocq-goals-quit
  "C-c C-c" #'rocq-goals-quit
  "s f" #'rocq-toggle-show-stack
  "s s" #'rocq-toggle-show-shelf)

(dolist (digit (number-sequence ?1 ?9))
  (keymap-set rocq-goal-mode-map (string digit) #'rocq-select-goal-number))

(define-derived-mode rocq-goal-mode special-mode "Rocq Goal"
  "Major mode of the goal buffer.
The buffer shows either the selected goal in full or a summary of all
goals; \\<rocq-goal-mode-map>\\[rocq-toggle-goals-view] switches between
the two.  \\[rocq-next-goal] and \\[rocq-previous-goal] cycle through
the focused goals, a digit selects a goal by number, and in the summary
\\[rocq-select-goal] selects the goal on the line.  \\[rocq-goals-quit]
goes back to the Rocq buffer.

\\{rocq-goal-mode-map}"
  (setq-local truncate-lines nil)
  (rocq--setup-font-lock))

(defun rocq--goal-buffer ()
  "Return the goal buffer, creating it if needed."
  (let ((buffer (get-buffer-create rocq--goal-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'rocq-goal-mode)
        (rocq-goal-mode)))
    buffer))

(defun rocq--display-goal-buffer ()
  "Display the goal buffer, if it exists, without selecting it.
A window already showing it is reused; otherwise `display-buffer'
chooses one, which `display-buffer-alist' can customize.  Return the
window."
  (when-let* ((buffer (get-buffer rocq--goal-buffer-name)))
    (if (eq (window-buffer) buffer)
        (selected-window)
      (display-buffer buffer
                      '((display-buffer-reuse-window
                         display-buffer-reuse-mode-window)
                        (inhibit-same-window . t)
                        (mode . rocq-goal-mode))))))

(defun rocq--goal-request-state ()
  "Return the state of the buffer that the goals at point depend on.
That is the text, through `buffer-chars-modified-tick', and point.
Text property changes do not count: Eglot refontifies the buffer right
after connecting, and an automatic request made for that would
supersede the one that started the server."
  (list (buffer-chars-modified-tick) (point)))

(defface rocq-goal-face
  `()
  "Face for Rocq goals.")

(defface rocq-mode-last-request
  `((t :background "DarkSeaGreen1"))
  "Face for sentence of last goal request.")

(defun rocq-go-to-last-request ()
  "Move point to the sentence of the last goal request."
  (interactive)
  (if-let* ((overlay rocq--last-request-overlay)
            (pos (overlay-start overlay)))
      (goto-char pos)
    (message "%s"
             (propertize "No last goal request to go back to."
                         'face 'error))))

;;;; Goal data

(defun rocq--get-goal (spec)
  "Return the goal SPEC designates in `rocq--goals', or nil."
  (eglot--dbind (goals stack shelf given_up) rocq--goals
    (ignore-errors
      (pcase spec
        (`(goal . ,i) (aref goals i))
        (`(stack . (,height ,side . ,num))
         (aref (elt (aref stack height) side) num))
        (`(shelf . ,i) (aref shelf i))
        (`(given-up . ,i) (aref given_up i))))))

(defun rocq--goal-specs ()
  "Return the specs of all goals of `rocq--goals', focused ones first."
  (eglot--dbind (goals stack shelf given_up) rocq--goals
    (append
     (cl-loop for i below (length goals) collect `(goal . ,i))
     (cl-loop for height below (length stack)
              append (cl-loop for side in '(0 1)
                              append (cl-loop for num below
                                              (length (elt (aref stack height)
                                                           side))
                                              collect
                                              `(stack . (,height ,side
                                                         . ,num)))))
     (cl-loop for i below (length shelf) collect `(shelf . ,i))
     (cl-loop for i below (length given_up) collect `(given-up . ,i)))))

(defun rocq--focused-count ()
  "Return the number of focused goals."
  (length (plist-get rocq--goals :goals)))

(defun rocq--unfocused-count ()
  "Return the number of unfocused goals, those outside the current bullet."
  (cl-loop for level across (plist-get rocq--goals :stack)
           sum (+ (length (elt level 0)) (length (elt level 1)))))

(defun rocq--goal-evar (goal)
  "Return the existential variable of GOAL, or nil."
  (plist-get (plist-get goal :info) :evar))

(defun rocq--goal-name (goal)
  "Return the name of GOAL, or nil if it has none."
  (let ((name (plist-get (plist-get goal :info) :name)))
    (cond
     ((stringp name) name)
     ((and (vectorp name) (> (length name) 1))
      (if-let* ((dirpath (plist-get name :dirpath))
                (basename (plist-get name :basename)))
          (mapconcat (lambda (x) (elt x 1))
                     (nreverse (vconcat (vector basename) (elt dirpath 1)))
                     ".")
        (elt name 1))))))

(defun rocq--select (spec)
  "Make SPEC the selected goal."
  (setq rocq--selected-goal spec
        rocq--selected-evar (rocq--goal-evar (rocq--get-goal spec))))

(defun rocq--set-goals (source goals messages error)
  "Record a response in the goal buffer.
SOURCE is the Rocq buffer it is about, and GOALS, MESSAGES and ERROR
its fields.  The selected goal is kept if it still exists; otherwise
the first focused goal is selected."
  (setq rocq--source source
        rocq--goals goals
        rocq--messages (append messages nil)
        rocq--error error)
  (rocq--select
   (or (and rocq--selected-evar
            (cl-find-if (lambda (spec)
                          (equal (rocq--goal-evar (rocq--get-goal spec))
                                 rocq--selected-evar))
                        (rocq--goal-specs)))
       (and (> (rocq--focused-count) 0) '(goal . 0)))))

;;;; Rendering

(defun rocq--plural (count noun)
  "Return COUNT and NOUN, with NOUN in the plural if COUNT is not 1."
  (format "%d %s%s" count noun (if (= count 1) "" "s")))

(defun rocq--no-goals-text ()
  "Return the text explaining why there is no focused goal."
  (let ((unfocused (rocq--unfocused-count))
        (shelved (length (plist-get rocq--goals :shelf)))
        (given-up (length (plist-get rocq--goals :given_up))))
    (cond
     ((null rocq--goals) "Not in a proof.")
     ((> unfocused 0)
      (format "This subproof is complete, but there %s %s."
              (if (= unfocused 1) "is" "are")
              (rocq--plural unfocused "unfocused goal")))
     ((or (> shelved 0) (> given-up 0))
      (format "No more goals, but there %s %s."
              (if (= (+ shelved given-up) 1) "is" "are")
              (string-join
               (delq nil
                     (list (and (> shelved 0)
                                (rocq--plural shelved "shelved goal"))
                           (and (> given-up 0)
                                (rocq--plural given-up "given up goal"))))
               " and ")))
     (t "No more goals."))))

(defun rocq--goal-header ()
  "Return the header line of the goal buffer."
  (let* ((focused (rocq--focused-count))
         (unfocused (rocq--unfocused-count))
         (shelved (length (plist-get rocq--goals :shelf)))
         (given-up (length (plist-get rocq--goals :given_up)))
         (name (rocq--goal-name (rocq--get-goal rocq--selected-goal)))
         (name (if name (format " (%s)" name) ""))
         (summary (eq rocq--view 'summary))
         (main
          (cond
           ((null rocq--goals) "Not in a proof")
           (summary (rocq--plural focused "goal"))
           ((null rocq--selected-goal)
            (if (zerop (+ unfocused shelved given-up))
                "No more goals"
              "No focused goals"))
           (t (pcase rocq--selected-goal
                (`(goal . ,i) (format "Goal %d/%d%s" (1+ i) focused name))
                (`(stack . ,_) (concat "Unfocused goal" name))
                (`(shelf . ,i) (format "Shelved goal %d/%d%s" (1+ i) shelved
                                       name))
                (`(given-up . ,i) (format "Given up goal %d/%d%s" (1+ i)
                                          given-up name))))))
         (parts
          (delq nil
                (list main
                      (and (> unfocused 0) (format "%d unfocused" unfocused))
                      (and (> shelved 0) (format "%d shelved" shelved))
                      (and (> given-up 0) (format "%d given up" given-up))
                      (and rocq--error "error")
                      (and rocq--messages
                           (rocq--plural (length rocq--messages) "message"))))))
    (concat " " (string-join parts "  ·  ")
            (propertize (if summary "   TAB: selected goal" "   TAB: all goals")
                        'face 'shadow))))

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

(defun rocq--insert-message (message)
  "Insert MESSAGE, a string or a plist with `:level' and `:text'."
  (let* ((text (if (stringp message) message (plist-get message :text)))
         (level (and (listp message) (plist-get message :level)))
         (face (pcase level (1 'error) (2 'warning))))
    (insert (if face (propertize text 'font-lock-face face) text) "\n")))

(defun rocq--insert-focused-view ()
  "Insert the selected goal, with the error and messages of the sentence."
  (when rocq--error
    (insert (propertize rocq--error 'font-lock-face 'error) "\n\n"))
  (if-let* ((goal (rocq--get-goal rocq--selected-goal)))
      (rocq--insert-goal goal)
    (insert (rocq--no-goals-text) "\n"))
  (when rocq--messages
    (insert "\n" (propertize "Messages" 'font-lock-face 'bold) "\n")
    (mapc #'rocq--insert-message rocq--messages)))

(defun rocq--insert-goal-line (spec indent)
  "Insert a summary line for the goal SPEC, indented by INDENT columns."
  (let* ((goal (rocq--get-goal spec))
         (name (rocq--goal-name goal))
         (number (pcase spec (`(goal . ,i) (1+ i))))
         (start (point)))
    (insert (if (equal spec rocq--selected-goal) "▶ " "  ")
            (make-string indent ?\s))
    (when number
      (insert (format "%d" number)))
    (when name
      (insert (format "(%s)" name)))
    (when (or number name)
      (insert ": "))
    (insert (propertize (string-trim (replace-regexp-in-string
                                      "[ \t\n]+" " " (plist-get goal :ty)))
                        'font-lock-face 'rocq-goal-face))
    (insert "\n")
    (put-text-property start (point) 'rocq--goal spec)))

(defun rocq--insert-section (title specs shown toggle)
  "Insert the goals SPECS under TITLE, or only their count if SHOWN is nil.
TOGGLE is the variable controlling SHOWN, which \\<rocq-goal-mode-map>\
\\[rocq-select-goal] on the title flips."
  (when specs
    (insert "\n")
    (let ((start (point)))
      (insert (propertize (format "%s (%d)%s" title (length specs)
                                  (if shown "" "  RET to show"))
                          'font-lock-face 'bold)
              "\n")
      (put-text-property start (point) 'rocq--toggle toggle))
    (when shown
      (mapc (lambda (spec) (rocq--insert-goal-line spec 0)) specs))))

(defun rocq--insert-summary-view ()
  "Insert one line per goal, with the unfocused ones around them if shown."
  (eglot--dbind (goals stack shelf given_up) rocq--goals
    (let ((depth (length stack)))
      (when rocq-show-stack
        (cl-loop for height from (1- depth) downto 0
                 do (dotimes (num (length (elt (aref stack height) 0)))
                      (rocq--insert-goal-line `(stack . (,height 0 . ,num))
                                              (- depth height 1)))))
      (if (zerop (length goals))
          (insert (if rocq-show-stack (make-string (+ depth 2) ?\s) "")
                  (rocq--no-goals-text) "\n")
        (dotimes (i (length goals))
          (rocq--insert-goal-line `(goal . ,i) (if rocq-show-stack depth 0))))
      (when rocq-show-stack
        (cl-loop for height below depth
                 do (dotimes (num (length (elt (aref stack height) 1)))
                      (rocq--insert-goal-line `(stack . (,height 1 . ,num))
                                              (- depth height 1)))))
      (unless rocq-show-stack
        (rocq--insert-section
         "Unfocused" (cl-remove-if-not (lambda (spec) (eq (car spec) 'stack))
                                       (rocq--goal-specs))
         nil 'rocq-show-stack))
      (rocq--insert-section
       "Shelved" (cl-loop for i below (length shelf) collect `(shelf . ,i))
       rocq-show-shelf 'rocq-show-shelf)
      (rocq--insert-section
       "Given up" (cl-loop for i below (length given_up)
                           collect `(given-up . ,i))
       rocq-show-shelf 'rocq-show-shelf))))

(defun rocq--goto-selected-line ()
  "Move point to the summary line of the selected goal, if any."
  (goto-char (point-min))
  (when rocq--selected-goal
    (let ((pos (point-min)))
      (while (and pos (not (equal (get-text-property pos 'rocq--goal)
                                  rocq--selected-goal)))
        (setq pos (next-single-property-change pos 'rocq--goal)))
      (when pos
        (goto-char pos)))))

(defun rocq--refresh-goals (&optional display preserve)
  "Redraw the goal buffer, which must be current, from the last response.
Display it if DISPLAY is non-nil or `rocq-display-goals' is `always'.
With PRESERVE, windows showing the focused view keep their position, so
that automatic refreshes do not scroll."
  (let ((inhibit-read-only t)
        (positions (mapcar (lambda (w) (list w (window-start w)
                                             (window-point w)))
                           (get-buffer-window-list (current-buffer) nil t))))
    (erase-buffer)
    (setq header-line-format (rocq--goal-header))
    (setq truncate-lines (eq rocq--view 'summary))
    (if (eq rocq--view 'summary)
        (progn
          (rocq--insert-summary-view)
          (rocq--goto-selected-line))
      (rocq--insert-focused-view)
      (goto-char (point-min)))
    (dolist (position positions)
      (pcase-let ((`(,window ,start ,point) position))
        (when (window-live-p window)
          (if (and preserve (eq rocq--view 'goal))
              (progn
                (set-window-start window (min start (point-max)))
                (set-window-point window (min point (point-max))))
            (set-window-point window (point))))))
    (when (or display (eq rocq-display-goals 'always))
      (rocq--display-goal-buffer))))

;;;; Commands

(defun rocq--return-to-source (buffer)
  "Select the window of the Rocq buffer the goal BUFFER is about."
  (when-let* ((source (buffer-local-value 'rocq--source buffer))
              ((buffer-live-p source)))
    (pop-to-buffer source '(display-buffer-reuse-window
                            (inhibit-same-window . t)))))

(defun rocq-switch-to-goals ()
  "Show the summary of all goals and move to the goal buffer.
There, \\<rocq-goal-mode-map>\\[rocq-select-goal] selects the goal on
the line and \\[rocq-goals-quit] comes back.  From a Rocq buffer, the
goals at point are requested first if needed."
  (interactive)
  (when (derived-mode-p 'rocq-mode)
    (rocq-request))
  (let ((buffer (rocq--goal-buffer)))
    (with-current-buffer buffer
      (setq rocq--view 'summary)
      (rocq--refresh-goals))
    (select-window (rocq--display-goal-buffer))
    (rocq--goto-selected-line)))

(defun rocq-goals-quit ()
  "Go back to the Rocq buffer, leaving the goal buffer on the focused view."
  (interactive)
  (let ((buffer (rocq--goal-buffer)))
    (with-current-buffer buffer
      (unless (eq rocq--view 'goal)
        (setq rocq--view 'goal)
        (rocq--refresh-goals)))
    (rocq--return-to-source buffer)))

(defun rocq-toggle-goals-view ()
  "Switch the goal buffer between the selected goal and the summary."
  (interactive)
  (with-current-buffer (rocq--goal-buffer)
    (setq rocq--view (if (eq rocq--view 'summary) 'goal 'summary))
    (rocq--refresh-goals t)))

(defun rocq--select-and-show (spec)
  "Select the goal SPEC, show it in full and go back to the Rocq buffer."
  (let ((buffer (rocq--goal-buffer)))
    (with-current-buffer buffer
      (rocq--select spec)
      (setq rocq--view 'goal)
      (rocq--refresh-goals t))
    (when (eq (window-buffer) buffer)
      (rocq--return-to-source buffer))))

(defun rocq-select-goal ()
  "Select the goal on this line of the summary, or unfold the section."
  (interactive)
  (cond
   ((get-text-property (point) 'rocq--goal)
    (rocq--select-and-show (get-text-property (point) 'rocq--goal)))
   ((get-text-property (point) 'rocq--toggle)
    (let ((variable (get-text-property (point) 'rocq--toggle)))
      (set variable (not (symbol-value variable)))
      (rocq--refresh-goals)))
   (t (user-error "No goal on this line"))))

(defun rocq-select-goal-number ()
  "Select the focused goal whose number is the key typed."
  (interactive)
  (let ((number (- last-command-event ?0))
        (count (with-current-buffer (rocq--goal-buffer)
                 (rocq--focused-count))))
    (unless (<= 1 number count)
      (user-error "No goal %d" number))
    (rocq--select-and-show `(goal . ,(1- number)))))

(defun rocq-next-goal (&optional arg)
  "Select the next focused goal, ARG times, and display it.
Cycles around at the end.  Negative ARG moves backward."
  (interactive "p")
  (setq arg (or arg 1))
  (with-current-buffer (rocq--goal-buffer)
    (let ((count (rocq--focused-count)))
      (when (zerop count)
        (user-error "No focused goals"))
      (let ((current (pcase rocq--selected-goal
                       (`(goal . ,i) i)
                       (_ (if (> arg 0) -1 0)))))
        (rocq--select `(goal . ,(mod (+ current arg) count)))
        (rocq--refresh-goals t)))))

(defun rocq-previous-goal (&optional arg)
  "Select the previous focused goal, ARG times, and display it."
  (interactive "p")
  (rocq-next-goal (- (or arg 1))))

(defun rocq-toggle-show-stack ()
  "Toggle whether the goal summary lists unfocused goals."
  (interactive)
  (with-current-buffer (rocq--goal-buffer)
    (setq rocq-show-stack (not rocq-show-stack))
    (rocq--refresh-goals)))

(defun rocq-toggle-show-shelf ()
  "Toggle whether the goal summary lists shelved and given up goals."
  (interactive)
  (with-current-buffer (rocq--goal-buffer)
    (setq rocq-show-shelf (not rocq-show-shelf))
    (rocq--refresh-goals)))

;;;; Requests

(defun rocq--ensure-server ()
  "Return the server managing the current buffer, starting one if needed.
Starting it waits until the server is initialized, for at most
`eglot-connect-timeout' seconds, so that the buffer is managed by Eglot
when this returns.  Signal an error if that fails."
  (unless (eglot-managed-p)
    ;; A prefix argument would make `eglot' prompt for the server
    ;; command and drop the `rocq--lsp-server' class, so hide it.
    (let ((current-prefix-arg nil)
          (eglot-sync-connect t))
      (call-interactively #'eglot))
    (unless (eglot-managed-p)
      (eglot--error "Could not start a server for %s" (current-buffer))))
  (eglot--current-server-or-lose))

(defun rocq-request (&optional display)
  "Request current Rocq status.
If no coq-lsp server manages the buffer, start one first, which makes
this the command to turn coq-lsp on when `rocq-mode-start-server' is nil
or after `eglot-shutdown'.  Interactively, or if DISPLAY is non-nil,
also display the goal buffer once the status is known, even if a later
request answers first.  Otherwise, the goal buffer is only displayed
according to `rocq-display-goals'."
  (interactive (list t))
  (let ((serv (rocq--ensure-server))
        (state (rocq--goal-request-state))
        (buf (current-buffer)))
    (when display
      (setq rocq--display-pending t))
    (if (equal state rocq--last-request-state)
        ;; Nothing changed since the last request, so its result is
        ;; already in the goal buffer, unless it has not arrived yet.
        (when (and display (rocq--display-goal-buffer))
          (setq rocq--display-pending nil))
      (setq rocq--last-request-state state)
      (jsonrpc-async-request
       serv
       :proof/goals
       (list
        :textDocument (eglot--TextDocumentIdentifier)
        :position (eglot--pos-to-lsp-position (point)))
       :success-fn
       (eglot--lambda (textDocument range goals messages error)
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
             (let ((display (or display rocq--display-pending)))
               (setq rocq--display-pending nil)
               (with-current-buffer (rocq--goal-buffer)
                 (rocq--set-goals buf goals messages error)
                 (rocq--refresh-goals display 'preserve))))))))))

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
  (if-let* ((path (expand-file-name (rocq--uri-to-path uri)))
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
    (if-let* ((path (expand-file-name (rocq--uri-to-path uri)))
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
    (when-let* ((server (eglot-current-server)))
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
  (when-let* ((start (rocq--comment-start)))
    (cons start
          (save-excursion
            (goto-char start)
            (if (forward-comment 1) (point) (point-max))))))

(defun rocq-fill-paragraph (&optional justify)
  "Fill the paragraph of the comment at point.
Only the text of the comment is touched: surrounding code is never
filled, and blank lines inside the comment separate paragraphs.
Outside comments, do nothing."
  (when-let* ((bounds (rocq--comment-bounds)))
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
    ;; Identifiers may contain primes; everything else is punctuation,
    ;; so that operators such as `=>' and `:=' form tokens of their own.
    (modify-syntax-entry ?' "_" st)
    (dolist (c (string-to-list "\\$%@?!#~^`/<>=:;,|-+"))
      (modify-syntax-entry c "." st))
    st)
  "Syntax table for `rocq-mode', with nestable `(* *)' comments.")

(defvar-keymap rocq-mode-map
  :doc "Keymap for Rocq interaction."
  "C-c C-," #'rocq-request
  "C-c C-M-," #'rocq-go-to-last-request
  "C-c C-c" #'rocq-switch-to-goals
  "C-c C-n" #'rocq-next-goal
  "C-c C-p" #'rocq-previous-goal
  "C-c C-s" #'rocq-save-vo
  "C-c C-r" #'rocq-reload-vos
  "M-a" #'rocq-backward-sentence
  "M-e" #'rocq-forward-sentence)

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
When nil, start it on demand with \\[rocq-request] or \\[eglot].  Either
way, a buffer whose project already has a running server is managed by
it right away."
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
              fill-paragraph-function #'rocq-fill-paragraph
              indent-line-function #'rocq-indent-line
              indent-tabs-mode nil
              parse-sexp-ignore-comments t
              electric-indent-chars (append '(?| ?}) electric-indent-chars))
  ;; Emacs 30 lets sentence commands such as `kill-sentence' use this.
  (when (boundp 'forward-sentence-function)
    (setq-local forward-sentence-function #'rocq-forward-sentence))
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
