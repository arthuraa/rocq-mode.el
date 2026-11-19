;;; rocq-mode-tests.el --- Tests for rocq-mode  -*- lexical-binding:t -*-

;;; Commentary:

;; Run with `make test', or:
;;
;;   emacs --batch -Q -L . -l test/rocq-mode-tests.el \
;;     -f ert-run-tests-batch-and-exit
;;
;; The server tests drive the mode against `fake-lsp.py', a small LSP
;; server that logs what it receives, and need python3 and git; they are
;; skipped when either is missing.  The indentation, sentence motion
;; and filling tests need nothing.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'rocq-mode)

(defconst rocq-test-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of the test files.")

(defconst rocq-test-fake-server
  (expand-file-name "fake-lsp.py" rocq-test-directory)
  "The fake LSP server script.")

(defvar rocq-test-project nil
  "Directory of the temporary project of the current test.")

(defvar rocq-test-log nil
  "Log file of the fake server in the current test.")

(defun rocq-test--server-tests-possible-p ()
  "Whether the tools the server tests need are available."
  (and (executable-find "python3") (executable-find "git")))

(defun rocq-test--cleanup (buffers-before)
  "Shut down servers, kill buffers not in BUFFERS-BEFORE, delete the project."
  (dolist (server (cl-remove-duplicates
                   (apply #'append
                          (hash-table-values eglot--servers-by-project))))
    (ignore-errors (eglot-shutdown server nil 1 t)))
  (dolist (buffer (buffer-list))
    (unless (memq buffer buffers-before)
      (with-current-buffer buffer (set-buffer-modified-p nil))
      (kill-buffer buffer)))
  (delete-directory rocq-test-project t))

(defmacro rocq-test-with-project (files &rest body)
  "Run BODY in a fresh git project holding FILES, served by the fake server.
FILES is a list of (NAME . CONTENT) relative to the project, which
`rocq-test-project' names while BODY runs."
  (declare (indent 1))
  `(let* ((rocq-test-project
           (file-name-as-directory (make-temp-file "rocq-test-" t)))
          (rocq-test-log (expand-file-name "lsp.log" rocq-test-project))
          (eglot-server-programs
           (list (cons 'rocq-mode (list 'rocq--lsp-server "python3"
                                        rocq-test-fake-server rocq-test-log))))
          (eglot-events-buffer-config '(:size 0))
          (eglot-sync-connect t)
          (auto-mode-alist (cons '("\\.v\\'" . rocq-mode) auto-mode-alist))
          (rocq-mode-start-server t)
          (rocq-display-goals 'explicit)
          (rocq-mode-file-progress-debounce-time 0)
          (buffers-before (buffer-list)))
     (call-process "git" nil nil nil "-C" rocq-test-project "init" "-q")
     (dolist (file ,files)
       (let ((path (expand-file-name (car file) rocq-test-project)))
         (make-directory (file-name-directory path) t)
         (with-temp-file path (insert (cdr file)))))
     (unwind-protect (progn ,@body)
       (rocq-test--cleanup buffers-before))))

(defmacro rocq-test-with-display-log (var &rest body)
  "Run BODY with VAR collecting the buffer names given to `display-buffer'."
  (declare (indent 1))
  (let ((advice (make-symbol "advice")))
    `(let* ((,var nil)
            (,advice (lambda (buffer &rest _)
                       (push (buffer-name (get-buffer buffer)) ,var))))
       (advice-add 'display-buffer :before ,advice)
       (unwind-protect (progn ,@body)
         (advice-remove 'display-buffer ,advice)))))

(defun rocq-test-open (name)
  "Visit NAME in the project and let `eglot-ensure' act."
  (find-file (expand-file-name name rocq-test-project))
  (run-hooks 'post-command-hook)
  (current-buffer))

(defun rocq-test-wait (predicate &optional timeout)
  "Wait until PREDICATE returns non-nil or TIMEOUT seconds pass.
Return the value of PREDICATE."
  (let ((deadline (+ (float-time) (or timeout 5))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (funcall predicate)))

(defun rocq-test-log-methods ()
  "Return the methods the fake server has received, in order."
  (when (file-exists-p rocq-test-log)
    (with-temp-buffer
      (insert-file-contents rocq-test-log)
      (mapcar (lambda (line) (car (split-string line " ")))
              (split-string (buffer-string) "\n" t)))))

(defun rocq-test-method-count (method)
  "Return how many times the fake server has received METHOD."
  (cl-count method (rocq-test-log-methods) :test #'equal))

(defun rocq-test-folders ()
  "Return the Rocq workspace folders of the current server."
  (rocq-workspace-folders (eglot-current-server)))

(defun rocq-test-goals-p ()
  "Whether the goal buffer holds a response."
  (when-let* ((buffer (get-buffer "*Rocq Goal*")))
    (buffer-local-value 'rocq--source buffer)))

(defun rocq-test-forget-goals ()
  "Forget the response in the goal buffer, to wait for a new one."
  (when-let* ((buffer (get-buffer "*Rocq Goal*")))
    (with-current-buffer buffer
      (setq rocq--source nil))))

(defun rocq-test-header ()
  "Return the header line of the goal buffer, without text properties."
  (substring-no-properties
   (buffer-local-value 'header-line-format (get-buffer "*Rocq Goal*"))))

(defun rocq-test-goal-text ()
  "Return the text of the goal buffer."
  (with-current-buffer "*Rocq Goal*"
    (buffer-substring-no-properties (point-min) (point-max))))

(defun rocq-test-request-at-line (line)
  "Request the goals at the end of LINE, counted from 0, and wait."
  (goto-char (point-min))
  (forward-line line)
  (end-of-line)
  (rocq-test-forget-goals)
  (call-interactively #'rocq-request)
  (should (rocq-test-wait #'rocq-test-goals-p)))

;;; Project roots

(ert-deftest rocq-test-default-project-root ()
  "The project root is the nearest directory with a project file."
  (let ((dir (file-name-as-directory (make-temp-file "rocq-test-" t))))
    (unwind-protect
        (progn
          (make-directory (concat dir "a/b/") t)
          (with-temp-file (concat dir "a/_RocqProject") (insert ""))
          (with-temp-buffer
            (setq buffer-file-name (concat dir "a/b/F.v")
                  default-directory (concat dir "a/b/"))
            (should (equal (rocq-default-project-root) (concat dir "a/")))
            (let ((rocq-project-files '("Other")))
              (should-not (rocq-default-project-root)))
            (let ((rocq-project-root "../"))
              (should (equal (rocq--project-root) (concat dir "a/"))))))
      (delete-directory dir t))))

(ert-deftest rocq-test-registers-project-root ()
  "Opening a file registers its project as a workspace folder, once."
  (skip-unless (rocq-test--server-tests-possible-p))
  (rocq-test-with-project '(("sub/_CoqProject" . "-Q . Sub\n")
                            ("sub/A.v" . "Definition a := 1.\n")
                            ("sub/B.v" . "Definition b := 2.\n")
                            ("C.v" . "Definition c := 3.\n"))
    (let ((sub (expand-file-name "sub/" rocq-test-project)))
      (rocq-test-open "sub/A.v")
      (should (eglot-managed-p))
      (should (equal (rocq-test-folders) (list sub)))
      ;; Eglot's own folders are kept.
      (should (seq-some (lambda (folder)
                          (equal (plist-get folder :name) rocq-test-project))
                        (eglot-workspace-folders (eglot-current-server))))
      (rocq-test-open "sub/B.v")
      (should (equal (rocq-test-folders) (list sub)))
      (rocq-test-open "C.v")
      (should (equal (rocq-test-folders) (list sub)))
      ;; The folder was announced before the first file was opened.
      (should (rocq-test-wait
               (lambda ()
                 (= 3 (rocq-test-method-count "textDocument/didOpen")))))
      (let ((methods (rocq-test-log-methods)))
        (should (< (cl-position "workspace/didChangeWorkspaceFolders" methods
                                :test #'equal)
                   (cl-position "textDocument/didOpen" methods
                                :test #'equal)))))))

(ert-deftest rocq-test-rocqproject-and-dir-locals ()
  "_RocqProject is recognized and `rocq-project-root' overrides detection."
  (skip-unless (rocq-test--server-tests-possible-p))
  (rocq-test-with-project
      '(("sub/_RocqProject" . "-Q . Sub\n")
        ("sub/A.v" . "Definition a := 1.\n")
        ("custom/.keep" . "")
        ("other/.dir-locals.el"
         . "((rocq-mode . ((rocq-project-root . \"../custom/\"))))\n")
        ("other/D.v" . "Definition d := 4.\n"))
    (rocq-test-open "sub/A.v")
    (should (equal (rocq-test-folders)
                   (list (expand-file-name "sub/" rocq-test-project))))
    (rocq-test-open "other/D.v")
    (should (member (expand-file-name "custom/" rocq-test-project)
                    (rocq-test-folders)))
    (let ((custom (expand-file-name "custom/" rocq-test-project)))
      (rocq-remove-workspace-folder custom)
      (should-not (member custom (rocq-test-folders))))))

;;; Server lifecycle

(ert-deftest rocq-test-shutdown-clears-state ()
  "Shutting the server down removes overlays and silences the timers."
  (skip-unless (rocq-test--server-tests-possible-p))
  (rocq-test-with-project '(("_CoqProject" . "-Q . Top\n")
                            ("A.v" . "Definition a := 1.\n"))
    (rocq-test-open "A.v")
    (should (rocq-test-wait (lambda () rocq-mode--processing-overlays)))
    (rocq-auto-goals-at-point-mode 1)
    (rocq-follow-viewport-mode 1)
    (eglot-shutdown (eglot-current-server) nil 1)
    (should-not (eglot-managed-p))
    (should-not rocq-mode--processing-overlays)
    (should (zerop (cl-count-if
                    (lambda (overlay)
                      (eq (overlay-get overlay 'face)
                          'rocq-mode-processing-face))
                    (overlays-in (point-min) (point-max)))))
    ;; Without a server, the idle timer is not even scheduled and the
    ;; viewport notification does nothing.
    (run-hooks 'post-command-hook)
    (should-not rocq--idle-goals-timer)
    (rocq-mode--update-view (point-min) (point-max) (current-buffer))))

(ert-deftest rocq-test-manual-start ()
  "With `rocq-mode-start-server' nil, servers start on demand and are shared."
  (skip-unless (rocq-test--server-tests-possible-p))
  (rocq-test-with-project '(("sub/_CoqProject" . "-Q . Sub\n")
                            ("sub/A.v" . "Definition a := 1.\n")
                            ("sub/B.v" . "Definition b := 2.\n")
                            ("other/_CoqProject" . "-Q . Other\n")
                            ("other/C.v" . "Definition c := 3.\n"))
    (let ((rocq-mode-start-server nil))
      (rocq-test-open "sub/A.v")
      (should-not (eglot-managed-p))
      (should-not (eglot-current-server))
      (rocq-auto-goals-at-point-mode 1)
      (run-hooks 'post-command-hook)
      (should-not rocq--idle-goals-timer)
      (call-interactively #'eglot)
      (should (eglot-managed-p))
      ;; Files of a project with a running server are picked up at once.
      (rocq-test-open "sub/B.v")
      (should (eglot-managed-p))
      (rocq-test-open "other/C.v")
      (should (eglot-managed-p))
      (should (member (expand-file-name "other/" rocq-test-project)
                      (rocq-test-folders))))))

(ert-deftest rocq-test-request-starts-server ()
  "`rocq-request' starts a server when needed, also after a shutdown."
  (skip-unless (rocq-test--server-tests-possible-p))
  (rocq-test-with-project '(("_CoqProject" . "-Q . Top\n")
                            ("A.v" . "Lemma a : forall n, n = n.\nProof.\n"))
    (let ((rocq-mode-start-server nil))
      (rocq-test-with-display-log displayed
        (rocq-test-open "A.v")
        (should-not (eglot-managed-p))
        (goto-char (point-max))
        (call-interactively #'rocq-request)
        (should (eglot-managed-p))
        (should (cl-typep (eglot-current-server) 'rocq--lsp-server))
        (should (rocq-test-wait #'rocq-test-goals-p))
        (should (member "*Rocq Goal*" displayed))
        (should (= 1 (rocq-test-method-count "proof/goals")))
        ;; Unchanged state: the goal buffer is shown without a request.
        (setq displayed nil)
        (call-interactively #'rocq-request)
        (should (equal displayed '("*Rocq Goal*")))
        (should (= 1 (rocq-test-method-count "proof/goals")))
        ;; After a shutdown, a prefix argument must not reach `eglot',
        ;; and the goal buffer comes up again.
        (eglot-shutdown (eglot-current-server) nil 1)
        (should-not (eglot-managed-p))
        (rocq-test-forget-goals)
        (setq displayed nil)
        (let ((current-prefix-arg '(4)))
          (call-interactively #'rocq-request))
        (should (cl-typep (eglot-current-server) 'rocq--lsp-server))
        (should (rocq-test-wait #'rocq-test-goals-p))
        (should (= 2 (rocq-test-method-count "proof/goals")))
        (should (member "*Rocq Goal*" displayed))))))

(ert-deftest rocq-test-request-display-survives ()
  "A request made to display goals is not lost before its response.
Fontification, which Eglot triggers right after connecting, must not
make it stale, and an automatic request superseding it must display
its own response instead."
  (skip-unless (rocq-test--server-tests-possible-p))
  (rocq-test-with-project '(("A.v" . "Lemma a : forall n, n = n.\nProof.\n"))
    (rocq-test-with-display-log displayed
      (rocq-test-open "A.v")
      (should (eglot-managed-p))
      (goto-char (point-max))
      (call-interactively #'rocq-request)
      (with-silent-modifications
        (put-text-property (point-min) (1+ (point-min)) 'face 'bold))
      ;; An automatic request for the same text sends nothing.
      (rocq-request)
      (should (rocq-test-wait #'rocq-test-goals-p))
      (should (= 1 (rocq-test-method-count "proof/goals")))
      (should (member "*Rocq Goal*" displayed))
      ;; The text changes before the response arrives, and an automatic
      ;; request is made for the new state.
      (setq displayed nil)
      (rocq-test-forget-goals)
      (insert "\n")
      (call-interactively #'rocq-request)
      (insert "\n")
      (rocq-request)
      (should (rocq-test-wait #'rocq-test-goals-p))
      (should (= 3 (rocq-test-method-count "proof/goals")))
      (should (member "*Rocq Goal*" displayed)))))

;;; Goal display

(ert-deftest rocq-test-goal-display ()
  "Only explicit commands display the goal buffer, unless asked otherwise."
  (skip-unless (rocq-test--server-tests-possible-p))
  (rocq-test-with-project '(("_CoqProject" . "-Q . Top\n")
                            ("A.v" . "Lemma a : forall n, n = n.\nProof.\n"))
    (rocq-test-with-display-log displayed
      (rocq-test-open "A.v")
      (goto-char (point-max))
      (setq displayed nil)
      ;; An automatic refresh, as from the idle timer, displays nothing.
      (rocq-request)
      (should (rocq-test-wait #'rocq-test-goals-p))
      (should-not displayed)
      ;; An explicit request with unchanged state displays, without asking.
      (call-interactively #'rocq-request)
      (should (equal displayed '("*Rocq Goal*")))
      (should (= 1 (rocq-test-method-count "proof/goals")))
      ;; After an edit, the explicit request asks and then displays.
      (setq displayed nil)
      (insert "intros n.\n")
      (rocq-test-forget-goals)
      (call-interactively #'rocq-request)
      (should (rocq-test-wait #'rocq-test-goals-p))
      (should (equal displayed '("*Rocq Goal*")))
      ;; An automatic refresh after an edit still displays nothing.
      (setq displayed nil)
      (insert "reflexivity.\n")
      (rocq-test-forget-goals)
      (rocq-request)
      (should (rocq-test-wait #'rocq-test-goals-p))
      (should-not displayed)
      ;; The goal buffer commands display what they act on.
      (rocq-switch-to-goals)
      (should (eq major-mode 'rocq-goal-mode))
      (should (equal displayed '("*Rocq Goal*")))
      (setq displayed nil)
      (rocq-goals-quit)
      (should (equal (buffer-name) "A.v"))
      (should-not (member "*Rocq Goal*" displayed))
      (setq displayed nil)
      (rocq-toggle-goals-view)
      (should (equal displayed '("*Rocq Goal*")))
      (rocq-toggle-goals-view)
      ;; With `always', automatic refreshes display too.
      (let ((rocq-display-goals 'always))
        (switch-to-buffer "A.v")
        (setq displayed nil)
        (insert "Qed.\n")
        (rocq-test-forget-goals)
        (rocq-request)
        (should (rocq-test-wait #'rocq-test-goals-p))
        (should (equal displayed '("*Rocq Goal*"))))
      (should (= 4 (rocq-test-method-count "proof/goals"))))))

(ert-deftest rocq-test-goal-buffer ()
  "The goal buffer: header, views, cycling, selection and empty states.
The fake server answers according to the line asked about, see
`fake-lsp.py'."
  (skip-unless (rocq-test--server-tests-possible-p))
  (rocq-test-with-project
      `(("A.v" . ,(concat "Lemma a : forall n, n = n.\nProof.\n  intros n.\n"
                          "  exact I.\n  idtac.\n(* after *)\n")))
    (rocq-test-with-display-log displayed
      (rocq-test-open "A.v")
      (rocq-test-request-at-line 2)
      (should (equal (rocq-test-header)
                     (concat " Goal 1/2  ·  1 unfocused  ·  1 shelved"
                             "  ·  1 message   TAB: all goals")))
      (should (equal (rocq-test-goal-text)
                     "n = n\n──────\nn : nat\n\nMessages\na warning\n"))
      ;; Cycling from the Rocq buffer wraps around.
      (rocq-next-goal)
      (should (string-prefix-p " Goal 2/2 (second)" (rocq-test-header)))
      (should (string-prefix-p "True /\\\n  True\n" (rocq-test-goal-text)))
      (rocq-next-goal)
      (should (string-prefix-p " Goal 1/2 " (rocq-test-header)))
      (rocq-previous-goal)
      (should (string-prefix-p " Goal 2/2" (rocq-test-header)))
      ;; The selection follows the goal across refreshes.
      (beginning-of-line)
      (rocq-test-forget-goals)
      (rocq-request)
      (should (rocq-test-wait #'rocq-test-goals-p))
      (should (string-prefix-p " Goal 2/2" (rocq-test-header)))
      ;; The summary, entered from the Rocq buffer.
      (setq displayed nil)
      (rocq-switch-to-goals)
      (should (eq major-mode 'rocq-goal-mode))
      (should (equal displayed '("*Rocq Goal*")))
      (should (string-prefix-p " 2 goals  ·  1 unfocused" (rocq-test-header)))
      (should (equal (rocq-test-goal-text)
                     (concat "  1: n = n\n▶ 2(second): True /\\ True\n\n"
                             "Unfocused (1)  RET to show\n\n"
                             "Shelved (1)  RET to show\n")))
      (should (looking-at "▶ 2"))
      ;; Selecting a line shows the goal and returns to the Rocq buffer.
      (goto-char (point-min))
      (rocq-select-goal)
      (should (equal (buffer-name) "A.v"))
      (should (string-prefix-p " Goal 1/2 " (rocq-test-header)))
      (should (eq (buffer-local-value 'rocq--view (get-buffer "*Rocq Goal*"))
                  'goal))
      ;; So does a digit; `q' returns without selecting.
      (rocq-switch-to-goals)
      (let ((last-command-event ?2))
        (rocq-select-goal-number))
      (should (equal (buffer-name) "A.v"))
      (should (string-prefix-p " Goal 2/2" (rocq-test-header)))
      (rocq-switch-to-goals)
      (rocq-next-goal)
      (should (looking-at "▶ 1"))
      (search-forward "Unfocused")
      (rocq-select-goal)
      (should (string-match-p "^  False$" (rocq-test-goal-text)))
      (rocq-goals-quit)
      (should (equal (buffer-name) "A.v"))
      (should (string-prefix-p " Goal 1/2" (rocq-test-header)))
      ;; A failing sentence shows its error; a vanished goal falls back
      ;; to the first one.
      (rocq-test-request-at-line 3)
      (should (equal (rocq-test-header)
                     (concat " Goal 1/1 (second)  ·  1 unfocused  ·  1 shelved"
                             "  ·  error  ·  1 message   TAB: all goals")))
      (should (string-prefix-p "an error\n\nTrue" (rocq-test-goal-text)))
      ;; Empty states.
      (rocq-test-request-at-line 4)
      (should (string-prefix-p " No focused goals  ·  1 unfocused"
                               (rocq-test-header)))
      (should (string-prefix-p
               "This subproof is complete, but there is 1 unfocused goal.\n"
               (rocq-test-goal-text)))
      (should-error (rocq-next-goal))
      (rocq-test-request-at-line 5)
      (should (string-prefix-p " Not in a proof" (rocq-test-header)))
      (should (string-prefix-p "Not in a proof.\n" (rocq-test-goal-text))))))

;;; Indentation

(defun rocq-test-reindent (text &optional keep)
  "Return TEXT reindented from scratch as Rocq code.
Unless KEEP is non-nil, indentation outside comments is removed first."
  (with-temp-buffer
    (insert text)
    (rocq-mode)
    (unless keep
      (goto-char (point-min))
      (while (not (eobp))
        (unless (nth 8 (syntax-ppss (line-beginning-position)))
          (delete-horizontal-space))
        (forward-line 1)))
    (let ((inhibit-message t))
      (indent-region (point-min) (point-max)))
    (buffer-string)))

(ert-deftest rocq-test-indent-corpus ()
  "The indentation corpus is a fixed point of reindentation."
  (let ((text (with-temp-buffer
                (insert-file-contents
                 (expand-file-name "indent.v" rocq-test-directory))
                (buffer-string))))
    (should (equal (rocq-test-reindent text) text))))

(ert-deftest rocq-test-indent-proof-offset ()
  "`rocq-indent-proof-offset' controls the indentation of tactics."
  (let ((flat "Lemma a : True.\nProof.\nexact I.\nQed.\n"))
    (should (equal (rocq-test-reindent flat)
                   "Lemma a : True.\nProof.\n  exact I.\nQed.\n"))
    (let ((rocq-indent-proof-offset 0))
      (should (equal (rocq-test-reindent flat) flat)))))

(ert-deftest rocq-test-indent-sections ()
  "`rocq-indent-sections' indents the contents of sections."
  (let ((text (concat "Section S.\n  Variable x : nat.\n  Lemma l : True.\n"
                      "  Proof.\n    exact I.\n  Qed.\nEnd S.\n")))
    (let ((rocq-indent-sections t))
      (should (equal (rocq-test-reindent text) text)))
    (should (equal (rocq-test-reindent text)
                   (concat "Section S.\nVariable x : nat.\nLemma l : True.\n"
                           "Proof.\n  exact I.\nQed.\nEnd S.\n")))))

(ert-deftest rocq-test-indent-keeps-point ()
  "Indenting a line keeps point on the same text."
  (with-temp-buffer
    (insert "Proof.\nintros n.\n")
    (rocq-mode)
    (goto-char (point-max))
    (search-backward " n.")
    (rocq-indent-line)
    (should (looking-at " n\\.$"))
    (should (equal (buffer-string) "Proof.\n  intros n.\n"))))

;;; Sentence motion

(defconst rocq-test-sentences-text
  (concat "Lemma a : forall n : nat, n = n. (* about n. *)\n"
          "Proof.\n"
          "  intros n; destruct n as [|m].\n"
          "  - reflexivity.\n"
          "  - { rewrite (Nat.add_0_r m)... }\n"
          "  all: { exact (f_equal S eq_refl). }\n"
          "Qed.\n"
          "Notation \"[ x ; .. ; y ]\" := (cons x .. (cons y nil) ..).\n"
          "Definition s := \"a. b\".\n"
          "Definition r := {| f := 1; g := 2 |}.\n"
          "Lemma b : {{{ True }}} e {{{ RET #(); True }}}.\n"
          "Proof. { { exact I. } } Qed.\n"
          "Check 1.\n")
  "Code exercising sentence motion.")

(defun rocq-test-sentences ()
  "Return the sentences of the current buffer, stepping forward.
Check on the way that `rocq-backward-sentence' finds the start of each
sentence from its end and from just inside it, and that stepping
backward from the end of the buffer visits the same starts."
  (goto-char (point-min))
  (let (sentences starts)
    (while (not (save-excursion (forward-comment (buffer-size)) (eobp)))
      (let* ((prev (point))
             (end (progn (rocq-forward-sentence) (point)))
             (start (save-excursion (rocq-backward-sentence) (point))))
        (should (> end prev))
        (should (<= prev start))
        (should (= start (save-excursion
                           (goto-char (min end (1+ start)))
                           (rocq-backward-sentence)
                           (point))))
        (push start starts)
        (push (buffer-substring-no-properties start end) sentences)))
    (goto-char (point-max))
    (let (chain prev)
      (while (progn (setq prev (point))
                    (rocq-backward-sentence)
                    (< (point) prev))
        (push (point) chain))
      ;; Before the first sentence, point lands at the start of the buffer.
      (when (and chain starts (< (car chain) (car (last starts))))
        (pop chain))
      (should (equal chain (reverse starts))))
    (nreverse sentences)))

(ert-deftest rocq-test-sentence-list ()
  "Sentence motion splits code like Proof General does."
  (with-temp-buffer
    (insert rocq-test-sentences-text)
    (rocq-mode)
    (should (equal (rocq-test-sentences)
                   '("Lemma a : forall n : nat, n = n." "Proof."
                     "intros n; destruct n as [|m]." "-" "reflexivity."
                     "-" "{" "rewrite (Nat.add_0_r m)..." "}" "all: {"
                     "exact (f_equal S eq_refl)." "}" "Qed."
                     "Notation \"[ x ; .. ; y ]\" := \
(cons x .. (cons y nil) ..)."
                     "Definition s := \"a. b\"."
                     "Definition r := {| f := 1; g := 2 |}."
                     "Lemma b : {{{ True }}} e {{{ RET #(); True }}}."
                     "Proof." "{" "{" "exact I." "}" "}" "Qed." "Check 1.")))))

(ert-deftest rocq-test-sentence-from-inside ()
  "Motion from inside comments, strings, brackets and blocks."
  (with-temp-buffer
    (insert rocq-test-sentences-text)
    (rocq-mode)
    (cl-flet ((from (text)
                (goto-char (point-min))
                (search-forward text))
              (forward-lands-on (regexp)
                (rocq-forward-sentence)
                (should (looking-back regexp (line-beginning-position))))
              (backward-lands-on (regexp)
                (rocq-backward-sentence)
                (should (looking-at regexp))))
      ;; Comments between sentences are skipped.
      (from "about") (forward-lands-on "Proof\\.")
      (from "about") (backward-lands-on "Lemma a")
      ;; Periods in strings and brackets do not count.
      (from "a. ") (forward-lands-on "\"a\\. b\"\\.")
      (from "a. ") (backward-lands-on "Definition s")
      (from "cons y") (forward-lands-on "\\.\\.)\\.")
      (from "cons y") (backward-lands-on "Notation")
      ;; Each brace of a proof block is a sentence; glued braces are not.
      (from "m)... }") (backward-lands-on "}")
      (from "m)... }") (forward-lands-on "all: {")
      (from "True }}} e") (forward-lands-on "}}}\\.")
      (from "True }}} e") (backward-lands-on "Lemma b")
      ;; Counts, including negative ones.
      (from "all: ")
      (rocq-forward-sentence 3)
      (should (looking-back "eq_refl)\\. }" (line-beginning-position)))
      (rocq-forward-sentence -2)
      (should (looking-at "exact (f_equal"))
      (rocq-backward-sentence -1)
      (should (looking-back "eq_refl)\\." (line-beginning-position))))))

(ert-deftest rocq-test-sentence-corpus ()
  "Forward and backward motion agree on the indentation corpus."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "indent.v" rocq-test-directory))
    (rocq-mode)
    (dolist (sentence (rocq-test-sentences))
      (should (string-match-p "\\`[^ \t\n]" sentence))
      (should (string-match-p "\\(?:\\.\\|[-+*{}]\\)\\'" sentence)))))

(ert-deftest rocq-test-sentence-commands ()
  "The standard sentence commands follow Rocq sentences on Emacs 30."
  (skip-unless (boundp 'forward-sentence-function))
  (with-temp-buffer
    (insert rocq-test-sentences-text)
    (rocq-mode)
    (goto-char (point-min))
    (forward-sentence)
    (should (looking-back "n = n\\." (line-beginning-position)))
    (kill-sentence)
    (should (looking-at "\n  intros n;"))))

;;; Filling

(ert-deftest rocq-test-fill-paragraph ()
  "Filling touches one comment paragraph and never the code."
  (with-temp-buffer
    (insert "(* First paragraph of the comment, long enough to be refilled"
            " by the test.\n\n   Second paragraph. *)\n"
            "Definition x := 1. (* trailing *)\n")
    (rocq-mode)
    (setq fill-column 40)
    (goto-char (point-min))
    (search-forward "long")
    (fill-paragraph)
    (let ((lines (split-string (buffer-string) "\n")))
      (should (string-prefix-p "(* First" (nth 0 lines)))
      (should (cl-every (lambda (line) (<= (length line) 40))
                        (seq-take lines 4)))
      (should (member "   Second paragraph. *)" lines))
      (should (member "Definition x := 1. (* trailing *)" lines)))
    ;; Filling on a code line does nothing.
    (let ((before (buffer-string)))
      (goto-char (point-max))
      (search-backward "Definition")
      (fill-paragraph)
      (should (equal (buffer-string) before)))))

(provide 'rocq-mode-tests)
;;; rocq-mode-tests.el ends here
