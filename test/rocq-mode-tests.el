;;; rocq-mode-tests.el --- Tests for rocq-mode  -*- lexical-binding:t -*-

;;; Commentary:

;; Run with `make test', or:
;;
;;   emacs --batch -Q -L . -l test/rocq-mode-tests.el \
;;     -f ert-run-tests-batch-and-exit
;;
;; The server tests drive the mode against `fake-lsp.py', a small LSP
;; server that logs what it receives, and need python3 and git; they are
;; skipped when either is missing.  The indentation and filling tests
;; need nothing.

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
  "Whether the status buffer of the current buffer holds goals."
  (with-current-buffer (rocq--status-buffer (current-buffer)) rocq--goals))

(defun rocq-test-forget-goals ()
  "Forget the goals of the status buffer, to wait for new ones."
  (with-current-buffer (rocq--status-buffer (current-buffer))
    (setq rocq--goals nil)))

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
        ;; After a shutdown, a prefix argument must not reach `eglot'.
        (eglot-shutdown (eglot-current-server) nil 1)
        (should-not (eglot-managed-p))
        (rocq-test-forget-goals)
        (let ((current-prefix-arg '(4)))
          (call-interactively #'rocq-request))
        (should (cl-typep (eglot-current-server) 'rocq--lsp-server))
        (should (rocq-test-wait #'rocq-test-goals-p))
        (should (= 2 (rocq-test-method-count "proof/goals")))))))

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
      ;; The status buffer commands display what they act on.
      (rocq-switch-to-status)
      (should (eq major-mode 'rocq-status-mode))
      (should (equal (reverse displayed) (list (buffer-name) "*Rocq Goal*")))
      (setq displayed nil)
      (rocq-select-goal)
      (should (equal displayed '("*Rocq Goal*")))
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
