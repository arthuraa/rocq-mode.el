;;; rocq-indent.el --- Indentation for rocq-mode  -*- lexical-binding:t -*-

;; This file is part of rocq-mode.

;;; Commentary:

;; Indentation of Rocq code without a parser.  The indentation of a
;; line is decided from three questions, each answered with
;; `syntax-ppss' and a backward scan bounded by the enclosing group:
;;
;; - Inside a comment, lines align with the comment text.
;; - Inside brackets, lines hang from the first token after the opener,
;;   or sit one `rocq-indent-offset' in when the opener ends its line.
;; - Otherwise the line either starts a new sentence, whose indentation
;;   follows from the previous sentence (proof bodies, bullets, `Qed'),
;;   or continues one, in which case the construct governing the line
;;   (`match' branches, `fun' bodies, `let', `if') decides.
;;
;; A sentence ends at a `.' followed by whitespace, at a bullet, or at a
;; proof block `{ ... }'.  Bullets and braces are recognised as such
;; only at the start of a sentence, so operators and record notations
;; are not mistaken for them.  The `..' of recursive notations is not a
;; terminator, while `...' is.
;;
;; The same scanner drives `rocq-forward-sentence' and
;; `rocq-backward-sentence'.  For motion, as in Proof General, a bullet
;; and each brace of a proof block are sentences of their own, and
;; comments are skipped.

;;; Code:

(require 'cl-lib)

(defcustom rocq-indent-offset 2
  "Indentation of branch bodies, continuation lines and nested bullets."
  :type 'integer
  :group 'rocq)

(defcustom rocq-indent-proof-offset 2
  "Indentation of proof bodies relative to their `Proof' sentence.
Some projects set this to 0 and start tactics in column zero."
  :type 'integer
  :group 'rocq)

(defcustom rocq-indent-match-offset 0
  "Indentation of `|' branches relative to their `match'."
  :type 'integer
  :group 'rocq)

(defcustom rocq-indent-sections nil
  "Whether to indent the contents of sections and modules."
  :type 'boolean
  :group 'rocq)

(defconst rocq-indent--bullet-regexp
  "\\(?:-+\\|\\++\\|\\*+\\)\\(?:[ \t]\\|$\\)"
  "Regexp matching a bullet at the start of a sentence.")

(defconst rocq-indent--proof-start-regexp
  "\\(?:Proof\\|Next[ \t\n]+Obligation\\|Obligation\\)\\_>"
  "Regexp matching the first word of a sentence that opens a proof.")

(defconst rocq-indent--proof-end-regexp
  (regexp-opt '("Qed" "Defined" "Admitted" "Abort" "Save") 'symbols)
  "Regexp matching the first word of a sentence that closes a proof.")

(defconst rocq-indent--statement-regexp
  (regexp-opt '("Lemma" "Theorem" "Definition" "Fixpoint" "CoFixpoint"
                "Example" "Instance" "Corollary" "Remark" "Fact"
                "Property" "Proposition" "Goal" "Let" "Program")
              'symbols)
  "Regexp matching commands whose proof may lack a `Proof' sentence.")

(defconst rocq-indent--section-regexp
  (regexp-opt '("Section" "Module") 'symbols)
  "Regexp matching commands that open a section or module.")

(defconst rocq-indent--match-keywords '("match" "lazymatch" "multimatch")
  "Keywords opening a `match', closed by `end'.")

(defconst rocq-indent--fun-keywords '("fun" "λ")
  "Keywords opening an anonymous function.")

(defconst rocq-indent--binder-keywords '("forall" "exists" "∀" "∃" "let" "if")
  "Keywords whose body, when on the next line, is indented from them.")

;;; Tokens

(defun rocq-indent--space-after-p (pos)
  "Whether POS is followed by whitespace, a comment or the end of buffer."
  (let ((c (char-after pos)))
    (or (null c)
        (memq c '(?\s ?\t ?\n ?\r))
        (and (eq c ?\() (eq (char-after (1+ pos)) ?*)))))

(defun rocq-indent--backward-token (limit)
  "Move backward over the token before point, staying after LIMIT.
Whitespace and comments are skipped first.  A balanced group or a
string counts as one token, and point is left on its opener.  Return
a list (STRING START END), with STRING \"()\" for a group and \"\\\"\"
for a string, or nil when LIMIT is reached or the group is unbalanced."
  (forward-comment (- (point)))
  (let ((end (point)))
    (when (> end limit)
      (let ((syntax (char-syntax (char-before))))
        (cond
         ((memq syntax '(?\) ?\"))
          (condition-case nil
              (progn
                (backward-sexp)
                (if (< (point) limit)
                    (progn (goto-char limit) nil)
                  (list (if (eq syntax ?\") "\"" "()") (point) end)))
            (scan-error (goto-char limit) nil)))
         ((eq syntax ?\() nil)
         ((memq syntax '(?w ?_))
          (skip-syntax-backward "w_" limit)
          (list (buffer-substring-no-properties (point) end) (point) end))
         (t
          (skip-syntax-backward "." limit)
          (when (= (point) end)
            (backward-char))
          ;; A period ending a sentence is a token of its own even when
          ;; glued to other punctuation.
          (when (and (eq (char-before end) ?.)
                     (rocq-indent--space-after-p end))
            (let ((start (point)))
              (goto-char end)
              (skip-chars-backward "." start)))
          (list (buffer-substring-no-properties (point) end)
                (point) end)))))))

(defun rocq-indent--token-kind (token limit)
  "Return the kind of TOKEN if it ends a sentence, else nil.
The kinds are `dot' for a period, `bullet' for a bullet and `brace' for
a proof block.  LIMIT bounds the scan deciding whether a bullet or a
brace sits at the start of a sentence."
  (pcase-let ((`(,string ,start ,end) token))
    (cond
     ((string-match-p "\\`\\.+\\'" string)
      (and (not (equal string ".."))
           (rocq-indent--space-after-p end)
           'dot))
     ((string-match-p "\\`\\(?:-+\\|\\++\\|\\*+\\)\\'" string)
      (and (rocq-indent--space-after-p end)
           (rocq-indent--sentence-start-p start limit)
           'bullet))
     ((equal string "()")
      (and (rocq-indent--proof-block-p start) 'brace)))))

(defun rocq-indent--sentence-start-p (pos limit)
  "Whether POS, after LIMIT, is at the start of a sentence."
  (save-excursion
    (goto-char pos)
    (let ((token (rocq-indent--backward-token limit)))
      (or (null token)
          (and (rocq-indent--token-kind token limit) t)))))

(defun rocq-indent--previous-sentence-end (limit)
  "Scan back from point for the end of the previous sentence.
Return (KIND START END FIRST): KIND, START and END describe the token
ending that sentence, as in `rocq-indent--token-kind', and are nil when
LIMIT is reached first; FIRST is the start of the earliest token
skipped on the way, or nil if none was.  Point is left at START, or at
LIMIT."
  (let (first result)
    (while (let ((token (rocq-indent--backward-token limit)))
             (when token
               (let ((kind (rocq-indent--token-kind token limit)))
                 (if kind
                     (progn (setq result (list kind (nth 1 token)
                                               (nth 2 token)))
                            nil)
                   (setq first (nth 1 token))
                   t)))))
    (append (or result (list nil nil nil)) (list first))))

(defun rocq-indent--sentence-start (pos limit)
  "Return the start of the sentence whose terminator is at POS."
  (save-excursion
    (goto-char pos)
    (or (nth 3 (rocq-indent--previous-sentence-end limit)) pos)))

(defun rocq-indent--backward-find (targets limit)
  "Scan back from point for the first token among TARGETS.
Tokens inside a `match' closed by `end' are skipped.  Return the
token (STRING START END) with point at its start, or nil if none is
found before LIMIT."
  (let ((depth 0) found)
    (while (let ((token (rocq-indent--backward-token limit)))
             (when token
               (let ((string (car token)))
                 (cond
                  ((and (zerop depth) (member string targets))
                   (setq found token)
                   nil)
                  ((equal string "end")
                   (cl-incf depth)
                   t)
                  ((member string rocq-indent--match-keywords)
                   (when (> depth 0) (cl-decf depth))
                   t)
                  (t t))))))
    found))

;;; Columns

(defun rocq-indent--opener (pos)
  "Return the innermost unclosed bracket before POS, without moving point."
  (save-excursion (nth 1 (syntax-ppss pos))))

(defun rocq-indent--column (pos)
  "Return the column of POS."
  (save-excursion (goto-char pos) (current-column)))

(defun rocq-indent--indentation (pos)
  "Return the indentation of the line containing POS."
  (save-excursion (goto-char pos) (current-indentation)))

(defun rocq-indent--word-at (pos)
  "Return the word or bar at POS, or nil."
  (save-excursion
    (goto-char pos)
    (cond ((looking-at "\\(?:\\sw\\|\\s_\\)+") (match-string-no-properties 0))
          ((looking-at "|\\(?:[^|}-]\\|$\\)") "|"))))

(defun rocq-indent--group-start (opener)
  "Return the position after OPENER, skipping the bar of `{|'."
  (if (and (eq (char-after opener) ?{) (eq (char-after (1+ opener)) ?|))
      (+ opener 2)
    (1+ opener)))

(defun rocq-indent--group-text (opener)
  "Return the first token after OPENER on its line, or nil if none."
  (save-excursion
    (goto-char (rocq-indent--group-start opener))
    (skip-chars-forward " \t")
    (and (not (eolp)) (not (looking-at "(\\*")) (point))))

(defun rocq-indent--group-base (opener)
  "Indentation of the first line inside OPENER, or of the buffer if nil."
  (cond ((null opener) 0)
        ((rocq-indent--group-text opener)
         (rocq-indent--column (rocq-indent--group-text opener)))
        (t (+ (rocq-indent--indentation opener) rocq-indent-offset))))

(defun rocq-indent--proof-block-p (opener)
  "Whether the brace at OPENER opens a proof block rather than a term.
A proof block starts a sentence, possibly after a goal selector, at top
level or inside another proof block.  Braces glued together, as in the
`{{{ P }}}' notation, and the `{|' of records never open one."
  (and (eq (char-after opener) ?{)
       (not (eq (char-before opener) ?{))
       (not (memq (char-after (1+ opener)) '(?{ ?|)))
       (let* ((outer (rocq-indent--opener opener))
              (limit (rocq-indent--group-start-or-min outer)))
         (and (or (null outer) (rocq-indent--proof-block-p outer))
              (rocq-indent--sentence-start-p
               (rocq-indent--selector-start opener limit) limit)))))

(defun rocq-indent--selector-start (pos limit)
  "Return the start of the goal selector before POS, or POS if none.
A selector is a number, `all' or a bracketed name followed by a colon,
as in `2: {'.  LIMIT bounds the scan."
  (save-excursion
    (goto-char pos)
    (let ((colon (rocq-indent--backward-token limit)))
      (if (not (equal (car colon) ":"))
          pos
        (let ((selector (rocq-indent--backward-token limit)))
          (if (and selector
                   (or (equal (car selector) "all")
                       (string-match-p "\\`[0-9]+\\'" (car selector))
                       (and (equal (car selector) "()")
                            (eq (char-after (nth 1 selector)) ?\[))))
              (nth 1 selector)
            pos))))))

(defun rocq-indent--group-start-or-min (opener)
  "Return the start of the group opened at OPENER, or of the buffer."
  (if opener (rocq-indent--group-start opener) (point-min)))

(defun rocq-indent--closer-column (opener)
  "Indentation of a line closing the group opened at OPENER."
  (if (rocq-indent--group-text opener)
      (rocq-indent--column opener)
    (rocq-indent--indentation opener)))

(defun rocq-indent--line-base (pos)
  "Column from which constructs at POS are indented.
That is the hanging column of the group POS is in when it sits on the
opener's line, and the indentation of its line otherwise."
  (let* ((opener (rocq-indent--opener pos))
         (text (and opener (rocq-indent--group-text opener))))
    (if (and text
             (= (rocq-indent--indentation-line text)
                (rocq-indent--indentation-line pos)))
        (rocq-indent--column text)
      (rocq-indent--indentation pos))))

(defun rocq-indent--indentation-line (pos)
  "Return the beginning of the line containing POS."
  (save-excursion (goto-char pos) (line-beginning-position)))

;;; Rules

(defun rocq-indent--comment (start)
  "Indentation of a line inside the comment opened at START.
The closer aligns with the opener.  Other lines are brought at least to
the column of the comment text, but a deeper indentation is kept, so
that lists and code examples laid out by hand survive reindentation."
  (let ((column (rocq-indent--column start)))
    (if (looking-at "\\*+)")
        column
      (max (current-indentation)
           (save-excursion
             (goto-char start)
             (looking-at "(\\*+")
             (let ((width (- (match-end 0) (match-beginning 0))))
               (goto-char (match-end 0))
               (skip-chars-forward " \t")
               (if (eolp)
                   (+ column width 1)
                 (current-column))))))))

(defun rocq-indent--sentence-opens-proof-p (token)
  "Whether TOKEN, the first of a sentence, opens a proof or states a goal."
  (save-excursion
    (goto-char (nth 1 token))
    (or (looking-at rocq-indent--proof-start-regexp)
        (looking-at rocq-indent--statement-regexp))))

(defun rocq-indent--bullet-column (tok limit fallback)
  "Indentation of the bullet at TOK, or FALLBACK if it has no sibling.
A sibling is an earlier bullet of the same kind not separated from TOK
by a bullet further left, scanning back to LIMIT or to the sentence
opening the proof."
  (save-excursion
    (goto-char tok)
    (looking-at rocq-indent--bullet-regexp)
    (let ((mine (string-trim (match-string-no-properties 0)))
          (floor most-positive-fixnum)
          first found)
      (while (let* ((token (rocq-indent--backward-token limit))
                    (kind (and token (rocq-indent--token-kind token limit))))
               (cond
                ((null token) nil)
                ((eq kind 'bullet)
                 (let ((column (rocq-indent--column (nth 1 token))))
                   (if (and (equal (car token) mine) (<= column floor))
                       (progn (setq found column) nil)
                     (setq floor (min floor column) first nil)
                     t)))
                (kind
                 ;; The sentence after this terminator started with FIRST.
                 (prog1 (not (and first
                                  (rocq-indent--sentence-opens-proof-p first)))
                   (setq first nil)))
                (t (setq first token) t))))
      (or found fallback))))

(defun rocq-indent--proof-column (tok limit fallback)
  "Indentation of the proof-closing sentence at TOK.
It aligns with the sentence that opened the proof, found by scanning
back to LIMIT, and falls back to FALLBACK less `rocq-indent-offset'."
  (save-excursion
    (goto-char tok)
    (let (first found)
      (while (let* ((token (rocq-indent--backward-token limit))
                    (kind (and token (rocq-indent--token-kind token limit))))
               (cond
                ((and (or (null token) kind)
                      first
                      (rocq-indent--sentence-opens-proof-p first))
                 (setq found (rocq-indent--column (nth 1 first)))
                 nil)
                ((null token) nil)
                (kind (setq first nil) t)
                (t (setq first token) t))))
      (or found (max 0 (- fallback rocq-indent-proof-offset))))))

(defun rocq-indent--section-p (start end)
  "Whether the sentence from START to END opens a section or module."
  (save-excursion
    (goto-char start)
    (and (looking-at rocq-indent--section-regexp)
         (not (re-search-forward ":=" end t)))))

(defun rocq-indent--sentence-base (start limit)
  "Base indentation of a sentence following the one terminated at START.
That is normally the column of the previous sentence, but when several
sentences share a line the first of them counts, and a proof body is
indented by `rocq-indent-offset' from its `Proof'.  LIMIT bounds the
scan."
  (let* ((sstart (rocq-indent--sentence-start start limit))
         (proof-end (save-excursion
                      (goto-char sstart)
                      (looking-at rocq-indent--proof-end-regexp)))
         (proof-start nil)
         (section nil)
         (result nil))
    (while (not result)
      (save-excursion
        (goto-char sstart)
        (when (looking-at rocq-indent--proof-start-regexp)
          (setq proof-start t))
        (when (and rocq-indent-sections (rocq-indent--section-p sstart start))
          (setq section t)))
      (pcase-let ((`(,kind ,tstart ,tend)
                   (save-excursion
                     (goto-char sstart)
                     (rocq-indent--previous-sentence-end limit))))
        (if (and kind (= (rocq-indent--indentation-line tend)
                         (rocq-indent--indentation-line sstart)))
            ;; The sentence shares its line with an earlier one.
            (pcase kind
              ('bullet
               (setq result (+ (rocq-indent--column tstart) (- tend tstart) 1)))
              ('brace (setq result (rocq-indent--column tstart)))
              ('dot (setq start tstart
                          sstart (rocq-indent--sentence-start tstart limit))))
          (setq result (rocq-indent--column sstart)))))
    (cond (proof-end result)
          (proof-start (+ result rocq-indent-proof-offset))
          (section (+ result rocq-indent-offset))
          (t result))))

(defun rocq-indent--new-sentence (tok kind start end opener limit)
  "Indentation of a sentence starting at TOK.
KIND, START and END describe the terminator of the previous sentence,
see `rocq-indent--previous-sentence-end'.  OPENER is the innermost
bracket around TOK and LIMIT the start of its group."
  (let ((base
         (pcase kind
           ('nil (rocq-indent--group-base opener))
           ('bullet (+ (rocq-indent--column start) (- end start) 1))
           ('brace (rocq-indent--column
                    (rocq-indent--selector-start start limit)))
           ('dot (rocq-indent--sentence-base start limit)))))
    (save-excursion
      (goto-char tok)
      (cond
       ((looking-at rocq-indent--bullet-regexp)
        (rocq-indent--bullet-column tok limit base))
       ((looking-at rocq-indent--proof-end-regexp)
        (rocq-indent--proof-column tok limit base))
       ((and rocq-indent-sections (looking-at "End\\_>"))
        (max 0 (- base rocq-indent-offset)))
       (t base)))))

(defun rocq-indent--continuation (tok sent opener)
  "Indentation of TOK, which continues the sentence starting at SENT.
OPENER is the innermost bracket around TOK."
  (let* ((scol (rocq-indent--column sent))
         (default (if (and opener (not (rocq-indent--proof-block-p opener)))
                      scol
                    (+ scol rocq-indent-offset)))
         (word (rocq-indent--word-at tok))
         (find (lambda (targets)
                 (goto-char tok)
                 (rocq-indent--backward-find targets sent)))
         (column (lambda (token) (rocq-indent--column (nth 1 token))))
         (base (lambda (token)
                 (+ (rocq-indent--line-base (nth 1 token))
                    rocq-indent-offset))))
    (save-excursion
      (pcase word
        ("|"
         (let ((match (funcall find rocq-indent--match-keywords)))
           (cond (match (+ (funcall column match) rocq-indent-match-offset))
                 ((funcall find '("|")) (current-column))
                 (opener scol)
                 (t (+ scol rocq-indent-offset)))))
        ("end"
         (let ((match (funcall find rocq-indent--match-keywords)))
           (if match (funcall column match) default)))
        ("with"
         (let ((match (funcall find rocq-indent--match-keywords)))
           (if match (funcall column match) scol)))
        ((or "then" "else")
         (let ((if (funcall find '("if"))))
           (if if (funcall column if) default)))
        ("in"
         (let ((let (funcall find '("let"))))
           (if let (funcall column let) default)))
        (_
         (let ((token (funcall find
                               (append '("=>" "|" "in" ":=" "then" "else"
                                         "with")
                                       rocq-indent--binder-keywords
                                       rocq-indent--fun-keywords
                                       rocq-indent--match-keywords))))
           (pcase (car token)
             ("=>"
              (let ((head (rocq-indent--backward-find
                           (cons "|" rocq-indent--fun-keywords) sent)))
                (pcase (car head)
                  ("|" (+ (funcall column head) rocq-indent-offset))
                  ((or "fun" "λ") (funcall base head))
                  (_ default))))
             ("|" (+ (funcall column token) rocq-indent-offset))
             ("in"
              (let ((let (rocq-indent--backward-find '("let") sent)))
                (if let (funcall column let) default)))
             (":="
              (let ((let (rocq-indent--backward-find '("let") sent)))
                (if let (+ (funcall column let) rocq-indent-offset) default)))
             ((or "then" "else")
              (let ((if (rocq-indent--backward-find '("if") sent)))
                (if if (+ (funcall column if) rocq-indent-offset) default)))
             ("with"
              (let ((match (rocq-indent--backward-find
                            rocq-indent--match-keywords sent)))
                (if match
                    (+ (funcall column match) rocq-indent-offset)
                  default)))
             ((or "match" "lazymatch" "multimatch")
              (+ (funcall column token) rocq-indent-offset))
             ((pred (lambda (string)
                      (or (member string rocq-indent--fun-keywords)
                          (member string rocq-indent--binder-keywords))))
              (funcall base token))
             (_ default))))))))

(defun rocq-indent--attributes-only-p (from to)
  "Whether the text between FROM and TO consists of attributes `#[...]'."
  (save-excursion
    (goto-char from)
    (while (and (< (point) to) (looking-at "#\\["))
      (forward-char 1)
      (condition-case nil (forward-sexp 1) (scan-error (goto-char to)))
      (forward-comment (buffer-size)))
    (>= (point) to)))

(defun rocq-indent--code (tok opener)
  "Indentation of the code line whose first token is at TOK.
OPENER is the innermost unclosed bracket before TOK, or nil."
  (let ((limit (if opener (rocq-indent--group-start opener) (point-min))))
    (goto-char tok)
    (if (and opener (looking-at "\\(?:|}\\|[])}]\\)"))
        (rocq-indent--closer-column opener)
      (pcase-let ((`(,kind ,start ,end ,first)
                   (rocq-indent--previous-sentence-end limit)))
        (if (and first (not (rocq-indent--attributes-only-p first tok)))
            (rocq-indent--continuation tok first opener)
          ;; A command after attributes on their own line starts the
          ;; sentence as far as indentation is concerned.
          (rocq-indent--new-sentence tok kind start end opener limit))))))

(defun rocq-indent--compute ()
  "Return the indentation of the current line, or nil to leave it alone."
  (save-excursion
    (back-to-indentation)
    (let* ((tok (point))
           (ppss (syntax-ppss tok)))
      (cond
       ((nth 3 ppss) nil)
       ((nth 4 ppss) (rocq-indent--comment (nth 8 ppss)))
       (t (rocq-indent--code tok (nth 1 ppss)))))))

(defun rocq-indent-line ()
  "Indent the current line as Rocq code.
See `rocq-indent-offset', `rocq-indent-match-offset' and
`rocq-indent-sections'."
  (interactive)
  (let ((indent (rocq-indent--compute)))
    (cond ((null indent) 'noindent)
          ((<= (current-column) (current-indentation))
           (indent-line-to indent))
          (t (save-excursion (indent-line-to indent))))))

;;; Sentence motion

(defun rocq-indent--sentence-context (pos)
  "Return (LEVEL . OUTSIDE) for POS.
LEVEL is the proof block enclosing POS, or nil at top level.  OUTSIDE is
POS moved out of the string, comment and brackets around it that are
part of a sentence, so that a backward scan from there sees whole
tokens."
  (let* ((ppss (save-excursion (syntax-ppss pos)))
         (outside (or (nth 8 ppss) pos))
         (opener (nth 1 ppss)))
    (while (and opener (not (rocq-indent--proof-block-p opener)))
      (setq outside opener
            opener (rocq-indent--opener opener)))
    (cons opener outside)))

(defun rocq-indent--forward-terminator (level)
  "Move past the next sentence terminator at LEVEL, a proof block or nil.
Return the position after it, or go to the end of the buffer and
return nil if there is none."
  (let ((limit (rocq-indent--group-start-or-min level))
        found)
    (while (and (not found)
                (re-search-forward "\\.+\\|[-+*]+\\|[{}]" nil t))
      (let* ((start (match-beginning 0))
             (end (match-end 0))
             (ppss (save-excursion (syntax-ppss start))))
        (when (and (not (nth 8 ppss))
                   (eq (nth 1 ppss) level)
                   (pcase (char-after start)
                     (?. (and (/= (- end start) 2)
                              (rocq-indent--space-after-p end)))
                     (?{ (rocq-indent--proof-block-p start))
                     (?} level)
                     (_ (and (rocq-indent--space-after-p end)
                             (rocq-indent--sentence-start-p start limit)))))
          (setq found end))))
    (goto-char (or found (point-max)))
    found))

(defun rocq-indent--backward-sentence ()
  "Move to the start of the sentence at point, or of the previous one.
Point is at the start of a sentence when only whitespace and comments
separate it from the previous terminator; it then moves to the start of
the previous sentence."
  (pcase-let* ((`(,level . ,outside)
                (rocq-indent--sentence-context (point)))
               (limit (rocq-indent--group-start-or-min level)))
    (goto-char outside)
    (pcase-let ((`(,kind ,start ,end ,first)
                 (rocq-indent--previous-sentence-end limit)))
      (cond
       (first (goto-char first))
       ;; The previous token is a whole proof block: its closer is the
       ;; previous sentence.
       ((eq kind 'brace) (goto-char (1- end)))
       (kind (goto-char (rocq-indent--sentence-start start limit)))
       ;; At the start of a proof block, the previous sentence is the
       ;; brace opening it, with its goal selector if any.
       (level
        (goto-char (rocq-indent--selector-start
                    level (rocq-indent--group-start-or-min
                           (car (rocq-indent--sentence-context level))))))
       (t (goto-char limit))))))

(defun rocq-forward-sentence (&optional arg)
  "Move forward to the end of the sentence, ARG times.
A sentence ends after its terminating period, and a bullet or a brace
of a proof block is a sentence of its own.  Comments are skipped.  With
a negative ARG, move backward to the start of the sentence instead, see
`rocq-backward-sentence'.  Return point."
  (interactive "^p")
  (setq arg (or arg 1))
  (while (> arg 0)
    (rocq-indent--forward-terminator
     (car (rocq-indent--sentence-context (point))))
    (setq arg (1- arg)))
  (while (< arg 0)
    (rocq-indent--backward-sentence)
    (setq arg (1+ arg)))
  (point))

(defun rocq-backward-sentence (&optional arg)
  "Move backward to the start of the sentence, ARG times.
When point is already at the start of a sentence, move to the start of
the previous one.  With a negative ARG, move forward instead, see
`rocq-forward-sentence'.  Return point."
  (interactive "^p")
  (rocq-forward-sentence (- (or arg 1))))

(provide 'rocq-indent)
;;; rocq-indent.el ends here
