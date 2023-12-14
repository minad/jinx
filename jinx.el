;;; jinx.el --- Enchanted Spell Checker -*- lexical-binding: t -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2023
;; Version: 1.0
;; Package-Requires: ((emacs "27.1") (compat "29.1.4.0"))
;; Homepage: https://github.com/minad/jinx
;; Keywords: convenience, wp

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Jinx is a fast just-in-time spell-checker for Emacs.  Jinx
;; highlights misspelled words in the text of the visible portion of
;; the buffer.  For efficiency, Jinx highlights misspellings lazily,
;; recognizes window boundaries and text folding, if any.  For
;; example, when unfolding or scrolling, only the newly visible part
;; of the text is checked if it has not been checked before.  Each
;; misspelling can be corrected from a list of dictionary words
;; presented as a completion menu.

;; Installing Jinx is straight-forward and configuring should not need
;; much intervention.  Jinx can be used completely on its own, but can
;; also safely co-exist with Emacs's built-in spell-checker Ispell.

;; Jinx's high performance and low resource usage comes from directly
;; calling the widely-used API of the Enchant library (see
;; https://abiword.github.io/enchant/).  Jinx automatically compiles
;; jinx-mod.c and loads the dynamic module at startup.  By binding
;; directly to the native Enchant API, Jinx avoids the slower backend
;; process communication with Aspell.

;; See the manual for further information.

;;; Code:

(require 'compat)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;;;; Customization

(defgroup jinx nil
  "Enchanted Spell Checker."
  :link '(info-link :tag "Info Manual" "(jinx)")
  :link '(url-link :tag "Homepage" "https://github.com/minad/jinx")
  :link '(emacs-library-link :tag "Library Source" "jinx.el")
  :group 'text
  :prefix "jinx-")

(defcustom jinx-delay 0.2
  "Idle timer delay."
  :type 'float)

(defcustom jinx-languages
  (replace-regexp-in-string
   "\\..*\\'" ""
   (or (bound-and-true-p current-locale-environment)
       (pcase (getenv "LANG")
         ((or "C" "POSIX") nil)
         (lang lang))
       "en_US"))
  "Dictionary language codes, as a string separated by whitespace."
  :type 'string)

;;;###autoload
(put 'jinx-languages 'safe-local-variable #'stringp)

(defcustom jinx-include-faces
  '((prog-mode font-lock-comment-face
               font-lock-doc-face
               font-lock-string-face)
    (conf-mode font-lock-comment-face
               font-lock-string-face)
    ;; `yaml-mode' and `yaml-ts-mode' are text-modes,
    ;; while they should better be conf- or prog-modes.
    (yaml-mode font-lock-comment-face
               font-lock-string-face)
    (yaml-ts-mode font-lock-comment-face
                  font-lock-string-face))
  "Alist of faces per major mode.
These faces mark regions which should be included in spell
checking."
  :type '(alist :key-type symbol :value-type (repeat face)))

(defcustom jinx-camel-modes
  '(java-mode java-ts-mode js-mode js-ts-mode ruby-mode ruby-ts-mode rust-mode
    rust-ts-mode haskell-mode kotlin-mode swift-mode csharp-mode csharp-ts-mode
    objc-mode typescript-ts-mode typescript-mode python-mode python-ts-mode
    dart-mode go-mode go-ts-mode scala-mode groovy-mode)
  "Modes where camelCase or PascalCase words should be accepted.
Set to t to enable camelCase everywhere."
  :type '(choice (const t) (repeat symbol)))

(defcustom jinx-exclude-faces
  '((markdown-mode
     markdown-code-face markdown-html-attr-name-face
     markdown-html-attr-value-face markdown-html-tag-name-face
     markdown-inline-code-face markdown-link-face markdown-markup-face
     markdown-plain-url-face markdown-reference-face markdown-url-face)
    (org-mode
     org-block org-block-begin-line org-block-end-line org-code org-cite
     org-cite-key org-date org-document-info-keyword org-done org-drawer
     org-footnote org-formula org-latex-and-related org-link org-macro
     org-meta-line org-property-value org-special-keyword org-tag org-todo
     org-verbatim org-warning
     ;; org-modern
     org-modern-tag org-modern-date-active org-modern-date-inactive)
    (tex-mode
     tex-math font-latex-math-face font-latex-sedate-face
     font-latex-verbatim-face font-lock-function-name-face
     font-lock-keyword-face font-lock-variable-name-face)
    (texinfo-mode
     font-lock-function-name-face font-lock-keyword-face
     font-lock-variable-name-face)
    (rst-mode
     rst-literal rst-external rst-directive rst-definition rst-reference)
    (sgml-mode
     font-lock-function-name-face font-lock-variable-name-face)
    (emacs-lisp-mode
     font-lock-constant-face font-lock-warning-face)
    (message-mode
     message-header-cc message-header-name message-header-newsgroups
     message-header-other message-header-to message-header-xheader
     message-cited-text-1 message-cited-text-2 message-cited-text-3
     message-cited-text-4 gnus-cite-1 gnus-cite-2 gnus-cite-3
     gnus-cite-4 gnus-cite-5 gnus-cite-6 gnus-cite-7 gnus-cite-8
     gnus-cite-9 gnus-cite-10 gnus-cite-11))
  "Alist of faces per major mode.
These faces mark regions which should be excluded in spell
checking."
  :type '(alist :key-type symbol :value-type (repeat face)))

(defcustom jinx-exclude-regexps
  '((emacs-lisp-mode "Package-Requires:.*$")
    (t "[A-Z]+\\>"         ;; Uppercase words
       "-+\\>"             ;; Hyphens used as lines or bullet points
       "\\w*?[0-9]\\w*\\>" ;; Words with numbers, hex codes
       "[a-z]+://\\S-+"    ;; URI
       "<?[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>?" ;; Email
       "\\(?:Local Variables\\|End\\):\\s-*$" ;; Local variable indicator
       "jinx-\\(?:languages\\|local-words\\):\\s-+.*$")) ;; Local variables
  "List of excluded regexps per major mode."
  :type '(alist :key-type symbol :value-type (repeat regexp)))

(defcustom jinx-suggestion-distance 3
  "Maximal edit distance for session words to be included in suggestions."
  :type 'natnum)

(defcustom jinx-menu-suggestions 10
  "Maximal number of suggestions shown in the context menu."
  :type 'natnum)

(defvar-local jinx-local-words ""
  "File-local words, as a string separated by whitespace.")

;;;###autoload
(put 'jinx-local-words 'safe-local-variable #'stringp)

;;;###autoload
(put 'jinx-mode 'safe-local-variable #'not)

;;;; Faces

(defgroup jinx-faces nil
  "Faces used by Jinx."
  :group 'jinx
  :group 'faces)

(defface jinx-misspelled
  '((((class color) (min-colors 88) (background dark)
      (supports :underline (:style wave)))
     :underline (:style wave :color "#d2b580"))
    (((class color) (min-colors 88) (background light)
      (supports :underline (:style wave)))
     :underline (:style wave :color "#5f4400"))
    (t :underline t :inherit error))
  "Face used for misspelled words.")

(defface jinx-highlight
  '((t :inherit isearch))
  "Face used to highlight current misspelled word during correction.")

(defface jinx-save
  '((t :inherit warning))
  "Face used for the save actions during correction.")

(defface jinx-key
  '((t :inherit completions-annotations))
  "Face used for the select key during correction.")

(defface jinx-annotation
  '((t :inherit completions-annotations))
  "Face used for the annotation during correction.")

;;;; Keymaps

(defvar-keymap jinx-overlay-map
  :doc "Keymap attached to misspelled words."
  "<down-mouse-3>" #'jinx-correct-menu
  "M-n" #'jinx-next
  "M-p" #'jinx-previous
  "M-$" #'jinx-correct)

(fset 'jinx-overlay-map jinx-overlay-map)

(defvar-keymap jinx-repeat-map
  :doc "Repeat keymap for navigation commands."
  :repeat (:exit (jinx-correct))
  "M-n" #'jinx-next
  "M-p" #'jinx-previous
  "n" #'jinx-next
  "p" #'jinx-previous
  "$" #'jinx-correct)

(defvar-keymap jinx-correct-map
  :doc "Keymap active in the correction minibuffer."
  "SPC" #'self-insert-command
  "M-n" #'jinx-next
  "M-p" #'jinx-previous
  "M-$" #'jinx-previous
  "0 <t>" #'jinx-correct-select)
(dotimes (i 9)
  (define-key jinx-correct-map (vector (+ ?1 i)) #'jinx-correct-select))

(defvar-keymap jinx-mode-map
  :doc "Keymap used when Jinx is active.")

(easy-menu-define jinx-mode-menu jinx-mode-map
  "Menu used when Jinx is active."
  '("Jinx"
    ["Correct nearest" jinx-correct]
    ["Correct all" (jinx-correct t)
     :keys "\\[universal-argument] \\[jinx-correct]"]
    ["Change languages" jinx-languages]
    "----"
    ["Manual" (info "(jinx)")]
    ["Customize" (customize-group 'jinx)]
    ["Turn off" (jinx-mode -1)]))

;;;; Internal variables

(defvar jinx--reschedule-hooks
  '(window-state-change-hook window-scroll-functions post-command-hook)
  "Hooks which reschedule the spell checking timer, see `jinx--reschedule'.")

(defvar jinx--predicates
  (list #'jinx--face-ignored-p
        #'jinx--regexp-ignored-p
        #'jinx--word-valid-p)
  "Predicate functions called at point with argument START.
Predicate should return t if the word before point is valid.
Predicate may return a position to skip forward.")

(defvar jinx--timer nil
  "Global timer to check pending regions.")

(defvar jinx--base-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?$ "_" st)
    (modify-syntax-entry ?% "_" st)
    (modify-syntax-entry '(#x80 . #x9f) "_" st)        ;; Control characters
    (modify-syntax-entry '(#x2150 . #x2bff) "_" st)    ;; Number Forms - Misc. Arrows
    (modify-syntax-entry '(#xfe00 . #xfe0f) "_" st)    ;; Variation Selectors
    (modify-syntax-entry '(#x1cf00 . #x1d7ff) "_" st)  ;; Znamenny Musical - Math. Alpha.
    (modify-syntax-entry '(#x1ee00 . #x1fbff) "_" st)  ;; Arabic Math. - Legacy Computing
    st)
  "Base syntax table for spell checking.")

(defvar jinx--select-keys
  "123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "Quick select keys used by `jinx-correct'.")

(defvar jinx--save-keys
  `((?@ . ,#'jinx--save-personal)
    (?* . ,#'jinx--save-file)
    (?+ . ,#'jinx--save-session))
  "Keys for save actions used by `jinx-correct'.")

(defvar-local jinx--exclude-faces nil
  "List of excluded faces.")

(defvar-local jinx--include-faces nil
  "List of included faces.")

(defvar-local jinx--exclude-regexp nil
  "Ignore regexp.")

(defvar-local jinx--camel nil
  "Accept camel case.")

(defvar-local jinx--dicts nil
  "List of dictionaries.")

(defvar-local jinx--syntax-table nil
  "Syntax table used during checking.")

(defvar-local jinx--session-words nil
  "List of words accepted in this session.")

;;;; Declarations for the bytecode compiler

(defvar repeat-mode)
(defvar jinx-mode)
(declare-function jinx--mod-check nil)
(declare-function jinx--mod-add nil)
(declare-function jinx--mod-suggest nil)
(declare-function jinx--mod-dict nil)
(declare-function jinx--mod-describe nil)
(declare-function jinx--mod-langs nil)
(declare-function jinx--mod-wordchars nil)

;;;; Overlay properties

(put 'jinx-overlay 'evaporate             t)
(put 'jinx-overlay 'face                  'jinx-misspelled)
(put 'jinx-overlay 'mouse-face            '(jinx-misspelled jinx-highlight))
(put 'jinx-overlay 'modification-hooks    (list #'jinx--overlay-modified))
(put 'jinx-overlay 'insert-in-front-hooks (list #'jinx--overlay-modified))
(put 'jinx-overlay 'insert-behind-hooks   (list #'jinx--overlay-modified))
(put 'jinx-overlay 'keymap                'jinx-overlay-map)

;;;; Predicates

(defun jinx--regexp-ignored-p (start)
  "Return non-nil if word at START matches ignore regexps."
  (save-excursion
    (goto-char start)
    (when (and jinx--exclude-regexp (looking-at-p jinx--exclude-regexp))
      (save-match-data
        (looking-at jinx--exclude-regexp)
        (match-end 0)))))

(defun jinx--face-ignored-p (start)
  "Return non-nil if face at START of word is ignored."
  (let ((face (get-text-property start 'face)))
    (or
     (and jinx--include-faces
          (if (listp face)
              (cl-loop for f in face never (memq f jinx--include-faces))
            (not (memq face jinx--include-faces))))
     (and jinx--exclude-faces
          (if (listp face)
              (cl-loop for f in face thereis (memq f jinx--exclude-faces))
            (memq face jinx--exclude-faces))))))

(defun jinx--word-valid-p (start)
  "Return non-nil if word at START is valid."
  (let ((word (buffer-substring-no-properties start (point)))
        case-fold-search)
    (or (member word jinx--session-words)
        ;; Allow capitalized words
        (and (string-match-p "\\`[[:upper:]][[:lower:]]+\\'" word)
             (cl-loop
              for w in jinx--session-words
              thereis (and (string-equal-ignore-case word w)
                           (string-match-p "\\`[[:lower:]]+\\'" w))))
        (cl-loop for dict in jinx--dicts
                 thereis (jinx--mod-check dict word)))))

;;;; Internal functions

(defun jinx--in-base-buffer (&rest app)
  "Apply APP in `buffer-base-buffer', as required by `jit-lock' functions."
  (with-current-buffer (or (buffer-base-buffer) (current-buffer))
    (apply app)))

(defun jinx--overlay-modified (overlay &rest _)
  "Delete modified OVERLAY.
This function is a modification hook for the overlay."
  (delete-overlay overlay))

(defun jinx--find-visible-pending (start end flag)
  "Find (in)visible and (non-)pending region between START and END.
FLAG must be t or nil."
  (while (and (< start end)
              (eq flag
                  (not (and (get-text-property start 'jinx--pending)
                            (not (invisible-p start))))))
    (setq start (next-single-char-property-change
                 start 'jinx--pending nil
                 (next-single-char-property-change start 'invisible nil end))))
  start)

(defun jinx--check-pending (start end)
  "Check pending visible region between START and END."
  (let ((pos start)
        (retry (and (eq (window-buffer) (current-buffer))
                    (symbolp real-last-command)
                    (string-match-p "self-insert-command\\'"
                                    (symbol-name real-last-command))
                    (window-point))))
    (while (< pos end)
      (let* ((from (jinx--find-visible-pending pos end t))
             (to (jinx--find-visible-pending from end nil)))
        (if (< from to)
            (setq pos (jinx--check-region from to retry))
          (setq pos to))))))

(defun jinx--check-region (start end &optional retry)
  "Check region between START and END.
Optionally RETRY word at given position.  Return updated END
position."
  (let ((st (syntax-table)) case-fold-search
        retry-start retry-end)
    (unwind-protect
        (with-silent-modifications
          (save-excursion
            (save-match-data
              ;; Use dictionary-dependent syntax table
              (set-syntax-table jinx--syntax-table)
              ;; Ensure that region starts and ends at word boundaries
              (goto-char start)
              (unless (looking-at-p "\\<")
                (re-search-backward "\\<\\|^")
                (setq start (match-beginning 0)))
              (goto-char end)
              (unless (looking-at-p "\\>")
                (re-search-forward "\\>\\|$")
                (setq end (match-beginning 0)))
              (jinx--delete-overlays start end)
              (goto-char start)
              (while (re-search-forward "\\<\\w+\\>" end t)
                (let ((word-start (match-beginning 0))
                      (word-end (match-end 0)))
                  ;; No quote or apostrophe at start or end
                  (while (and (< word-start word-end)
                              (let ((c (char-after word-start)))
                                (or (= c ?') (= c ?’))))
                    (cl-incf word-start))
                  (while (and (< word-start word-end)
                              (let ((c (char-before word-end)))
                                (or (= c ?') (= c ?’))))
                    (cl-decf word-end))
                  (while (< word-start word-end)
                    (let ((subword-end word-end))
                      (when jinx--camel
                        (goto-char word-start)
                        (when (looking-at "\\([[:upper:]]?[[:lower:]]+\\)\\(?:[[:upper:]][[:lower:]]+\\)+\\>")
                          (setq subword-end (match-end 1))))
                      (goto-char subword-end)
                      (pcase (run-hook-with-args-until-success 'jinx--predicates word-start)
                        ((and (pred integerp) skip)
                         (goto-char (max subword-end (min end skip))))
                        ('nil
                         (if (and retry (<= word-start retry subword-end))
                             (setq retry-start word-start retry-end subword-end retry nil)
                           (overlay-put (make-overlay word-start subword-end) 'category 'jinx-overlay))))
                      (setq word-start subword-end)))))
              (remove-list-of-text-properties start end '(jinx--pending))
              (when retry-start
                (put-text-property retry-start retry-end 'jinx--pending t)))))
      (set-syntax-table st)))
  end)

(defun jinx--get-overlays (start end &optional visible)
  "Return misspelled word overlays between START and END.
If VISIBLE is non-nil, only include visible overlays."
  (let ((pt (point)) before overlays)
    (dolist (ov (overlays-in start end))
      (when (and (eq (overlay-get ov 'category) 'jinx-overlay)
                 (not (and visible (invisible-p (overlay-start ov)))))
        (push ov overlays)))
    (setq overlays
          (sort overlays
                (lambda (a b) (< (overlay-start a) (overlay-start b)))))
    (while (and (cdr overlays) (<= (overlay-start (cadr overlays)) pt))
      (push (pop overlays) before))
    (nconc overlays (nreverse before))))

(cl-defun jinx--force-overlays (start end &key visible check)
  "Return misspelled word overlays between START and END, enforce checking.
If VISIBLE is non-nil, only include visible overlays.
If CHECK is non-nil, always check first."
  (or (and (not check) (jinx--get-overlays start end visible))
      (progn
        (let (set-message-function) ;; bug#63253: Broken `with-delayed-message'
          (with-delayed-message (1 "Fontifying...")
            (jinx--in-base-buffer #'jit-lock-fontify-now start end))
          (with-delayed-message (1 "Checking...")
            (jinx--check-region start end)))
        (jinx--get-overlays start end visible))
      (user-error (if visible "No misspelled word in visible text"
                    "No misspelled word in whole buffer"))))

(defun jinx--delete-overlays (start end)
  "Delete overlays between START and END."
  (dolist (ov (overlays-in start end))
    (when (eq (overlay-get ov 'category) 'jinx-overlay)
      (delete-overlay ov))))

(defun jinx--cleanup ()
  "Cleanup all overlays and trigger fontification."
  (with-silent-modifications
    (without-restriction
      (jinx--delete-overlays (point-min) (point-max))
      (remove-list-of-text-properties (point-min) (point-max) '(jinx--pending))
      (jinx--in-base-buffer #'jit-lock-refontify))))

(defun jinx--mark-pending (start end)
  "Mark region between START and END as pending."
  (put-text-property start end 'jinx--pending t)
  (unless inhibit-quit ;; non-nil for stealth locking
    (jinx--schedule))
  nil)

(defun jinx--mode-list (list)
  "Lookup by major mode in LIST."
  (cl-loop for (mode . vals) in list
           if (or (eq mode t) (derived-mode-p mode)) append vals))

(defun jinx--get-org-language ()
  "Get language from Org #+language keyword."
  (when (and (not (local-variable-p 'jinx-languages))
             (derived-mode-p #'org-mode))
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (when (re-search-forward "^ *#\\+language: +\\([a-z_]+\\) *$" nil t)
          (setq-local jinx-languages (match-string-no-properties 1)))))))

(defun jinx--timer-handler ()
  "Global timer handler, checking the pending regions in all windows."
  (setq jinx--timer nil)
  (dolist (frame (frame-list))
    (dolist (win (window-list frame 'no-miniwindow))
      (when-let ((buffer (window-buffer win))
                 ((buffer-local-value 'jinx-mode buffer)))
        (with-current-buffer buffer
          (jinx--check-pending (window-start win) (window-end win)))))))

(defun jinx--reschedule (&rest _)
  "Restart the global idle timer."
  (when jinx--timer
    (cancel-timer jinx--timer)
    (setq jinx--timer nil))
  (jinx--schedule))

(defun jinx--schedule ()
  "Start the global idle timer."
  (when (and (not jinx--timer)
             (not completion-in-region-mode) ;; Corfu completion
             (get-buffer-window)) ;; Buffer visible
    (setq jinx--timer
          (run-with-idle-timer jinx-delay
                               nil #'jinx--timer-handler))))

(defun jinx--load-module ()
  "Compile and load dynamic module."
  (unless (fboundp #'jinx--mod-dict)
    (unless module-file-suffix
      (error "Jinx: Dynamic modules are not supported"))
    (let* ((mod-name (file-name-with-extension "jinx-mod" module-file-suffix))
           (mod-file (locate-library mod-name t)))
      (unless mod-file
        (let* ((cc (or (getenv "CC")
                       (seq-find #'executable-find '("gcc" "clang" "cc"))
                       (error "Jinx: No C compiler found")))
               (c-name (file-name-with-extension mod-name ".c"))
               (default-directory (file-name-directory
                                   (or (locate-library c-name t)
                                       (error "Jinx: %s not found" c-name))))
               (command
                `(,cc "-I." "-O2" "-Wall" "-Wextra" "-fPIC" "-shared"
                  "-o" ,mod-name ,c-name
                  ,@(split-string-and-unquote
                     (condition-case nil
                         (car (process-lines "pkg-config" "--cflags" "--libs" "enchant-2"))
                       (error "-I/usr/include/enchant-2 -lenchant-2"))))))
          (with-current-buffer (get-buffer-create "*jinx module compilation*")
            (let ((inhibit-read-only t))
              (erase-buffer)
              (compilation-mode)
              (insert (string-join command " ") "\n")
              (if (equal 0 (apply #'call-process (car command) nil (current-buffer) t (cdr command)))
                  (insert (message "Jinx: %s compiled successfully" mod-name))
                (let ((msg (format "Jinx: Compilation of %s failed" mod-name)))
                  (insert msg)
                  (pop-to-buffer (current-buffer))
                  (error msg)))))
          (setq mod-file (expand-file-name mod-name))))
      (module-load mod-file))))

(defmacro jinx--correct-guard (&rest body)
  "Guard BODY during correction loop."
  `(cl-letf (((symbol-function #'jinx--timer-handler) #'ignore) ;; Inhibit
             (repeat-mode nil)) ;; No repeating of jinx-next and jinx-previous
     (unless jinx-mode (jinx-mode 1))
     ,@body))

(defun jinx--correct-highlight (overlay fun)
  "Highlight and show OVERLAY during FUN."
  (declare (indent 1))
  (let (restore)
    (goto-char (overlay-end overlay))
    (unwind-protect
        (progn
          (if (and (derived-mode-p #'org-mode)
                   (fboundp 'org-fold-show-set-visibility)
                   (fboundp 'org-fold-core-get-regions)
                   (fboundp 'org-fold-core-region))
              ;; New Org 9.6 fold-core API
              (let ((regions (delq nil (org-fold-core-get-regions
                                        :with-markers t :from (point-min) :to (point-max)))))
                (org-fold-show-set-visibility 'canonical)
                (push (lambda ()
                        (cl-loop for (beg end spec) in regions do
                                 (org-fold-core-region beg end t spec)))
                      restore))
            (dolist (ov (overlays-in (pos-bol) (pos-eol)))
              (let ((inv (overlay-get ov 'invisible)))
                (when (and (invisible-p inv) (overlay-get ov 'isearch-open-invisible))
                  (push (if-let ((fun (overlay-get ov 'isearch-open-invisible-temporary)))
                            (progn
                              (funcall fun ov nil)
                              (lambda () (funcall fun ov t)))
                          (overlay-put ov 'invisible nil)
                          (lambda () (overlay-put ov 'invisible inv)))
                        restore)))))
          (let ((hl (make-overlay (overlay-start overlay) (overlay-end overlay))))
            (overlay-put hl 'face 'jinx-highlight)
            (overlay-put hl 'window (selected-window))
            (push (lambda () (delete-overlay hl)) restore))
          (funcall fun))
      (mapc #'funcall restore))))

(defun jinx--recheck-overlays ()
  "Recheck all overlays in buffer after a dictionary update."
  (save-excursion
    (without-restriction
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when (eq (overlay-get ov 'category) 'jinx-overlay)
          (goto-char (overlay-end ov))
          (when (jinx--word-valid-p (overlay-start ov))
            (delete-overlay ov)))))))

(defun jinx--correct-setup ()
  "Setup minibuffer for correction."
  (let ((message-log-max nil)
        (inhibit-message t))
    (use-local-map (make-composed-keymap (list jinx-correct-map) (current-local-map)))
    (when (and (eq completing-read-function #'completing-read-default)
               (not (bound-and-true-p vertico-mode))
               (not (bound-and-true-p icomplete-mode)))
      (minibuffer-completion-help))))

(defun jinx--add-suggestion (list ht word group)
  "Add suggestion WORD to LIST and HT.
The word will be associated with GROUP and get a prefix key."
  (unless (gethash word ht)
    (add-text-properties
     0 (length word)
     (list 'jinx--group group
           'jinx--prefix
           (let ((idx (1+ (hash-table-count ht))))
             (cond
              ((< idx 10)
               (format #("%d " 0 3 (face jinx-key))
                       idx))
              ((< (- idx 10) (length jinx--select-keys))
               (format #("0%c " 0 4 (face jinx-key))
                       (aref jinx--select-keys (- idx 10)))))))
     word)
    (push word list)
    (puthash word t ht))
  list)

(defun jinx--session-suggestions (word)
  "Retrieve suggestions for WORD from session."
  (mapcar #'cdr
          (sort (cl-loop for w in jinx--session-words
                         for d = (string-distance word w)
                         if (<= d jinx-suggestion-distance)
                         collect (cons d w))
                #'car-less-than-car)))

(defun jinx--correct-suggestions (word)
  "Retrieve suggestions for WORD from all dictionaries."
  (let ((ht (make-hash-table :test #'equal))
        (list nil))
    (dolist (dict jinx--dicts)
      (let* ((desc (jinx--mod-describe dict))
             (group (format "Suggestions from dictionary ‘%s’ - %s"
                            (car desc) (cdr desc))))
        (dolist (w (jinx--mod-suggest dict word))
          (setq list (jinx--add-suggestion list ht w group)))))
    (dolist (w (jinx--session-suggestions word))
      (setq list (jinx--add-suggestion list ht w "Suggestions from session")))
    (cl-loop for (key . fun) in jinx--save-keys
             for actions = (funcall fun nil key word) do
             (unless (consp (car actions)) (setq actions (list actions)))
             (cl-loop for (k w a) in actions do
                      (push (propertize
                             (concat (propertize (if (stringp k) k (char-to-string k))
                                                 'face 'jinx-save 'rear-nonsticky t)
                                     w)
                             'jinx--group "Accept and save"
                             'jinx--suffix (format #(" [%s]" 0 5 (face jinx-annotation)) a))
                            list)))
    (nreverse list)))

(defun jinx--correct-affixation (cands)
  "Affixate CANDS during completion."
  (cl-loop for cand in cands collect
           (list cand
                 (or (get-text-property 0 'jinx--prefix cand) "")
                 (or (get-text-property 0 'jinx--suffix cand) ""))))

(defun jinx--correct-annotation (cand)
  "Annotate CAND during completion."
  (if-let ((prefix (get-text-property 0 'jinx--prefix cand)))
      (format #(" (%s)" 0 5 (face jinx-key)) (string-trim prefix))
    (get-text-property 0 'jinx--suffix cand)))

(defun jinx--correct-group (word transform)
  "Group WORD during completion, TRANSFORM candidate if non-nil."
  (if transform
      word
    (get-text-property 0 'jinx--group word)))

(defun jinx--correct-table (suggestions)
  "Completion table for SUGGESTIONS."
  (lambda (str pred action)
    (if (eq action 'metadata)
        `(metadata (category . jinx)
                   (display-sort-function . ,#'identity)
                   (cycle-sort-function . ,#'identity)
                   (group-function . ,#'jinx--correct-group)
                   (affixation-function . ,#'jinx--correct-affixation)
                   (annotation-function . ,#'jinx--correct-annotation))
      (complete-with-action action suggestions str pred))))

(cl-defun jinx--correct-overlay (overlay &key info initial)
  "Correct word at OVERLAY.
Optionally show prompt INFO and insert INITIAL input."
  (catch 'jinx--goto
    (let* ((word (buffer-substring-no-properties
                  (overlay-start overlay) (overlay-end overlay)))
           (choice
            (jinx--correct-highlight overlay
              (lambda ()
                (when (or (< (point) (window-start)) (> (point) (window-end nil t)))
                  (recenter))
                (minibuffer-with-setup-hook
                    #'jinx--correct-setup
                  (or (completing-read
                       (format "Correct ‘%s’%s: " word (or info ""))
                       (jinx--correct-table
                        (jinx--correct-suggestions word))
                       nil nil initial t word)
                      word)))))
           (len (length choice)))
      (pcase (and (> len 0) (assq (aref choice 0) jinx--save-keys))
        (`(,key . ,fun)
         (funcall fun 'save key (if (> len 1) (substring choice 1) word))
         (jinx--recheck-overlays))
        ((guard (not (equal choice word)))
         (jinx--correct-replace overlay choice)))
      nil)))

(defun jinx--correct-replace (overlay word)
  "Replace OVERLAY with WORD."
  (when-let ((start (overlay-start overlay))
             (end (overlay-end overlay)))
    (undo-boundary)
    (delete-overlay overlay)
    (goto-char end)
    (insert-before-markers word)
    (delete-region start end)))

(defun jinx--load-dicts ()
  "Load dictionaries and setup syntax table."
  (setq jinx--dicts (delq nil (mapcar #'jinx--mod-dict
                                      (split-string jinx-languages)))
        jinx--syntax-table (make-syntax-table jinx--base-syntax-table))
  (unless jinx--dicts
    (message "Jinx: No dictionaries available for %S" jinx-languages))
  (dolist (dict jinx--dicts)
    (cl-loop for c across (jinx--mod-wordchars dict) do
             (modify-syntax-entry c "w" jinx--syntax-table)))
  (modify-syntax-entry ?' "w" jinx--syntax-table)
  (modify-syntax-entry ?’ "w" jinx--syntax-table)
  (modify-syntax-entry ?. "." jinx--syntax-table))

(defun jinx--bounds-of-word ()
  "Return bounds of word at point using `jinx--syntax-table'."
  (save-excursion
    (save-match-data
      (with-syntax-table jinx--syntax-table
        (unless (looking-at-p "\\<")
          (re-search-backward "\\<"))
        (when (re-search-forward "\\<\\w+\\>" nil t)
          (cons (match-beginning 0) (match-end 0)))))))

;;;; Save functions

(defun jinx--save-personal (save key word)
  "Save WORD in personal dictionary.
If SAVE is non-nil save, otherwise format candidate given action KEY."
  (if save
      (let ((idx (seq-position word key (lambda (x y) (not (equal x y))))))
        (jinx--mod-add (or (nth idx jinx--dicts)
                           (user-error "Invalid dictionary"))
                       (substring word idx)))
    (cl-loop
     for dict in jinx--dicts for idx from 1
     for at = (make-string idx key)
     for ann = (format "Personal:%s" (car (jinx--mod-describe dict))) collect
     (list at word ann))))

(defun jinx--save-file (save key word)
  "Save WORD in file-local variable.
If SAVE is non-nil save, otherwise format candidate given action KEY."
  (if save
      (progn
        (add-to-list 'jinx--session-words word)
        (setq jinx-local-words
              (string-join
               (sort (delete-dups
                      (cons word (split-string jinx-local-words)))
                     #'string<)
               " "))
        (add-file-local-variable 'jinx-local-words jinx-local-words))
    (list key word "File")))

(defun jinx--save-session (save key word)
  "Save WORD for the current session.
If SAVE is non-nil save, otherwise format candidate given action KEY."
  (if save
      (add-to-list 'jinx--session-words word)
    (list key word "Session")))

;;;; Public commands

;;;###autoload
(defun jinx-languages (langs &optional global)
  "Set languages locally or globally to LANGS.
LANGS should be one or more language codes as a string, separated
by whitespace.  When called interactively, the language codes are
read via `completing-read-multiple'.  If the prefix argument
GLOBAL is non-nil, the languages are changed globally for all
buffers.  See also the variable `jinx-languages'."
  (interactive
   (list
    (progn
      (jinx--load-module)
      (string-join
       (or (completing-read-multiple
            (format "Change languages (%s): "
                    (string-join (split-string jinx-languages) ", "))
            (delete-dups (jinx--mod-langs)) nil t)
           (user-error "No languages selected"))
       " "))
    current-prefix-arg))
  (unless jinx-mode (jinx-mode 1))
  (cond
   (global
    (kill-local-variable 'jinx-languages)
    (setq-default jinx-languages langs))
   (t
    (setq-local jinx-languages langs)
    (when (or (assq 'jinx-languages file-local-variables-alist)
              (and buffer-file-name
                   (y-or-n-p "Save `jinx-languages' as file-local variable? ")))
      (add-file-local-variable 'jinx-languages jinx-languages)
      (setf (alist-get 'jinx-languages file-local-variables-alist) jinx-languages))))
  (jinx--load-dicts)
  (jinx--cleanup))

;;;###autoload
(defun jinx-correct-all ()
  "Correct all misspelled words in the buffer."
  (interactive "*")
  (jinx--correct-guard
   (let* ((overlays (jinx--force-overlays (point-min) (point-max) :check t))
          (count (length overlays))
          (idx 0))
     (push-mark)
     (while-let ((ov (nth idx overlays)))
       (if-let (((overlay-buffer ov))
                (skip (jinx--correct-overlay ov :info (format " (%d of %d)" (1+ idx) count))))
           (setq idx (mod (+ idx skip) count))
         (cl-incf idx))))))

;;;###autoload
(defun jinx-correct-nearest ()
  "Correct nearest misspelled word."
  (interactive "*")
  (save-excursion
    (jinx--correct-guard
     (let* ((overlays (jinx--force-overlays (window-start) (window-end) :visible t))
            (count (length overlays))
            (idx 0))
       ;; Not using `while-let' is intentional here.
       (while (when-let ((ov (nth idx overlays)))
                (if (overlay-buffer ov)
                    (when-let ((skip (jinx--correct-overlay ov)))
                      (setq idx (mod (+ idx skip) count)))
                  (cl-incf idx)))))))) ;; Skip deleted overlay

;;;###autoload
(defun jinx-correct-word (&optional start end initial)
  "Correct word between START and END, by default the word before point.
Suggest corrections even if the word is not misspelled.
Optionally insert INITIAL input in the minibuffer."
  (interactive)
  (unless (and start end)
    (setf (cons start end) (or (jinx--bounds-of-word)
                               (user-error "No word at point"))))
  (save-excursion
    (jinx--correct-guard
     (while-let ((skip (let ((ov (make-overlay start end)))
                         (unwind-protect
                             (jinx--correct-overlay ov :initial initial)
                         (delete-overlay ov)))))
       (forward-to-word skip)
       (when-let ((bounds (jinx--bounds-of-word)))
         (setf (cons start end) bounds
               initial nil))))))

;;;###autoload
(defun jinx-correct (&optional arg)
  "Correct word depending on prefix ARG.
This command dispatches to the following commands:
  - `jinx-correct-nearest': If prefix ARG is nil, correct nearest
    misspelled word.
  - `jinx-correct-all': If prefix ARG is 4, corresponding to
    \\[universal-argument] pressed once, correct all misspelled words.
  - `jinx-correct-word': If prefix ARG is 16, corresponding to
    \\[universal-argument] pressed twice, correct word before point."
  (interactive "*P")
  (pcase arg
    ('nil (jinx-correct-nearest))
    ('(16) (jinx-correct-word))
    (_ (jinx-correct-all))))

(defun jinx-correct-select ()
  "Quick selection key for corrections."
  (interactive)
  (let* ((keys (this-command-keys-vector))
         (word (nth (if (eq (aref keys 0) ?0)
                        (+ 9 (or (seq-position jinx--select-keys (aref keys 1)) 999))
                      (- (aref keys 0) ?1))
                    (all-completions "" minibuffer-completion-table))))
    (unless (and word (get-text-property 0 'jinx--prefix word))
      (user-error "Invalid select key `%s'" (key-description keys)))
    (delete-minibuffer-contents)
    (insert word)
    (exit-minibuffer)))

(defun jinx-next (n)
  "Go to to Nth next misspelled word."
  (interactive "p")
  (unless (= n 0)
    (if (minibufferp)
        (throw 'jinx--goto n)
      (let ((ov (jinx--force-overlays (point-min) (point-max))))
        (unless (or (> n 0) (<= (overlay-start (car ov)) (point) (overlay-end (car ov))))
          (cl-incf n))
        (goto-char (overlay-end (nth (mod n (length ov)) ov)))))))

(defun jinx-previous (n)
  "Go to to Nth previous misspelled word."
  (interactive "p")
  (jinx-next (- n)))

(defun jinx-correct-menu (event)
  "Popup mouse menu to correct misspelling at EVENT."
  (interactive "e")
  (when-let ((pt (posn-point (event-start event)))
             (ov (car (jinx--get-overlays pt pt t))))
    (let ((menu nil)
          (word (buffer-substring-no-properties
                 (overlay-start ov) (overlay-end ov))))
      (dolist (dict jinx--dicts)
        (when-let ((desc (jinx--mod-describe dict))
                   (suggestions (jinx--mod-suggest dict word)))
          (push `[,(concat (car desc) " - " (cdr desc)) :active nil] menu)
          (cl-loop for w in suggestions repeat jinx-menu-suggestions do
                   (push `[,w (jinx--correct-replace ,ov ,w)] menu))))
      (when-let ((suggestions (jinx--session-suggestions word)))
        (push ["Session" :active nil] menu)
        (cl-loop for w in suggestions repeat jinx-menu-suggestions do
          (push `[,w (jinx--correct-replace ,ov ,w)] menu)))
      (push ["Accept and save" :active nil] menu)
      (cl-loop for (key . fun) in jinx--save-keys
               for actions = (funcall fun nil key word) do
               (unless (consp (car actions)) (setq actions (list actions)))
               (cl-loop for (k w a) in actions do
                        (push `[,a (jinx-correct-word
                                    ,(overlay-start ov) ,(overlay-end ov)
                                    ,(concat (if (stringp k) k (char-to-string k)) w))]
                              menu)))
      (popup-menu (easy-menu-create-menu
                   (format "Correct `%s'" word)
                   (delete-dups (nreverse menu)))
                  event))))

;;;###autoload
(define-minor-mode jinx-mode
  "Enchanted Spell Checker."
  :lighter (:eval (concat " Jinx[" jinx-languages "]"))
  :group 'jinx
  :keymap jinx-mode-map
  (cond
   ((buffer-base-buffer) ;; Do not enable in indirect buffers
    (when jinx-mode
      (jinx-mode -1)))
   (jinx-mode
    (jinx--load-module)
    (let ((enable-local-variables :safe))
      (hack-local-variables))
    (jinx--get-org-language)
    (setq jinx--exclude-regexp
          (when-let ((regexps (jinx--mode-list jinx-exclude-regexps)))
            (mapconcat (lambda (r) (format "\\(?:%s\\)" r))
                       regexps "\\|"))
          jinx--include-faces (jinx--mode-list jinx-include-faces)
          jinx--exclude-faces (jinx--mode-list jinx-exclude-faces)
          jinx--camel (or (eq jinx-camel-modes t)
                          (seq-some #'derived-mode-p jinx-camel-modes))
          jinx--session-words (split-string jinx-local-words))
    (jinx--load-dicts)
    (dolist (hook jinx--reschedule-hooks)
      (add-hook hook #'jinx--reschedule nil t))
    (jit-lock-register #'jinx--mark-pending))
   (t
    (mapc #'kill-local-variable '(jinx--exclude-regexp jinx--include-faces
                                  jinx--exclude-faces jinx--camel
                                  jinx--dicts jinx--syntax-table
                                  jinx--session-words))
    (dolist (hook jinx--reschedule-hooks)
      (remove-hook hook #'jinx--reschedule t))
    (jit-lock-unregister #'jinx--mark-pending)
    (jinx--cleanup))))

(defcustom global-jinx-modes '(text-mode prog-mode conf-mode)
  "List of modes where Jinx should be enabled.
The variable can either be t, nil or a list of t, nil, mode
symbols or elements of the form (not modes)."
  :type '(repeat sexp))

;;;###autoload
(define-globalized-minor-mode global-jinx-mode
  jinx-mode jinx--on
  :group 'jinx)

(defun jinx--on ()
  "Turn `jinx-mode' on."
  (when (and (not (or noninteractive
                      buffer-read-only
                      (buffer-base-buffer) ;; Do not enable in indirect buffers
                      (eq (aref (buffer-name) 0) ?\s)))
             ;; TODO backport `easy-mmode--globalized-predicate-p'
             (or (eq t global-jinx-modes)
                 (eq t (cl-loop for p in global-jinx-modes thereis
                                (pcase-exhaustive p
                                  ('t t)
                                  ('nil 0)
                                  ((pred symbolp) (and (derived-mode-p p) t))
                                  (`(not . ,m) (and (seq-some #'derived-mode-p m) 0)))))))
    (jinx-mode 1)))

(put #'jinx-correct-menu 'completion-predicate #'ignore)
(put #'jinx-correct-select 'completion-predicate #'ignore)
(put #'jinx-next 'command-modes '(jinx-mode))
(put #'jinx-previous 'command-modes '(jinx-mode))

(provide 'jinx)
;;; jinx.el ends here
