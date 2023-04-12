;;; jinx.el --- Enchanted Spell Checker -*- lexical-binding: t -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Package-Requires: ((emacs "27.1") (compat "29.1.4.0"))
;; Created: 2023
;; Version: 0.5
;; Homepage: https://github.com/minad/jinx
;; Keywords: convenience, wp

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
;; of the text is checked, if it has not been checked before.  Each
;; misspelling can then be corrected from a list of dictionary words
;; presented as completion candidates in a list.

;; Installing Jinx is straight-forward and configuring takes not much
;; intervention.  Jinx can safely co-exist with Emacs's built-in
;; spell-checker.

;; Jinx's high performance and low resource usage comes from directly
;; calling the widely-used API of the Enchant library (see
;; https://abiword.github.io/enchant/).  Jinx automatically compiles
;; jinx-mod.c and loads the dynamic module at startup.  By binding
;; directly to the native Enchant API, Jinx avoids the slower backend
;; process communication with Aspell.

;; See the manual for further information.

;;; Code:

(require 'compat)
(eval-when-compile (require 'cl-lib))

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
  "Face used to highlight current misspelling during correction.")

(defface jinx-accept
  '((t :inherit font-lock-negation-char-face))
  "Face used for the accept action during correction.")

(defcustom jinx-languages
  (replace-regexp-in-string
   "\\..*\\'" ""
   (or (bound-and-true-p current-locale-environment)
       (getenv "LANG")
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
               font-lock-string-face))
  "Alist of faces per major mode.
These faces mark regions which should be included in spell
checking."
  :type '(alist :key-type symbol :value-type (repeat face)))

(defcustom jinx-camel-modes
  '(java-mode javascript-mode
    java-ts-mode javascript-ts-mode
    ruby-mode ruby-ts-mode
    rust-mode rust-ts-mode)
  "Modes where camelCase or PascalCase words should be accepted.
Set to t to enable camelCase everywhere."
  :type '(choice (const t) (repeat symbol)))

(defcustom jinx-exclude-faces
  '((markdown-mode
     markdown-code-face markdown-html-attr-name-face
     markdown-html-attr-value-face markdown-html-tag-name-face
     markdown-inline-code-face markdown-link-face
     markdown-markup-face markdown-plain-url-face
     markdown-reference-face markdown-url-face)
    (org-mode
     org-block org-block-begin-line org-block-end-line
     org-code org-cite org-cite-key org-date org-footnote
     org-formula org-latex-and-related org-link org-meta-line
     org-property-value org-ref-cite-face org-special-keyword
     org-tag org-todo org-todo-keyword-done
     org-todo-keyword-habt org-todo-keyword-kill
     org-todo-keyword-outd org-todo-keyword-todo
     org-todo-keyword-wait org-verbatim
     org-modern-tag org-modern-date-active
     org-modern-date-inactive)
    (tex-mode
     font-latex-math-face font-latex-sedate-face
     font-latex-verbatim-face font-lock-function-name-face
     font-lock-keyword-face font-lock-variable-name-face)
    (texinfo-mode
     font-lock-function-name-face font-lock-keyword-face
     font-lock-variable-name-face)
    (rst-mode
     rst-literal rst-external rst-directive rst-definition
     rst-reference)
    (sgml-mode
     font-lock-function-name-face font-lock-variable-name-face)
    (emacs-lisp-mode
     font-lock-constant-face font-lock-warning-face)
    (message-mode
     message-header-name))
  "Alist of faces per major mode.
These faces mark regions which should be excluded in spell
checking."
  :type '(alist :key-type symbol :value-type (repeat face)))

(defcustom jinx-exclude-regexps
  '((emacs-lisp-mode "Package-Requires:.*$")
    (t "[A-Z]+\\>"         ;; Uppercase words
       "\\w*?[0-9]\\w*\\>" ;; Words with numbers, hex codes
       "[a-z]+://\\S-+"    ;; URI
       "<?[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>?" ;; Email
       "\\(?:Local Variables\\|End\\):\\s-*$" ;; Local variable indicator
       "jinx-\\(?:languages\\|local-words\\):\\s-+.*$")) ;; Local variables
  "List of excluded regexps per major mode."
  :type '(alist :key-type symbol :value-type (repeat regexp)))

(defcustom jinx-include-modes
  '(text-mode prog-mode conf-mode)
  "List of modes included by `global-jinx-mode'."
  :type '(repeat symbol))

(defvar-local jinx-local-words ""
  "File-local words, as a string separated by whitespace.")

;;;###autoload
(put 'jinx-local-words 'safe-local-variable #'stringp)

;;;; Keymaps

(defvar-keymap jinx-misspelled-map
  :doc "Keymap attached to misspelled words."
  "<mouse-1>" #'jinx-correct)

(fset 'jinx-misspelled-map jinx-misspelled-map)

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

(defvar jinx--predicates
  (list #'jinx--face-ignored-p
        #'jinx--regexp-ignored-p
        #'jinx--word-valid-p)
  "Predicate functions called at point with argument START.
Predicate should return t if the word before point is valid.
Predicate may return a position to skip forward.")

(defvar jinx--timer nil
  "Global timer to check pending regions.")

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

(defvar jinx-mode)
(declare-function jinx--mod-check nil)
(declare-function jinx--mod-add nil)
(declare-function jinx--mod-suggest nil)
(declare-function jinx--mod-dict nil)
(declare-function jinx--mod-describe nil)
(declare-function jinx--mod-langs nil)
(declare-function jinx--mod-wordchars nil)
(declare-function org-fold-core-region "org-fold-core")
(declare-function org-fold-core-get-regions "org-fold-core")

;;;; Overlay properties

(put 'jinx 'evaporate             t)
(put 'jinx 'face                  'jinx-misspelled)
(put 'jinx 'mouse-face            '(jinx-misspelled jinx-highlight))
(put 'jinx 'modification-hooks    (list #'jinx--overlay-modified))
(put 'jinx 'insert-in-front-hooks (list #'jinx--overlay-modified))
(put 'jinx 'insert-behind-hooks   (list #'jinx--overlay-modified))
(put 'jinx 'keymap                'jinx-misspelled-map)

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
  (let ((word (buffer-substring-no-properties start (point))))
    (or (member word jinx--session-words)
        (cl-loop for dict in jinx--dicts
                 thereis (jinx--mod-check dict word)))))

;;;; Internal functions

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
        (skip (and (symbolp real-last-command)
                   (string-match-p "self-insert-command\\'"
                                   (symbol-name real-last-command))
                   (point))))
    (while (< pos end)
      (let* ((from (jinx--find-visible-pending pos end t))
             (to (jinx--find-visible-pending from end nil)))
        (if (< from to)
            (setq pos (jinx--check-region from to skip))
          (setq pos to))))))

(defun jinx--force-check-region (start end)
  "Enforce spell-check of region between START and END."
  (with-delayed-message (1 "Fontifying...")
    (jit-lock-fontify-now))
  (with-delayed-message (1 "Checking...")
    (jinx--check-region start end)))

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
              (re-search-backward "\\s-\\|^")
              (setq start (match-end 0))
              (goto-char end)
              (re-search-forward "\\s-\\|$")
              (setq end (match-beginning 0))
              (jinx--delete-overlays start end)
              (goto-char start)
              (while (re-search-forward "\\<\\w+\\>" end t)
                (let ((word-start (match-beginning 0))
                      (word-end (match-end 0)))
                  ;; No quote or apostrophe at start or end
                  (while (and (< word-start word-end)
                              (let ((c (char-after word-start)))
                                (or (= c 39) (= c 8217))))
                    (cl-incf word-start))
                  (while (and (< word-start word-end)
                              (let ((c (char-before word-end)))
                                (or (= c 39) (= c 8217))))
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
                           (overlay-put (make-overlay word-start subword-end) 'category 'jinx))))
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
      (when (and (eq (overlay-get ov 'category) 'jinx)
                 (not (and visible (invisible-p (overlay-start ov)))))
        (push ov overlays)))
    (setq overlays
          (sort overlays
                (lambda (a b) (< (overlay-start a) (overlay-start b)))))
    (while (and (cdr overlays)
                (> (abs (- (overlay-end (car overlays)) pt))
                   (abs (- (overlay-start (cadr overlays)) pt))))
      (push (pop overlays) before))
    (nconc overlays (nreverse before))))

(defun jinx--delete-overlays (start end)
  "Delete overlays between START and END."
  (dolist (ov (overlays-in start end))
    (when (eq 'jinx (overlay-get ov 'category))
      (delete-overlay ov))))

(defun jinx--cleanup ()
  "Cleanup all overlays and trigger fontification."
  (with-silent-modifications
    (save-restriction
      (widen)
      (jinx--delete-overlays (point-min) (point-max))
      (remove-list-of-text-properties (point-min) (point-max) '(jinx--pending))
      (jit-lock-refontify))))

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
             (derived-mode-p 'org-mode))
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
    (let ((default-directory
           (file-name-directory (locate-library "jinx.el" t)))
          (module (file-name-with-extension "jinx-mod" module-file-suffix)))
      (unless (file-exists-p module)
        (let ((command
               `("cc" "-I." "-O2" "-Wall" "-Wextra" "-fPIC" "-shared"
                 "-o" ,module ,(file-name-with-extension module ".c")
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
                  (insert (message "Jinx: %s compiled successfully" module))
                (let ((msg (format "Jinx: Compilation of %s failed" module)))
                  (insert msg)
                  (pop-to-buffer (current-buffer))
                  (error msg)))))))
      (module-load (expand-file-name module)))))

(defun jinx--annotate-suggestion (word)
  "Annotate WORD during completion."
  (get-text-property 0 'jinx--annotation word))

(defun jinx--group-suggestion (word transform)
  "Group WORD during completion, TRANSFORM candidate if non-nil."
  (if transform
      word
    (get-text-property 0 'jinx--group word)))

(defun jinx--suggestions (word)
  "Retrieve suggestions for WORD."
  (delete-dups
   (nconc
    (cl-loop
     for dict in jinx--dicts nconc
     (let* ((suggs (jinx--mod-suggest dict word))
            (desc (jinx--mod-describe dict))
            (group (format "Suggestions from dictionary ‘%s’ (%s)"
                           (car desc) (cdr desc))))
       (dolist (sugg suggs suggs)
         (put-text-property 0 (length sugg) 'jinx--group group sugg))))
    (cl-loop
     for dict in jinx--dicts for idx from 1 nconc
     (let* ((at (propertize (make-string idx ?@)
                            'face 'jinx-accept
                            'rear-nonsticky t))
            (desc (jinx--mod-describe dict))
            (group "Accept and save word")
            (ann (format " [Personal dictionary ‘%s’]" (car desc))))
       (list (propertize (concat at word)
                         'jinx--group group 'jinx--annotation ann)
             (propertize (concat at (downcase word))
                         'jinx--group group 'jinx--annotation ann))))
    (list
     (propertize (concat #("*" 0 1 (face jinx-accept rear-nonsticky t)) word)
                 'jinx--group "Accept and save word"
                 'jinx--annotation " [File]")
     (propertize (concat #("#" 0 1 (face jinx-accept rear-nonsticky t)) word)
                 'jinx--group "Accept and save word"
                 'jinx--annotation " [Session]")))))

(defun jinx--suggestion-table (word)
  "Completion table for WORD suggestions."
  (setq word (jinx--suggestions word))
  (lambda (str pred action)
    (if (eq action 'metadata)
        '(metadata (display-sort-function . identity)
                   (cycle-sort-function . identity)
                   (category . jinx)
                   (group-function . jinx--group-suggestion)
                   (annotation-function . jinx--annotate-suggestion))
      (complete-with-action action word str pred))))

(defun jinx--with-highlight (overlay recenter fun)
  "Highlight and show OVERLAY during FUN, optionally RECENTER."
  (declare (indent 2))
  (let (restore)
    (goto-char (overlay-end overlay))
    (unwind-protect
        (progn
          (if (and (derived-mode-p #'org-mode)
                   (fboundp 'org-fold-show-set-visibility))
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
                  (push (if-let (fun (overlay-get ov 'isearch-open-invisible-temporary))
                            (progn
                              (funcall fun ov nil)
                              (lambda () (funcall fun ov t)))
                          (overlay-put ov 'invisible nil)
                          (lambda () (overlay-put ov 'invisible inv)))
                        restore)))))
          (when recenter (recenter))
          (let ((hl (make-overlay (overlay-start overlay) (overlay-end overlay))))
            (overlay-put hl 'face 'jinx-highlight)
            (overlay-put hl 'window (selected-window))
            (push (lambda () (delete-overlay hl)) restore))
          (funcall fun))
      (mapc #'funcall restore))))

(defun jinx--recheck-overlays ()
  "Recheck all overlays in buffer after a dictionary update."
  (save-excursion
    (save-restriction
      (widen)
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when (eq (overlay-get ov 'category) 'jinx)
          (goto-char (overlay-end ov))
          (when (jinx--word-valid-p (overlay-start ov))
            (delete-overlay ov)))))))

(defun jinx--correct (overlay &optional recenter info)
  "Correct word at OVERLAY with optional RECENTER and prompt INFO."
  (let* ((word (buffer-substring-no-properties
                (overlay-start overlay) (overlay-end overlay)))
         (selected
          (jinx--with-highlight overlay recenter
            (lambda ()
              (or (completing-read (format "Correct ‘%s’%s: " word (or info ""))
                                   (jinx--suggestion-table word)
                                   nil nil nil t word)
                  word)))))
    (if (string-match-p "\\`[@#*]" selected)
        (let* ((new-word (replace-regexp-in-string "\\`[@#*]+" "" selected))
               (idx (- (length selected) (length new-word) 1)))
          (when (equal new-word "") (setq new-word word))
          (cond
           ((string-prefix-p "#" selected)
            (add-to-list 'jinx--session-words new-word))
           ((string-prefix-p "*" selected)
            (add-to-list 'jinx--session-words new-word)
            (setq jinx-local-words
                  (string-join
                   (sort (delete-dups
                          (cons new-word (split-string jinx-local-words)))
                         #'string<)
                   " "))
            (add-file-local-variable 'jinx-local-words jinx-local-words))
           (t (jinx--mod-add (or (nth idx jinx--dicts)
                                 (user-error "Invalid dictionary"))
                             new-word)))
          (jinx--recheck-overlays))
      (when-let (((not (equal selected word)))
                 (start (overlay-start overlay))
                 (end (overlay-end overlay)))
        (delete-overlay overlay)
        (goto-char end)
        (insert-before-markers selected)
        (delete-region start end)))))

(defun jinx--load-dicts ()
  "Load dictionaries and setup syntax table."
  (setq jinx--dicts (delq nil (mapcar #'jinx--mod-dict
                                      (split-string jinx-languages)))
        jinx--syntax-table (make-syntax-table))
  (unless jinx--dicts
    (message "Jinx: No dictionaries available for %S" jinx-languages))
  (dolist (dict jinx--dicts)
    (cl-loop for c across (jinx--mod-wordchars dict) do
             (modify-syntax-entry c "w" jinx--syntax-table)))
  (modify-syntax-entry ?$ "_" jinx--syntax-table)
  (modify-syntax-entry ?% "_" jinx--syntax-table)
  (modify-syntax-entry ?' "w" jinx--syntax-table)
  (modify-syntax-entry ?. "." jinx--syntax-table))

;;;; Public commands

;;;###autoload
(defun jinx-languages (&optional global)
  "Change languages locally or globally.
With prefix argument GLOBAL non-nil change the languages globally."
  (interactive "*P")
  (jinx--load-module)
  (when-let ((langs
              (completing-read-multiple
               (format "Change languages (%s): "
                       (string-join (split-string jinx-languages) ", "))
               (delete-dups (jinx--mod-langs)) nil t)))
    (setq langs (string-join langs " "))
    (cond
     (global
      (kill-local-variable 'jinx-languages)
      (setq-default jinx-languages langs))
     (t
      (setq-local jinx-languages langs)
      (when (y-or-n-p "Save `jinx-languages' as file-local variable? ")
        (add-file-local-variable 'jinx-languages jinx-languages))))
    (jinx--load-dicts)
    (jinx--cleanup)))

;;;###autoload
(defun jinx-correct (&optional all)
  "Correct nearest misspelled word.
If prefix argument ALL non-nil correct all misspellings."
  (interactive "*P")
  (unless jinx-mode (jinx-mode 1))
  (cl-letf (((symbol-function #'jinx--timer-handler) #'ignore) ;; Inhibit
            (old-point (and (not all) (point-marker))))
    (unwind-protect
        (if (not all)
            (jinx--correct
             (or (car (jinx--get-overlays (window-start) (window-end) 'visible))
                 (progn
                   (jinx--force-check-region (window-start) (window-end))
                   (car (jinx--get-overlays (window-start) (window-end) 'visible)))
                 (user-error "No misspelling in visible text")))
          (push-mark)
          (jinx--force-check-region (point-min) (point-max))
          (let* ((overlays (or (jinx--get-overlays (point-min) (point-max))
                               (user-error "No misspellings in whole buffer")))
                 (count (length overlays)))
            (cl-loop for ov in overlays for idx from 1
                     if (overlay-buffer ov) do ;; Could be already deleted
                     (jinx--correct ov 'recenter
                                    (format " (%d of %d, RET to skip)"
                                            idx count)))))
      (when old-point (goto-char old-point))
      (jit-lock-refontify))))

;;;###autoload
(define-minor-mode jinx-mode
  "Enchanted Spell Checker."
  :lighter (" " jinx-languages)
  :group 'jinx
  :keymap jinx-mode-map
  (cond
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
                          (cl-loop for m in jinx-camel-modes
                                   thereis (derived-mode-p m)))
          jinx--session-words (split-string jinx-local-words))
    (jinx--load-dicts)
    (add-hook 'window-state-change-hook #'jinx--reschedule nil t)
    (add-hook 'window-scroll-functions #'jinx--reschedule nil t)
    (add-hook 'post-command-hook #'jinx--reschedule nil t)
    (jit-lock-register #'jinx--mark-pending))
   (t
    (mapc #'kill-local-variable '(jinx--exclude-regexp jinx--include-faces
                                  jinx--exclude-faces jinx--camel
                                  jinx--dicts jinx--syntax-table
                                  jinx--session-words))
    (remove-hook 'window-state-change-hook #'jinx--reschedule t)
    (remove-hook 'window-scroll-functions #'jinx--reschedule t)
    (remove-hook 'post-command-hook #'jinx--reschedule t)
    (jit-lock-unregister #'jinx--mark-pending)
    (jinx--cleanup))))

;;;###autoload
(define-globalized-minor-mode global-jinx-mode jinx-mode jinx--on :group 'jinx)

(defun jinx--on ()
  "Turn `jinx-mode' on."
  (when (and (not (or noninteractive
                      buffer-read-only
                      (eq (aref (buffer-name) 0) ?\s)))
             (seq-some #'derived-mode-p jinx-include-modes))
    (jinx-mode 1)))

(provide 'jinx)
;;; jinx.el ends here
