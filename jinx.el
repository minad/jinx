;;; jinx.el --- Enchanted just-in-time spell checker -*- lexical-binding: t -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Package-Requires: ((emacs "27.1") (compat "29.1.4.0"))
;; Created: 2023
;; Version: 0.2
;; Homepage: https://github.com/minad/jinx

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

;; Jinx provides just-in-time spell-checking via libenchant
;; (https://abiword.github.io/enchant/).  The package aims to achieve
;; high performance and low resource usage, without impacting your
;; editing experience.  Overall Jinx should just work out of the box
;; without much intervention.
;;
;; Jinx highlights misspellings lazily only in the visible part of the
;; text.  The window boundaries and text folding are taken into
;; account.  Jinx binds directly to the native libenchant API, such
;; that process communication with a backend Aspell process can be
;; avoided.  Libenchant is widely used as spell-checking API by text
;; editors and supports Nuspell, Hunspell, Aspell and a few lesser
;; known backends.  Jinx automatically compiles and loads the native
;; module at startup.  Libenchant must be installed on your system for
;; compilation.  If `pkg-config' is available it will be used to
;; locate libenchant.  On Debian or Ubuntu, install the packages
;; `libenchant-2-2', `libenchant-2-dev' and `pkg-config'.
;;
;; Jinx supports multiple languages in a buffer at the same time via
;; the `jinx-languages' customization variable.  It offers flexible
;; settings to ignore misspellings via faces (`jinx-exclude-faces' and
;; `jinx-include-faces'), regular expressions (`jinx-exclude-regexps')
;; and programmable predicates.  Jinx comes preconfigured for the most
;; important major modes.
;;
;; Jinx offers three auto-loaded entry points , the modes
;; `global-jinx-mode', `jinx-mode' and the command `jinx-correct'.
;; You can either enable `global-jinx-mode' or add `jinx-mode' to the
;; hooks of the modes.
;;
;; (add-hook 'emacs-startup-hook #'global-jinx-mode)
;;
;; (dolist (hook '(text-mode-hook prog-mode-hook conf-mode-hook))
;;   (add-hook hook #'jinx-mode))
;;
;; In order to correct misspellings bind `jinx-correct' to a
;; convenient key in your configuration.  Jinx is independent of the
;; Ispell package, so you can reuse the binding M-$ which is bound to
;; `ispell-word' by default.  When pressing M-$, Jinx offers
;; correction suggestions for the misspelling next to point.  If the
;; prefix key C-u is pressed, the entire buffer is spell-checked.
;;
;; (keymap-global-set "<remap> <ispell-word>" #'jinx-correct)
;;
;; Jinx offers a similar UI as Augusto Stoffel's jit-spell package and
;; borrows ideas from it.  Jit-spell uses Ispell process communication
;; instead of a native API.  It does not restrict the highlighting to
;; the visible text.  In my setup I observed an increase in load and
;; latency as a consequence, in particular in combination with stealth
;; locking and commands which trigger fontification eagerly like
;; `consult-line' from my Consult package.
;;
;; The technique to spell-check only the visible text was inspired by
;; Campbell Barton's spell-fu package.  Spell-fu maintains the
;; dictionary itself via a hash table, which results in high memory
;; usage for languages with compound words or inflected word forms.
;; In Jinx we avoid the complexity of managing the dictionary and
;; access the advanced spell-checker algorithms directly via
;; libenchant (affixation, compound words, etc.).

;;; Code:

(require 'compat)
(eval-when-compile (require 'cl-lib))

;;;; Customization

(defgroup jinx nil
  "Enchanted just-in-time spell checker."
  :link '(info-link :tag "Info Manual" "(jinx)")
  :link '(url-link :tag "Homepage" "https://github.com/minad/jinx")
  :link '(emacs-library-link :tag "Library Source" "jinx.el")
  :group 'text
  :prefix "jinx-")

(defcustom jinx-delay 0.2
  "Idle timer delay."
  :type 'float)

(defface jinx-misspelled
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "red"))
    (t :underline t :inherit error))
  "Face used for misspelled words.")

(defface jinx-highlight
  '((t :inherit isearch))
  "Face used to highlight current misspelling during correction.")

(defface jinx-accept
  '((t :inherit font-lock-negation-char-face))
  "Face used for the accept action during correction.")

(defcustom jinx-languages "en"
  "List of languages."
  :type '(choice string (repeat string)))

;;;###autoload
(put 'jinx-languages 'safe-local-variable
     (lambda (val)
       (or (stringp val)
           (and (listp val)
                (catch 'break
                  (dolist (s val t)
                    (unless (stringp s)
                      (throw 'break nil))))))))

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
     org-modern-tag org-modern-date-active org-modern-date-inactive)
    (tex-mode
     font-latex-math-face font-latex-sedate-face
     font-lock-function-name-face font-lock-keyword-face
     font-lock-variable-name-face)
    (texinfo-mode
     font-lock-function-name-face font-lock-keyword-face
     font-lock-variable-name-face)
    (rst-mode
     rst-literal rst-external rst-directive rst-definition
     rst-reference)
    (sgml-mode
     font-lock-function-name-face font-lock-variable-name-face)
    (emacs-lisp-mode
     font-lock-constant-face font-lock-warning-face))
  "Alist of faces per major mode.
These faces mark regions which should be excluded in spell
checking."
  :type '(alist :key-type symbol :value-type (repeat face)))

(defcustom jinx-exclude-regexps
  '((emacs-lisp-mode "Package-Requires:.*$")
    (t "[A-Z]+\\>"         ;; Uppercase words
       "\\w*?[0-9]\\w*\\>" ;; Words with numbers, hex codes
       "[a-z]+://\\S-+"    ;; URI
       "<?[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>?")) ;; Email
  "List of excluded regexps."
  :type '(alist :key-type symbol :value-type (repeat regexp)))

(defcustom jinx-include-modes
  '(text-mode prog-mode conf-mode)
  "List of modes included by `global-jinx-mode'."
  :type '(repeat symbol))

;;;; Internal variables

(defvar jinx--predicates
  (list #'jinx--face-ignored-p
        #'jinx--regexp-ignored-p
        #'jinx--word-valid-p
        #'jinx--flyspell-ignored-p)
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

(defvar-local jinx--dicts nil
  "List of dictionaries.")

(defvar-local jinx--syntax-table nil
  "Syntax table used during checking.")

(defvar-local jinx--mode-syntax-table nil
  "Original syntax table of the mode.")

(defvar-local jinx--session-words nil
  "List of words accepted in this session.")

;;;; Declarations for the bytecode compiler

(defvar jinx-mode)
(declare-function jinx--mod-check nil)
(declare-function jinx--mod-add nil)
(declare-function jinx--mod-suggest nil)
(declare-function jinx--mod-dict nil)
(declare-function jinx--mod-describe nil)
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

(defvar-keymap jinx-misspelled-map
  :doc "Keymap attached to misspelled words."
  "<mouse-1>" #'jinx-correct)

(fset 'jinx-misspelled-map jinx-misspelled-map)

;;;; Predicates

(defun jinx--regexp-ignored-p (start)
  "Return non-nil if word at START matches ignore regexps."
  (save-excursion
    (let (case-fold-search)
      (goto-char start)
      (when (and jinx--exclude-regexp (looking-at-p jinx--exclude-regexp))
        (save-match-data
          (looking-at jinx--exclude-regexp)
          (match-end 0))))))

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

(defun jinx--flyspell-ignored-p (_start)
  "Check if word before point is ignored.
This predicate uses the `flyspell-mode-predicate' provided by
some Emacs modes."
  (when-let ((pred (or (bound-and-true-p flyspell-generic-check-word-predicate)
                       (get major-mode 'flyspell-mode-predicate))))
    (with-syntax-table jinx--mode-syntax-table
      (ignore-errors (not (funcall pred))))))

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

(defun jinx--check-pending ()
  "Check pending visible regions."
  (let* ((start (window-start))
         (end (window-end))
         (pos start))
    (while (< pos end)
      (let* ((from (jinx--find-visible-pending pos end t))
             (to (jinx--find-visible-pending from end nil)))
        (if (< from to)
            (setq pos (cdr (jinx--check-region from to)))
          (setq pos to))))))

(defun jinx--check-region (start end)
  "Check region between START and END.
Returns a pair of updated (START END) bounds."
  (let ((jinx--mode-syntax-table (syntax-table)))
    (unwind-protect
        (with-silent-modifications
          (save-excursion
            (save-match-data
              ;; Ensure that region starts and ends at word boundaries
              (goto-char start)
              (re-search-backward "[[:blank:]]\\|^")
              (setq start (match-end 0))
              (goto-char end)
              (re-search-forward "[[:blank:]]\\|$")
              (setq end (match-beginning 0))
              (jinx--delete-overlays start end)
              ;; Use dictionary-dependent syntax table
              (set-syntax-table jinx--syntax-table)
              (goto-char start)
              (while (re-search-forward "\\<\\w+\\>" end t)
                (let ((word-start (match-beginning 0))
                      (word-end (point)))
                  ;; No quote or apostrophe at end
                  (while (and (< word-start word-end)
                              (let ((c (char-before word-end))) (or (= c 39) (= c 8217))))
                    (cl-decf word-end))
                  (when (< word-start word-end)
                    (goto-char word-end)
                    (pcase (run-hook-with-args-until-success 'jinx--predicates word-start)
                      ((and (pred integerp) skip) (goto-char (max word-end (min end skip))))
                      ('nil (overlay-put (make-overlay word-start word-end) 'category 'jinx))))))
              (remove-list-of-text-properties start end '(jinx--pending)))
            (set-syntax-table jinx--mode-syntax-table)))))
  (cons start end))

(defun jinx--get-overlays (start end)
  "Return misspelled words overlays between START and END."
  (cl-loop for ov in (overlays-in start end)
           if (eq (overlay-get ov 'category) 'jinx) collect ov))

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
      (with-current-buffer (window-buffer win)
        (when jinx-mode
          (with-selected-window win
            (jinx--check-pending)))))))

(defun jinx--reschedule (&rest _)
  "Restart the global idle timer."
  (when jinx--timer
    (cancel-timer jinx--timer)
    (setq jinx--timer nil))
  (jinx--schedule))

(defun jinx--schedule ()
  "Start the global idle timer."
  (when (and (not jinx--timer) (get-buffer-window))
    (setq jinx--timer
          (run-with-idle-timer jinx-delay
                               nil #'jinx--timer-handler))))

(defun jinx--load-module ()
  "Compile and load native module."
  (unless (fboundp #'jinx--mod-dict)
    (unless module-file-suffix
      (error "Jinx: Native modules are not supported"))
    (let ((default-directory
           (file-name-directory (locate-library "jinx.el" t)))
          (module (concat "jinx-mod" module-file-suffix)))
      (unless (file-exists-p module)
        (let ((command
               `("cc" "-O2" "-Wall" "-Wextra" "-fPIC" "-shared" "-Wl,--no-as-needed"
                 ,@(split-string-and-unquote
                    (condition-case nil
                        (car (process-lines "pkg-config" "--cflags" "--libs" "enchant-2"))
                      (error "-I/usr/include/glib-2.0 -I/usr/include/enchant-2 -lenchant-2")))
                 ,@(and source-directory
                        (list (concat "-I" (file-name-concat source-directory "src/"))))
                 "-o" ,module ,(file-name-with-extension module ".c"))))
          (with-current-buffer (get-buffer-create "*jinx module compilation*")
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (string-join command " ") "\n")
              (if (equal 0 (apply #'call-process (car command) nil (current-buffer) t (cdr command)))
                  (insert (message "Jinx: %s compiled successfully" module))
                (let ((msg (format "Jinx: Compilation of %s failed" module)))
                  (insert msg)
                  (compilation-mode)
                  (pop-to-buffer (current-buffer))
                  (error msg)))))))
      (module-load (expand-file-name module)))))

(defun jinx--force-overlays (start end)
  "Enforce spell-check of region between START and END.
Return list of overlays, see `jinx--get-overlays'."
  (with-delayed-message (1 "Fontifying...")
    (jit-lock-fontify-now))
  (with-delayed-message (1 "Checking...")
    (setq start (jinx--check-region start end)
          end (cdr start) start (car start)))
  (jinx--get-overlays start end))

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
  (save-restriction
    (widen)
    (dolist (ov (jinx--get-overlays (point-min) (point-max)))
      (goto-char (overlay-end ov))
      (when (jinx--word-valid-p (overlay-start ov))
        (delete-overlay ov)))))

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
    (if (string-match-p "\\`[@#]" selected)
        (let* ((new-word (replace-regexp-in-string "\\`[@#]+" "" selected))
               (idx (- (length selected) (length new-word) 1)))
          (when (equal new-word "") (setq new-word word))
          (if (string-prefix-p "#" selected)
              (unless (member new-word jinx--session-words)
                (push new-word jinx--session-words))
            (jinx--mod-add (or (nth idx jinx--dicts)
                               (user-error "Invalid dictionary"))
                           new-word))
          (jinx--recheck-overlays))
      (when-let (((not (equal selected word)))
                 (start (overlay-start overlay))
                 (end (overlay-end overlay)))
        (delete-overlay overlay)
        (goto-char end)
        (insert-before-markers selected)
        (delete-region start end)))))

(defun jinx--nearest-overlay ()
  "Find nearest misspelled word."
  (let ((overlays (or (jinx--get-overlays (window-start) (window-end))
                      (jinx--force-overlays (window-start) (window-end))))
        nearest)
    (dolist (ov overlays nearest)
      (when (and (not (invisible-p (overlay-start ov)))
                 (or (not nearest)
                     (< (abs (- (overlay-start ov) (point)))
                        (abs (- (overlay-start nearest) (point))))))
        (setq nearest ov)))))

;;;; Public commands

;;;###autoload
(defun jinx-correct (&optional all)
  "Correct nearest misspelled word.
If predicate argument ALL is given correct all misspellings."
  (interactive "*P")
  (unless jinx-mode (jinx-mode 1))
  (unwind-protect
      (save-window-excursion
        (save-excursion
          (cl-letf (((symbol-function #'jinx--timer-handler) #'ignore)) ;; Inhibit
            (if all
                (let* ((overlays
                        (or (sort (jinx--force-overlays (point-min) (point-max))
                                  (lambda (a b) (< (overlay-start a) (overlay-start b))))
                            (user-error "No misspellings in whole buffer")))
                       (count (length overlays)))
                  (cl-loop for ov in overlays for idx from 1
                           if (overlay-buffer ov) do ;; Could be already deleted
                           (jinx--correct ov 'recenter
                                          (format " (%d of %d, RET to skip)"
                                                  idx count))))
              (jinx--correct (or (jinx--nearest-overlay)
                                 (user-error "No misspelling in visible text")))))))
    (jit-lock-refontify)))

;;;###autoload
(define-minor-mode jinx-mode
  "Enchanted just-in-time spell checker."
  :global nil :group 'jinx
  (cond
   (jinx-mode
    (jinx--load-module)
    (hack-local-variables)
    (jinx--get-org-language)
    (setq jinx--exclude-regexp
          (when-let ((regexps (jinx--mode-list jinx-exclude-regexps)))
            (mapconcat (lambda (r) (format "\\(?:%s\\)" r))
                       regexps "\\|"))
          jinx--include-faces (jinx--mode-list jinx-include-faces)
          jinx--exclude-faces (jinx--mode-list jinx-exclude-faces)
          jinx--dicts (delq nil (mapcar #'jinx--mod-dict
                                        (ensure-list jinx-languages)))
          jinx--syntax-table (make-syntax-table))
    (unless jinx--dicts
      (message "Jinx: No dictionaries available for `jinx-languages' = %S"
               jinx-languages))
    (modify-syntax-entry ?' "w" jinx--syntax-table)
    (modify-syntax-entry ?$ "_" jinx--syntax-table)
    (modify-syntax-entry ?% "_" jinx--syntax-table)
    (dolist (dict jinx--dicts)
      (cl-loop for c across (jinx--mod-wordchars dict) do
               (modify-syntax-entry c "w" jinx--syntax-table)))
    (add-hook 'window-state-change-hook #'jinx--reschedule nil t)
    (add-hook 'window-scroll-functions #'jinx--reschedule nil t)
    (add-hook 'post-command-hook #'jinx--reschedule nil t)
    (jit-lock-register #'jinx--mark-pending))
   (t
    (kill-local-variable 'jinx--exclude-regexp)
    (kill-local-variable 'jinx--include-faces)
    (kill-local-variable 'jinx--exclude-faces)
    (kill-local-variable 'jinx--dicts)
    (kill-local-variable 'jinx--syntax-table)
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
