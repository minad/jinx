;;; jinx.el --- Just-in-time spell-checking via libenchant -*- lexical-binding: t -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Package-Requires: ((emacs "27.1") (compat "29.1.4.0"))
;; Created: 2023
;; Version: 0.1
;; Homepage: https://github.com/minad/jinx

;;; Commentary:

;; Jinx provides just-in-time spell-checking checking via libenchant.
;; The package aims to achieve high performance and low resource
;; usage, without impacting your editing experience.
;;
;; Jinx highlights misspellings lazily only in the visible area of the
;; window.  Jinx binds directly to the native libenchant API, such
;; that process communication with a backend Aspell process can be
;; avoided.  Libenchant supports Hunspell, Aspell and a few lesser
;; known backends.  Jinx will automatically compile and load the
;; native module.  Libenchant must be available for compilation.  If
;; `pkg-config' is available it will be used to locate libenchant.  On
;; Debian or Ubuntu, install the packages `libenchant-2',
;; `libenchant-2-dev' and `pkg-config'.
;;
;; Jinx supports multiple languages at the same time in a buffer via
;; the `jinx-languages' customization variable and offers flexible
;; settings to ignore misspellings via faces (`jinx-exclude-faces' and
;; `jinx-include-faces') and programmable predicates.  Jinx comes
;; preconfigured for the most important major modes.
;;
;; Jinx offers two main autoloaded entry points , the mode `jinx-mode'
;; and the command `jinx-correct'.  In your configuration add
;; `jinx-mode' to the hooks of the modes and bind `jinx-correct' to a
;; convenient key.  Since Jinx is independent of the Ispell package,
;; we can reuse the binding M-$ which is bound to `ispell-word' by
;; default.  When pressing M-$, Jinx offers correction suggestions for
;; the misspelling next to point.  If a prefix argument C-u M-$ is
;; passed, the entire buffer is spell-checked.
;;
;; (dolist (hook '(prog-mode-hook text-mode-hook org-mode-hook))
;;   (add-hook hook #'jinx-mode))
;;
;; (keymap-global-set "<remap> <ispell-word>" #'jinx-correct)
;;
;; The technique to spell-check only the visible region of the window
;; was inspired by Campbell Barton's spell-fu package.  Spell-fu
;; maintains the dictionary itself via a hash table, which results in
;; high memory usage for languages with many inflected word forms.  In
;; Jinx we can avoid the complexity of managing the dictionary, since
;; libenchant does that for us.  Nevertheless spell-fu performs very
;; well as a spell-checker.
;;
;; Jinx offers a similar UI as Augusto Stoffel's jit-spell package and
;; borrows ideas from it.  Jit-spell uses Ispell process communication
;; instead of a native API.  It does not restrict the highlighting to
;; the visible area.  In my setup I observed an increase in load and
;; latency as a consequence, in particular in combination with stealth
;; locking and commands which trigger eager fontification like
;; `consult-line'.

;;; Code:

(require 'compat)
(eval-when-compile (require 'cl-lib))

;;;; Customization

(defgroup jinx nil
  "Just-in-time spell-checking via libenchant."
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
    (latex-mode
     font-latex-math-face font-latex-sedate-face
     font-lock-function-name-face font-lock-keyword-face
     font-lock-variable-name-face)
    (sgml-mode
     font-lock-function-name-face font-lock-variable-name-face)
    (emacs-lisp-mode
     font-lock-constant-face font-lock-warning-face))
  "Alist of faces per major mode.
These faces mark regions which should be excluded in spell
checking."
  :type '(alist :key-type symbol :value-type (repeat face)))

;;;; Internal variables

(defvar jinx--predicates
  (list #'jinx--face-ignored-p
        #'jinx--flyspell-ignored-p)
  "Predicate functions called with two arguments START and END.")

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

(defconst jinx--directory (file-name-directory (macroexp-file-name))
  "Installation directory used for compilation.")

;;;; Declarations for the bytecode compiler

(defvar jinx-mode)
(declare-function jinx--mod-check nil)
(declare-function jinx--mod-add nil)
(declare-function jinx--mod-suggest nil)
(declare-function jinx--mod-dict nil)
(declare-function jinx--mod-describe nil)

;;;; Predicates

(defun jinx--face-ignored-p (start _end)
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

(defun jinx--flyspell-ignored-p (_start end)
  "Check if word before END is ignored.
This predicate uses the `flyspell-mode-predicate' provided by
some Emacs modes."
  (when-let ((pred (or (bound-and-true-p flyspell-generic-check-word-predicate)
                       (get major-mode 'flyspell-mode-predicate))))
    (save-excursion
      (goto-char end)
      (ignore-errors (not (funcall pred))))))

;;;; Internal functions

(defun jinx--overlay-modified (overlay &rest _)
  "Delete modified OVERLAY.
This function is a modification hook for the overlay."
  (delete-overlay overlay))

(defun jinx--check-pending (start end)
  "Check pending regions between START and END."
  (while-let ((from (text-property-any start end 'jinx--pending t))
              (to (or (text-property-any from end 'jinx--pending nil) end)))
    (jinx--check-region from to)
    (setq start to)))

(defun jinx--check-region (start end)
  "Check region between START and END."
  (with-silent-modifications
    (save-excursion
      (jinx--delete-overlays start end)
      (let ((line-start start))
        (while (< line-start end)
          (goto-char line-start)
          (let ((line-end (pos-eol)))
            (when (> line-end line-start)
              ;;(message "CHECKING %S" (buffer-substring-no-properties line-start line-end))
              (pcase-dolist (`(,word-start . ,word-end)
                             (jinx--mod-check
                              (car jinx--dicts) ;; TODO multiple dicts
                              (buffer-substring-no-properties line-start line-end)))
                ;;(message "%S %S" word-start word-end)
                (cl-incf word-start line-start)
                (cl-incf word-end line-start)
                (unless (run-hook-with-args-until-success 'jinx--predicates word-start word-end)
                  (overlay-put (make-overlay word-start word-end) 'category 'jinx))))
            (setq line-start (1+ line-end)))))
      (remove-list-of-text-properties start end '(jinx--pending)))))

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
  (save-excursion
    (goto-char start)
    (setq start (pos-bol))
    (goto-char end)
    (setq end (pos-eol))
    (put-text-property start end 'jinx--pending t)
    ;; inhibit-quit is non-nil for stealth locking
    (unless inhibit-quit (jinx--schedule))
    `(jit-lock-bounds ,start . ,end)))

(defun jinx--annotate-suggestion (word)
  "Annotate WORD during completion."
  (and (string-prefix-p "@" word)
       (format " - Save in personal d%s"
               (substring (get-text-property 0 'jinx--group word) 1))))

(defun jinx--group-suggestion (word transform)
  "Group WORD during completion, TRANSFORM candidate if non-nil."
  (if transform
      word
    (get-text-property 0 'jinx--group word)))

(defun jinx--suggestions (word)
  "Retrieve suggestions for WORD."
  (delete-dups
   (cl-loop
    for dict in jinx--dicts for idx from 1
    nconc
    (let* ((suggs (jinx--mod-suggest dict word))
           (at (make-string idx ?@))
           (desc (jinx--mod-describe dict))
           (group (format "Dictionary ‘%s’ (%s)" (car desc) (cdr desc))))
      (setq suggs `(,@suggs ,(concat at word)
                            ,(concat at (downcase word))))
      (dolist (sugg suggs suggs)
        (put-text-property 0 (length sugg) 'jinx--group group sugg))))))

(defun jinx--suggestion-table (word)
  "Completion table for WORD suggestions."
  (setq word (jinx--suggestions word))
  (lambda (str pred action)
    (if (eq action 'metadata)
        '(metadata (display-sort-function . identity)
                   (cycle-sort-function . identity)
                   (group-function . jinx--group-suggestion)
                   (annotation-function . jinx--annotate-suggestion))
      (complete-with-action action word str pred))))

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
          (jinx--check-pending (window-start win) (window-end win)))))))

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
    (let ((default-directory jinx--directory)
          (module "jinx-mod.so"))
      (unless (file-exists-p module)
        (let ((command
               `("cc" "-O2" "-Wall" "-Wextra" "-fPIC" "-shared" "-Wl,--no-as-needed"
                 ,@(split-string-and-unquote
                    (condition-case nil
                        (car (process-lines "pkg-config" "--cflags" "--libs" "enchant-2"))
                      (error "-I/usr/include/enchant-2 -lenchant-2")))
                 ,@(and source-directory
                        (list (concat "-I" (file-name-concat source-directory "src/"))))
                 "-o" ,module ,(file-name-with-extension module ".c"))))
          (setq command (mapconcat #'shell-quote-argument command " "))
          (with-current-buffer (get-buffer-create "*jinx module compilation*")
            (insert command "\n")
            (shell-command command (current-buffer)))))
      (module-load (expand-file-name module)))))

;;;; Public commands

(defun jinx--get-overlays (start end)
  "Return misspelled words overlays between START and END."
  (cl-loop for ov in (overlays-in start end)
           if (eq (overlay-get ov 'category) 'jinx) collect ov))

(defun jinx--force-overlays (start end)
  "Return misspelled words overlays between START and END.
If no misspellings are found, the region is rechecked."
  (or
   (jinx--get-overlays start end)
   (let (message-log-max)
     (message "Fontifying...")
     (jit-lock-fontify-now)
     (message "Checking...")
     (jinx--check-region start end)
     (message "Done")
     (jinx--get-overlays start end))
   (error "No misspelled word found")))

(defun jinx--correct (overlay &optional recenter)
  "Correct word at OVERLAY, optionally RECENTER."
  (let* ((start (overlay-start overlay))
         (end (goto-char (overlay-end overlay)))
         (word (buffer-substring-no-properties start end))
         (hl nil)
         (selected
          (unwind-protect
              (progn
                (when recenter (recenter))
                (setq hl (make-overlay start end))
                (overlay-put hl 'face 'jinx-highlight)
                (overlay-put hl 'window (selected-window))
                (or (completing-read
                     (format "Correct ‘%s’ (RET to skip): " word)
                     (jinx--suggestion-table word)
                     nil nil nil t word)
                    word))
            (delete-overlay hl))))
    (if-let ((len (save-match-data
                    (and (string-match "\\`@+" selected)
                         (match-end 0)))))
        (save-restriction
          (widen)
          (jinx--mod-add (nth (1- len) jinx--dicts)
                         (substring selected len))
          (dolist (overlay (jinx--get-overlays (point-min) (point-max)))
            (unless (jinx--mod-check
                     (car jinx--dicts) ;; TODO multiple dicts
                     (buffer-substring-no-properties
                      (overlay-start overlay)
                      (overlay-end overlay)))
              (delete-overlay overlay))))
      (unless (equal selected word)
        (delete-overlay overlay)
        (insert-before-markers selected)
        (delete-region start end)))))

(defun jinx--nearest-overlay ()
  "Find nearest misspelled word."
  (let* ((overlays (jinx--force-overlays (window-start) (window-end)))
         (nearest (car overlays)))
    (dolist (ov (cdr overlays) nearest)
      (when (< (abs (- (overlay-start ov) (point)))
               (abs (- (overlay-start nearest) (point))))
        (setq nearest ov)))))

;;;###autoload
(defun jinx-correct (&optional all)
  "Correct nearest misspelled word.
If predicate argument ALL is given correct all misspellings."
  (interactive "P")
  (unless jinx-mode (jinx-mode 1))
  (unwind-protect
      (save-window-excursion
        (save-excursion
          (cl-letf (((symbol-function #'jinx--timer-handler) #'ignore)) ;; Inhibit
            (if all
                (dolist (overlay
                         (sort (jinx--force-overlays (point-min) (point-max))
                               (lambda (a b) (< (overlay-start a) (overlay-start b)))))
                  (when (overlay-buffer overlay) ;; Could be already deleted
                    (jinx--correct overlay 'recenter)))
              (jinx--correct (jinx--nearest-overlay))))))
    (jit-lock-refontify)))

;;;###autoload
(define-minor-mode jinx-mode
  "Just-in-time spell-checking via libenchant."
  :global nil
  (cond
   (jinx-mode
    (jinx--load-module)
    (hack-local-variables)
    (jinx--get-org-language)
    (setq jinx--include-faces (jinx--mode-list jinx-include-faces)
          jinx--exclude-faces (jinx--mode-list jinx-exclude-faces)
          jinx--dicts (delq nil (mapcar #'jinx--mod-dict
                                        (ensure-list jinx-languages))))
    (add-hook 'window-state-change-hook #'jinx--reschedule nil t)
    (add-hook 'window-scroll-functions #'jinx--reschedule nil t)
    (add-hook 'post-command-hook #'jinx--reschedule nil t)
    (jit-lock-register #'jinx--mark-pending))
   (t
    (kill-local-variable 'jinx--include-faces)
    (kill-local-variable 'jinx--exclude-faces)
    (kill-local-variable 'jinx--dicts)
    (remove-hook 'window-state-change-hook #'jinx--reschedule t)
    (remove-hook 'window-scroll-functions #'jinx--reschedule t)
    (remove-hook 'post-command-hook #'jinx--reschedule t)
    (jit-lock-unregister #'jinx--mark-pending)
    (jinx--cleanup))))

;;;; Overlay properties

(put 'jinx 'evaporate             t)
(put 'jinx 'face                  'jinx-misspelled)
(put 'jinx 'modification-hooks    (list #'jinx--overlay-modified))
(put 'jinx 'insert-in-front-hooks (list #'jinx--overlay-modified))
(put 'jinx 'insert-behind-hooks   (list #'jinx--overlay-modified))

(provide 'jinx)
;;; jinx.el ends here
