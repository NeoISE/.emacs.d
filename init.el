;;; init.el -- My init file for Emacs -*- lexical-binding: t; -*-
;;
;; Orig. Author:
;;     Name: Maniroth Ouk
;;     Email: maniroth_ouk@outlook.com
;; Last Updated: <12 Jan. 2019 -- 00:50 (Central Standard Time) by Maniroth Ouk>
;; License: MIT
;;
;;; Commentary:
;;
;; I, Maniroth Ouk, am sorry to people whose codes I featured here in my init
;; and in various external init configs that I did not mention. I have been
;; building this init for my personal use since 2009, so code-snippets'
;; information do fall through the cracks around here since the beginning.
;;
;; Table Of Contents:
;;  + §0: Preliminary Configs
;;     - §0 §§0: Version Requirements
;;     - §0 §§1: Misc. Preparations
;;  + §1: Package Management
;;  + §2: General Configs
;;     - §2 §§0: Load-Path
;;     - §2 §§1: Various Variables
;;     - §2 §§2: Tabs, Alignment, etc.
;;     - §2 §§3: Backups and Recent-Files
;;     - §2 §§4: Yasnippet and Auto-Complete
;;     - §2 §§5: Spelling
;;  + §3: Keybindings
;;  + §4: Graphical Settings
;;
;;; Code:

;; §0: Preliminary Configs

;; §0 §§0: Version Requirements

;; Compare emacs version to make sure everything is able to work
(defconst emacs-major-version-minimum 26
  "The minimum major version of emacs for this configuration.")
(defconst emacs-minor-version-minimum 1
  "The minimum minor version of emacs for this configuration.")

(defconst emacs-version-minimum (format "%d.%d"
                                        emacs-major-version-minimum
                                        emacs-minor-version-minimum)
  "The full version string for the minimum version of emacs for this config.")

(when (version< emacs-version emacs-version-minimum)
  (error "The minimum emacs version that will work with this config is \"%s\";
Your current emacs version is \"%s\"."
         emacs-version-minimum emacs-version))


;; §0 §§1: Misc. Preparations

(defun advice--timestamp-message (format-string &rest args)
  "Provides for time-stamping the messages in the `*Messages*' buffer.

Taken from ``https://is.gd/t9VpW4'' with minor adjustments:
  * Uses the built-in function `messages-buffer' instead of direct name.
  * Replaces the first line conditional with a check for `nil' format-string (first `and' statement).
    - Now checks if the format-string is a string (so that it is a valid message) and is not empty (second `and' statement), and
    - If `message-log-max' is non-nil (third `and' statement).
    The new check will be more likely to prevent empty time-stamps (i.e. when message's format is `nil').
  * Replaces the `if' statement to a `when' statement because there was no else clause to begin with.
  * Reduced the time-string to being just a call to the function `format-time-string'."
  (when (and (stringp format-string)
             (not (string= format-string ""))
             message-log-max)
    (let ((deactivate-mark nil)
          (inhibit-read-only t))
      (with-current-buffer (messages-buffer)
        (goto-char (point-max))
        (when (not (bolp))
          (newline))
        (insert "[" (format-time-string "%FT%T.%6N") "] ")))))

(advice-add 'message :before 'advice--timestamp-message)

;; the other config files
(defvar config-template-directory
  (file-name-as-directory (concat user-emacs-directory "templates"))
  "The directory that contains template configurations.")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  ;; makes the file if it doesn't exist
  (copy-file (expand-file-name "custom.el" config-template-directory) custom-file))

(defvar user-sensitive-file
  (expand-file-name "sensitive.el" user-emacs-directory)
  "A file with user-sensitive data like name, email, etc.")
(unless (file-exists-p user-sensitive-file)
  ;; makes the file if it doesn't exist
  (copy-file (expand-file-name "sensitive.el" config-template-directory) user-sensitive-file)
  (message "%s" "Set up paradox integration asap")
  (message "%s" "Set up location asap"))

(defun advice--suppress-messages (orig-fun &rest args)
  "Suppresses messages from `orig-fun' (stopping message from going to echo area and from getting logged in the message buffer), which can clog up message logs quickly."
  (let (message-log-max
        (inhibit-message t))
    (apply orig-fun args)))

(defun advice--suppress-messages-logging (orig-fun &rest args)
  "Suppresses messages from `orig-fun' from getting logged in the message buffer."
  (let (message-log-max)
    (apply orig-fun args)))

(defun current-buffer-major-mode nil
  "Returns the current buffers major mode."
  (with-current-buffer (current-buffer)
    major-mode))



;; §1: Package Management

;; Set up the package sources, makes melpa higher than gnu
(require 'package)
(unless (gnutls-available-p)
  (error "%s" "gnutls-available-p: not available; unable to use tls authenication"))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-archive-priorities '(("melpa" . 10)
                                   ("gnu"   . 0)))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Tell emacs about my packages setup.
;; Packages that I want to move with me when I move to a clean system.
;; The variable will be changed again by the current emacs when the load-file
;; for the custom-file occurs to match the actual packages for the system.
(setq package-selected-packages `(csharp-mode
                                  markdown-mode
                                  powershell
                                  js2-mode
                                  json-mode
                                  matlab-mode
                                  haskell-mode
                                  lua-mode

                                  ;; minor modes for improved editing
                                  smart-tabs-mode
                                  stickyfunc-enhance
                                  visual-regexp
                                  visual-fill-column
                                  highlight-thing
                                  smart-mark
                                  origami

                                  ;; graphic improvements
                                  apropospriate-theme
                                  smart-mode-line
                                  modern-cpp-font-lock

                                  ;; Emacs UI improvement
                                  popwin
                                  browse-kill-ring
                                  ido-grid-mode
                                  ido-completing-read+
                                  smex
                                  ranger

                                  ;; Package manager
                                  async
                                  paradox

                                  ;; tab-completions
                                  yasnippet
                                  java-snippets
                                  haskell-snippets
                                  company
                                  yasnippet-snippets))
(package-install-selected-packages)

;; leaves the loading after the installation of needed files
(load custom-file)

(require 'async)
(require 'paradox)

;; perform asynchronous byte compliation of package files
(async-bytecomp-package-mode t)
(setq async-bytecomp-allowed-packages '(all))

;; load sensitive data, require for paradox token access
(load user-sensitive-file)

;; set up paradox, the package manager of choice
(unless paradox-github-token
  (setq paradox-github-token t))
(setq paradox-automatically-star t
      paradox-execute-asynchronously t
      paradox-lines-per-entry 2)
(paradox-enable)



;; §2: General Configs

;; §2 §§0: Load-Path

;; adds some paths to look for functions, etc.
(add-to-list 'load-path (expand-file-name "vendor" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elisp"  user-emacs-directory))
(add-to-list 'load-path (expand-file-name "themes" user-emacs-directory))


;; §2 §§1: Various Variables

(require 'recentf)
(require 'recentf-advice)
(require 'csharp-mode)
(require 'powershell)
(require 'smart-tabs-mode)
(require 'smex)
(require 'ido-completing-read+)
(require 'popwin)
(require 'yasnippet)
(require 'company)
(require 'ido-grid-mode)
(require 'smart-mark)
(require 'stickyfunc-enhance)
(require 'origami)
(require 'browse-kill-ring)
(require 'markdown-mode)
(require 'js)
(require 'js2-mode)
(require 'haskell-mode)
(require 'haskell-snippets)
(require 'ruby-mode)
(require 'lua-mode)
(require 'sundial)
(require 'visual-regexp)

;; don't show the startup message
(setq inhibit-startup-message t)

;; set the home directory in windows
(when (eq system-type 'windows-nt)
  (setq-default default-directory (file-name-as-directory
                                   (getenv "UserProfile"))))

;; prefer UTF-8
(setq locale-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; always syntax highlight
(global-font-lock-mode t)

;; mode line settings
(setq column-number-mode t              ; column number in the mode line
      line-number-mode t                ; line number in the mode line
      size-indication-mode t            ; size in the mode line
      display-time-format "%a, %d %b %Y (%l:%M %p)")
(display-time-mode t)

;; scrolling
(setq scroll-step 1                     ; one line at a time
      scroll-margin 4                   ; 4 lines before the edge
      ;; in the event that we scroll with mouse
      mouse-wheel-scroll-amount '(1 ((shift) . 4) ((control)))
      mouse-wheel-follow-mouse t        ; just like other editors
      mouse-wheel-progressive-speed t)

;; echo keys faster
(setq echo-keystrokes 0.05)

;; kill the beeps
(setq ring-bell-function 'ignore)

;; warn about large files over 25MB
(setq large-file-warning-threshold (* 25 1024 1024))

;; confirm with only one letter
(defalias 'yes-or-no-p 'y-or-n-p)

;; why do you want to leave so soon
(setq confirm-kill-emacs 'yes-or-no-p)

;; don't show the cursor in non-active window
(setq-default cursor-in-non-selected-windows nil)

;; line numbering
(global-display-line-numbers-mode)

;; force newer files
(setq load-prefer-newer t)

;; resolve symlinks
(setq-default find-file-visit-truename t)

;; requires emacs to place a newline at the end of the file
(setq require-final-newline 'visit-save)

;; setup markdown mode
(add-to-list 'auto-mode-alist '("\\.m\\(arkdown\\|d\\)\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; single space ends a sentence
(setq sentence-end-double-space nil)

;; activate smex
(smex-initialize)

;; activate origami, code-folding
(global-origami-mode)

;; like setting ido-create-new-buffer to 'always; force no confirmation
(setq confirm-nonexistent-file-or-buffer nil)

;; improves ido
(setq ido-enable-prefix nil
      ido-enable-regexp t               ; more powerful
      ido-case-fold t
      ido-enable-dot-prefix nil
      ido-enable-flex-matching t
      ido-enable-tramp-completion t
      ido-enable-last-directory-history t
      ido-use-filename-at-point nil
      ido-use-url-at-point nil
      ido-create-new-buffer 'always
      ido-ignore-buffers '("\\` ")
      ido-ignore-directories '("\\`\\.\\{1,2\\}\\(\\'\\|/\\)" ; . .. ./ ../
                               "\\`CVS/"
                               "-- \\(dummy\\|old\\)/"
                               "\\`\\.\\(git\\|svn\\)/")
      ido-ignore-files '("\\`\\.\\{1,2\\}\\(\\'\\|/\\)" ; . .. ./ ../
                         "\\`CVS/"
                         "\\`#"
                         "\\`\\.#"      ; original ver. ".#" blocked C# and F#
                         "-- \\(dummy\\|old\\)/"
                         "\\`\\.\\(git\\|svn\\)/")
      ido-ignore-extensions t)
(ido-mode t)
(ido-everywhere t)
(ido-ubiquitous-mode t)
;; turn on grid look for ido-mode
(ido-grid-mode t)

;; highlight brackets
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; insert pairs
(electric-pair-mode t)

;; access to subwords
(global-subword-mode t)

;; activates popwin-mode
;; prevents permanent windows from opening for *Help*, etc.
(popwin-mode t)

;; delete selected content when highlighted
(delete-selection-mode t)

;; browse the kill-ring
(setq browse-kill-ring-highlight-current-entry t
      browse-kill-ring-show-preview t)

;; putting the function declaration in header-line
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(semantic-mode t)

;; going back to old anchor position
(smart-mark-mode)

;; use js2-mode instead of js-mode for javascript files
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; but give js-mode a better linting capacity
(add-hook 'js-mode-hook 'js2-minor-mode)
(setq js2-highlight-level 3)

;; functions should be with its comment-docs
(setq narrow-to-defun-include-comments t)

;; enable commands
(dolist (command '(narrow-to-region
                   narrow-to-page
                   narrow-to-defun
                   upcase-region
                   downcase-region))
  (put command 'disabled nil))

;; visual-regexp settings
(setq vr/auto-show-help t
      vr/default-feedback-limit 75
      vr/default-replace-preview nil
      vr/match-separator-use-custom-face t
      vr/plain nil)



;; §2 §§2: Tabs, Alignment, etc.

(defun advice--force-use-spaces-only (orig-fun &rest args)
  "Forces functions that have this advice to be using only spaces without hard tabs."
  (let ((indent-tabs-mode nil))
    (apply orig-fun args)))

(defun hook--force-use-spaces-only nil
  "Forces the buffer to not use TABS to indent. Used as a hook to a mode."
  (setq indent-tabs-mode nil))

(setq-default fill-column 80
              tab-width 4
              ;; The following is not necessary. As far as I know, since version
              ;; 25+, the default nil value will already use `tab-width' as the
              ;; deciding value.
              ;;tab-stop-list (number-sequence 4 200 4)
              tab-stop-list nil         ; to enforce the above statement
              indent-tabs-mode t)

;; prevent align* from inserting hard tabs that create alignment issues with
;; mismatching tab sizes on different systems
(dolist (command '(align
                   align-regexp))
  (advice-add command :around #'advice--force-use-spaces-only))

;; no literal tabs for modes
(dolist (hook '(emacs-lisp-mode-hook
                lisp-mode-hook
                lisp-interaction-mode-hook

                powershell-mode-hook

                python-mode-hook

                nxml-mode-hook
                html-mode-hook
                ))
  (add-hook hook #'hook--force-use-spaces-only))

;; make offsets in cc-mode the same as tab-width
(defvaralias 'c-basic-offset 'tab-width)

;; set the style for cc-mode
(setq c-default-style '((java-mode   . "java")
                        (awk-mode    . "awk")
                        (c++-mode    . "bsd")
                        (c-mode      . "bsd")
                        (csharp-mode . "bsd")
                        (other       . "linux")))

;; powershell indent variables
(defvaralias 'powershell-indent 'tab-width)
(defvaralias 'powershell-continuation-indent 'powershell-indent)

(add-hook 'powershell-mode-hook
          (lambda nil
            (setq fill-column 115)))

;; ruby indent variables
(defvaralias 'ruby-indent-level 'tab-width)
(setq-default ruby-indent-tabs-mode t)

;; lua indentations
(defvaralias 'lua-indent-level 'tab-width)
(setq-default lua-indent-string-contents nil)

;; smart-tabs-mode config
;; add c-sharp to smart-tab-mode
(smart-tabs-add-language-support csharp csharp-mode-hook
  ((c-indent-line . c-basic-offset)
   (c-indent-region . c-basic-offset)))

;; add lua to smart-tab-mode
(smart-tabs-add-language-support lua lua-mode-hook
  ((lua-indent-line . lua-indent-level)))

;; activate smart-tab-mode
(smart-tabs-insinuate 'c 'c++ 'csharp 'java 'javascript 'cperl 'ruby 'lua)

;; soft wrapping
(dolist (hook '(text-mode-hook
                prog-mode-hook))
  (add-hook hook 'visual-line-mode))



;; §2 §§3: Backups and Recent-Files

;; a symlink should be set up for the backup to be more portable
;; if symlink or folder does not exist, create a plain directory to put backups
(unless (file-exists-p (expand-file-name "saves" user-emacs-directory))
  (make-directory (expand-file-name "saves" user-emacs-directory)))
(let ((backup-location (file-name-as-directory
                        (expand-file-name "saves" user-emacs-directory))))
  (setq backup-directory-alist `(("." . ,backup-location))
        backup-by-copying nil
        backup-by-copying-when-linked t
        backup-by-copying-when-mismatch t
        backup-by-copying-when-privileged-mismatch nil
        delete-old-versions t
        kept-new-versions 5
        kept-old-versions 0
        version-control t))

;; sets the file to save the recent files to
(setq recentf-save-file (expand-file-name "recentf" user-emacs-directory))

;; sets the maximum files to store in list
(setq recentf-max-saved-items 20000)

;; exclude certain files from the list
(setq recentf-exclude `("/elpa/" "/tmp/" "/ssh:" "/sudo:" "/scp:"
                        ,(regexp-quote
                          (file-name-nondirectory ido-save-directory-list-file))
                        ,(regexp-quote
                          (file-name-nondirectory smex-save-file))
                        ,(regexp-quote
                          (file-name-nondirectory recentf-save-file))
                        ,(regexp-quote
                          (file-name-nondirectory user-sensitive-file))
                        ,(regexp-quote
                          (file-name-nondirectory custom-file))))

;; sets the amount of files to show
(setq recentf-max-menu-items 100)

;; run cleanup during idle time, after 300 seconds
(setq recentf-auto-cleanup 300)

;; set up the advices for recentf functions
(advice-add 'recentf-save-list :around #'recentf-advice--suppress-list-writes)
(advice-add 'recentf-cleanup :around #'advice--suppress-messages)
;; comment the following if one wants the echo message for preventing unnecessary recentf-list saves
(advice-add 'recentf-save-list :around #'advice--suppress-messages)

;; run recentf mode
(recentf-mode t)

;; run the saving list function after 120 seconds of emacs being idle
;; maybe not needed, considering removal of the following lines
;;(run-with-idle-timer 120 t 'recentf-save-list)

;; run the saving list function every 20 minutes
(run-at-time nil (* 20 60) 'recentf-save-list)



;; §2 §§4: Yasnippet and Auto-Complete

;; yasnippet mode settings
;; (yas-reload-all)
;; (add-hook 'prog-mode-hook 'yas-minor-mode)
(setq yas-prompt-functions '(yas-ido-prompt
                             yas-completing-prompt)
      yas-indent-line 'auto
      yas-also-auto-indent-first-line t
      yas-also-indent-empty-lines nil
      yas-snippet-revival t
      yas-triggers-in-field t)
(yas-global-mode t)

;;; auto-complete settings
(add-hook 'after-init-hook #'global-company-mode)



;; §2 §§5: Spelling

;; Since aspell is good with CamelCases, it is tried first before falling back
;; on hunspell, and if none are available, then disable spell checking.
(cond
 ((executable-find "aspell")
  ;; found aspell on this system
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra"
                            "--run-together"
                            "--run-together-limit=10"
                            "--run-together-min=2")
        ispell-list-command "--list"))

 ((executable-find "hunspell")
  ;; found hunspell on this system
  (setq ispell-program-name "hunspell"
        ispell-local-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]"
                                         "[']" nil ("-d" "en_US") nil utf-8))
        ispell-local-dictionary "en_US"))

 (t
  (setq ispell-program-name nil)
  (message "Cannot find any spell checkers on this system...")))

;; performance speedup
(setq flyspell-issue-message-flag nil)

;; enable flyspell in text mode
(add-hook 'text-mode-hook 'flyspell-mode)

(defun disable-flyspell-mode nil
  (interactive)
  (flyspell-mode -1))

;; but take it off of change-log and log-edit
(dolist (hook '(change-log-mode-hook
                log-edit-mode-hook))
  (add-hook hook #'disable-flyspell-mode))


(defun advice--extend-flyspell-generic-progmode-verify-with-more-rules (orig-fun &rest args)
  "It extends onto \\[flyspell-generic-progmode-verify] with additions that are specific to certain programming language modes.
The function returns nil for places that the spell checker should `not' check; otherwise the function returns t."
  (and
   (or
    ;; the old predicate
    (apply orig-fun args)

    ;; new addition: `whitelist', additional regions to check
    ;; TODO: add some whitelist rules
    )

   ;; new addition: `blacklist', regions that should not be tested
   (let ((-current-major-mode (current-buffer-major-mode))
         -current-target)
     (not
      ;; a t in this block means we matched a `blacklist' rule, but we need to
      ;; negate that (to a nil) to signify to the outer `and' that we don't want
      ;; to check the words (successful `blacklist')
      ;;
      ;; otherwise we went through each `rule' (the `progn' blocks) and each
      ;; rule did not apply (gives nil), so the `or' will return nil and the
      ;; `not' turns that to t to signify to the outer `and' that we only care
      ;; for the result of the `whitelist' above.
      (or
       (progn
         ;; exclude #include in C and C++
         (setq -current-target (buffer-substring-no-properties
                                (line-beginning-position)
                                (line-end-position)))
         (and
          (or (equal -current-major-mode 'c-mode)
              (equal -current-major-mode 'c++-mode))
          (string-match "^#\\([[:space:]]*\\)include\\b" -current-target))))))))
(advice-add 'flyspell-generic-progmode-verify :around
            #'advice--extend-flyspell-generic-progmode-verify-with-more-rules)

(add-hook 'prog-mode-hook 'flyspell-prog-mode)



;; §3: Keybindings

(require 'windmove)
(require 'xah-copy)
(require 'xah-cut)

;; most used keys
(global-set-key (kbd "<f6>") 'isearch-forward-regexp)
(global-set-key (kbd "<f7>") 'isearch-backward-regexp)
(global-set-key (kbd "<f8>") 'ranger)
(global-set-key (kbd "<f9>") 'split-window-below)
(global-set-key (kbd "<f10>") 'split-window-right) ; no need for menu-bar-open

;; paradox
;; easy to remember to apply paradox before action (p before ...)
(global-set-key (kbd "C-c C-p p") 'paradox-list-packages)
(global-set-key (kbd "C-c C-p u") 'paradox-upgrade-packages)

;; windmove
(global-set-key (kbd "C-c w") 'windmove-up)
(global-set-key (kbd "C-c s") 'windmove-down)
(global-set-key (kbd "C-c a") 'windmove-left)
(global-set-key (kbd "C-c d") 'windmove-right)

;; powershell
;; default keys for powershell-mode is horrible
(define-key powershell-mode-map (kbd "C-'") 'powershell-quote-selection)
(define-key powershell-mode-map (kbd "C-\"") 'powershell-quote-selection)
(define-key powershell-mode-map (kbd "M-'") 'powershell-unquote-selection)
(define-key powershell-mode-map (kbd "M-\"") 'powershell-unquote-selection)
(define-key powershell-mode-map (kbd "C-`") 'powershell-escape-selection)

;; origami
(define-key origami-mode-map (kbd "C-c C-f") 'origami-toggle-node)
(define-key origami-mode-map (kbd "C-c C-S-f t")
  'origami-recursively-toggle-node)
(define-key origami-mode-map (kbd "C-c C-S-f c") 'origami-close-all-nodes)
(define-key origami-mode-map (kbd "C-c C-S-f o") 'origami-open-all-nodes)
(define-key origami-mode-map (kbd "C-c C-S-f u") 'origami-undo)
(define-key origami-mode-map (kbd "C-c C-S-f r") 'origami-redo)
(define-key origami-mode-map (kbd "C-c C-S-f R") 'origami-reset)

;; visual-regexp
(global-set-key (kbd "C-c r") 'vr/replace)
(global-set-key (kbd "C-c q") 'vr/query-replace)

;; recentf
(global-set-key (kbd "C-c C-r") 'recentf-open-files)

;; smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "<menu>") 'smex)
(global-set-key (kbd "<apps>") 'smex)
(global-set-key (kbd "M-S-x") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'execute-extended-command)
(global-set-key (kbd "C-c <menu>") 'execute-extended-command)
(global-set-key (kbd "C-c <apps>") 'execute-extended-command)

;; yasnippet
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-<tab>") 'yas-expand)

;; cut, copy, kill-line
(global-set-key (kbd "C-w") 'xah-cut-line-or-region)
(global-set-key (kbd "M-w") 'xah-copy-line-or-region)
(global-set-key (kbd "C-k") 'kill-whole-line)

;; browse-kill-ring
(global-set-key (kbd "M-y") 'browse-kill-ring)



;; §4: Graphical Settings

;;;; Graphics configuration
(require 'modern-cpp-font-lock)
(require 'hl-line)

;; disable tool-bar
(tool-bar-mode -1)

(when (eq system-type 'windows-nt)
  ;; scroll-bars on windows looks bad
  (scroll-bar-mode -1))

;; enable better font lock for c++
(modern-c++-font-lock-global-mode t)

;; this function only works under the assumption of graphical display
(defun maximum-pixel-height (&optional display sys-type)
  "Returns the maximum possible height in pixels that can be visible for SYS-TYPE if non-nil, otherwise calculate based on the current SYSTEM-TYPE.

The parameter DISPLAY is used to avert a negative size issue when called under daemon mode."
  (let ((sys (or sys-type
                 system-type)))
    ;; take off some pixels that is not addressable or not visible
    ;; also take off some for horizontal taskbar
    (- (display-pixel-height display) 54
       (cond ((eq sys 'windows-nt) 32)
             ((eq sys 'darwin) 22)
             (t 0)))))

;; this function only works under the assumption of graphical display
(defun maximum-pixel-width (&optional display sys-type)
  "Returns the maximum possible width in pixels that can be visible for SYS-TYPE if non-nil, otherwise calculate based on the current SYSTEM-TYPE.

The parameter DISPLAY is used to avert a negative size issue when called under daemon mode."
  (let ((sys (or sys-type
                 system-type)))
    (- (display-pixel-width display) 48)))

(defun my-initial-frame-setup (&optional frame)
  "This function sets up the initial frame of the emacs process."
  (when (daemonp)
    ;; redo the hook when daemon process
    (remove-hook 'after-make-frame-functions 'my-initial-frame-setup)
    (add-hook 'after-make-frame-functions 'my-default-frame-setup))

  (let ((frm (or frame
                 (selected-frame))))
    (if (display-graphic-p frm)
        ;; the frame is graphical

        (progn
          (let* ((frame-resize-pixelwise t)
                 (max-height (maximum-pixel-height frm))
                 (max-width (maximum-pixel-width frm))
                 ;; 85 percent on each side of the viewable window
                 (prefer-height (* 0.85 max-height))
                 (prefer-width (* 0.85 max-width)))
            (set-frame-position frm
                                (truncate (/ (- max-width prefer-width) 2))
                                (truncate (/ (- max-height prefer-height) 2)))
            (set-frame-size frm
                            (truncate prefer-width)
                            (truncate prefer-height) t))

          ;; set up the colorings
          (my-color-and-graphics-setup))
      ;; the frame is not graphical
      (let ((available-color (display-visual-class)))
        (cond ((or (eq available-color 'static-color)
                   (eq available-color 'pseudo-color))
               ;; a limited number of colors, used as an indicator of TTY
               (menu-bar-mode -1)

               ;; change color
               (custom-set-faces
                '(mode-line-buffer-id ((t (:background "green"
                                                       :foreground "blue"
                                                       :weight bold))))
                '(show-paren-match ((t (:background "magenta"
                                                    :foreground "black"))))
                '(show-paren-mismatch ((t (:background "red"
                                                       :foreground "black")))))

               ;; custom key settings, probably not necessary for other systems
               (global-set-key (kbd "<f13>") 'execute-extended-command)
               (global-set-key (kbd "<f14> u") 'undo)
               (global-set-key (kbd "<f14> m a") 'mark-whole-buffer)
               (global-set-key (kbd "<f14> m s") 'set-mark-command))

              ((or (eq available-color 'true-color)
                   (eq available-color 'direct-color))
               ;; a large available set of colors, good for themes
               (my-color-and-graphics-setup))

              (t
               (message "No support for the following color class: %s"
                        available-color)))))))

(defun my-default-frame-setup (&optional frame)
  "This function sets up the frames that follow the initial frame."
  ;; only time this function will get either choices in one process is if it is a daemon
  (let ((frm (or frame
                 (selected-frame))))
    (if (display-graphic-p frm)
        ;; the frame is graphical
        (progn
          (let* ((frame-resize-pixelwise t)
                 (max-height (maximum-pixel-height frm))
                 (prefer-height (* 0.64 max-height)))
            (set-frame-height frm (truncate prefer-height) nil t))
          (set-frame-width frm (+ fill-column hl-linum/margin-space)))
      ;; the frame is not graphical
      (let ((available-color (display-visual-class)))
        (cond ((or (eq available-color 'static-color)
                   (eq available-color 'pseudo-color))
               ;; a limited number of colors, an indicator of TTY
               ;; though under TTY, there probably won't be another frame in use
               )
              ((or (eq available-color 'true-color)
                   (eq available-color 'direct-color))
               ;; a large available set of colors, good for themes
               ))))))

(defun my-color-and-graphics-setup nil
  ;; requires
  (require 'visual-fill-column)
  (require 'smart-mode-line)
  (require 'highlight-thing)
  (require 'shift-cursor)
  (require 'speedbar-icons-theme)
  (require 'my-prettify-symbols)

  ;; smart mode line settings
  (setq sml/shorten-modes t
        sml/name-width 25
        sml/mode-width 'full)

  ;; home directory on windows is different than unix
  (when (eq system-type 'windows-nt)
    (let ((new-replacer '())
          (my-home-dir (file-name-as-directory (getenv "UserProfile"))))
      (add-to-list 'new-replacer
                   `(,my-home-dir ":Home:") t)
      (add-to-list 'new-replacer
                   '(":Home:[Dd]ocument[s]?/" ":Doc:") t)
      (add-to-list 'new-replacer
                   '(":Home:[Dd]ownload[s]?/" ":DL:") t)
      (add-to-list 'new-replacer
                   '(":Home:\\([Oo]ne\\|[Ss]ky\\)[Dd]rive/" ":Cloud:") t)

      (setq sml/replacer-regexp-list (append sml/replacer-regexp-list
                                             new-replacer))))

  (add-to-list 'sml/replacer-regexp-list
               '("^:Doc:git-dev/" ":Git:") t)
  ;; non-greedy, takes only the project name
  (add-to-list 'sml/replacer-regexp-list
               '("^:Git:\\(.*?\\)/" ":G/\\1:") t)
  (add-to-list 'sml/replacer-regexp-list
               '("^:Git:\\(.*\\)/src/main/java/" ":G/\\1/SMJ:") t)
  (add-to-list 'sml/replacer-regexp-list
               '("^\\(.*\\)/\\(.*?\\)/src/main/java/" ":\\2/SMJ:") t)

  ;; highlight the current line for easier preview
  (global-hl-line-mode)

  ;; do some margin fiddling with visual-fill-column
  (global-visual-fill-column-mode)
  (setq-default visual-fill-column-fringes-outside-margins nil)

  ;; markdown header scaling
  (setq markdown-header-scaling t
        markdown-header-scaling-values '(1.4 1.2 1.15 1.1 1.0 0.9))

  ;; load the themes for quicker access
  (load-theme 'apropospriate-light nil t)
  (load-theme 'apropospriate-dark nil t)
  (load-theme 'smart-mode-line-respectful nil t)

  ;; turn on sml
  (setq sml/theme 'respectful)
  (sml/setup)

  (defun sundial-daytime-config ()
    "Turns off the dark theme and turn on the light theme and other configurations for daytime."
    (disable-theme 'apropospriate-dark)
    (enable-theme 'apropospriate-light))

  (defun sundial-nighttime-config ()
    "Turns off the light theme and turn on the dark theme and other configurations for nighttime."
    (disable-theme 'apropospriate-light)
    (enable-theme 'apropospriate-dark))

  (defun sundial-common-config ()
    "Configurations that are common to both day(time) and night(time)."
    ;; makes the markdown header noticable
    (markdown-update-header-faces markdown-header-scaling
                                  markdown-header-scaling-values))

  (add-hook 'sundial-daytime-hook #'sundial-common-config)
  (add-hook 'sundial-daytime-hook #'sundial-daytime-config)
  (add-hook 'sundial-nighttime-hook #'sundial-common-config)
  (add-hook 'sundial-nighttime-hook #'sundial-nighttime-config)
  (sundial-start)

  ;; comments should be italics always
  (custom-set-faces
   '(font-lock-comment-delimiter-face ((t (:slant italic))))
   '(font-lock-comment-face ((t (:slant italic)))))

  ;; increase line space for readability
  (setq-default line-spacing 0.25)

  ;; highlight-thing
  (setq highlight-thing-delay-seconds 3.5
        highlight-thing-case-sensitive-p t
        highlight-thing-limit-to-defun nil
        highlight-thing-limit-to-region-in-large-buffers-p t
        highlight-thing-narrow-region-lines 45
        highlight-thing-large-buffer-limit large-file-warning-threshold
        highlight-thing-prefer-active-region nil
        highlight-thing-all-visible-buffers-p t)
  (add-hook 'prog-mode-hook 'highlight-thing-mode)

  ;; different cursors to indicate different writing modes
  (global-shift-cursor-mode t)

  ;; declutter symbolic words
  (global-prettify-symbols-mode t)
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  (my-prettify-symbols-default-config)

  (setq speedbar-show-unknown-files t)
  (add-hook 'speedbar-mode-hook (lambda nil
                                  (display-line-numbers-mode -1))))

;; execute the graphical section under different circumstance
;; since daemon and terminal session are somewhat related, the use of daemonp is
;; used to distinguish the two
(if (daemonp)
    (progn
      (dolist (frm-alist '(initial-frame-alist
                           default-frame-alist))
        (if (eq system-type 'windows-nt)
            (if (member "Hack" (font-family-list))
                (add-to-list frm-alist '(font . "Hack-10"))
              ;; else
              (add-to-list frm-alist '(font . "Consolas-10"))
              ;; For math symbols
              (set-fontset-font t '(#X2200 . #X22EF) "Dejavu Sans Mono")
              ;; arrows
              (set-fontset-font t '(#X2190 . #X21E9) "Dejavu Sans Mono"))
          (if (member "Hack" (font-family-list))
              (add-to-list frm-alist '(font . "Hack-10"))
            (add-to-list frm-alist '(font . "DejaVu Sans Mono-10")))))

      (add-hook 'after-make-frame-functions 'my-initial-frame-setup))
  ;; when not started as a daemon
  (progn
    ;; fonts are not shared across systems, so we use different fonts
    (if (eq system-type 'windows-nt)
        (if (member "Hack" (font-family-list))
            (set-frame-font "Hack-10" t t)
          ;; else
          (set-frame-font "Consolas-10" t t)
          ;; For math symbols
          (set-fontset-font t '(#X2200 . #X22EF) "Dejavu Sans Mono")
          ;; arrows
          (set-fontset-font t '(#X2190 . #X21E9) "Dejavu Sans Mono"))
      (if (member "Hack" (font-family-list))
          (set-frame-font "Hack-10" t t)
        (set-frame-font "DejaVu Sans Mono-10" t t)))

    (my-initial-frame-setup)
    (add-hook 'after-make-frame-functions 'my-default-frame-setup)))


;; Local Variables:
;; indent-tabs-mode: nil
;; time-stamp-pattern: "16/Last Updated:[ \t]+\\\\?[\"<]+%02d %3b. %:y -- %02H:%02M (%Z) by %U\\\\?[\">]"
;; eval: (add-hook 'before-save-hook 'time-stamp nil t)
;; End:
