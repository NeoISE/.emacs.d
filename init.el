;;; init.el -- My init file for Emacs.
;;
;; Orig. Author:
;;     Name: Maniroth Ouk
;;     Email: maniroth_ouk@outlook.com
;; Last Updated: <14 Jan. 2017 -- 22:01 (Central Standard Time) by Maniroth Ouk>
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
;;  + §0: Version Requirements
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

;; §0: Version Requirements

;; Compare emacs version to make sure everything is able to work
(defconst emacs-major-version-minimum 25
  "The minimum major version of emacs for this configuration.")
(defconst emacs-minor-version-minimum 1
  "The minimum minor version of emacs for this configuration.")
(when (or (< emacs-major-version emacs-major-version-minimum)
          (and (= emacs-major-version emacs-major-version-minimum)
               (< emacs-minor-version emacs-minor-version-minimum)))
  (error "The minimum emacs version that will work with this config is %d.%d"
         emacs-major-version-minimum emacs-minor-version-minimum))



;; §1: Package Management

;; Set up the package sources, makes melpa higher than gnu
(require 'package)
(add-to-list 'package-archives
             `("melpa" . ,(if (gnutls-available-p)
                              "https://melpa.org/packages/"
                            "http://melpa.org/packages/")) t)
(setq package-archive-priorities '(("melpa" . 10)
                                   ("gnu"   . 0)))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; move annoying emacs-made configs to separate file, load later though
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  ;; makes the file if it doesn't exist
  (write-region (concat ";; system-specific configs" "\n") nil custom-file t))

;; Tell emacs about my packages setup.
;; Packages that I want to move with me when I move to a clean system.
;; The variable will be changed again by the current emacs when the load-file
;; for the custom-file occurs to match the actual packages for the system.
(setq package-selected-packages `(
                                  ;; major modes for editing files
                                  csharp-mode
                                  markdown-mode
                                  powershell

                                  ;; minor modes for improved editing
                                  smart-tabs-mode
                                  stickyfunc-enhance
                                  visual-regexp
                                  visual-fill-column
                                  highlight-thing
                                  smart-mark
                                  origami

                                  ;; tab-completions
                                  yasnippet
                                  java-snippets
                                  auto-complete
                                  ,@(unless (eq system-type 'windows-nt)
                                      ;; clang is not on windows setup
                                      '(auto-complete-c-headers
                                        auto-complete-clang-async))

                                  ;; graphic improvements
                                  zenburn-theme
                                  solarized-theme
                                  theme-changer
                                  smart-mode-line
                                  smart-mode-line-powerline-theme
                                  rainbow-mode
                                  modern-cpp-font-lock

                                  ;; Emacs UI improvement
                                  sr-speedbar
                                  popwin
                                  browse-kill-ring
                                  direx
                                  ido-grid-mode
                                  ace-popup-menu
                                  ido-ubiquitous
                                  smex
                                  ranger

                                  ;; Package manager
                                  async
                                  paradox))
(package-install-selected-packages)     ; install if not installed

;; leaves the loading after the installation of needed files
(load custom-file)

(require 'async)
(require 'paradox)

;; load sensitive data, require for paradox token access
(defvar user-sensitive-file
  (expand-file-name "sensitive.el" user-emacs-directory)
  "A file with user-sensitive data like name, email, etc.")
(unless (file-exists-p user-sensitive-file)
  ;; makes the file if it doesn't exist
  (write-region
   (concat ";; User sensitive data" "\n"
           ";; Do not share openly (i.e. over the internet)" "\n\n"
           ";; (require 'paradox)" "\n"
           ";; (setq paradox-github-token \"<token>\")" "\n\n"
           ";; (setq user-full-name \"first last\")" "\n"
           ";; (setq user-mail-address \"your_name@mail.com\")" "\n")
   nil user-sensitive-file t)
  (message "Set up paradox integration asap"))
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
(require 'csharp-mode)
(require 'powershell)
(require 'smart-tabs-mode)
(require 'smex)
(require 'ido-ubiquitous)
(require 'ace-popup-menu)
(require 'popwin)
(require 'direx)
(require 'yasnippet)
(require 'auto-complete)
(require 'auto-complete-config)
(require 'ido-grid-mode)
(require 'smart-mark)
(require 'stickyfunc-enhance)
(require 'origami)
(require 'browse-kill-ring)
(require 'markdown-mode)

;; don't show the startup message
(setq inhibit-startup-message t)

;; set the home directory in windows
(when (eq system-type 'windows-nt)
  (setq-default default-directory (file-name-as-directory
                                   (getenv "UserProfile"))))

;; set up the location services
(setq calendar-location-name "Pasadena, TX"
      calendar-latitude 29.65
      calendar-longitude -95.15)

;; prefer UTF-8
(setq default-buffer-file-coding-system 'utf-8
      locale-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; always syntax highlight
(global-font-lock-mode t)

;; mode line settings
(setq column-number-mode t              ; column number in the mode line
      line-number-mode t                ; line number in the mode line
      size-indication-mode t            ; size in the mode line
      display-time-format "%a, %d %b %Y (%l:%M %p)")
(display-time-mode t)

;; one line at a time, 4 lines before the edge
(setq scroll-step 1
      scroll-margin 4)

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
(global-linum-mode t)

;; force newer files
(setq load-prefer-newer t)

;; resolve symlinks
(setq-default find-file-visit-truename t)

;; requires emacs to place a newline at the end of the file
(setq require-final-newline "visit-save")

;; setup markdown mode
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode) t)
(add-to-list 'auto-mode-alist '("\\.md\\'"       . markdown-mode) t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode) t)

;; single space ends a sentence
(setq sentence-end-double-space nil)

;; activate smex
(smex-initialize)

;; activate origami, code-folding
(global-origami-mode)

;; improves ido
(ido-mode t)
(ido-everywhere t)
(ido-ubiquitous-mode t)
;; turn on grid look for ido-mode
(ido-grid-mode t)

;; activates ace-popup-menu
(ace-popup-menu-mode t)

;; highlight brackets
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; insert pairs
(electric-pair-mode t)

;; access to subwords
(global-subword-mode t)

;; activates popwin-mode
(popwin-mode t)

;; make a direx window to the left
(push '(direx:direx-mode :position left :width 25 :stick t :dedicated t)
      popwin:special-display-config)

;; delete selected content when highlighted
(delete-selection-mode t)

;; browse the kill-ring
(setq browse-kill-ring-highlight-current-entry t
      browse-kill-ring-show-preview t)

;; tells alignment to use only spaces
(defadvice align-regexp (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))

;; putting the function declaration in header-line
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(semantic-mode t)

;; going back to old anchor position
(smart-mark-mode)

;; disallow killing of scratch
(defun prevent-scratch-buffer-kill nil
  (if (equal (buffer-name (current-buffer)) "*scratch*")
      (progn
        (delete-region (point-min) (point-max))
        nil)
    t))
(add-hook 'kill-buffer-query-functions 'prevent-scratch-buffer-kill)



;; §2 §§2: Tabs, Alignment, etc.

(setq-default fill-column 80
              tab-width 4
              tab-stop-list (number-sequence 4 200 4)
              indent-tabs-mode t)

;; no literal tabs for modes
(dolist (hook '(emacs-lisp-mode-hook
                lisp-mode-hook
                lisp-interaction-mode-hook))
  (add-hook hook (lambda nil
                   (setq indent-tabs-mode nil))))

;; make offsets in cc-mode the same as tab-width
(defvaralias 'c-basic-offset 'tab-width)

;; set the style for cc-mode
(setq c-default-style '((java-mode   . "java")
                        (awk-mode    . "awk")
                        (c++-mode    . "bsd")
                        (c-mode      . "bsd")
                        (csharp-mode . "bsd")
                        (other       . "linux")))

;; disable comment-line offsets
(add-hook 'after-init-hook (lambda nil
                             (when (fboundp 'c-set-offset)
                               (c-set-offset 'comment-intro 0))))

;; powershell indent variables
(defvaralias 'powershell-indent 'tab-width)
(defvaralias 'powershell-continuation-indent 'powershell-indent)

;; improve powershell indentation
;; as of 11 Jan. 2017, there was no way to indent-region through powershell.el
(defun powershell-indent-line-or-region nil
  "Supplies the ability to indent over a region if a region is active, otherwise indent the current line."
  (interactive)
  (if (region-active-p)
      (indent-region (region-beginning) (region-end))
    (powershell-indent-line)))

(add-hook 'powershell-mode-hook
          (lambda nil
            (setq fill-column 115)))

;; smart-tabs-mode config
;; ;; add c-sharp to smart-tab-mode
;; (smart-tabs-add-language-support csharp csharp-mode-hook
;;   ((c-indent-line . c-basic-offset)
;;    (c-indent-region . c-basic-offset)))

;; ;; add powershell to smart-tab-mode
;; (smart-tabs-add-language-support powershell powershell-mode-hook
;;   ((powershell-indent-line . powershell-indent)))

;; ;; activate smart-tab-mode
;; (smart-tabs-insinuate 'c 'c++ 'csharp 'java 'javascript 'cperl 'python
;;                       'powershell 'ruby 'nxml)

;; but, for some reason, we have to do it manually to turn on smart-tab-mode
(dolist (mode-indent '(c-mode-common-hook
                       js2-mode-hook
                       cperl-mode-hook
                       python-mode-hook
                       powershell-mode-hook))
  ;; add the minor mode to the major mode
  (add-hook mode-indent 'smart-tabs-mode))

;; add the indentation offset to smart-tabs
(smart-tabs-advice c-indent-line c-basic-offset)
(smart-tabs-advice c-indent-region c-basic-offset)
(smart-tabs-advice js2-indent-line js2-basic-offset)
(smart-tabs-advice cperl-indent-line cperl-indent-level)
(smart-tabs-advice python-indent-line-1 python-indent)
(smart-tabs-advice powershell-indent-line powershell-indent)

;; soft wrapping
(dolist (hook '(text-mode-hook
                prog-mode-hook))
  (add-hook hook 'visual-line-mode))

;; auto-indent pasted code in prog-modes
(defun yank--indent-yanked-region (&rest r)
  "Auto indent the pasted content, for `prog-mode'.
Can be cancelled in an active mode with the universal prefix, C-u."
  (and (not current-prefix-arg)
       (derived-mode-p 'prog-mode)
       (let ((mark-even-if-inactive transient-mark-mode))
         (indent-region (region-beginning) (region-end) nil))))
(dolist (command '(yank
                   yank-pop))
  (advice-add command :after #'yank--indent-yanked-region))



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
                          (file-name-nondirectory ac-comphist-file))
                        ,(regexp-quote
                          (file-name-nondirectory user-sensitive-file))
                        ,(regexp-quote
                          (file-name-nondirectory custom-file))))

;; sets the amount of files to show
(setq recentf-max-menu-items 100)

;; run cleanup during idle time, after 60 seconds
(setq recentf-auto-cleanup 60)

;; set up the advices for recentf
(load "recentf-advice")

;; run recentf mode
(recentf-mode t)

;; run the saving list function every 30 seconds from the point of emacs idle
(run-with-idle-timer 30 t 'recentf-save-list)

;; run the saving list function every 25 minutes just in case though
(run-at-time nil (* 25 60) 'recentf-save-list)



;; §2 §§4: Yasnippet and Auto-Complete

;; yasnippet mode settings
(yas-reload-all)
(add-hook 'prog-mode-hook 'yas-minor-mode)

;;; auto-complete settings
(global-auto-complete-mode t)
(setq ac-auto-start 3
      ac-use-fuzzy t
      ac-show-menu-immediately-on-auto-complete t)
(ac-linum-workaround)
(ac-flyspell-workaround)

;; ac-sources is complicated:
;;  default => "in-buffer", "other same buffers"
;;   prog-mode => "yasnippet", default
;;    c/c++ => "clang-async", "semantic", "c-headers", prog-mode
;;    css => "css-property", prog-mode
;;    emacs-lisp => "symbols", "variables", "functions", "features", prog-mode
(defvar my-default-sources '(ac-source-words-in-buffer
                             ac-source-words-in-same-mode-buffers))
(defvar my-prog-mode-sources (append '(ac-source-yasnippet)
                                     my-default-sources))
(defvar my-c-c++-mode-sources '(ac-source-semantic
                                ac-source-semantic-raw))
(defvar my-css-mode-sources (append '(ac-source-css-property)
                                    my-prog-mode-sources))
(defvar my-emacs-lisp-mode-sources (append '(ac-source-symbols
                                             ac-source-variables
                                             ac-source-functions
                                             ac-source-features)
                                           my-prog-mode-sources))

(setq-default ac-sources my-default-sources)

(add-hook 'text-mode-hook (lambda nil
                            (setq ac-sources my-default-sources)))

(add-hook 'prog-mode-hook (lambda nil
                            (setq ac-sources my-prog-mode-sources)))

;; additional sources for c and c++, no clang on windows
(if (eq system-type 'windows-nt)
    (progn
      (dolist (hook '(c-mode-hook
                      c++-mode-hook))
        (add-hook hook
                  (lambda nil
                    (setq ac-sources (append my-c-c++-mode-sources
                                             my-prog-mode-sources))))))
  (progn
    (require 'auto-complete-clang-async)
    (require 'auto-complete-c-headers)

    (setq my-c-c++-mode-sources (append '(ac-source-clang-async)
                                          my-ac-sources-for-c-c++))
    (add-to-list 'my-ac-sources-for-c-c++ '(ac-source-c-headers) t)
    
    (defun my-auto-completion-c-mode-hook nil
      "Custom sources for C and C++"
      (setq ac-sources (append my-c-c++-mode-sources
                               my-prog-mode-sources))
      (ac-clang-launch-completion-process))
    (add-hook 'c-mode-hook 'my-auto-completion-c-mode-hook)
    (add-hook 'c++-mode-hook 'my-auto-completion-c-mode-hook)

    ;; set the includes directory for ac-source-c-headers
    (setq achead:include-directories
          (append achead:include-directories '("/usr/include/c++/6.2.1")))))


;; additional sources for elisp
(add-hook 'emacs-lisp-mode-hook
          (lambda nil
            (setq ac-sources my-emacs-lisp-mode-sources)))



;; §2 §§5: Spelling

(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"
                          "--run-together"
                          "--run-together-limit=5"
                          "--run-together-min=2")
      ispell-list-command "--list")

;; performance speedup
(setq flyspell-issue-message-flag nil)

;; enable flyspell in text mode
(add-hook 'text-mode-hook 'flyspell-mode)
;; but take it off of change-log and log-edit
(dolist (hook '(change-log-mode-hook
                log-edit-mode-hook))
  (add-hook hook (lambda nil
                   (flyspell-mode -1))))

(defun my-cc-mode-flyspell-prog-mode nil
  "Specifically run `flyspell-mode', but use custom spell-checking verification."
  (interactive)
  (setq flyspell-generic-check-word-p 'my-flyspell-generic-progmode-verify)
  (flyspell-mode t)
  (run-hooks 'flyspell-prog-mode-hook))

(defun my-flyspell-generic-progmode-verify nil
  "My custom spell-checking verification, for use in `my-cc-mode-flyspell-prog-mode'."
  ;; should return non-nil for places that needs to be checked
  (and
   (let ((f (get-text-property (point) 'face)))
     (memq f flyspell-prog-text-faces))
   (not
    (string-match "^#\\([:space:]*\\)include\\b" (thing-at-point 'line t)))))

;; flyspell-prog-mode for prog-mode
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
;; custom c++ helper
(add-hook 'c-mode-common-hook 'my-cc-mode-flyspell-prog-mode)



;; §3: Keybindings

(require 'windmove)
(require 'visual-regexp)
(require 'mouse-select-linum)
(require  'recentf-ido)
(require 'xah-copy)
(require 'xah-cut)

;; most used keys
(global-set-key (kbd "<f7>") 'ranger)
(global-set-key (kbd "<f8>") 'direx:jump-to-directory-other-window)
(global-set-key (kbd "<f9>") 'split-window-below)
(global-set-key (kbd "<f10>") 'split-window-right) ; no need for menu-bar-open
(global-set-key (kbd "<f11>") 'find-file)
(global-set-key (kbd "<f12>") 'ansi-term)

;; paradox
;; easy to remember to apply paradox before action (p before ...)
(global-set-key (kbd "C-c C-p p") 'paradox-list-packages)
(global-set-key (kbd "C-c C-p u") 'paradox-upgrade-packages)

;; windmove
(global-set-key (kbd "C-c w") 'windmove-up)
(global-set-key (kbd "C-c s") 'windmove-down)
(global-set-key (kbd "C-c a") 'windmove-left)
(global-set-key (kbd "C-c d") 'windmove-right)

;; mouse-select line
(global-set-key (kbd "<left-margin> <down-mouse-1>") 'mouse-down-select-linum)
(global-set-key (kbd "<left-margin> <mouse-1>") 'mouse-up-select-linum)
(global-set-key (kbd "<left-margin> <drag-mouse-1>") 'mouse-up-select-linum)

;; powershell
(define-key powershell-mode-map "\t" 'powershell-indent-line-or-region)

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
(global-set-key (kbd "C-c M-r") 'recentf-ido-find-file)

;; smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "<menu>") 'smex)
(global-set-key (kbd "M-S-x") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'execute-extended-command)
(global-set-key (kbd "C-c <menu>") 'execute-extended-command)

;; yasnippet
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-<tab>") 'yas-expand)

;; auto-complete
(define-key ac-completing-map (kbd "<tab>") 'ac-complete)
(define-key ac-completing-map (kbd "TAB") 'ac-complete)
(define-key ac-completing-map (kbd "S-<tab>") 'ac-expand-previous)
(define-key ac-completing-map (kbd "S-TAB") 'ac-expand-previous)
(define-key ac-completing-map "\r" nil)

;; cut, copy, kill-line
(global-set-key (kbd "C-w") 'xah-cut-line-or-region)
(global-set-key (kbd "M-w") 'xah-copy-line-or-region)
(global-set-key (kbd "C-k") 'kill-whole-line) ; not kill to the right

;; browse-kill-ring
(global-set-key (kbd "M-y") 'browse-kill-ring) ; not yank-pop



;; §4: Graphical Settings

;;;; Graphics configuration
(require 'modern-cpp-font-lock)
(require 'hl-line)
(require 'hl-linum)

;; disable tool-bar
(tool-bar-mode -1)

;; allows highlighting the current line number and padded dynamic formatting
(hl-linum/enable)

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
                            (truncate prefer-height) t)
            )

          ;; set up the colorings
          (my-color-and-graphics-setup)
          )
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
               (global-set-key (kbd "<f14> m s") 'set-mark-command)
               )
              
              ((or (eq available-color 'true-color)
                   (eq available-color 'direct-color))
               ;; a large available set of colors, good for themes
               (my-color-and-graphics-setup)
               )
              
              (t
               (message "No support the following color class: %s"
                        available-color))
              ))
      ))
  )

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
          (set-frame-width frm (+ fill-column hl-linum/margin-space))
          )
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
               )
              ))
      ))
  )

(defun my-color-and-graphics-setup nil
  ;; requires
  (require 'visual-fill-column)
  (require 'solarized-light-theme)
  (require 'my-solarized-light)
  (require 'my-zenburn)
  (require 'theme-changer)
  (require 'smart-mode-line)
  (require 'smart-mode-line-powerline-theme)
  (require 'smart-mode-line-light-powerline-theme)
  (require 'highlight-thing)
  (require 'highlight-indent-guides)
  (require 'rainbow-mode)
  (require 'sr-speedbar)
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

  ;; have colorful text
  (add-hook 'prog-mode-hook 'rainbow-mode)
  
  ;; do some margin fiddling with visual-fill-column
  (global-visual-fill-column-mode)
  (setq-default visual-fill-column-fringes-outside-margins nil)
  (defun linum-update--compensate-left-margin (&rest r)
    "When the left margin changes due to line additions or removal, the right margin does not shrink or grow (respectively).
Thus, this advice is created to get the margins spaced correctly."
    (when visual-fill-column-mode
      (unless (equal visual-fill-column-width
                     (+ fill-column
                        hl-linum/margin-space))
        (setq visual-fill-column-width (+ fill-column
                                          hl-linum/margin-space))
        (visual-fill-column--set-margins)

        ;; `visual-fill-column--set-margins' cancels linum; fix left margin
        (let ((curr-right-margins (cdr (window-margins)))
              (window (get-buffer-window (current-buffer))))
          (if curr-right-margins
              (set-window-margins window hl-linum/margin-space curr-right-margins)
            (set-window-margins window hl-linum/margin-space)))
        )
      ))
  (advice-add 'linum-update :after #'linum-update--compensate-left-margin)

  ;; markdown header scaling
  (setq markdown-header-scaling t
        markdown-header-scaling-values '(1.8 1.4 1.2 1.1 1.0 0.9))

  ;; solarized theme variables
  (setq solarized-distinct-fringe-background t
        solarized-height-minus-1 0.9
        solarized-height-plus-1 1.1
        solarized-height-plus-2 1.2
        solarized-height-plus-3 1.4
        solarized-height-plus-4 1.8
        )

  ;; changes the theme from light to dark with time
  (defun change-theme--reload-sml-and-theme-config (orig-funct &rest args)
    (let*
        ((today (theme-changer-sunrise-sunset-times (theme-changer-today)))
         (sunrise-today (first today))
         (sunset-today (second today)))
      (if (theme-changer-daytime-p sunrise-today sunset-today)
          (progn
            ;; turn off previous sml themes
            (disable-theme 'smart-mode-line-light-powerline)
            (disable-theme 'smart-mode-line-powerline)
            ;; run the original function
            (apply orig-funct args)
            ;; turn on sml
            (setq sml/theme 'light-powerline)
            (sml/setup)
            ;; customize the theme
            (my-solarized-light-config)
            (markdown-update-header-faces markdown-header-scaling
                                          markdown-header-scaling-values))
        (progn
          ;; turn off previous sml themes
          (disable-theme 'smart-mode-line-light-powerline)
          (disable-theme 'smart-mode-line-powerline)
          ;; run the original function
          (apply orig-funct args)
          ;; turn on sml
          (setq sml/theme 'powerline)
          (sml/setup)
          ;; customize the theme
          (my-zenburn-config)
          (markdown-update-header-faces markdown-header-scaling
                                        markdown-header-scaling-values))
        )))
  (advice-add 'change-theme :around #'change-theme--reload-sml-and-theme-config)
  (change-theme 'solarized-light 'zenburn)

  ;; increase line space for readability
  (setq-default line-spacing 0.25)

  ;; highlight-thing
  (setq highlight-thing-delay-seconds 1.5
        highlight-thing-case-sensitive-p t)
  (add-hook 'prog-mode-hook 'highlight-thing-mode)

  ;; different cursors to indicate different writing modes
  (global-shift-cursor-mode t)

  ;; declutter symbolic words
  (global-prettify-symbols-mode t)
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  (my-prettify-symbols-default-config)

  ;; sr-speedbar
  (setq speedbar-show-unknown-files t)
  (setq sr-speedbar-skip-other-window-p t
        sr-speedbar-right-side nil)
  (add-hook 'speedbar-mode-hook (lambda nil
                                  (linum-mode -1)))
  (sr-speedbar-open)
  (with-current-buffer sr-speedbar-buffer-name
    (setq window-size-fixed 'width))

  ;; after affecting the speedbar buffer, we move back to the scratch buffer
  (switch-to-buffer "*scratch*")
  )

;; execute the graphical section under different circumstance
;; since daemon and terminal session are somewhat related, the use of daemonp is
;; used to distinguish the two
(if (daemonp)
    (progn
      (dolist (frm-alist '(initial-frame-alist
                           default-frame-alist))
        (if (eq system-type 'windows-nt)
            (add-to-list frm-alist '(font . "Consolas-10"))
          (add-to-list frm-alist '(font . "Hack-10"))))
      
      (add-hook 'after-make-frame-functions 'my-initial-frame-setup))
  ;; when not started as a daemon
  (progn
    ;; fonts are not shared across systems, so we use different fonts
    (if (eq system-type 'windows-nt)
        (set-frame-font "Consolas-10" t t)
      (set-frame-font "Hack-10" t t))
    
    (my-initial-frame-setup)
    (add-hook 'after-make-frame-functions 'my-default-frame-setup)))


;; Local Variables:
;; indent-tabs-mode: nil
;; time-stamp-pattern: "16/Last Updated:[ \t]+\\\\?[\"<]+%02d %3b. %:y -- %02H:%02M (%Z) by %U\\\\?[\">]"
;; eval: (add-hook 'before-save-hook 'time-stamp nil t)
;; End:
