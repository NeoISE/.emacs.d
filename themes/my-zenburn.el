;;; my-zenburn.el -- Changes to the zenburn theme.
;;
;; Orig. Author:
;;     Name: Maniroth Ouk
;;     Email: maniroth_ouk@outlook.com
;; Last Updated: <31 Dec. 2018 -- 22:37 (Central Standard Time) by Maniroth Ouk>
;; License: MIT
;;
;;; Commentary:
;;
;; The credit of the actual zenburn theme for Emacs goes to @bbatsov on Github.
;; I only merely changed the theme to fit my taste.
;;
;;; Code:

(defun my-zenburn-config nil
  (require 'zenburn-theme)
  
  (zenburn-with-color-variables
    (custom-theme-set-faces
     'zenburn
     ;; hi-lock-mode (pink, green and blue needs to be fix)
     `(hi-yellow ((t (:background ,zenburn-bg+3
                                  :foreground ,zenburn-yellow-2
                                  :weight bold))))
     `(hi-pink ((t (:background ,zenburn-bg+3
                                :foreground ,zenburn-fg
                                :weight bold))))
     `(hi-green ((t (:background ,zenburn-bg+3
                                 :foreground ,zenburn-fg
                                 :weight bold))))
     `(hi-blue ((t (:background ,zenburn-bg+3
                                :foreground ,zenburn-fg
                                :weight bold))))

     ;; paren-mode
     `(show-paren-match ((t (:foreground ,zenburn-fg
                                         :background ,zenburn-bg+3
                                         :weight bold))))
     ;; no matter what, an error is red highlight with white text
     '(show-paren-mismatch ((t (:background "#ba5353" :foreground "#e5dada"))))
     ))

  (custom-set-faces
   ;; comments are slanted for emphasis
   '(font-lock-comment-face ((t (:slant italic))))
   '(font-lock-comment-delimiter-face ((t (:slant italic)))))
  )

(provide 'my-zenburn)

;; Local Variables:
;; lexical-binding: t
;; indent-tabs-mode: nil
;; time-stamp-pattern: "16/Last Updated:[ \t]+\\\\?[\"<]+%02d %3b. %:y -- %02H:%02M (%Z) by %U\\\\?[\">]"
;; eval: (add-hook 'before-save-hook 'time-stamp nil t)
;; End:
