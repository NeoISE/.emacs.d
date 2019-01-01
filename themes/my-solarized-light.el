;;; my-solarized-light.el -- Changes to the solarized-light theme -*- lexical-binding: t; -*-
;;
;; Orig. Author:
;;     Name: Maniroth Ouk
;;     Email: maniroth_ouk@outlook.com
;; Last Updated: <31 Dec. 2018 -- 22:51 (Central Standard Time) by Maniroth Ouk>
;; License: MIT
;;
;;; Commentary:
;;
;; The credit of the actual solarized theme for Emacs goes to @bbatsov on
;; Github. I only merely changed the theme to fit my taste.
;;
;;; Code:

(defun my-solarized-light-config nil
  (require 'solarized-light-theme)
  
  (solarized-with-color-variables
   'light
   (custom-theme-set-faces
    'solarized-light
    ;; linum-mode
    `(linum ((t (:background ,base03))))

    ;; show-paren-mode
    `(show-paren-match ((t (:weight bold))))
    ;; no matter what, an error is red highlight with white text
    '(show-paren-mismatch ((t (:background "#ba5353" :foreground "#e5dada"))))
    ))

  (custom-set-faces
   ;; comments are slanted for emphasis
   '(font-lock-comment-face ((t (:slant italic))))
   '(font-lock-comment-delimiter-face ((t (:slant italic)))))
  )

(provide 'my-solarized-light)

;; Local Variables:
;; indent-tabs-mode: nil
;; time-stamp-pattern: "16/Last Updated:[ \t]+\\\\?[\"<]+%02d %3b. %:y -- %02H:%02M (%Z) by %U\\\\?[\">]"
;; eval: (add-hook 'before-save-hook 'time-stamp nil t)
;; End:
