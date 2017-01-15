;;; recentf-advice.el -- Several advice for recentf functions.
;;
;; Author/Modifier:
;;     Name: Maniroth Ouk
;;     Email: maniroth_ouk@outlook.com
;; Last Updated: <14 Jan. 2017 -- 22:27 (Central Standard Time) by Maniroth Ouk>
;; License: MIT
;;
;;; Commentary:
;;
;; I forgot where I found these.
;;
;; All modifications of the original code, my additions, are under the MIT
;; license. Credits for the original code are given to the original author.
;;
;;; Code:

(require 'recentf)

(defvar my-recentf-list-prev nil
  "Stores the previous recent files list of the current session.")

(defadvice recentf-save-list (around no-message activate)
  "If `recentf-list' and previous recentf-list are equal, do nothing.
Also, suppress output from going to the user."
  (unless (equal recentf-list my-recentf-list-prev)
    (cl-flet ((message (format-string &rest args)
                       (eval `(format ,format-string ,@args)))
              (write-file (file &optional confirm)
                          (let ((str (buffer-string)))
                            (with-temp-file file
                              (insert str)))))
             ad-do-it
             (setq my-recentf-list-prev recentf-list))))

(defadvice recentf-cleanup (around no-message activate)
  "Suppress the output from `message' to minibuffer"
  (cl-flet ((message (format-string &rest args)
                     (eval `(format ,format-string ,@args))))
           ad-do-it))

(provide 'recentf-advice)

;; Local Variables:
;; indent-tabs-mode: nil
;; time-stamp-pattern: "16/Last Updated:[ \t]+\\\\?[\"<]+%02d %3b. %:y -- %02H:%02M (%Z) by %U\\\\?[\">]"
;; eval: (add-hook 'before-save-hook 'time-stamp nil t)
;; End:
