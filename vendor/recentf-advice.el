;;; recentf-advice.el -- Several advice for recentf functions.
;;
;; Author/Modifier:
;;     Name: Maniroth Ouk
;;     Email: maniroth_ouk@outlook.com
;; Last Updated: <31 Dec. 2018 -- 22:35 (Central Standard Time) by Maniroth Ouk>
;; License: MIT
;;
;;; Commentary:
;;
;; Rewriting of the original defadvice codes from unknown origins.
;;
;; Contains some advises that concern recentf calls.
;;
;;; Code:

(require 'recentf)
(require 'cl-lib)

(defvar recentf-advice-previous-recentf-list nil
  "Stores the recent files list from the last modification of the current session.")

(defun recentf-advice--suppress-list-writes (orig-fun &rest args)
  "Suppresses excessive writes to disk of the recentf list.

Works by comparing the list from the last change to the current list stored in `recentf-list'."
  (if (equal recentf-list
             recentf-advice-previous-recentf-list)
      (message "%s" "recentf mode: stopped an unnecessary flush to file.")
    (setq recentf-advice-previous-recentf-list recentf-list)
    (apply orig-fun args)
    (message "%s" "recentf mode: wrote recentf list to file.")))

(defun recentf-advice--suppress-messages (orig-fun &rest args)
  "Suppresses messages from `orig-fun', which can clog up message logs quickly, especially for `recentf-cleanup' if many files are removed from the list at once."
  (cl-flet ((message (format-string &rest margs)
                     nil))
    (apply orig-fun args)))


(provide 'recentf-advice)

;; Local Variables:
;; lexical-binding: t
;; indent-tabs-mode: nil
;; time-stamp-pattern: "16/Last Updated:[ \t]+\\\\?[\"<]+%02d %3b. %:y -- %02H:%02M (%Z) by %U\\\\?[\">]"
;; eval: (add-hook 'before-save-hook 'time-stamp nil t)
;; End:
