;;; recentf-ido.el -- Ido completion for finding file through recentf.
;;
;; Author/Modifier:
;;     Name: Maniroth Ouk
;;     Email: maniroth_ouk@outlook.com
;; Last Updated: <14 Jan. 2017 -- 22:27 (Central Standard Time) by Maniroth Ouk>
;; License: MIT
;;
;;; Commentary:
;;
;; modified from "https://www.emacswiki.org/emacs/RecentFiles"
;;
;; All modifications of the original code, my additions, are under the MIT
;; license. Credits for the original code are given to the original author.
;;
;;; Code:

(require 'ido)
(require 'recentf)

(defun recentf-ido-find-file nil
  "Find a recent file using Ido, for files not shown in the regular windows."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(provide 'recentf-ido)

;; Local Variables:
;; indent-tabs-mode: nil
;; time-stamp-pattern: "16/Last Updated:[ \t]+\\\\?[\"<]+%02d %3b. %:y -- %02H:%02M (%Z) by %U\\\\?[\">]"
;; eval: (add-hook 'before-save-hook 'time-stamp nil t)
;; End:
