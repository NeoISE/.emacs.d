;;; xah-cut.el -- A better way to cut text in Emacs.
;;
;; Author/Modifier:
;;     Name: Maniroth Ouk
;;     Email: maniroth_ouk@outlook.com
;; Last Updated: <14 Jan. 2017 -- 22:27 (Central Standard Time) by Maniroth Ouk>
;; License: MIT
;;
;;; Commentary:
;;
;; modified from "http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html"
;;
;; All modifications of the original code, my additions, are under the MIT
;; license. Credits for the original code are given to the original author.
;;
;;; Code:

(defun xah-cut-line-or-region nil
  "Cut the current line, or text selection.
When `universal-argument' is called first,
cut whole buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2015-06-10"
  (interactive)
  (if current-prefix-arg
      (progn ; not kill-region because we don't want to include previous kills
        (kill-new (buffer-string))
        (delete-region (point-min) (point-max)))
    (progn (if (use-region-p)
               (kill-region (region-beginning) (region-end) t)
             (kill-region
              (line-beginning-position) (line-beginning-position 2))))))

(provide 'xah-cut)

;; Local Variables:
;; indent-tabs-mode: nil
;; time-stamp-pattern: "16/Last Updated:[ \t]+\\\\?[\"<]+%02d %3b. %:y -- %02H:%02M (%Z) by %U\\\\?[\">]"
;; eval: (add-hook 'before-save-hook 'time-stamp nil t)
;; End:
