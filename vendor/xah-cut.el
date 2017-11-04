;;; xah-cut.el -- A better way to cut text in Emacs.
;;
;; Orig. Author:
;;     Name: Xah Lee
;;     Website: xahlee.org
;; Modifier:
;;     Name: Maniroth Ouk
;;     Email: maniroth_ouk@outlook.com
;; Last Updated: <22 Sep. 2017 -- 23:40 (Central Daylight Time) by Maniroth Ouk>
;; License: MIT
;;
;;; Commentary:
;;
;; This file contains a modification of code from
;; "http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html"
;;
;; All additions and changes of the original code listed here are under the MIT
;; license. Credit(s) for the original code, from the web-page listed above,
;; are given to the original author, Xah Lee, and any other contributor(s)
;; listed on the web-paged above. Credit(s) for the original code may not be
;; claimed by or attributed to any such persons not listed, on the web-page of
;; the original code above, as contributors or authors or to any persons not
;; acknowledged as contributor(s) by the author Xah Lee; such persons that may
;; not claim credit for the original code include, but are not limited to,
;; modifier(s) of the original code like myself (Maniroth Ouk) and any
;; modifier(s) of derivatives of the original code like the following file.
;;
;;; Code:

(defun xah-cut-line-or-region nil
  "Cut the current line, or text selection.
When `universal-argument' is called first, cut whole buffer (respects `narrow-to-region').

Originally from URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html' under Version 2015-06-10"
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
