;;; xah-copy.el -- A better way to copy in Emacs.
;;
;; Orig. Author:
;;     Name: Xah Lee
;;     Website: xahlee.org
;; Modifier:
;;     Name: Maniroth Ouk
;;     Email: maniroth_ouk@outlook.com
;; Last Updated: <22 Sep. 2017 -- 23:39 (Central Daylight Time) by Maniroth Ouk>
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

(defun xah-copy-line-or-region nil
  "Copy current line, or text selection.
When called repeatedly, append copy subsequent lines.
When `universal-argument' is called first, copy whole buffer (respects `narrow-to-region').

Originally from URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html' under Version 2016-06-18"
(interactive)
(let (-p1 -p2)
  (if current-prefix-arg
      (setq -p1 (point-min) -p2 (point-max))
    (if (use-region-p)
        (setq -p1 (region-beginning) -p2 (region-end))
      (setq -p1 (line-beginning-position) -p2 (line-end-position))))
  (if (eq last-command this-command)
      (progn
        (progn                     ; Exit if there's no more next line
          (end-of-line)
          (forward-char)
          (backward-char))
        (kill-append "\n" nil)
        (kill-append
         (buffer-substring-no-properties
          (line-beginning-position) (line-end-position)) nil)
        (message "Line copy appended"))
    (progn
      (kill-ring-save -p1 -p2)
      (if current-prefix-arg
          (message "Buffer text copied")
        (message "Text copied"))))
  (end-of-line)
  (forward-char)))

(provide 'xah-copy)

;; Local Variables:
;; indent-tabs-mode: nil
;; time-stamp-pattern: "16/Last Updated:[ \t]+\\\\?[\"<]+%02d %3b. %:y -- %02H:%02M (%Z) by %U\\\\?[\">]"
;; eval: (add-hook 'before-save-hook 'time-stamp nil t)
;; End:
