;;; xah-copy.el -- A better way to copy in Emacs -*- lexical-binding: t; -*-
;;
;; Orig. Author:
;;     Name: Xah Lee
;;     Website: xahlee.org
;; Modifier:
;;     Name: Maniroth Ouk
;;     Email: maniroth_ouk@outlook.com
;; Last Updated: <31 Dec. 2018 -- 22:48 (Central Standard Time) by Maniroth Ouk>
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

Originally from URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html' under Version 2017-07-08"
  (interactive)
  (cond
   (current-prefix-arg
    (progn
      (kill-ring-save (point-min) (point-max))
      (message "All text in visible buffer is copied")))
   ((use-region-p)
    (progn
      (kill-ring-save (region-beginning) (region-end))
      (message "Text in active region copied")))
   ((eq last-command this-command)
    (if (eobp)
        (message "Empty line at end of buffer.")
      (progn
        (kill-append "\n" nil)
        (kill-append (buffer-substring-no-properties (line-beginning-position)
                                                     (line-end-position))
                     nil)
        (message "Line copy appended")
        (progn
          (end-of-line)
          (forward-char)))))
   ((eobp)
    (if (eq (char-before) 10)
        (message "Empty line at the end of buffer.")
      (progn
        (kill-ring-save (line-beginning-position) (line-end-position))
        (end-of-line)
        (message "Line Copied"))))
   (t
    (progn
      (kill-ring-save (line-beginning-position) (line-end-position))
      (end-of-line)
      (forward-char)
      (message "Line Copied")))))

(provide 'xah-copy)

;; Local Variables:
;; indent-tabs-mode: nil
;; time-stamp-pattern: "16/Last Updated:[ \t]+\\\\?[\"<]+%02d %3b. %:y -- %02H:%02M (%Z) by %U\\\\?[\">]"
;; eval: (add-hook 'before-save-hook 'time-stamp nil t)
;; End:
