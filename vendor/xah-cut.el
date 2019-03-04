;;; xah-cut.el -- A better way to cut text in Emacs -*- lexical-binding: t; -*-
;;
;; Orig. Author:
;;     Name: Xah Lee
;;     Website: xahlee.org
;; Modifier:
;;     Name: Maniroth Ouk
;;     Email: maniroth_ouk@outlook.com
;; Last Updated: <04 Mar. 2019 -- 01:16 (Central Standard Time) by Maniroth Ouk>
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

(defun xah-cut-line-or-region (&optional arg)
  "Cut the current line, text selection, some number of contiguous lines, or whole buffer.

When called repeatedly (without `universal-argument'), cut and append the subsequent lines to the first cut line.

When called with a prefix argument using `universal-argument', take the numeric value of the argument and cut the current line and that many lines above and below the current line.
If a value of 0 is passed, then cut the whole buffer (respects `narrow-to-region').

Originally from URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html' under Version 2015-06-10"
  (interactive "P")
  (cond
   (arg                                 ; there is a prefix argument
    (let ((num (abs (prefix-numeric-value arg))))
      (if (= 0 num)
          (progn
            (kill-new (buffer-string))
            (delete-region (point-min) (point-max))
            (message "%s" "Cut the whole buffer"))
        (let ((downstep-left num)
              p-end)
          (save-excursion
            (setq downstep-left (forward-line num))
            ;; now we are either
            ;; 1. at the end of the right line    (copy)
            ;; 2. end of buffer on non-empty line (copy)
            ;; 3. end of buffer on empty line
            ;; 3a. empty line is current line     (copy)
            ;; 3b. is not current line            (do not copy)
            (when (and (eobp)           ; 2 or 3
                       (equal (line-beginning-position) (line-end-position)) ; 3
                       (not (= downstep-left num)))
              ;; 3b
              (forward-line -1))
            (setq p-end (line-end-position)))
          (save-restriction
            (narrow-to-region (line-beginning-position (- 1 num)) p-end)
            (kill-new (buffer-string))
            (delete-region (point-min) (point-max)))
          (message "%s" "Cut the surrounding texts")))))
   ((use-region-p)
    (kill-region (region-beginning) (region-end) t)
    (message "%s" "Cut the active region"))
   (t
    (kill-region (line-beginning-position) (line-beginning-position 2))
    (message "%s" "Cut the line"))))

(provide 'xah-cut)

;; Local Variables:
;; indent-tabs-mode: nil
;; time-stamp-pattern: "16/Last Updated:[ \t]+\\\\?[\"<]+%02d %3b. %:y -- %02H:%02M (%Z) by %U\\\\?[\">]"
;; eval: (add-hook 'before-save-hook 'time-stamp nil t)
;; End:
