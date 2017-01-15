;;; xah-copy.el -- A better way to copy in Emacs.
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

(defun xah-copy-line-or-region nil
  "Copy current line, or text selection.
When called repeatedly, append copy subsequent lines.
When `universal-argument' is called first, copy whole buffer
(respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2016-06-18"
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
