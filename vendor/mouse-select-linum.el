;;; mouse-select-linum.el -- Select/Highlight lines from the linum margins.
;;
;; Author/Modifier:
;;     Name: Maniroth Ouk
;;     Email: maniroth_ouk@outlook.com
;; Last Updated: <14 Jan. 2017 -- 22:27 (Central Standard Time) by Maniroth Ouk>
;; License: MIT
;;
;;; Commentary:
;;
;; modified from "https://www.emacswiki.org/emacs/LineNumbers#toc11"
;;
;; All modifications of the original code, my additions, are under the MIT
;; license. Credits for the original code are given to the original author.
;;
;; Disclaimer - still a work in progress
;;
;;; Code:

(defvar mouse-down-line-number nil
  "Represent the line at the mouse event.")

(defun line-at-click nil
  (save-excursion
    (let ((click-y (cdr (cdr (mouse-position))))
          (line-move-visual-store line-move-visual))
      (setq line-move-visual t)
      (goto-char (window-start))
      (next-line (1- click-y))
      (setq line-move-visual line-move-visual-store)
      (- (line-number-at-pos) 2))))

(defun mouse-down-select-linum nil
  (interactive)
  (goto-line (line-at-click))
  (set-mark (point))
  (setq mouse-down-line-number (line-number-at-pos)))

(defun mouse-up-select-linum nil
  (interactive)
  (when mouse-down-line-number
    (let (mouse-up-line)
      (setq mouse-up-line (line-at-click))
      (if (> mouse-up-line mouse-down-line-number)
          (progn
            (goto-line mouse-down-line-number)
            (set-mark (point))
            (goto-line mouse-up-line)
            (end-of-line))
        (progn
          (goto-line mouse-down-line-number)
          (set-mark (line-end-position))
          (goto-line mouse-up-line)
          (beginning-of-line)))
      (setq mouse-down-line-number nil))))

(provide 'mouse-select-linum)

;; Local Variables:
;; indent-tabs-mode: nil
;; time-stamp-pattern: "16/Last Updated:[ \t]+\\\\?[\"<]+%02d %3b. %:y -- %02H:%02M (%Z) by %U\\\\?[\">]"
;; eval: (add-hook 'before-save-hook 'time-stamp nil t)
;; End:
