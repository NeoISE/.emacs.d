;;; hl-linum.el -- Highlights the current line number in the margins.
;;
;; Author/Modifier:
;;     Name: Maniroth Ouk
;;     Email: maniroth_ouk@outlook.com
;; Last Updated: <14 Jan. 2017 -- 22:26 (Central Standard Time) by Maniroth Ouk>
;; License: MIT
;;
;;; Commentary:
;;
;; modified from an answer at
;; "http://stackoverflow.com/questions/10591334/colorize-current-line-number"
;;
;; All modifications of the original code, my additions, are under the MIT
;; license. Credits for the original code are given to the original author.
;;
;;; Code:

(require 'hl-line)

;;; Faces
;; for highlighting the current line number in the margin
(defface hl-linum
  `((t :inherit linum
       :background ,(face-background 'hl-line nil t)
       :weight bold))
  "Face for the current line number, in the margin."
  :group 'linum)

;;; Variables
(defvar hl-linum/format-string " %3d "
  "The special string that formats the line number in the margin.")
(defvar hl-linum/margin-space 5
  "The total space, in characters, that the `linum-format' has taken up.")
(defvar hl-linum/current-line-number 0
  "The number of the current line of the cursor.")

;;; Functions
(defun hl-linum/make-format-string nil
  "A function that dynamically finds the maximum lines to be used in `linum-format'."
  ;; width is strictly the length of the line number string plus left space
  ;; so 1 is added to be the space after
  (let* ((width (1+ (length (number-to-string
                             (count-lines (point-min) (point-max))))))
         (format-string (concat "%" (number-to-string width) "d ")))
    (setq hl-linum/format-string format-string)
    (setq hl-linum/margin-space (1+ width))))
(defun hl-linum/format (line-number)
  "Set up the linum properties."
  (propertize (format hl-linum/format-string line-number) 'face
              (if (and (eq line-number hl-linum/current-line-number)
                       (or hl-line-mode
                           global-hl-line-mode))
                  'hl-linum
                'linum)))
(defun linum-update--current-linum-update (original-funct &rest args)
  (let ((hl-linum/current-line-number (line-number-at-pos)))
    (apply original-funct args)))

;;;###autoload
(defun hl-linum/enable nil
  (interactive)
  (add-hook 'linum-before-numbering-hook 'hl-linum/make-format-string)
  (setq linum-format 'hl-linum/format)
  (advice-add 'linum-update :around #'linum-update--current-linum-update))

;;;###autoload
(defun hl-linum/disable nil
  (interactive)
  (setq linum-format 'dynamic)
  (remove-hook 'linum-before-numbering-hook 'hl-linum/make-format-string)
  (advice-remove 'linum-update #'linum-update--current-linum-update))

(provide 'hl-linum)

;; Local Variables:
;; indent-tabs-mode: nil
;; time-stamp-pattern: "16/Last Updated:[ \t]+\\\\?[\"<]+%02d %3b. %:y -- %02H:%02M (%Z) by %U\\\\?[\">]"
;; eval: (add-hook 'before-save-hook 'time-stamp nil t)
;; End:
