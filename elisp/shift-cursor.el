;;; shift-cursor.el -- Changes cursor based on input method.
;;
;; Orig. Author:
;;     Name: Maniroth Ouk
;;     Email: maniroth_ouk@outlook.com
;; Last Updated: <31 Dec. 2018 -- 22:36 (Central Standard Time) by Maniroth Ouk>
;; License: MIT
;;
;;; Commentary:
;;
;; This was my first minor mode that I created. The purpose was to be able to
;; distinguish between `insert' and `overwrite' in a graphical Emacs. The minor
;; mode is still "rough" and very basic.
;;
;;; Code:

(defgroup shift-cursor nil
  "Shift-Cursor mode options.
`overwrite-mode' has to be toggled again to have changed settings appear."
  :group 'faces)

(defcustom shift-cursor/insert-cursor 'bar
  "The cursor that is used when the user is in `insert-mode'.
Follows the same value choices as `cursor-type', since this is just passed on.

Like `cursor-type', this variable is buffer-local to conform to the standards."
  :type 'symbol
  :group 'shift-cursor)

(defcustom shift-cursor/overwrite-cursor 'box
  "The cursor that is used when the user is in `overwrite-mode'.
Follows the same value choices as `cursor-type', since this is just passed on.

Like `cursor-type', this variable is buffer-local to conform to the standards."
  :type 'symbol
  :group 'shift-cursor)

(defun shift-cursor/maybe-change-cursor nil
  "Possibly change the current cursor, if the right mode is active."
  (if overwrite-mode
      (setq cursor-type shift-cursor/overwrite-cursor)
    (setq cursor-type shift-cursor/insert-cursor)))

(defun shift-cursor/maybe-activate nil
  (unless shift-cursor-mode
    (shift-cursor-mode t)))

;;;###autoload
(define-minor-mode shift-cursor-mode
  "Minor mode that shifts the cursor to represent `insert-mode' or `overwrite-mode'"
  :init nil
  :lighter " ShC"
  :global nil
  :group 'shift-cursor

  ;; buffer-local locking
  (make-local-variable 'shift-cursor/insert-cursor)
  (make-local-variable 'shift-cursor/overwrite-cursor)

  ;; if the mode is called, toggle
  (if shift-cursor-mode
      (progn
        (add-hook 'overwrite-mode-hook 'shift-cursor/maybe-change-cursor)
        (shift-cursor/maybe-change-cursor))
    (remove-hook 'overwrite-mode-hook 'shift-cursor/maybe-change-cursor)
    (setq cursor-type t)))

;;;###autoload
(define-globalized-minor-mode global-shift-cursor-mode
  shift-cursor-mode
  shift-cursor/maybe-activate
  :group 'shift-cursor)

(provide 'shift-cursor)

;; Local Variables:
;; lexical-binding: t
;; indent-tabs-mode: nil
;; time-stamp-pattern: "16/Last Updated:[ \t]+\\\\?[\"<]+%02d %3b. %:y -- %02H:%02M (%Z) by %U\\\\?[\">]"
;; eval: (add-hook 'before-save-hook 'time-stamp nil t)
;; End:
