;;; shift-cursor.el -- Changes cursor based on input method -*- lexical-binding: t; -*-
;;
;; Orig. Author:
;;     Name: Maniroth Ouk
;;     Email: maniroth_ouk@outlook.com
;; Last Updated: <19 Jan. 2019 -- 01:18 (Central Standard Time) by Maniroth Ouk>
;; License: MIT
;;
;;; Commentary:
;; Originally this minor-mode did what most editors do when `overwrite-mode' is active, which is to change the cursor to `bar' when in ``insert mode'' and to `box' when in ``overwrite mode''. Now, this minor-mode has been abstracted. This abstraction will allow one to change the cursor when user specific predicates (conditions) are satistied.
;;
;;; Original Commentary:
;; This was my first minor mode that I created. The purpose was to be able to
;; distinguish between `insert' and `overwrite' in a graphical Emacs. The minor
;; mode is still "rough" and very basic.
;;
;;; Code:

(defgroup shift-cursor nil
  "Changes the cursor when certain conditions are met."
  :version "25.1"
  :group 'cursor)

(defcustom shift-cursor/insert-cursor 'bar
  "The cursor that is used when the user is in `insert-mode'.
Follows the same value choices as `cursor-type', since this is just passed on.

Like `cursor-type', this variable is buffer-local to conform to the standards.

This customization variable is obsolete and one should customize the variable `shift-cursor/predicate-cursor-alist' to customize the `shift-cursor' minor-mode."
  :type 'symbol
  :group 'shift-cursor)

(defcustom shift-cursor/overwrite-cursor 'box
  "The cursor that is used when the user is in `overwrite-mode'.
Follows the same value choices as `cursor-type', since this is just passed on.

Like `cursor-type', this variable is buffer-local to conform to the standards.

This customization variable is obsolete and one should customize the variable `shift-cursor/predicate-cursor-alist' to customize the `shift-cursor' minor-mode."
  :type 'symbol
  :group 'shift-cursor)

(defcustom shift-cursor/predicate-cursor-alist '((overwrite-mode . box))
  "This association list variable controls what cursor to be used when `shift-cursor-mode', or the global variant `global-shift-cursor-mode', is active.

Each value of this association list is of the form (PREDICATE . CURSOR-TYPE), PREDICATE is a condition that is tested and must be non-nil before CURSOR-TYPE is selected as the cursor type.
If PREDICATE evaluates to nil, then the next cons cell is analyzed until the a PREDICATE evaluates to a non-nil value or all cons cells are exhausted without a single non-nil PREDICATE.
If the association list is exhausted without a non-nil PREDICATE, then the value of `shift-cursor/default-cursor' is used instead.

The CURSOR-TYPE part of the cons cells should be valid value for the variable `cursor-type'."
  :type 'alist
  :version "26.1"
  :group 'shift-cursor)

(defcustom shift-cursor/default-cursor 'bar
  "This is the cursor to use if the association list `shift-cursor/predicate-cursor-alist' does not produce a cursor.

The value of this variable must also be a valid value for the variable `cursor-type'."
  :type '(radio (const :tag "Frame Default" :value t)
                (const :tag "None" :value nil)
                (const :tag "Filled Box" :value box)
                (const :tag "Hollow Box" :value hollow)
                (const :tag "Vertical Bar" :value bar)
                (cons :tag "Vertical Bar with custom width"
                      (const bar) (integer :tag "Width"))
                (const :tag "Horizontal Bar" :value hbar)
                (cons :tag "Horizontal Bar with custom height"
                      (const hbar) (integer :tag "Height"))
                (other :tag "Anything Else" foo))
  :version "26.1"
  :group 'shift-cursor)

(make-obsolete-variable 'shift-cursor/insert-cursor
                        'shift-cursor/predicate-cursor-alist
                        "26.1")
(make-obsolete-variable 'shift-cursor/overwrite-cursor
                        'shift-cursor/predicate-cursor-alist
                        "26.1")

(make-variable-buffer-local 'shift-cursor/insert-cursor)
(make-variable-buffer-local 'shift-cursor/overwrite-cursor)
(make-variable-buffer-local 'shift-cursor/predicate-cursor-alist)
(make-variable-buffer-local 'shift-cursor/default-cursor)

;; private variables
(defvar-local shift-cursor/old-cursor-type nil
  "Holds the value for `cursor-type' when `shift-cursor-mode' is active.")

(defun shift-cursor/maybe-change-cursor nil
  "Possibly change the current cursor, if the right mode is active."
  (let ((cursor-alist (append shift-cursor/predicate-cursor-alist
                              `((t . ,shift-cursor/default-cursor))))
        chosen-cursor)
    (while (and (not chosen-cursor)
                cursor-alist)
      (let ((-cons-pred-cursor (car cursor-alist)))
        (setq cursor-alist (cdr cursor-alist))
        (when (eval (car -cons-pred-cursor))
          (setq chosen-cursor `(,(cdr -cons-pred-cursor))))))
    (setq cursor-type (car chosen-cursor))))

(defun shift-cursor/maybe-change-cursor-legacy nil
  "Possibly change the current cursor, if the right mode is active.

This is the legacy method of `shift-cursor-mode': we are only updating the cursor to type `shift-cursor/overwrite-cursor' when `overwrite-mode' is active, otherwise, the cursor is of type `shift-cursor/insert-cursor'.

There are still some issues with the abstraction of `shift-cursor-mode', so this legacy method is preferred while the abstraction of the minor mode is worked on."
  (if overwrite-mode
      (setq cursor-type shift-cursor/overwrite-cursor)
    (setq cursor-type shift-cursor/insert-cursor)))

;;;###autoload
(define-minor-mode shift-cursor-mode
  "Minor mode that shifts the cursor of the current buffer under certain conditions.

The conditions (predicates) and cursors that would activate with those predicates are configured in `shift-cursor/predicate-cursor-alist'."
  :init nil
  :lighter " SC"
  :global nil
  :group 'shift-cursor

  ;; if the mode is called, toggle
  (if shift-cursor-mode
      (progn
        (when shift-cursor-legacy-mode
          (shift-cursor-legacy-mode -1))

        ;; save it to be restored
        (setq shift-cursor/old-cursor-type cursor-type)
        (add-hook 'post-command-hook #'shift-cursor/maybe-change-cursor nil t)
        (shift-cursor/maybe-change-cursor))
    (remove-hook 'post-command-hook #'shift-cursor/maybe-change-cursor t)
    ;; restore the old cursor
    (setq cursor-type shift-cursor/old-cursor-type)))

;;;###autoload
(define-minor-mode shift-cursor-legacy-mode
  "This is the legacy method of `shift-cursor-mode': we are only updating the cursor to type `shift-cursor/overwrite-cursor' when `overwrite-mode' is active, otherwise, the cursor is of type `shift-cursor/insert-cursor'.

There are still some issues with the abstraction of `shift-cursor-mode', so this legacy method is preferred while the abstraction of the minor mode is worked on."
  :init nil
  :lighter " lSC"
  :global nil
  :group 'shift-cursor

  (if shift-cursor-legacy-mode
      (progn
        (when shift-cursor-mode
          (shift-cursor-mode -1))

        (setq shift-cursor/old-cursor-type cursor-type)
        (add-hook 'overwrite-mode-hook #'shift-cursor/maybe-change-cursor-legacy)
        (shift-cursor/maybe-change-cursor))
    (remove-hook 'overwrite-mode-hook #'shift-cursor/maybe-change-cursor-legacy)
    (setq cursor-type shift-cursor/old-cursor-type)))

(defun shift-cursor/maybe-activate nil
  (unless shift-cursor-mode
    (shift-cursor-mode t)))

(defun shift-cursor/maybe-activate-legacy nil
  (unless shift-cursor-legacy-mode
    (shift-cursor-legacy-mode t)))

;;;###autoload
(define-globalized-minor-mode global-shift-cursor-mode
  shift-cursor-mode
  shift-cursor/maybe-activate
  :group 'shift-cursor)

;;;###autoload
(define-globalized-minor-mode global-shift-cursor-legacy-mode
  shift-cursor-legacy-mode
  shift-cursor/maybe-activate-legacy
  :group 'shift-cursor)

(provide 'shift-cursor)

;; Local Variables:
;; indent-tabs-mode: nil
;; time-stamp-pattern: "16/Last Updated:[ \t]+\\\\?[\"<]+%02d %3b. %:y -- %02H:%02M (%Z) by %U\\\\?[\">]"
;; eval: (add-hook 'before-save-hook 'time-stamp nil t)
;; End:
