;;; my-prettify-symbols.el -- Contains customization to prettify-symbols-mode.
;;
;; Orig. Author:
;;     Name: Maniroth Ouk
;;     Email: maniroth_ouk@outlook.com
;; Last Updated: <14 Jan. 2017 -- 22:26 (Central Standard Time) by Maniroth Ouk>
;; License: MIT
;;
;;; Code:

(defvar my-math-operator-prettify-alist '()
  "A list of known math operators and how they should be displayed instead.")
(push '("<=" . (?\s (Br . Bl) ?\s (Bc . Bc) ?≤))
      my-math-operator-prettify-alist)
(push '(">=" . (?\s (Br . Bl) ?\s (Bc . Bc) ?≥))
      my-math-operator-prettify-alist)
(push '("==" . (?\s (Br . Bl) ?\s (Bc . Bc) ?≡))
      my-math-operator-prettify-alist)
(push '("!=" . (?\s (Br . Bl) ?\s (Bc . Bc) ?≠))
      my-math-operator-prettify-alist)
(push '("+-" . (?\s (Br . Bl) ?\s (Bc . Bc) ?±))
      my-math-operator-prettify-alist)

(defvar my-arrows-prettify-alist '()
  "A list of common implementations of arrows in text.")
(push '("<-" . (?\s (Br . Bl) ?\s (Bc . Bc) ?←))
      my-arrows-prettify-alist)
(push '("->" . (?\s (Br . Bl) ?\s (Bc . Bc) ?→))
      my-arrows-prettify-alist)
(push '("<->" . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bl . Br) ?↔))
      my-arrows-prettify-alist)

(defvar my-powershell-operator-prettify-alist '()
  "Used to reduce the somewhat enigmatic logical operators to being more like C-style operators.")
(push '("-gt" . ?>)
      my-powershell-operator-prettify-alist)
(push '("-lt" . ?<)
      my-powershell-operator-prettify-alist)
(push '("-le" . ?≤)
      my-powershell-operator-prettify-alist)
(push '("-ge" . ?≥)
      my-powershell-operator-prettify-alist)
(push '("-eq" . ?≡)
      my-powershell-operator-prettify-alist)
(push '("-ne" . ?≠)
      my-powershell-operator-prettify-alist)

;; (defun non-repeat-combine-prettify-alist (list-dest lists-added)
;;   "Used to combine the prettify symbol alists.

;; The first arg, LIST-DEST, represent the lists that is to be added/appended to and once combined, is the returned value.

;; Remaining arguments, LISTS-ADDED, should be a list of con cells that will try to be added to LIST-DEST."
;;   (progn
;;     (dolist (rules lists-added)
;;       (dolist (old list-dest)
;;         (unless (string-equal (car rules)
;;                        (car old))
;;           (setq list-dest (append list-dest
;;                                   rules)))))

;;     (identity list-dest)))

(defun my-prettify-symbols-default-config nil
  (setq lisp--prettify-symbols-alist (append lisp--prettify-symbols-alist
                                             my-math-operator-prettify-alist
                                             my-arrows-prettify-alist))

  (eval-after-load 'cc-mode
    '(add-hook
      'c-mode-common-hook
      (lambda nil
        (setq prettify-symbols-alist (append prettify-symbols-alist
                                             my-math-operator-prettify-alist
                                             my-arrows-prettify-alist)))))

  (eval-after-load 'powershell
    '(add-hook
      'powershell-mode-hook
      (lambda nil
        (setq prettify-symbols-alist
              (append prettify-symbols-alist
                      my-powershell-operator-prettify-alist
                      my-arrows-prettify-alist)))))
  )


(provide 'my-prettify-symbols)

;; Local Variables:
;; indent-tabs-mode: nil
;; time-stamp-pattern: "16/Last Updated:[ \t]+\\\\?[\"<]+%02d %3b. %:y -- %02H:%02M (%Z) by %U\\\\?[\">]"
;; eval: (add-hook 'before-save-hook 'time-stamp nil t)
;; End:
