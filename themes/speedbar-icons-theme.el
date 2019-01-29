;;; speedbar-icons-theme.el -- Changes the icons on the speedbar -*- lexical-binding: t; -*-
;;
;; Orig. Author:
;;     Name: Maniroth Ouk
;;     Email: maniroth_ouk@outlook.com
;; Last Updated: <28 Jan. 2019 -- 23:02 (Central Standard Time) by Maniroth Ouk>
;; License: MIT
;;
;;; Commentary:
;;
;; After the change to the newer "sleeker" look for the theming, I decided to
;; overhaul the looks of the icons. The redesign was to conform to Google's
;; Material Design suggestions with the colors chosen from their website, "https://material.io/tools/color/".
;; The new colors were chosen to be as close to the original colors of the icons
;; as possible. In addition to changing the colors, some new colors are also
;; used to enrich the icons, using 4-6 colors instead of just using 2-3 colors.
;;
;; Also, the redesign included some new icons.
;;
;;; Original Commentary:
;;
;; I have seen some hating the icons that emacs used for the speedbar, and I do
;; too. Since I do have an active speedbar, thanks to sr-speedbar, I decided to
;; make a set of icons for the speedbar. I'm calling the set `Metallic-Blue'.
;;
;; I am filling in icons as I go deeper into the speedbar, so there are some
;; icons that are not available yet.
;;
;; I am contemplating making the icons available through Emacs' package system.
;;
;;; Code:

(require 'speedbar)

(defgroup speedbar-icons-theme nil
  "My icon theme set for the speedbar."
  :version "25.1"
  :group 'speedbar)

;; Todo: I need to complete a larger icon set, 32x32.
(defcustom sit/use-large-icons nil
  "Use larger icons for the speedbar.
The large icons are 32x32, while small icons are 24x24 pixels."
  :type 'boolean
  :group 'speedbar-icons-theme)

(add-to-list 'image-load-path
             (file-name-as-directory
              (expand-file-name "icons" user-emacs-directory)))

(defimage sit/folder-blank
  ((:type png :file "24x24/folder.png" :ascent center))
  "Image of a folder.")

(defimage sit/folder-plus
  ((:type png :file "24x24/folder-plus.png" :ascent center))
  "Image of a folder that is expandable.")

(defimage sit/folder-minus
  ((:type png :file "24x24/folder-minus.png" :ascent center))
  "Image of a folder that can be collapse.")

(defimage sit/lock
  ((:type png :file "24x24/lock.png" :ascent center))
  "Image of a lock, used for private or locked files.")

(defimage sit/mail
  ((:type png :file "24x24/mail.png" :ascent center))
  "Image of a mail letter.")

(defimage sit/page-empty
  ((:type png :file "24x24/page-empty.png" :ascent center))
  "Image of a blank page/file.")

(defimage sit/page-unknown
  ((:type png :file "24x24/page-unknown.png" :ascent center))
  "Image of a file that is not known or that is unclear.")

(defimage sit/page-selected
  ((:type png :file "24x24/page-selected.png" :ascent center))
  "Image of a file that is highlighted or selected.")

(defimage sit/page-unselected
  ((:type png :file "24x24/page-unselected.png" :ascent center))
  "Image of a file that is not highlighted nor selected.")

(defimage sit/box-open
  ((:type png :file "24x24/box-open.png" :ascent center))
  "Image of a box that is open.")

(defimage sit/box-closed
  ((:type png :file "24x24/box-closed.png" :ascent center))
  "Image of a box that is closed.")

(defimage sit/tag
  ((:type png :file "24x24/tag.png" :ascent center))
  "Image of a basic tag.")

(defimage sit/tag-empty
  ((:type png :file "24x24/tag-empty.png" :ascent center))
  "Image of a basic tag that is empty.")

(defimage sit/tag-selected
  ((:type png :file "24x24/tag-selected.png" :ascent center))
  "Image of a tag that is selected/highlighted.")

(defimage sit/tag-unselected
  ((:type png :file "24x24/tag-unselected.png" :ascent center))
  "Image of a tag that is not selected/highlighted.")

(defimage sit/tag-type
  ((:type png :file "24x24/tag-type.png" :ascent center))
  "Image of a tag that is used to indicate types.")

(defimage sit/checkout
  ((:type png :file "24x24/checkout-cart.png" :ascent center))
  "Image of a cart used to denote checkout.")

(defimage sit/gift-box-open
  ((:type png :file "24x24/gift-box-open.png" :ascent center))
  "Image of a gift box that is open to denote objects which are out of date.")

(defimage sit/gift-box-closed
  ((:type png :file "24x24/gift-box-closed.png" :ascent center))
  "Image of a gift box used to denote objects.")

(setq speedbar-expand-image-button-alist
      '(("<+>" . sit/folder-plus)
        ("<->" . sit/folder-minus)
        ("< >" . sit/folder-blank)
        ("[+]" . sit/page-unselected)
        ("[-]" . sit/page-selected)
        ("[ ]" . sit/page-empty)
        ("[?]" . sit/page-unknown)
        ("{+}" . sit/box-open)
        ("{-}" . sit/box-closed)
        ("<M>" . sit/mail)
        ("<d>" . sit/tag)
        ("<i>" . sit/tag)
        (" =>" . sit/tag-empty)
        (" +>" . sit/tag-unselected)
        (" ->" . sit/tag-selected)
        (">"   . sit/tag)
        ("@"   . sit/tag-type)
        ("  @" . sit/tag-type)
        ("*"   . sit/checkout)
        ("#"   . sit/gift-box-closed)
        ("!"   . sit/gift-box-open)
        ;; ("//"  . ezimage-label)
        ("%"   . sit/lock)))

(provide 'speedbar-icons-theme)

;; Local Variables:
;; indent-tabs-mode: nil
;; time-stamp-pattern: "16/Last Updated:[ \t]+\\\\?[\"<]+%02d %3b. %:y -- %02H:%02M (%Z) by %U\\\\?[\">]"
;; eval: (add-hook 'before-save-hook 'time-stamp nil t)
;; End:
