;;; custom.el -- System specific configuration -*- lexical-binding: t; indent-tabs-mode: nil; -*-

(defun my-after-init-config nil
  "Settings that are to be run after loading of the main system-independent config, init.el."
  )
(add-hook 'after-init-hook #'my-after-init-config)
