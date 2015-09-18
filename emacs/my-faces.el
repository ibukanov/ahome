(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(column-number-mode t)
 '(blink-cursor-mode nil)
 '(scroll-bar-mode nil)
 '(menu-bar-mode nil)
)

(when (display-graphic-p)
  (tool-bar-mode -1))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "Liberation Mono"))))
 '(eshell-ls-archive-face ((t (:foreground "Orchid"))))
 '(eshell-ls-clutter-face ((t (:foreground "OrangeRed"))))
 '(eshell-ls-directory-face ((t (:foreground "SkyBlue"))))
 '(eshell-ls-executable-face ((t (:foreground "Green"))))
 '(eshell-ls-missing-face ((t (:foreground "Red"))))
 '(eshell-ls-special-face ((t (:foreground "Magenta"))))
 '(eshell-ls-symlink-face ((t (:foreground "Cyan"))))
 '(eshell-prompt-face ((t (:foreground "Pink")))))
