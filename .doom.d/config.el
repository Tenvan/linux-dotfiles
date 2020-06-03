;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here!
;;  ____ _____ 
;; |  _ \_   _|  Derek Taylor (DistroTube)
;; | | | || |      http://www.youtube.com/c/DistroTube
;; | |_| || |      http://www.gitlab.com/dwt1/
;; |____/ |_|
;;        
;; A customized config for Doom Emacs (https://github.com/hlissner/doom-emacs)     
;; Modified by Derek Taylor (http://www.gitlab.com/dwt1/)

;; Setting the font.
(setq doom-font (font-spec :family "Mononoki Nerd Font Mono" :size 15))
;;(setq doom-font (font-spec :family "SauceCodePro Nerd Font" :size 15))

;; Setting the theme
(setq doom-theme 'doom-palenight)

;; Setting the neotree width to be adjustable.
(setq neo-window-fixed-size nil)

;; Sets transparency for focuses and unfocused frames.
;; (set-frame-parameter (selected-frame) 'alpha '(95 . 80))
;; (add-to-list 'default-frame-alist '(alpha . (95 . 80)))

;; Setting the indent guides to show a pipe character.
;; (def-package! highlight-indent-guides
;;   :commands highlight-indent-guides-mode
;;   :hook (prog-mode . highlight-indent-guides-mode)
;;   :config
;;   (setq highlight-indent-guides-method 'character
;;         highlight-indent-guides-character ?\|
;;         highlight-indent-guides-delay 0.01
;;         highlight-indent-guides-responsive 'top
;;         highlight-indent-guides-auto-enabled nil))

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)
