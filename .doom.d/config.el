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

;; Sets binding to toggle line wrap on/off.
(global-set-key "\C-x\ t" 'toggle-truncate-lines)

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

(require 'emms-setup)
(emms-standard)
(emms-default-players)

;; Setting up mu4e which is an email client that works within emacs.
;; You must install mu4e and mbsync through your Linux distribution's
;; package manager.
(require 'mu4e)
(setq mu4e-get-mail-command "mbsync -c ~/.emacs.d/mu4e/.mbsyncrc -a"
      mu4e-update-interval  300)
      
;; Fields that are auto-filled in email such as the full name of the
;; sender, the salutation and signature.
(setq
   user-mail-address "derek@distrotube.com"
   user-full-name  "Derek Taylor"
   mu4e-compose-signature
    (concat
      "Derek Taylor\n"
      "http://www.youtube.com/DistroTube\n"))

;; Setting up smtp for sending mail. Make sure the gnutls command 
;; line utils are installed. Package 'gnutls-bin' in Debian/Ubuntu, 
;; and 'gnutls' in Arch.
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
   starttls-use-gnutls t
   smtpmail-starttls-credentials '(("smtp.1and1.com" 587 nil nil))
   smtpmail-auth-credentials
     '(("smtp.1and1.com" 587 "derek@distrotube.com" nil))
   smtpmail-default-smtp-server "smtp.1and1.com"
   smtpmail-smtp-server "smtp.1and1.com"
   smtpmail-smtp-service 587)
;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)
