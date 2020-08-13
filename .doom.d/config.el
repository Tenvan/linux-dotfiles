;;; config.el -*- lexical-binding: t; -*-

;; Personal Data ####
(setq user-full-name "stira"
      user-mail-address "ralf.stich@infoniqa.com")

(setq auth-sources '("~/.authinfo.gpg")
      auth-source-cache-expiry nil) ; default is 7200 (2h)

(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 tab-width 4                                      ; Set width for tabs
 uniquify-buffer-name-style 'forward              ; Uniquify buffer names
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      inhibit-compacting-font-caches t            ; When there are lots of glyphs, keep them in memory
      truncate-string-ellipsis "…")               ; Unicode ellispis are nicer than "...", and also save /precious/ space

(delete-selection-mode 1)                         ; Replace selection when inserting text
(display-time-mode 1)                             ; Enable time in the mode-line
;; (unless (equal "Battery status not available"
;;                (battery))
;;   (display-battery-mode 1))                       ; On laptops it's nice to know how much power you have
(global-subword-mode 1)                           ; Iterate through CamelCase words

(if (eq initial-window-system 'x)                 ; if started by emacs command or desktop file
    (toggle-frame-maximized)
  (toggle-frame-fullscreen))

(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))

(setq evil-vsplit-window-right t
      evil-split-window-below t)

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))

(setq +ivy-buffer-preview t)

(map! :map evil-window-map
      "SPC" #'rotate-layout
       ;; Navigation
       "<left>"     #'evil-window-left
       "<down>"     #'evil-window-down
       "<up>"       #'evil-window-up
       "<right>"    #'evil-window-right
       ;; Swapping windows
       "C-<left>"       #'+evil/window-move-left
       "C-<down>"       #'+evil/window-move-down
       "C-<up>"         #'+evil/window-move-up
       "C-<right>"      #'+evil/window-move-right)

(defun prefer-horizontal-split ()
  (set-variable 'split-height-threshold nil t)
  (set-variable 'split-width-threshold 40 t)) ; make this as low as needed
(add-hook 'markdown-mode-hook 'prefer-horizontal-split)

(setq-default major-mode 'org-mode)

(setq doom-font (font-spec :family "Iosevka Nerd Font" :size 16)
      doom-big-font (font-spec :family "Iosevka Nerd Font" :size 24)
      doom-variable-pitch-font (font-spec :family "Iosevka Nerd Font" :size 18)
      doom-serif-font (font-spec :family "Iosevka Nerd Font" :weight 'light))

(setq doom-theme 'doom-nord)
(delq! t custom-theme-load-path)

(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange"))

(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))

(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(setq display-line-numbers-type t)
;; (setq display-line-numbers-type 'relative)

(setq doom-fallback-buffer-name "► Doom"
      +doom-dashboard-name "► Doom")

(custom-set-faces! '(doom-modeline-evil-insert-state :weight bold :foreground "#339CDB"))

;; (setq neo-window-fixed-size nil)

(setq browse-url-browser-function 'firefox)

(map!
  (:after dired
    (:map dired-mode-map
     "C-x i" #'peep-dired
     )))
(evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file
                                             (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

(setq-hook! prog-mode select-enable-clipboard t)

(map! :n [mouse-8] 'better-jumper-jump-backward
      :n [mouse-9] 'better-jumper-jump-forward)

(setq frame-title-format
    '(""
      (:eval
       (if (s-contains-p org-roam-directory (or buffer-file-name ""))
           (replace-regexp-in-string ".*/[0-9]*-?" "🢔 " buffer-file-name)
         "%b"))
      (:eval
       (let ((project-name (projectile-project-name)))
         (unless (string= "-" project-name)
           (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))

;; -*- no-byte-compile: t; -*-

;; (package! rotate)

;; (package! xkcd)

;; (package! selectric-mode)

;;(package! wttrin :recipe (:local-repo "lisp" :no-byte-compile t))

;; (package! spray)

;; (package! theme-magic)

;; (package! flyspell-lazy :pin "3ebf68cc9e...")

;; (package! magit-delta :recipe (:host github :repo "dandavison/magit-delta") :pin "0c7d8b2359")

(use-package! vlf-setup
  :defer-incrementally vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)

;; (unpin! org)

;; (package! org-super-agenda)

;; (package! doct
;;   :recipe (:host github :repo "progfolio/doct"))

;; (package! org-pretty-table-mode
;;   :recipe (:host github :repo "Fuco1/org-pretty-table"))

;; (package! org-fragtog)

;; (package! org-pretty-tags)

;; (package! ox-gfm :pin "99f93011b0...")

;; (package! org-ref :pin "9a8053f0b0...")

;; (package! org-graph-view :recipe (:host github :repo "alphapapa/org-graph-view") :pin "13314338d7...")

;; (package! org-chef :pin "77f97ad07b...")

;; (package! org-plot :recipe (:local-repo "lisp" :no-byte-compile t))

;; (package! org-roam-server :pin "bfc7032741...")

;; (use-package org-roam-server
;;   :after org-roam
;;   :config
;;   (setq org-roam-server-host "127.0.0.1"
;;         org-roam-server-port 8078
;;         org-roam-server-export-inline-images t
;;         org-roam-server-authenticate nil
;;         org-roam-server-label-truncate t
;;         org-roam-server-label-truncate-length 60
;;         org-roam-server-label-wrap-length 20)
;;   (defun org-roam-server-open ()
;;     "Ensure the server is active, then open the roam graph."
;;     (interactive)
;;     (org-roam-server-mode 1)
;;     (browse-url-xdg-open (format "http://localhost:%d" org-roam-server-port))))

(use-package abbrev
  :init
  (setq-default abbrev-mode t)
  ;; a hook funtion that sets the abbrev-table to org-mode-abbrev-table
  ;; whenever the major mode is a text mode
  (defun tec/set-text-mode-abbrev-table ()
    (if (derived-mode-p 'text-mode)
        (setq local-abbrev-table org-mode-abbrev-table)))
  :commands abbrev-mode
  :hook
  (abbrev-mode . tec/set-text-mode-abbrev-table)
  :config
  (setq abbrev-file-name (expand-file-name "abbrev.el" doom-private-dir))
  (setq save-abbrevs 'silently))

(after! centaur-tabs
  (centaur-tabs-mode t)
  (setq centaur-tabs-height 36
        centaur-tabs-set-icons t
        centaur-tabs-modified-marker "o"
        centaur-tabs-close-button "×"
        centaur-tabs-set-bar 'above)
        centaur-tabs-gray-out-icons 'buffer
  (centaur-tabs-change-fonts "P22 Underground Book" 160))
;; (setq x-underline-at-descent-line t)

(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2)
  (setq company-show-numbers t)
(add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying.

(setq-default history-length 1000)
(setq-default prescient-history-length 1000)

(set-company-backend! '(text-mode
                        markdown-mode
                        gfm-mode)
  '(:seperate company-ispell
              company-files
              company-yasnippet))

(set-company-backend! 'ess-r-mode '(company-R-args company-R-objects company-dabbrev-code :separate))

(setq elcord-use-major-mode-as-main-icon t)

(setq eros-eval-result-prefix "⟹ ")

(after! evil (evil-escape-mode nil))

(after! flyspell (require 'flyspell-lazy) (flyspell-lazy-mode 1))

(use-package! info-colors
  :commands (info-colors-fontify-node))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)

(add-hook 'Info-mode-hook #'mixed-pitch-mode)

(setq ivy-read-action-function 'ivy-hydra-read-action)

(setq ivy-sort-max-size 50000)

(after! magit
  (magit-delta-mode +1))

(use-package! org-chef
  :commands (org-chef-insert-recipe org-chef-get-recipe-from-url))

(setq projectile-ignored-projects '("~/" "/tmp" "~/.emacs.d/.local/straight/repos/"))
(defun projectile-ignored-project-function (filepath)
  "Return t if FILEPATH is within any of `projectile-ignored-projects'"
  (or (mapcar (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects)))

(sp-local-pair
     '(org-mode)
     "<<" ">>"
     :actions '(insert))

(setq spray-wpm 500
      spray-height 700)

(add-hook 'doom-load-theme-hook 'theme-magic-from-emacs)

(setq which-key-idle-delay 0.5) ;; I need the help, I really do
(setq which-key-mode t)

(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))

(after! text-mode
  (add-hook! 'text-mode-hook
    ;; Apply ANSI color codes
    (with-silent-modifications
      (ansi-color-apply-on-region (point-min) (point-max)))))

(setq org-directory "~/Documents/org/")

(setq global-undo-tree-mode t)

;; (use-package! company
;;   :init
;;   (global-company-mode t)
;;   )
;;
;; number pad keys
;; <kp-decimal>
;; <kp-0>
;; <kp-1>
;; <kp-2>
;; <kp-3>
;; <kp-subtract>
;; <kp-divide>
;; <kp-multiply>

;;
;;Edits
;;
;; (define-key evil-visual-state-map (kbd "s-c") (kbd "\"+y"))
;; (define-key evil-insert-state-map  (kbd "s-v") (kbd "^R+"))
;; (define-key evil-ex-completion-map (kbd "s-v") (kbd "^R+"))
;; (define-key evil-ex-search-keymap  (kbd "s-v") (kbd "^R+"))

(global-set-key (kbd "C-S-v")    'undo-tree-visualize)
(global-set-key (kbd "C-S-z")    'undo-tree-undo)
(global-set-key (kbd "C-S-y")    'undo-tree-redo)

"delete"
(global-set-key (kbd "C-S-x")    'evil-delete)
(global-set-key (kbd "C-S-d")    'yank)

"comments"
(global-set-key (kbd "C-<kp-divide>") 'evilnc-comment-or-uncomment-lines)
(global-set-key (kbd "S-<kp-divide>") 'evilnc-comment-or-uncomment-paragraphs)

"Folding"
(global-set-key (kbd "C-S--")    'evil-close-folds)
(global-set-key (kbd "C--")      'evil-close-fold)
(global-set-key (kbd "C-S-+")    'evil-open-folds)
(global-set-key (kbd "C-+")      'evil-open-fold)

"indenting"
(global-set-key (kbd "M-<f8>")    'indent-region)
(global-set-key (kbd "C-<tab>")   'indent-relative)
(global-set-key (kbd "C-S-<tab>") 'indent-region)

"file operations"
(global-set-key (kbd "C-s")       'save-buffer)
(global-set-key (kbd "C-S-l")     'sort-lines)
(global-set-key (kbd "M-<down>")  'drag-stuff-down)
(global-set-key (kbd "M-<up>")    'drag-stuff-up)

"Konfigurations Management"
(global-set-key (kbd "C-S-r")     'doom/reload)

"Tools"
(global-set-key (kbd "<f3>")      'neotree-toggle)
(global-set-key (kbd "C-S-g")     'magit)

"Window Management"
(global-set-key (kbd "M-<left>")    'evil-window-prev)
(global-set-key (kbd "M-<right>")   'evil-window-next)
(global-set-key (kbd "M-S-<right>") 'next-buffer)
(global-set-key (kbd "M-S-<left>")  'previous-buffer)
(global-set-key (kbd "C-S-b")       'helm-buffers-list)

(custom-set-faces
 )
