(setq doom-font (font-spec :family "Iosevka Nerd Font" :size 15)
      doom-variable-pitch-font (font-spec :family "Iosevka Nerd Font" :size 15))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-nord t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(setq org-directory "~/Documents/org/")

(setq display-line-numbers-type t)
(global-set-key "\C-x\ t" 'toggle-truncate-lines)

(setq neo-window-fixed-size nil)

(setq browse-url-browser-function 'firefox)

(defun prefer-horizontal-split ()
  (set-variable 'split-height-threshold nil t)
  (set-variable 'split-width-threshold 40 t)) ; make this as low as needed
(add-hook 'markdown-mode-hook 'prefer-horizontal-split)

(map!
  (:after dired
    (:map dired-mode-map
     "C-x i" #'peep-dired
     )))
(evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file
                                             (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

(setq-hook! prog-mode select-enable-clipboard t)

(setq which-key-mode t)
(setq global-undo-tree-mode t)

(use-package! company
  :init
  (global-company-mode t)
  )
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

;; undo/redo mode and keys

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
