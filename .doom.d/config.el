(setq doom-font (font-spec :family "Mononoki Nerd Font" :size 15)
       doom-variable-pitch-font (font-spec :family "Mononoki Nerd Font" :size 15))

(setq doom-theme 'doom-palenight)

(setq org-directory "~/Dokumente/org/")

(setq display-line-numbers-type t)

;;
;; My Custom Configurations
;;
(setq-hook! prog-mode which-key-mode t)
(setq-hook! prog-mode global-undo-tree-mode t)
(setq-hook! prog-mode global-company-mode t)

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
(setq-hook! prog-mode select-enable-clipboard t)
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
(global-set-key (kbd "M-<right>")   'evil-window-next)
(global-set-key (kbd "M-S-<right>") 'next-buffer)
(global-set-key (kbd "M-S-<left>")  'previous-buffer)

(global-set-key "\C-x\ t" 'toggle-truncate-lines)

(setq neo-window-fixed-size nil)

(setq browse-url-browser-function 'eww-browse-url)

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

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

(setq md4rd-subs-active '(archlinux commandline DistroTube DoomEmacs emacs freesoftware lbry linux linux4noobs linuxmasterrace linnuxquestions orgmode qutebrowser suckless Ubuntu unixporn UsabilityPorn vim xmonad))

(custom-set-variables
 '(elfeed-feeds
   (quote
    (("https://www.reddit.com/r/linux.rss" reddit linux)
     ("https://opensource.com/feed" opensource linux)
     ("https://linux.softpedia.com/backend.xml" softpedia linux)
     ("https://itsfoss.com/feed/" itsfoss linux)
     ("http://feeds.feedburner.com/d0od" omgubuntu linux)
     ("https://www.computerworld.com/index.rss" computerworld linux)
     ("https://www.networkworld.com/category/linux/index.rss" networkworld linux)
     ("https://www.techrepublic.com/rssfeeds/topic/open-source/" techrepublic linux)
     ("https://betanews.com/feed" betanews linux)
     ("http://lxer.com/module/newswire/headlines.rss" lxer linux)
     ("https://distrowatch.com/news/dwd.xml" distrowatch linux))))
 '(package-selected-packages
   (quote
    (exwm peep-dired nav-flash elfeed))))

(custom-set-faces
 )
