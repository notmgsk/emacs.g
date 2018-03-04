;;; init.el --- user-init-file                    -*- lexical-binding: t -*-
;;; Early birds
(progn ;     startup
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `user-init-file'.")
  (message "Loading Emacs...done (%.3fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time)))
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (message "Loading %s..." user-init-file)
  (setq package-enable-at-startup nil)
  ;; (package-initialize)
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message "locutus")
  (setq initial-buffer-choice t)
  (setq initial-scratch-message "")
  (setq load-prefer-newer t)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0))

(progn ;    `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require  'borg)
  (borg-initialize))

(progn ;    `use-package'
  (require  'use-package)
  (setq use-package-verbose t))

(eval-and-compile
  (mapc #'(lambda (entry)
            (define-prefix-command (cdr entry))
            (bind-key (car entry) (cdr entry)))
        '(("<C-m>" . my-ctrl-m-map)

          ("C-h e" . my-ctrl-h-e-map)

          ("C-c e" . my-ctrl-c-e-map)
          ("C-c m" . my-ctrl-c-m-map)
          ("C-c y" . my-ctrl-c-y-map)

          ("C-."   . my-ctrl-dot-map)
          ("C-. =" . my-ctrl-dot-equals-map)
          ("C-. f" . my-ctrl-dot-f-map)
          ("C-. g" . my-ctrl-dot-g-map)
          ("C-. h" . my-ctrl-dot-h-map)
          ("C-. m" . my-ctrl-dot-m-map)
          ("C-. r" . my-ctrl-dot-r-map))))

(use-package diminish)

(use-package general
  :demand
  :load-path "site-lisp/general.el"
  :bind ("M-m"))

(use-package subr-x
  :config
  (put 'if-let   'byte-obsolete-info nil)
  (put 'when-let 'byte-obsolete-info nil))

(use-package auto-compile
  :demand t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  (setq auto-compile-display-buffer               nil)
  (setq auto-compile-mode-line-counter            t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest   t)
  (setq auto-compile-update-autoloads             t)
  (add-hook 'auto-compile-inhibit-compile-hook
            'auto-compile-inhibit-compile-detached-git-head))

(use-package ace-window
  :general
  ("M-m jw" #'ace-window)
  ("M-m kw" #'ace-delete-window))

(use-package avy
  :after ivy
  :general
  ("M-m jc" #'avy-goto-char)
  :config
  (avy-setup-default))

;; (use-package auctex)

(use-package epkg
  :defer t
  :init (setq epkg-repository
              (expand-file-name "var/epkgs/" user-emacs-directory)))

(use-package counsel
  :after ivy
  :demand t
  :diminish
  ;; :custom (counsel-find-file-ignore-regexp
  ;;          (concat "\\(\\`\\.\\|"
  ;;                  (regexp-opt completion-ignored-extensions)
  ;;                  "\\)"))
  :bind (("C-*"     . counsel-org-agenda-headlines)
         ("C-x C-f" . counsel-find-file)
         ("M-x"     . counsel-M-x))
  :commands (counsel-minibuffer-history
             counsel-find-library
             counsel-unicode-char)
  :init
  (bind-key "M-r" #'counsel-minibuffer-history minibuffer-local-map)
  :general
  ("M-m ry" #'counsel-yank-pop)
  :config
  (add-to-list 'ivy-sort-matches-functions-alist
	       '(counsel-find-file . ivy--sort-files-by-date)))

(use-package custom
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package elpy
  :hook (python-mode . electric-pair-mode)
  :bind (:map python-mode-map
              ("M-RET Va" . pyvenv-activate))
  :config
  (elpy-enable)
  (elpy-use-ipython))

(use-package helpful
  :bind (("C-h f"	. helpful-function)
         ("C-h C"	. helpful-command)
         ("C-h M"	. helpful-macro)
         ("C-h L"	. helpful-callable)
         ("C-h ."	. helpful-at-point)
         ("C-h v"	. helpful-variable)))

(use-package hl-todo)

(use-package ivy
  :demand
  :diminish

  :custom
  (ivy-dynamic-exhibit-delay-ms 200)
  (ivy-height 14)
  (ivy-initial-inputs-alist nil t)
  (ivy-magic-tilde nil)
  (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (ivy-use-virtual-buffers t)
  (ivy-wrap t)

  :config
  (ivy-mode 1)
  (ivy-set-occur 'ivy-switch-buffer 'ivy-switch-buffer-occur)
  (setq ivy-use-virtual-buffers t
	ivy-count-format "%d/%d "))

(use-package nav-flash
  :after swiper
  :config
  (advice-add #'swiper :after #'nav-flash-show))

(use-package pdf-tools
  :demand
  :config
  (pdf-tools-install))

(use-package server
  :config (or (server-running-p) (server-mode)))

(use-package slime
  :commands (slime slime-mode)
  :init
  (progn
    (setq slime-contribs '(slime-fancy
                           slime-indentation
                           slime-sbcl-exts
                           slime-scratch
                           slime-company)
          inferior-lisp-program "sbcl"
          slime-complete-symbol*-fancy t
          slime-complete-symbol-function 'slime-fuzzy-complete-symbol
          common-lisp-hyperspec-root "http://www.lispworks.com/reference/HyperSpec/"))
  :config
  (slime-setup))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper))
  :bind (:map swiper-map
              ("M-y" . yank)
              ("M-%" . swiper-query-replace)
              ("M-h" . swiper-avy)
              ("M-c" . swiper-mc))
  :bind (:map isearch-mode-map
              ("C-." . swiper-from-isearch))
  :config
  (advice-add #'swiper :after #'nav-flash-show))

(progn ;     startup
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time))))

;;; Long tail

(use-package dash
  :config (dash-enable-font-lock))

(use-package diff-hl
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t))

(use-package dired
  :defer t
  :config (setq dired-listing-switches "-alh"))

(use-package eldoc
  :config (global-eldoc-mode))

(use-package help
  :defer t
  :config (temp-buffer-resize-mode))

(progn ;    `isearch'
  (setq isearch-allow-scroll t))

;; (use-package lisp-mode
;;   :config
;;   (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
;;   (add-hook 'emacs-lisp-mode-hook 'reveal-mode)
;;   (defun indent-spaces-mode ()
;;     (setq indent-tabs-mode nil))
;;   (add-hook 'lisp-interaction-mode-hook #'indent-spaces-mode))

(use-package magit
  :defer t
  ;; :load-path ("site-lisp/magit/lisp")
  :commands (magit-clone)
  :general
  ("M-m gs" #'magit-status)
  :config
  (setq-default magit-repository-directories
                '(("~/hackery/" . 1)))
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))

(use-package multi-magit
  ;; :load-path "site-lisp/multi-magit"
  :after magit
  :commands (multi-magit-list-repositories
	     multi-magit-status))

(use-package multiple-cursors
  :defer t
  :general
  ("C->" #'mc/mark-next-like-this)
  ("C-<" #'mc/mark-previous-like-this)
  ("C-c C->" #'mc/mark-all-like-this)
  ("C-c (" #'mc/mark-all-in-region))

(use-package man
  :defer t
  :config (setq Man-width 80))

(use-package org
  :general
  ("C-c c" #'org-capture)
  ("M-m on" (lambda ()
              (interactive)
              (find-file org-default-notes-file)))
  :bind (:map org-mode-map
	      ("C-_" . undo)
	      ("<C-M-return>" . org-insert-item)
              ("C-k" . my/org-kill-dwim))
  :preface
  (defun my/org-kill-dwim ()
    (interactive)
    (cond
     ((and (outline-on-heading-p t)
           (= (point) (org-entry-beginning-position)))
      (org-cut-subtree))
     (t
      (org-kill-line))))
  :hook (org-mode . auto-fill-mode)
  :hook (org-mode . (lambda () (setq fill-column 80)))
  :config
  (require 'org-mu4e)
  (setq org-directory "~/hackery/org")
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-capture-templates
        `(("t" "todo" entry (file+headline ,org-default-notes-file "Tasks")
           "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n"))
        ;; '(("t" "Brief todo" entry (file+headline org-default-notes-file "Tasks")
        ;;    "* TODO %?\n  %i\n")
        ;;   ("T" "Todo" entry (file+headline org-default-notes-file "Tasks")
        ;;    "* TODO %?\n %i\n %a"))
        )
  (defun compress-org-link (arg)
    (interactive "P")
    (let ((url (thing-at-point 'url))
          (bounds (bounds-of-thing-at-point 'url)))
      (kill-region (car bounds) (cdr bounds))
      (insert (format "[[%s][%s]]"
                      url
                      (truncate-string-to-width url (if arg (prefix-numeric-value arg) fill-column) nil nil "..."))))))

(use-package mu4e
  :defer t
  :init
  (setq mail-user-agent 'mu4e-user-agent)
  :commands (mu4e)
  :config
  (require 'mu4e-contrib)
  (setq mu4e-completing-read-function 'completing-read)
  (setq mu4e-compose-dont-reply-to-self t)
  (setq mu4e-compose-signature-auto-include nil)
  (setq mu4e-maildir "~/Mail"
        mu4e-trash-folder "/Trash"
        mu4e-refile-folder "/Archive"
        mu4e-get-mail-command "offlineimap"
        mu4e-update-interval 300
        mu4e-compose-signature-auto-include nil
        mu4e-view-show-images t
        mu4e-view-show-addresses t
        mu4e-view-prefer-html t
        mu4e-html2text-command 'mu4e-shr2text
        shr-color-visible-luminance-min 60
        shr-color-visible-distance-min 5
        shr-width 80
        )
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "Unread/flagged messages"
                :query "flag:unread AND NOT flag:trashed OR flag:flagged"
                :key ?u))
  ;; Stops shr using funky colours that make things unreadable.
  (advice-add #'shr-colorize-region :around (defun shr-no-colourise-region (&rest ignore)))
  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

  (setq org-mu4e-link-query-in-headers-mode nil)
  (setq mu4e-sent-folder "/sent"
        mu4e-drafts-folder "/drafts"
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        message-send-mail-function 'smtpmail-send-it)
  (setq mu4e-headers-hide-predicate
        (lambda (msg)
          (member 'trashed (mu4e-message-field msg :flags))))

  (setq my/mu4e-uni-context
        (make-mu4e-context
         :name "Uni"
         :enter-func (lambda () (mu4e-message "Entering Uni context"))
         :leave-func (lambda () (mu4e-message "Leaving Uni context"))
         :match-func (lambda (msg)
                       (when msg
                         (mu4e-message-contact-field-matches
                          msg
                          '(:to :cc :bcc) '("ppyms3@nottingham.ac.uk"
                                            "ppyms3@exmail.nottingham.ac.uk"))))
         :vars '((user-mail-address . "ppyms3@nottingham.ac.uk")
                 (user-full-name . "Mark Skilbeck")
                 (mu4e-sent-folder . "/Uni/Sent Items")
                 (mu4e-drafts-folder . "/Uni/Drafts")
                 (user-mail-address . "ppyms3@exmail.nottingham.ac.uk")
                 (smtpmail-smtp-user . "ppyms3@ad.nottingham.ac.uk")
                 ;; (smtp-local-domain . "nottingham.ac.uk")
                 ;; (smtpmail-default-smtp-server . "smtp.nottingham.ac.uk")
                 ;; (smtpmail-smtp-server . "smtp.nottingham.ac.uk")
                 ;; (smtpmail-smtp-service . 25)
                 (smtp-local-domain . "office365.com")
                 (smtpmail-default-smtp-server . "smtp.office365.com")
                 (smtpmail-smtp-server . "smtp.office365.com")
                 (smtpmail-smtp-service . 587)
                 )))

  (setq my/mu4e-personal-context
        (make-mu4e-context
         :name "Personal"
         :enter-func (lambda () (mu4e-message "Entering Personal context"))
         :leave-func (lambda () (mu4e-message "Leaving Personal context"))
         :match-func (lambda (msg)
                       (when msg
                         (mu4e-message-contact-field-matches
                          msg
                          '(:to :cc :bcc) "markskilbeck@gmail.com")))
         :vars '((user-mail-address . "markskilbeck@gmail.com")
                 (user-full-name . "Mark Skilbeck")
                 (mu4e-sent-folder . "/Personal/Sent Mail")
                 (mu4e-drafts-folder . "/Personal/Drafts")
                 (smtpmail-smtp-user . "markskilbeck@gmail.com")
                 (smtp-local-domain . "gmail.com")
                 (smtpmail-default-smtp-server . "smtp.gmail.com")
                 (smtpmail-smtp-server . "smtp.gmail.com")
                 (smtpmail-smtp-service . 587))))

  (setq mu4e-contexts (list my/mu4e-personal-context my/mu4e-uni-context))
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-compose-context-policy 'ask))

(use-package nav-flash
  :defer
  :commands nav-flash-show)

(use-package paren
  :config (show-paren-mode))

(use-package paredit
  :diminish
  :hook ((lisp-mode emacs-lisp-mode) . paredit-mode)
  :bind (:map paredit-mode-map
              ("[")
              ("M-k"   . paredit-raise-sexp)
              ("M-I"   . paredit-splice-sexp)
              ("C-M-l" . paredit-recentre-on-sexp)

              ("C-. D" . paredit-forward-down)
              ("C-. B" . paredit-splice-sexp-killing-backward)
              ("C-. C" . paredit-convolute-sexp)
              ("C-. F" . paredit-splice-sexp-killing-forward)
              ("C-. a" . paredit-add-to-next-list)
              ("C-. A" . paredit-add-to-previous-list)
              ("C-. j" . paredit-join-with-next-list)
              ("C-. J" . paredit-join-with-previous-list))
  :bind (:map lisp-mode-map       ("<return>" . paredit-newline))
  :bind (:map emacs-lisp-mode-map ("<return>" . paredit-newline))
  :hook (paredit-mode
         . (lambda ()
             (unbind-key "M-r" paredit-mode-map)
             (unbind-key "M-s" paredit-mode-map)))
  :config
  (require 'eldoc)
  (eldoc-add-command 'paredit-backward-delete
                     'paredit-close-round))

(use-package pdf-tools
  :config
  (pdf-tools-install))

(use-package prog-mode
  :config (global-prettify-symbols-mode)
  (defun indicate-buffer-boundaries-left ()
    (setq indicate-buffer-boundaries 'left))
  (add-hook 'prog-mode-hook #'indicate-buffer-boundaries-left))

(use-package recentf
  :demand t
  :config (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:"))

(use-package savehist
  :config (savehist-mode))

(use-package saveplace
  :config (save-place-mode))

(use-package simple
  :config (column-number-mode))

(use-package smart-mode-line
  :config
  (setq sml/theme 'dark)
  (sml/setup))

(progn ;    `text-mode'
  (add-hook 'text-mode-hook #'indicate-buffer-boundaries-left))

(use-package tramp
  :defer t
  :config
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist '("localhost" nil nil))
  (add-to-list 'tramp-default-proxies-alist
               (list (regexp-quote (system-name)) nil nil)))

(use-package which-key
  :defer 5
  :diminish
  :commands which-key-mode
  :config
  (which-key-mode))

(progn ;     startup
  (message "Loading %s...done (%.3fs)" user-init-file
           (float-time (time-subtract (current-time)
                                      before-user-init-time)))
  (add-hook 'after-init-hook
            (lambda ()
              (message
               "Loading %s...done (%.3fs) [after-init]" user-init-file
               (float-time (time-subtract (current-time)
                                          before-user-init-time))))
            t))

(progn ;     personalize
  (let ((file (expand-file-name (concat (user-real-login-name) ".el")
                                user-emacs-directory)))
    (setq HOSTNAME (getenv "HOSTNAME"))
    (message server-name)
    (message HOSTNAME)

    (when (file-exists-p file)
      (load file))

    (setq my/emacs-font
      (if (and (equal HOSTNAME "earth")
               (equal server-name "mail"))
          "-CYEL-Iosevka Term-light-normal-normal-*-11-*-*-*-m-0-iso10646-1"
        "-CYEL-Iosevka Term-light-normal-normal-*-22-*-*-*-m-0-iso10646-1"))

    ;; Should maybe use (when window-system ...) here.
    (tool-bar-mode -1)
    (menu-bar-mode -1)
    (scroll-bar-mode -1)
    (load-theme 'solarized-dark t)
    (setq default-frame-alist
          `((font . ,my/emacs-font)
            (tool-bar-lines . 0)
            (menu-bar-lines . 0)
            (vertical-scroll-bars . nil)
            (horizontal-scroll-bars . nil)))
    (fset 'yes-or-no-p 'y-or-n-p)
    (set-frame-font my/emacs-font)
    (setq-default indent-tabs-mode nil)
    (global-hl-todo-mode)
    (blink-cursor-mode t)))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
