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
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message "locutus")
  (setq initial-buffer-choice t)
  (setq initial-scratch-message "")
  (setq load-prefer-newer t))

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

(use-package exec-path-from-shell  
  :config
  (exec-path-from-shell-initialize))

(use-package diminish)

(use-package general
  :demand t
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
  :bind ("M-m p l" . #'epkg-list-packages)
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
  :hook ((python-mode . electric-pair-mode)
         (python-mode . elpy-mode))
  :bind (:map python-mode-map
              ("M-RET Va" . pyvenv-activate)
              ("C-M-e" . python-nav-end-of-defun))
  :init
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt")
  (setenv "WORKON_HOME" "/Users/mark.skilbeck/anaconda3/envs")
  :config  
  (elpy-enable)
  (pyvenv-mode 1))

;; (use-package conda
;;   :after elpy
;;   :init
;;   (setq conda-anaconda-home "~/anaconda3/")
;;   :config
;;   (conda-env-initialize-interactive-shells)
;;   (conda-env-autoactivate-mode t))

(use-package helpful
  :bind (("C-h f"	. helpful-function)
         ("C-h C"	. helpful-command)
         ("C-h M"	. helpful-macro)
         ("C-h L"	. helpful-callable)
         ("C-h ."	. helpful-at-point)
         ("C-h v"	. helpful-variable)))

(use-package hl-todo
  :init
  (setq hl-todo-active-in-modes '(prog-mode text-mode)))

(use-package ivy
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

;; (use-package pdf-tools
;;   :defer 5 
;;   :config
;;   (pdf-tools-install))

(use-package server
  :config (or (server-running-p) (server-mode)))

(use-package slime
  :defer
  :commands (slime slime-mode)
  :init
  (setq slime-contribs '(slime-fancy
                         slime-indentation
                         slime-sbcl-exts
                         slime-scratch
                         slime-company)
        inferior-lisp-program "sbcl"
        slime-complete-symbol*-fancy t
        slime-complete-symbol-function 'slime-fuzzy-complete-symbol
        shr-width 100
        shr-use-fonts nil)
  :config
  (slime-setup)
  ;; TODO use-package
  (load "/Users/mark.skilbeck/quicklisp/clhs-use-local.el" t)
  (defun my/hyperspec-lookup-advice (fn &rest fnargs)
  (flet ((browse-url (url &rest args) (eww-browse-url url)))
    (apply fn fnargs)))
  (advice-add #'slime-documentation-lookup :around #'my/hyperspec-lookup-advice)
  ;; Add a quicklisp shortcut to the slime REPL
  (defun my/slime-read-ql-system (prompt &optional system-name)
    (let ((completion-ignore-case t))
      (completing-read prompt (slime-bogus-completion-alist
                               (slime-eval
                                `(cl:append (ql:list-local-systems)
                                            (cl:mapcar (cl:symbol-function 'ql-dist:name)
                                                       (ql:system-list)))))
                       nil t system-name)))

  (defun my/slime-repl-quickload (package)
    (interactive (list (let* ((p (slime-current-package))
                              (p (and p (slime-pretty-package-name p)))
                              (p (and (not (equal p (slime-lisp-package))) p)))
                         (my/slime-read-ql-system "System: " p))))
    (slime-repl-send-string (format "(ql:quickload %S)" package)))

  (defslime-repl-shortcut nil
    ("quicklisp quickload" "ql")
    (:handler 'my/slime-repl-quickload)
    (:one-liner "Quickload a system"))
  (defslime-repl-shortcut nil
    ("clear" "cl")
    (:handler 'slime-repl-clear-buffer)
    (:one-liner "Clear the REPL buffer")))

(use-package swiper
  :after ivy
  :defer
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
  :defer
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t))

(use-package dired
  :defer t
  :config (setq dired-listing-switches "-alh"))

(use-package dired-k
  :after dired
  :init
  (add-hook 'dired-initial-position-hook #'dired-k)
  (add-hook 'dired-after-readin-hook #'dired-k-no-revert)
  :config
  (setq dired-k-style 'git)
  (setq dired-k-human-readable t)
  (setq dired-k-padding 1)
  (setq-default dired-k-size-colors nil))

(use-package dired-narrow
  :after dired
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package eldoc
  :after elpy
  :config (global-eldoc-mode))

(use-package help
  :defer t
  :config (temp-buffer-resize-mode))

(progn ;    `isearch'
  (setq isearch-allow-scroll t))

(use-package magit
  :defer t
  ;; :load-path ("site-lisp/magit/lisp")
  :general
  ("M-m gs" #'magit-status)
  :commands (magit-clone my/rm-git-index-lock magit-status)
  :bind (:map magit-log-mode-map
              ("@" . (lambda ()
                       (interactive)
                       (magit-checkout (magit-branch-or-commit-at-point)))))
  :config
  (setq-default magit-repository-directories
                '(("~/hackery/" . 1)))
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append)
  (setq magit-diff-refine-hunk 'all)) 

;; (use-package multi-magit
;;   ;; :load-path "site-lisp/multi-magit"
;;   :after magit
;;   :commands (multi-magit-list-repositories
;; 	     multi-magit-status))

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
  :defer
  :general
  ("C-c c" #'org-capture)
  ("M-m on" (lambda ()
              (interactive)
              (find-file org-default-notes-file)))
  :bind (:map org-mode-map
	      ("C-_" . undo)
	      ("<C-M-return>" . org-insert-item)
              ("C-k" . my/org-kill-dwim))
  :bind (("C-c C-x C-i" . org-mode-clock-clock-in)
         ("C-c C-x C-o" . org-mode-clock-clock-out))
  :preface
  (defun org-latex-format-headline-colored-keywords-function
      (todo todo-type priority text tags info)
    (concat
     (cond ((string= todo "TODO")(and todo (format "{\\color{red}\\bfseries\\sffamily %s} " todo)))
           ((string= todo "DONE")(and todo (format "{\\color{green}\\bfseries\\sffamily %s} " todo))))
     (and priority (format "\\framebox{\\#%c} " priority))
     text
     (and tags
          (format "\\hfill{}\\textsc{%s}"
                  (mapconcat (lambda (tag) (org-latex-plain-text tag info))
                             tags ":")))))

  (setq org-latex-format-headline-function 'org-latex-format-headline-colored-keywords-function)

  (setq org-hide-emphasis-markers t)
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (defun my/org-kill-dwim ()
    (interactive)
    (cond
     ((and (outline-on-heading-p t)
           (= (point) (org-entry-beginning-position)))
      (org-cut-subtree))
     (t
      (org-kill-line))))

  (defun my/org-insert-link (url)
    (interactive "sURL: ")
    (let ((title (or (my/url-get-title url)
                     (read-string "Title: "))))
      (insert (format "[[%s][%s]]" url title))))

  (defun my/org-swap-link-with-title ()
    "Replaces a raw URL with an org link. Automatically retrieves
the URL's title (if available) and uses that for the org link
description. If no title can be found, user is prompted for one.

Note: depends on expand-region."
    (interactive)
    (let ((url (url-get-url-at-point)))
      (when url
        (er/mark-url)
        (kill-region (region-beginning)
                     (region-end))
        (my/org-insert-link url))))

  :hook (org-mode . auto-fill-mode)
  :hook (org-mode . (lambda () (setq fill-column 80)))
  :config
  ;; (require 'org-mu4e)
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
                      (truncate-string-to-width url (if arg (prefix-numeric-value arg) fill-column) nil nil "...")))))

  ;; Stop org putting check boxes where I don't want them
  (defun my/org-insert-todo-heading (arg)
    (previous-line)
    (goto-char (line-beginning-position))
    (when (not (and (search-forward "[" (line-end-position) t)
                    (org-at-item-checkbox-p)))
      (next-line)
      (goto-char (line-end-position))
      (delete-backward-char 4)))

  (advice-add 'org-insert-todo-heading :after #'my/org-insert-todo-heading))

(use-package org-bullets
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-journal
  :after org
  :demand t
  :bind ("M-m o j" . org-journal-new-entry))

;; (use-package mu4e
;;   :defer t
;;   :init
;;   (setq mail-user-agent 'mu4e-user-agent)
;;   :commands (mu4e)
;;   :config
;;   (require 'mu4e-contrib)
;;   (setq mu4e-completing-read-function 'completing-read)
;;   (setq mu4e-compose-dont-reply-to-self t)
;;   (setq mu4e-compose-signature-auto-include nil)
;;   (setq mu4e-maildir "~/Mail"
;;         mu4e-trash-folder "/Trash"
;;         mu4e-refile-folder "/Archive"
;;         mu4e-get-mail-command "offlineimap"
;;         mu4e-update-interval 300
;;         mu4e-compose-signature-auto-include nil
;;         mu4e-view-show-images t
;;         mu4e-view-show-addresses t
;;         mu4e-view-prefer-html t
;;         mu4e-html2text-command 'mu4e-shr2text
;;         mu4e-user-mail-address-list '("markskilbeck@gmail.com" "ppyms3@nottingham.ac.uk"
;;                                       "ppyms3@exmail.nottingham.ac.uk")
;;         shr-color-visible-luminance-min 60
;;         shr-color-visible-distance-min 5
;;         shr-width 80
;;         )
;;   (add-to-list 'mu4e-bookmarks
;;                (make-mu4e-bookmark
;;                 :name "Unread/flagged messages"
;;                 :query "flag:unread AND NOT flag:trashed OR flag:flagged"
;;                 :key ?u))
;;   ;; Stops shr using funky colours that make things unreadable.
;;   (advice-add #'shr-colorize-region :around (defun shr-no-colourise-region (&rest ignore)))
;;   (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;;   (setq org-mu4e-link-query-in-headers-mode nil)
;;   (setq mu4e-sent-folder "/sent"
;;         mu4e-drafts-folder "/drafts"
;;         smtpmail-default-smtp-server "smtp.gmail.com"
;;         smtpmail-smtp-server "smtp.gmail.com"
;;         smtpmail-smtp-service 587
;;         message-send-mail-function 'smtpmail-send-it)
;;   (setq mu4e-headers-hide-predicate
;;         (lambda (msg)
;;           (member 'trashed (mu4e-message-field msg :flags))))

;;   (setq my/mu4e-uni-context
;;         (make-mu4e-context
;;          :name "Uni"
;;          :enter-func (lambda () (mu4e-message "Entering Uni context"))
;;          :leave-func (lambda () (mu4e-message "Leaving Uni context"))
;;          :match-func (lambda (msg)
;;                        (when msg
;;                          (mu4e-message-contact-field-matches
;;                           msg
;;                           '(:to :cc :bcc) '("ppyms3@nottingham.ac.uk"
;;                                             "ppyms3@exmail.nottingham.ac.uk"))))
;;          :vars '((user-mail-address . "ppyms3@nottingham.ac.uk")
;;                  (user-full-name . "Mark Skilbeck")
;;                  (mu4e-sent-folder . "/Uni/Sent Items")
;;                  (mu4e-drafts-folder . "/Uni/Drafts")
;;                  (user-mail-address . "ppyms3@exmail.nottingham.ac.uk")
;;                  (smtpmail-smtp-user . "ppyms3@ad.nottingham.ac.uk")
;;                  ;; (smtp-local-domain . "nottingham.ac.uk")
;;                  ;; (smtpmail-default-smtp-server . "smtp.nottingham.ac.uk")
;;                  ;; (smtpmail-smtp-server . "smtp.nottingham.ac.uk")
;;                  ;; (smtpmail-smtp-service . 25)
;;                  (smtp-local-domain . "office365.com")
;;                  (smtpmail-default-smtp-server . "smtp.office365.com")
;;                  (smtpmail-smtp-server . "smtp.office365.com")
;;                  (smtpmail-smtp-service . 587)
;;                  )))

;;   (setq my/mu4e-personal-context
;;         (make-mu4e-context
;;          :name "Personal"
;;          :enter-func (lambda () (mu4e-message "Entering Personal context"))
;;          :leave-func (lambda () (mu4e-message "Leaving Personal context"))
;;          :match-func (lambda (msg)
;;                        (when msg
;;                          (mu4e-message-contact-field-matches
;;                           msg
;;                           '(:to :cc :bcc) "markskilbeck@gmail.com")))
;;          :vars '((user-mail-address . "markskilbeck@gmail.com")
;;                  (user-full-name . "Mark Skilbeck")
;;                  (mu4e-sent-folder . "/Personal/Sent Mail")
;;                  (mu4e-drafts-folder . "/Personal/Drafts")
;;                  (smtpmail-smtp-user . "markskilbeck@gmail.com")
;;                  (smtp-local-domain . "gmail.com")
;;                  (smtpmail-default-smtp-server . "smtp.gmail.com")
;;                  (smtpmail-smtp-server . "smtp.gmail.com")
;;                  (smtpmail-smtp-service . 587))))

;;   (setq mu4e-contexts (list my/mu4e-personal-context my/mu4e-uni-context))
;;   (setq mu4e-context-policy 'pick-first)
;;   (setq mu4e-compose-context-policy 'ask))

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

(use-package prescient)
(use-package ivy-prescient
  :after ivy
  :config (ivy-prescient-mode))

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

;; (use-package doom-modeline
;;   :demand t
;;   :init
;;   (setq x-underline-at-descent-line t)
;;   :hook (after-init . doom-modeline-init))

;; (use-package smart-mode-line
;;   :config
;;   (setq sml/theme 'respectful)
;;   (sml/setup))

(progn                                  ;    `text-mode'
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

;; (use-package circe
;;   :hook (circe-channel-mode-hook . me/circe-set-prompt)
;;   :init
;;   (setq auth-sources '("~/.authinfo"))
;;   (defun my/fetch-password (&rest params)
;;     (require 'auth-source)
;;     (let ((match (car (apply 'auth-source-search params))))
;;       (if match
;;           (let ((secret (plist-get match :secret)))
;;             (if (functionp secret)
;;                 (funcall secret)
;;               secret))
;;         (error "Password not found for %S" params))))
;;   (defun my/znc-password (server)
;;     (message "test")
;;     (my/fetch-password :login "mgsk" :machine "notmg.sk"))
;;   (setq
;;    circe-network-options
;;    '(("Freenode"
;;       :tls t
;;       :nick "mgsk"
;;       :channels ("#symbollox" "#bspwm"))
;;      ("znc"
;;       :host "notmg.sk"
;;       :port 7778
;;       :user "mgsk"
;;       :nick "mgsk"
;;       :pass my/znc-password
;;       :tls t))
;;    circe-reduce-lurker-spam t
;;    circe-format-say "{nick:10.10s}> {body}"
;;    circe-format-self-say "{nick:10.10s}> {body}"
;;    circe-format-server-lurker-activity
;;    "            *** First activity: {nick} joined {joindelta} ago."
;;    circe-server-send-unknown-command-p t)

;;   (defun me/circe-set-prompt ()
;;     (interactive)
;;     (lui-set-prompt (format "%s>" (circe-nick))))

;;   (defun my/circe-all-chat-buffers ()
;;     (mapcan (lambda (buf)
;;               (with-current-buffer buf
;;                 (with-circe-server-buffer
;;                   (circe-server-chat-buffers))))
;;             (circe-server-buffers))))

(use-package solarized
  :init (setq solarized-use-more-italic t
              solarized-use-less-bold t
              solarized-high-contrast-mode t
              solarized-distinct-fringe-background nil))

;; (use-package circe-color-nicks
;;   :after circe
;;   :config
;;   (enable-circe-color-nicks))

;; (use-package circe-notifications
;;   :after circe
;;   :init
;;   (setq circe-notifications-watch-strings '("mgsk")
;;         circe-notifications-alert-style 'osx-notifier)
;;   :hook '(circe-server-connected . enable-circe-notifications)
;;   :config
;;   ;; For some reason alert uses the message buffer on top of using
;;   ;; desktop notifications, which is kinda annoying. Redefine the
;;   ;; function to do nothing.
;;   (defun alert-message-notify (info) nil))

;; (use-package lui
;;   :hook (lui-mode . my/lui-setup)
;;   :init
;;   (setq
;;    lui-fill-type "          | "
;;    lui-fill-column 80
;;    lui-time-stamp-position 'right-margin
;;    lui-time-stamp-format "%H:%M")
;;   (defun my/lui-setup ()
;;     (interactive)
;;     (setq
;;      fringes-outside-margins t
;;      right-margin-width 5
;;      word-wrap t
;;      wrap-prefix "    ")))

;; (use-package lui-logging
;;   :after lui
;;   :config
;;   (enable-lui-logging-globally))

(use-package imenu
  :bind ("M-m i" . counsel-imenu))

;; (use-package expand-region)

(use-package org-jira
  :defer
  :init
  (setq jiralib-url "http://jira.lab.rigetti.com"))

(use-package url
  :defer 
  :config
  (defun my/url-get-title (url)
    (interactive "sURL: ")
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (or (search-forward "\n\n" nil t)
          (search-forward "\r\n\r\n" nil t))
      (let ((cp (point))
            (ep (point-max)))
        (set-mark cp)
        (push-mark ep)
        (let* ((aslist (cddr (libxml-parse-html-region cp ep)))
               (headers (cdr (assoc 'head aslist)))
               (title (caddr (assoc 'title headers))))
          title)))))

(use-package markdown-mode
  :commands (markdown-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook (markdown-mode . visual-line-mode)
  :init
  (setq markdown-command "multimarkdown"
        markdown-hide-markup t
        markdown-header-scaling t))

(use-package git-link)

;; (use-package ess-site)

;; (use-package ess-julia
;;   :demand t
;;   :commands julia)

(use-package flycheck)

(use-package lsp-mode
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)

(use-package company-lsp :commands company-lsp)

(use-package julia-mode
  :hook (julia-mode . lsp-mode))

(use-package lsp-julia
  :init
  (setq lsp-julia-command "/Applications/Julia-1.0.app/Contents/Resources/julia/bin/julia"))

(use-package julia-shell
  :after julia-mode
  :init
  (setq julia-shell-program "/Applications/Julia-1.0.app/Contents/Resources/julia/bin/julia")
  :bind (:map julia-mode-map
              ("C-c C-c" . julia-shell-run-region-or-line)))

(defun my/load-theme (variant)
  (interactive
   (list (completing-read "Theme variant: "
                          '("dark" "light"))))
  (cond
   ((string-equal variant "dark")
    (my/load-dark-theme))
   ((string-equal variant "light")
    (my/load-light-theme))))

(defun my/load-dark-theme ()
  (interactive)
  (load-theme/doom-Iosvkem)
  ;; (sml/apply-theme 'respectful)
  )

(defun my/load-light-theme ()
  (interactive)
  (load-theme/doom-solarized-light)
  ;; (sml/apply-theme 'light)
  )

;; TAKEN FROM https://github.com/kaushalmodi/.emacs.d/blob/e54b5b5b3943b8254a1315d9b9e69b8b9a259b29/setup-files/setup-visual.el#L29-L92
;;                     THEME-NAME           DARK   FCI-RULE-COLOR
(defconst my/themes '((solarized-dark       'dark  "gray40")
                      (dracula              'dark  "gray40")
                      (doom-dracula         'dark  "gray40")
                      (doom-Iosvkem         'dark  "gray40")
                      (misterioso           'dark  "gray40")
                      (zenburn-theme        'dark  "gray40")
                      (leuven               'light "gray")
                      (solarized-light      'light "gray")
                      (doom-solarized-light 'light "gray")
                      (github-modern-theme  'light "gray")
                      (default              'light "gray")) ; default emacs theme
  "Alist of themes I tend to switch to frequently.")

(defun my/disable-enabled-themes ()
  "Disable all enable themes except the one used by `smart-mode-line'.
This function is not meant for interactive use. A clean way to disable all
themes will be to run `M-x load-theme/default' (this function is generated
by the `modi/gen-all-theme-fns' macro. That will ensure that all
themes are disabled and also fix the faces for linum, fringe, etc."
  (dolist (theme custom-enabled-themes)
    (unless (string-match "smart-mode-line-" (format "%s" theme))
      (disable-theme theme))))

;; How can I create multiple defuns by looping through a list?
;; http://emacs.stackexchange.com/a/10122/115
(defun modi/gen-theme-fn (theme-name dark fci-rule-color)
  "Function to generate a function to disable all themes and enable the chosen
theme, while also customizing few faces outside the theme.
The theme loading functions are named “load-theme/THEME-NAME”.
Example: For `smyx' theme, the generated function will be `load-theme/smyx'.
The DARK variable should be set to `'dark' if the theme is dark and `'light'
if otherwise.
The FCI-RULE-COLOR is the color string to set the color for fci rules."
  (let ((theme-fn-name (intern (format "load-theme/%s" theme-name))))
    `(defun ,theme-fn-name ()
       (interactive)
       ;; `dark-theme' is set to `t' if `dark' value is `'dark'
       (setq dark-theme (equal ,dark 'dark))
       (my/disable-enabled-themes)
       (when (not (equal ',theme-name 'default))
         (load-theme ',theme-name t))
       (when (featurep 'defuns)
         (modi/blend-fringe))
       (when (featurep 'setup-linum)
         (modi/blend-linum))
       (when (featurep 'smart-mode-line)
         ;; (sml/apply-theme ,dark nil :silent)
         (sml/apply-theme (if (eq ,dark 'dark) 'respectful 'light) nil :silent)) ; apply sml theme silently
       (when (featurep 'fill-column-indicator)
         ;; Below commented code does not work
         ;; (setq fci-rule-color (face-foreground 'font-lock-comment-face))
         (setq fci-rule-color ,fci-rule-color)
         (modi/fci-redraw-frame-all-buffers)))))

(defmacro modi/gen-all-theme-fns ()
  `(progn ,@(mapcar
             (lambda (x) (modi/gen-theme-fn (nth 0 x) (nth 1 x) (nth 2 x)))
             my/themes)))

(modi/gen-all-theme-fns)
;; (pp (macroexpand '(modi/gen-all-theme-fns))) ; for debug

(defun my/lighten-color (color percent)
  ;; See https://stackoverflow.com/q/5560248
  ;; TODO Tidy this up
  (let* ((num (string-to-number (string-remove-prefix "#" color) 16))
         (amount (round (* 2.55 percent)))
         (R (+ (ash num -16) amount))
         (G (+ (logand (ash num -8) #x00FF) amount))
         (B (+ (logand num #x0000FF) amount)))
    (let ((num (+ #x1000000
                  (* (if (< R 255)
                         (if (< R 1)
                             0
                           R)
                       255)
                     #x10000)
                  (* (if (< G 255)
                         (if (< G 1)
                             0
                           G)
                       255)
                     #x100)
                  (if (< G 255)
                      (if (< G 1)
                          0
                        G)
                    255))))
      (format "#%06x" (logand num #xFFFFFF)))))

(use-package undo-tree
  :bind ("M-m u" . undo-tree-visualize)
  :config
  (global-undo-tree-mode))

(progn                                  ;     personalize
  (setq my/emacs-font-str
        "-*-Iosevka Nerd Font Mono-light-normal-normal-*-%S-*-*-*-m-0-iso10646-1")
  (setq my/emacs-font-sizes (list 12 14 16 18))
  (setq my/emacs-font (cl-loop for s in my/emacs-font-sizes
                               collect (format my/emacs-font-str s)))
  (defun my/rotate-font-size ()
    (interactive)
    (set-frame-font (first my/emacs-font))
    (setq my/emacs-font (-rotate -1 my/emacs-font)))

  (global-set-key (kbd "C-x C-8") #'my/rotate-font-size)

  (my/load-dark-theme)
  (setq default-frame-alist
        `((font . ,(first my/emacs-font))
          (tool-bar-lines . 0)
          (menu-bar-lines . 0)
          (vertical-scroll-bars . nil)
          (horizontal-scroll-bars . nil)))
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq-default indent-tabs-mode nil)
  (global-hl-todo-mode)
  (global-hl-line-mode)
  (blink-cursor-mode t)
  (setq split-width-threshold 120)
  (setq split-height-threshold 200)

  (when (eq system-type 'darwin) ;; mac specific settings
    (setq mac-option-modifier 'nil)
    (setq mac-command-modifier 'meta)
    (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
    (global-set-key (kbd "M-`") 'ns-next-frame)
    (global-set-key (kbd "M-h") 'ns-do-hide-emacs)

    (setq powerline-default-separator 'box)
    ;; Stop that ugly bell noise on macos. DUN. DUN. DUN. STFU.
    (setq ring-bell-function 'ignore)))

(message "Loading Emacs... finally done (%.3fs)"
         (float-time (time-subtract (current-time)
                                    before-init-time)))

(use-package beginend
  :init
  (defun my/beginning-of-line ()
    (interactive)
    (beginend--double-tap (point-at-bol) (back-to-indentation)))
  :bind ("C-a" . #'my/beginning-of-line))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
