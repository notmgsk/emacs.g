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
  (setq use-package-enable-imenu-support t)
  (setq use-package-verbose t)
  (require  'use-package))

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
          ("C-. r" . my-ctrl-dot-r-map)))
  (setq dk-keymap (make-sparse-keymap))
  (setq sl-keymap (make-sparse-keymap))

  (defun add-to-keymap (keymap bindings)
    (dolist (binding bindings)
      (define-key keymap (kbd (car binding)) (cdr binding))))

  (defun add-to-dk-keymap (bindings)
    (add-to-keymap dk-keymap bindings))

  (defun add-to-sl-keymap (bindings)
    (add-to-keymap sl-keymap bindings)))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package key-chord
  :demand t
  :config
  (key-chord-define-global "dk" dk-keymap)
  (key-chord-define-global "sl" sl-keymap)
  (add-to-dk-keymap
   '(("m" . execute-command-on-file-buffer)
     ("d" . dired-jump)
     ("b" . ivy-switch-buffer)
     ("r" . quickrun-region)
     ("R" . quickrun)
     ("<SPC>" . rgrep)
     ("s" . save-buffer)
     ("S" . howdoi-query-line-at-point-replace-by-code-snippet)
     ("t" . eshell-here)
     ("/" . find-name-dired)
     ("e" . eval-last-sexp)))
  (setq key-chord-two-keys-delay 0.05))

(use-package use-package-chords
  :demand t
  :config (key-chord-mode 1))

(use-package diminish)

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
  :chords
  ("jw" . ace-window)
  ("kw" . ace-delete-window))

(use-package avy
  :after ivy
  :config
  (avy-setup-default))

(use-package epkg
  :defer t
  :init (setq epkg-repository
              (expand-file-name "var/epkgs/" user-emacs-directory)))

(use-package counsel
  :after ivy
  :demand t
  :diminish
  :bind (("C-*"     . counsel-org-agenda-headlines)
         ("C-x C-f" . counsel-find-file)
         ("M-x"     . counsel-M-x))
  :chords
  ("lw" . counsel-M-x)
  :commands (counsel-minibuffer-history
             counsel-find-library
             counsel-unicode-char)
  :init (add-to-dk-keymap '(("y" . counsel-yank-pop)))
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
  (elpy-enable))

(use-package ein)

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

  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x C-b" . ivy-switch-buffer))

  :config
  (ivy-mode 1)
  (ivy-set-occur 'ivy-switch-buffer 'ivy-switch-buffer-occur)
  (setq ivy-use-virtual-buffers t
	ivy-count-format "%d/%d "))

(use-package nav-flash
  :after swiper
  :config
  (advice-add #'swiper :after #'nav-flash-show))

;; TODO Figure out how to have pdf-tools load only when i first
;; visit a pdf file
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
        slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
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

  (load-file "slimemacs.el")

  (defslime-repl-shortcut nil
    ("quicklisp quickload" "ql")
    (:handler 'my/slime-repl-quickload)
    (:one-liner "Quickload a system"))
  (defslime-repl-shortcut nil
    ("clear" "cl")
    (:handler 'slime-repl-clear-buffer)
    (:one-liner "Clear the REPL buffer")))

(use-package lispy
  :hook ((emacs-lisp-mode . lispy-mode)
         (lisp-mode . lispy-mode))
  :config
  (lispy-define-key lispy-mode-map ":" 'self-insert-command))

(use-package iedit
  :bind ("C-;" . iedit-mode))

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

(use-package project
             :init
             (add-to-dk-keymap '(("p" . project-find-regexp))))

(progn                                  ;     startup
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

(use-package eshell
  :init
  (defun eshell-here () (interactive) (eshell t))
  (add-to-dk-keymap '(("t" . #'eshell-here))))

(use-package elisp-mode)

(use-package ibuffer
  ;; :bind ("C-x C-b" . ibuffer)
  )

(use-package help
  :defer t
  :config (temp-buffer-resize-mode))

(progn ;    `isearch'
  (setq isearch-allow-scroll t))

(use-package magit
  :defer t
  :commands (magit-clone my/rm-git-index-lock magit-status)
  :bind ((:map sl-keymap
               ("s" . magit-status))
         (:map magit-log-mode-map
               ("@" . (lambda ()
                        (interactive)
                        (magit-checkout (magit-branch-or-commit-at-point))))))

  :config
  (setq-default magit-repository-directories
                '(("~/hackery/" . 1)))
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append)
  (setq magit-diff-refine-hunk 'all)
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"
        ghub-use-workaround-for-emacs-bug nil))

(use-package forge
  :after magit)

(use-package git-link
  :demand t)

(use-package multiple-cursors
  :defer t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)
         ("C-c (" . mc/mark-all-in-region)))

(use-package man
  :defer t
  :config (setq Man-width 80))

(use-package org
  :defer t
  :bind (("C-c c" . org-capture)
         ;; ("M-m  on" . (lambda ()
         ;;               (interactive)
         ;;               (find-file org-default-notes-file)))
         ("C-c C-x C-i" . org-mode-clock-clock-in)
         ("C-c C-x C-o" . org-mode-clock-clock-out)
         (:map org-mode-map
	       ("C-_" . undo)
	       ("<C-M-return>" . org-insert-item)
               ("C-k" . my/org-kill-dwim)))
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

  (setq org-latex-format-headline-function 'org-latex-format-headline-colored-keywords-function
        org-hide-emphasis-markers t)
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
  ;; :bind ("M-m o j" . org-journal-new-entry) 
  )

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

(use-package solarized
  :init (setq solarized-use-more-italic t
              solarized-use-less-bold t
              solarized-high-contrast-mode t
              solarized-distinct-fringe-background nil))

(use-package imenu
  :init (add-to-dk-keymap '(("i" . counsel-imenu))))

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

(use-package rotate)

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

(use-package doom-themes)
(use-package color-theme-sanityinc-tomorrow)

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
                      (doom-tomorrow-day    'light "gray")
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
  :init (add-to-dk-keymap '(("u" . undo-tree-visualize)))
  :config
  (global-undo-tree-mode))

(use-package simple
  :chords
  ("df" . undo))

(use-package project
  :init (add-to-dk-keymap '(("f" . project-find-regexp))))

(use-package wgrep)

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

  ;; Stop accidentally closing eamcs
  ;; (global-unset-key (kbd "C-x C-c"))

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

  (setq shell-file-name "/bin/bash")

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
