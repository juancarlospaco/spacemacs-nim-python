;; Garbage Collector GC runs only when when idle, or 1Gb RAM Max allocated if ever happens.
(setq gc-cons-threshold (eval-when-compile (* 1024 1024 1024)))
(run-with-idle-timer 2 t (lambda () (garbage-collect)))


;; Cache for file handler set to nil for Fast Startup, then Restored after.
(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)
(run-with-idle-timer
 5 nil
 (lambda ()
   (setq file-name-handler-alist file-name-handler-alist-original)
   (makunbound 'file-name-handler-alist-original)
 ))


;; Required, do not remove.
(defconst spacemacs-version         "0.200.13" "Spacemacs version.")
(defconst spacemacs-emacs-min-version   "25.0" "Minimal version of Emacs.")
(if (not (version<= spacemacs-emacs-min-version emacs-version))
    (error (concat "Your version of Emacs (%s) is too old. "
                   "Spacemacs requires Emacs version %s or above.")
           emacs-version spacemacs-emacs-min-version)
  (load-file (concat (file-name-directory load-file-name)
                     "core/core-load-paths.el"))
  (require 'core-spacemacs)
  (spacemacs/init)
  (configuration-layer/sync)
  (spacemacs-buffer/display-startup-note)
  (spacemacs/setup-startup-hook)
  (require 'server)
  (unless (server-running-p) (server-start)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; CUSTOM STUFF BELOW ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; DISABLE spacemacs unwanted stuff.
(evil-set-initial-state 'ibuffer-mode 'normal)
(evil-set-initial-state 'bookmark-bmenu-mode 'normal)
(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'sunrise-mode 'emacs)
(add-to-list 'evil-emacs-state-modes 'nav-mode)
(setq-default message-log-max nil)
(kill-buffer "*Messages*")
(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
            (kill-buffer buffer)))))
;; (define-key global-map [menu-bar options] nil)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)
(global-set-key (kbd "C-S-s") nil)


;; UTF-8 ALL
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq system-time-locale "C")


;; Cursor Beacon
(beacon-mode 1)


;; Execute source code
(require 'quickrun)


;; Syntax Checking
(flycheck-mode)


;; Title = full-path-filename + projectname|folder
(defun spacemacs//frame-title-format ()
"Return frame title with current project name, where applicable."
(let ((file buffer-file-name))
   (if file
       (concat (abbreviate-file-name file)
               (when (and (bound-and-true-p projectile-mode)
                          (projectile-project-p))
                 (format " [%s]" (projectile-project-name))))
       "%b")))
(when (display-graphic-p)
(setq frame-title-format '((:eval (spacemacs//frame-title-format)))))

(defun my/cleanup-buffer ()
  "Remove buffers trailing whitespace and tabs."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max))
    (untabify (point-min) (point-max))))

(defun my/windows2linux ()
  "Convert buffer format from Windows to Linux."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix 't) )

(defun my/reopen-as-root ()
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))

(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.
This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil
   '(("\\<\\(FIX\\(ME\\)?\\|BUG\\|HACK\\):" 1 font-lock-warning-face t)
     ("\\<\\(NOTE\\):" 1 'org-level-2 t)
     ("\\<\\(TODO\\):" 1 'org-todo t)
     ("\\<\\(DONE\\):" 1 'org-done t))
   ))

(add-hook 'prog-mode-hook 'font-lock-comment-annotations)


;; Use GUI,
(menu-bar-mode 1)
(tool-bar-mode 1)
(column-number-mode)
(show-paren-mode)
(blink-cursor-mode)
(setq visible-bell t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq user-full-name "Juan Carlos")
(setq user-mail-address "JuanCarlospaco@gmail.com")
(modify-all-frames-parameters '((fullscreen . maximized)))
(global-prettify-symbols-mode 1)

(setq-default
 whitespace-line-column 80
 whitespace-style       '(face lines-tail))
(add-hook 'prog-mode-hook #'whitespace-mode)

;; Tell emacs where is your personal elisp lib dir
(add-to-list 'load-path "~/.emacs.d/lisp/")
;; load the packaged named xyz.
;; (load "xyz") ;; best not to include the ending “.el” or “.elc”
;; (require 'tool-bar+)


(defun nim-check-toolbar ()
 "Nim check on the current file"
 (interactive)
 (message "'nim check' on the current file")
 (nim-check-current-file))

    (tool-bar-add-item "spell" 'nim-check-toolbar
               'nim-check-toolbar
               :help   "Nim check on the current file")

(defun nyan ()
 "Nyan the Screen"
 (interactive)
 (message "Nyan the Screen")
 (zone-nyan-preview))

    (tool-bar-add-item "help" 'zone-nyan-preview
               'zone-nyan-preview
               :help   "Nyan the Screen")



(defun nim-check-current-file ()
  "Nim check on the current file and revert the buffer"
  (interactive)
  (shell-command
   (format "~/.nimble/bin/nim check --verbosity=0 --hints:off %s"
       (shell-quote-argument (buffer-file-name))))
  (revert-buffer t t t))


(treemacs)
(define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
(add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?)


(nyan-mode)
(nyan-toggle-wavy-trail)
(nyan-start-animation)


(require 'highlight-tail)
(setq highlight-tail-colors '(("grey" . 0)
                              ("red" . 25)
                              ("blue" . 66)))
 (setq highlight-tail-steps 10
       highlight-tail-timer 0.5)
(highlight-tail-mode)

;; make whitespace-mode use just basic coloring
;;(setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))
(setq whitespace-display-mappings
  ;; all numbers are Unicode codepoint in decimal. ⁖ (insert-char 182 1)
  '(
    (space-mark 32 [183] [46]) ; 32 SPACE 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
    (newline-mark 10 [182 10]) ; 10 LINE FEED
    (tab-mark 9 [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
    ))

;; Change cursor color according to mode
(defvar hcz-set-cursor-color-color "")
(defvar hcz-set-cursor-color-buffer "")
(defun hcz-set-cursor-color-according-to-mode ()
  "change cursor color according to some minor modes."
  ;; set-cursor-color is somewhat costly, so we only call it when needed:
  (let ((color
         (if buffer-read-only "white"
           (if overwrite-mode "red"
             "yellow"))))
    (unless (and
             (string= color hcz-set-cursor-color-color)
             (string= (buffer-name) hcz-set-cursor-color-buffer))
      (set-cursor-color (setq hcz-set-cursor-color-color color))
      (setq hcz-set-cursor-color-buffer (buffer-name)))))
(add-hook 'post-command-hook 'hcz-set-cursor-color-according-to-mode)


;; Colores
(load-theme 'monokai t)

;; Auto-braces
; (autopair-global-mode)

;; Mini-mapa
(minimap-mode)

;; highlight words
(add-hook 'prog-mode-hook (lambda () (idle-highlight-mode t)))

;; indent guide
(use-package indent-guide
  :ensure t
  :config
  ;; Only show indent-guide in idle-time.
  (setq indent-guide-delay 0.5))
(indent-guide-global-mode)

(use-package company
  :ensure t
  :config
  ;; enable globally
  (global-company-mode 1))
(setq company-idle-delay 0.1)

;(use-package company-quickhelp
;  :ensure t
;  :init (company-quickhelp-mode 1))

;; Lineas largas no se cortan
(set-default 'truncate-lines t)

;; arbol de carpetitas
(require 'treemacs)
(treemacs-follow-mode t)
(treemacs-filewatch-mode t)
(treemacs-fringe-indicator-mode t)
(treemacs-resize-icons 32)

;; Paradox GitHub token
(require 'paradox)
(setq paradox-github-token "e902c09cb2dcdb2a6adaa96878e85bebb460b202")
(paradox-enable)

;; Tab Bar
(use-package tabbar
  :ensure t
  ;; :bind
  ;; ("<C-S-iso-lefttab>" . tabbar-backward)
  ;; ("<C-tab>" . tabbar-forward)

  :config
  (set-face-attribute
   'tabbar-button nil
   :box '(:line-width 1 :color "gray19"))

  (set-face-attribute
   'tabbar-selected nil
   :foreground "orange"
   :background "gray19"
   :box '(:line-width 1 :color "gray19"))

  (set-face-attribute
   'tabbar-unselected nil
   :foreground "black"
   :background "gray25"
   :box '(:line-width 1 :color "gray19"))

  (set-face-attribute
   'tabbar-highlight nil
   :foreground "blue"
   :background "skyblue"
   :underline nil
   :box '(:line-width 1 :color "gray19" :style nil))

  (set-face-attribute
   'tabbar-modified nil
   :foreground "red"
   :background "gray25"
   :box '(:line-width 1 :color "gray19"))

  (set-face-attribute
   'tabbar-selected-modified nil
   :foreground "red"
   :background "gray19"
   :box '(:line-width 1 :color "gray19"))

  (custom-set-variables
   '(tabbar-separator (quote (0.5))))

  ;; Change padding of the tabs
  ;; we also need to set separator to avoid overlapping tabs by highlighted tabs
  ;; (custom-set-variables
  ;;  '(tabbar-separator (quote (1.0))))
  (defun tabbar-buffer-tab-label (tab)
    "Return a label for TAB.
  That is, a string used to represent it on the tab bar."
    (let ((label  (if tabbar--buffer-show-groups
                      (format " [%s] " (tabbar-tab-tabset tab))
                    (format " %s " (tabbar-tab-value tab)))))
      ;; Unless the tab bar auto scrolls to keep the selected tab
      ;; visible, shorten the tab label to keep as many tabs as possible
      ;; in the visible area of the tab bar.
      (if tabbar-auto-scroll-flag
          label
        (tabbar-shorten
         label (max 1 (/ (window-width)
                         (length (tabbar-view
                                  (tabbar-current-tabset)))))))))

  (defun px-tabbar-buffer-select-tab (event tab)
    "On mouse EVENT, select TAB."
    (let ((mouse-button (event-basic-type event))
          (buffer (tabbar-tab-value tab)))
      (cond
       ((eq mouse-button 'mouse-2) (with-current-buffer buffer (kill-buffer)))
       ((eq mouse-button 'mouse-3) (pop-to-buffer buffer t))
       (t (switch-to-buffer buffer)))
      (tabbar-buffer-show-groups nil)))

  (defun px-tabbar-buffer-help-on-tab (tab)
    "Return the help string shown when mouse is onto TAB."
    (if tabbar--buffer-show-groups
        (let* ((tabset (tabbar-tab-tabset tab))
               (tab (tabbar-selected-tab tabset)))
          (format "mouse-1: switch to buffer %S in group [%s]"
                  (buffer-name (tabbar-tab-value tab)) tabset))
      (format "\
mouse-1 = Select %S\n\
mouse-2 = Close %S\n\
mouse-3 = Open %S on new Window"
              (buffer-name (tabbar-tab-value tab))
              (buffer-name (tabbar-tab-value tab))
              (buffer-name (tabbar-tab-value tab)))))

  (defun px-tabbar-buffer-groups ()
    "Sort tab groups."
    (list (cond ((or
                  (eq major-mode 'dired-mode)
                  (string-equal "*" (substring (buffer-name) 0 1))) "emacs")
                (t "user"))))
  (setq tabbar-help-on-tab-function 'px-tabbar-buffer-help-on-tab
        tabbar-select-tab-function 'px-tabbar-buffer-select-tab
        tabbar-buffer-groups-function 'px-tabbar-buffer-groups)

  :init
  (tabbar-mode 1))


;; No se de donde saca la imagen, pero cambiando el archivo local lo ignora :(
;; (require 'url)
;; (delete-file "~/.emacs.d/assets/spacemacs.svg")
;; (url-copy-file "https://source.unsplash.com/random/600x400" "~/.emacs.d/assets/spacemacs.svg")
