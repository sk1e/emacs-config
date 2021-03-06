;;;; -*- lexical-binding: t -*-
(require 'package)

(require 'cask "~/.cask/cask.el")


(cask-initialize)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(delete-selection-mode 1)
(show-paren-mode t)
(global-linum-mode 1)
(nyan-mode)
(ido-mode t)
(line-number-mode -1)


(defun hello-display (a b)
  (message ">>>> hello %s %s" a b))

(setq display-buffer-alist
      `(
        ;; (,(rx bos "/") . (hello-display))
        ("*Racket Describe*"  (lambda (buffer alist) t))
        ))
(defun tide-parse-error (response checker)
  (-map
   (lambda (diagnostic)
     (let* ((start (plist-get diagnostic :start))
            (line (plist-get start :line))
            (column (tide-column line (plist-get start :offset))))
       (flycheck-error-new-at line column 'error (plist-get diagnostic :text)
                              :checker checker
                              :id (plist-get diagnostic :code))))
   (let ((diagnostic (car (tide-plist-get response :body))))
     (-concat (plist-get diagnostic :syntaxDiag)
              (plist-get diagnostic :semanticDiag)))))

;; magit-auto-revert-mode
;; (require 'magit)
;; magit-

;; (global-auto-revert-mode 1)
;; (setq auto-revert-check-vc-info t)

;; (generate-new-buffer "/huita")
;; (display-buffer "/huita")

;; (defun wl-half (buffer alist)
;;   (with-disabled-lt-hook
;;    (delete-other-windows)
;;    (window--display-buffer buffer (split-window-right) 'window)))

;; (setq display-buffer-alist
;;       `((,(rx bos (or "*Messages*" "*Help*" "*Backtrace*"
;;                    (and (1+ ascii) "-l1og")
;;                    (and "*exec" (1+ ascii)))
;;            eos) . (wl-half))
;;      ;; (,(rx bos (1+ ascii) "-log" eos) . (wl-log))
;;      ("" (display-buffer-same-window))))


(setq display-time-day-and-date t
      display-time-24hr-format t
      message-log-max 1000
      max-mini-window-height 20
      make-backup-files nil
      linum-format "%4d"
      use-dialog-box nil
      indent-tabs-mode nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length -1
      org-src-fontify-natively t
      org-enforce-todo-dependencies t)


(defun racket-setup-company ()
  (when (boundp 'company-backends)
    (make-local-variable 'company-backends)
    (setq company-backends (delete 'company-dabbrev company-backends))
    (add-to-list 'company-backends 'company-dabbrev)
    (add-to-list 'company-backends 'company-files)))

(add-hook 'racket-mode-hook 'racket-setup-company)
(add-hook 'elixir-mode-hook 'alchemist-mode)

(setq-default indent-tabs-mode nil
              c-basic-offset 2)
(display-time-mode t)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'root 'scheme-indent-function 0)



(add-hook 'python-mode-hook 'capitalized-words-mode)
(add-hook 'js2-mode-hook 'capitalized-words-mode)
(add-hook 'web-mode-hook 'capitalized-words-mode)
(add-hook 'typescript-mode-hook 'capitalized-words-mode)

(require 'cl-lib)

(cl-defstruct tlc:unpackaged-dependency directory-name feature)



(defconst config:external-dependencies
  (cl-loop for (directory-name feature) in '(("emacs-window-layout" window-layout))
           collect (make-tlc:unpackaged-dependency
                    :directory-name directory-name
                    :feature feature)))


(cl-defun tlc:load-unpackaged-dependecies (dependencies)
  (cl-loop with load-directory = (file-name-directory load-file-name)
           for x in dependencies
           do (add-to-list 'load-path (expand-file-name
                                       (tlc:unpackaged-dependency-directory-name x)
                                       load-directory))
           do (require (tlc:unpackaged-dependency-feature x))))




(tlc:load-unpackaged-dependecies config:external-dependencies)


(defconst tlc:projects-directory "~/Projects")

(cl-defstruct tlc:fragile-dependency project-name branch)

(defun set-eq (list1 list2)
  (and (null (set-difference list1 list2))
       (null (set-difference list2 list1))))

(cl-defun tlc:fragile-dependency-directory (x)
  (ecase (tlc:fragile-dependency-branch x)
    (stable (expand-file-name (tlc:fragile-dependency-project-name x)
                              tlc:config-directory))
    (development (expand-file-name (tlc:fragile-dependency-project-name x)
                                   tlc:projects-directory))))


(cl-defun tlc:load-fragile-dependencies (dependencies)
  (cl-loop with load-directory = (file-name-directory load-file-name)
           for x in dependencies
           do (let* ((project-name (tlc:fragile-dependency-project-name x))
                     (dependency-directory (ecase (tlc:fragile-dependency-branch x)
                                             (stable (expand-file-name project-name load-directory))
                                             (development (expand-file-name project-name tlc:projects-directory)))))
                (require (intern project-name)
                         (expand-file-name (format "%s.el" project-name)
                                           (tlc:fragile-dependency-directory x))))))




(require 'eieio)
(require 'color)
(require 'help-mode)
(require 'flycheck)





(add-hook 'after-init-hook #'global-flycheck-mode)



(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'typescript-tslint 'web-mode)


(add-hook 'typescript-mode-hook (let (append-checker)
                                  (setq append-checker
                                        (lambda () (flycheck-add-next-checker 'tsx-tide
                                                                         '(warning . typescript-tslint)
                                                                         'append)
                                          (setq append-checker (lambda ()))))
                                  (lambda () (funcall append-checker))))


(add-hook 'before-save-hook 'whitespace-cleanup)

(setq flycheck-stylelintrc ".stylelintrc")

(setq-default flycheck-temp-prefix ".flycheck")


;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(json-jsonlist)))

(defun my-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
  ;;; http://web-mode.org/
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (add-to-list 'web-mode-comment-formats '("tsx" . "//" ))
  (add-to-list 'web-mode-comment-formats '("jsx" . "//" )))

(add-hook 'web-mode-hook  'my-web-mode-hook)

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))

(push '("\\.styl\\'" . jade-mode) auto-mode-alist)
(push '("\\.bash_aliases\\'" . shell-script-mode) auto-mode-alist)


(push '("\\.html\\'" . web-mode) auto-mode-alist)
(push '("\\.jsx\\'" . web-mode) auto-mode-alist)
(push '("\\.tsx\\'" . web-mode) auto-mode-alist)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.babelrc" . js2-mode))

(add-hook 'scss-mode-hook 'flycheck-mode)

(global-company-mode)

(company-quickhelp-mode 1)



(require 'jade-mode)
(require 'racket-mode)
(require 'custom-bindings)
(require 'utils)

(require 'drag-stuff)
(drag-stuff-global-mode 1)


(require 'yasnippet)
(yas-global-mode 1)

(defun my-pretty-lambda ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(
          ("lambda" . 955) ; λ
          )))


(add-hook 'racket-mode-hook 'my-pretty-lambda)
(add-hook 'emacs-lisp-mode-hook 'my-pretty-lambda)
(global-prettify-symbols-mode 1)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  (add-to-list (make-local-variable 'company-backends)
               '(company-files company-tide :separate company-yasnippet)))

(add-hook 'scss-mode-hook
          (lambda ()
            (setq-local company-dabbrev-char-regexp (rx "$" (1+ (in alpha num "-"))))
            (add-to-list (make-local-variable 'company-backends)
                         '(company-files company-css company-dabbrev :separate company-yasnippet))))

(add-hook 'scss-mode-hook 'rainbow-mode)
(require 'rainbow-mode)
(add-to-list 'rainbow-html-colors-major-mode-list 'scss-mode)

(setq company-tooltip-align-annotations t)

(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t
                                                                                  :placeOpenBraceOnNewLineForFunctions nil
                                                                                  :indentSize 2
                                                                                  :tabSize 2))
;; (add-to-list 'load-path "~/Projects/tide")

;; (require 'tide)

(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

(provide 'config)
