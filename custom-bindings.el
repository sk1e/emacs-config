;;;; -*- lexical-binding: t -*-
(require 'smerge-mode)
(require 'web-mode)
(require 'typescript-mode)

(defun shift-text (distance)
  (if (use-region-p)
      (let ((mark (mark)))
        (save-excursion
          (indent-rigidly (region-beginning)
                          (region-end)
                          distance)
          (push-mark mark t t)
          (setq deactivate-mark nil)))
    (indent-rigidly (line-beginning-position)
                    (line-end-position)
                    distance)))


;; (setq display-time-day-and-date t)
;; (setq display-time-format "%I:%M %p %e %b %y %_5j")
;; ;; (display-time)
;; (setq display-time-default-load-average nil)

(global-set-key (kbd "C-,") #'(lambda () (interactive) (scroll-down 3)))
(global-set-key (kbd "C-.") #'(lambda () (interactive) (scroll-up 3)))

;; (setq flycheck-javascript-eslint-executable "/home/kotik/Projects/life/node_modules/.bin/eslint")
;; (global-set-key (kbd "'") 'skeleton-pair-insert-maybe)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-f") 'ido-find-file)

(global-set-key (kbd "C-c s") (lambda ()
                                (interactive)
                                (delete-other-windows)
                                (magit-status)))

(global-set-key (kbd "C-`") #'(lambda () (interactive) (insert "`")))
(global-set-key (kbd "s-t") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-9") 'eval-buffer)
(global-set-key (kbd "C-;") 'eval-last-sexp)

(global-set-key (kbd "M-d") 'windmove-right)
(global-set-key (kbd "M-a") 'windmove-left)
(global-set-key (kbd "M-w") 'windmove-up)
(global-set-key (kbd "M-s") 'windmove-down)

(global-set-key (kbd "C-q") 'delete-other-windows)

(global-set-key (kbd "C-d") '(lambda ()
                               (interactive)
                               (describe-function (function-called-at-point))))

(global-set-key (kbd "s-r") 'comment-or-uncomment-region)

(global-set-key (kbd "s-x") 'clipboard-kill-region)
(global-set-key (kbd "s-c") 'clipboard-kill-ring-save)
(global-set-key (kbd "s-v") 'clipboard-yank)


(global-set-key (kbd "s-=") 'company-complete)

(global-set-key (kbd "M-<up>") 'drag-stuff-up)
(global-set-key (kbd "M-<down>") 'drag-stuff-down)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-S-s") 'swiper-all)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

(global-set-key (kbd "C-=") 'er/expand-region)

(global-unset-key (kbd "M-<right>"))
(global-unset-key (kbd "M-<left>"))


(define-key jade-mode-map (kbd "M-<right>") #'(lambda () (interactive) (shift-text 2)))
(define-key jade-mode-map (kbd "M-<left>")  #'(lambda () (interactive) (shift-text -2)))

(mapc (lambda (mode-map)
        (define-key mode-map (kbd "s-<f11>") 'tide-jump-to-definition)
        (define-key mode-map (kbd "s-<f12>") 'tide-jump-back)
        (define-key mode-map (kbd "s-f") 'tide-fix)
        (define-key mode-map (kbd "C-c r") 'tide-rename-symbol))
      (list web-mode-map typescript-mode-map))

(global-set-key [f1] (lambda () (interactive) (pt:load-project-from-string! "moneytoken")))
(global-set-key [f2] (lambda () (interactive) (pt:load-project-from-string! "ipsos-online-polls")))
(global-set-key [f3] (lambda () (interactive) (pt:load-project-from-string! "emacs-config")))


;; (let ((glob-map global-map))
;;   (cl-defun kb-disable-non-ctl-x-global-keymaps ()
;;     (interactive)
;;     (use-global-map (make-sparse-keymap))
;;     (global-set-key "\C-x" ctl-x-map))

;;   (cl-defun kb-restore-global-map ()
;;     (interactive)
;;     (use-global-map glob-map)))



(global-set-key [f9] 'flycheck-previous-error)
(global-set-key [f10] 'flycheck-next-error)

(setq skeleton-pair t)
(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)
(global-set-key "\"" 'skeleton-pair-insert-maybe)
(global-set-key "'" 'skeleton-pair-insert-maybe)
(global-set-key "`" (lambda ()
                      (interactive)
                      (let ((last-command-event ?\())
                        (call-interactively 'skeleton-pair-insert-maybe))))


(add-hook 'racket-mode-hook (lambda () (define-key racket-mode-map "[" 'skeleton-pair-insert-maybe)))

(setq smerge-mode-local-map
      (let ((smerge-mode-local-map (make-sparse-keymap)))
        (define-key smerge-mode-local-map (kbd "d") #'smerge-next)
        (define-key smerge-mode-local-map (kbd "a") #'smerge-prev)
        (define-key smerge-mode-local-map (kbd "m") #'smerge-keep-mine)
        (define-key smerge-mode-local-map (kbd "o") #'smerge-keep-other)
        (define-key smerge-mode-local-map (kbd "b") #'smerge-keep-all)
        (define-key smerge-mode-local-map (kbd "u") #'undo)
        (define-key smerge-mode-local-map (kbd "`") (lambda ()
                                                      (interactive)
                                                      (use-global-map global-map)))
        smerge-mode-local-map))

;; (set (make-local-variable 'project-buffer-modified-p) nil)


(defun smerge-activate-local-map ()
  (interactive)
  (use-global-map smerge-mode-local-map))


(define-key smerge-mode-map (kbd "C-c m") 'smerge-activate-local-map)

(define-key lisp-mode-map "'" 'self-insert-command)
(define-key emacs-lisp-mode-map "'" 'self-insert-command)
(define-key emacs-lisp-mode-map (kbd "C-j") 'eval-print-last-sexp)


(define-key racket-mode-map "'" 'self-insert-command)
(define-key lisp-interaction-mode-map "'" 'self-insert-command)

(define-key emacs-lisp-mode-map (kbd "RET") #'newline-and-indent)
(define-key lisp-mode-map (kbd "RET") #'newline-and-indent)

(provide 'custom-bindings)
