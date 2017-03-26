;;;; -*- lexical-binding: t -*-
   
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

(global-set-key (kbd "s-g") 'tide-jump-to-definition)
(global-set-key (kbd "s-=") 'company-complete)

(global-set-key (kbd "M-<up>") 'drag-stuff-up)
(global-set-key (kbd "M-<down>") 'drag-stuff-down)

(global-unset-key (kbd "M-<right>"))
(global-unset-key (kbd "M-<left>"))


(define-key jade-mode-map (kbd "M-<right>") #'(lambda () (interactive) (shift-text 2)))
(define-key jade-mode-map (kbd "M-<left>")  #'(lambda () (interactive) (shift-text -2)))


(global-set-key [f1] (lambda () (interactive) (dt:call! 'pt:load-project! "devtools-server")))

(global-set-key [f2] (lambda () (interactive) (dt:call! 'pt:load-project! "devtools-client")))
;; (global-set-key [f2] (lambda () (interactive) (dt:call! 'pt:load-project! "reputation-frontend")))
(global-set-key [f3] (lambda () (interactive) (dt:call! 'pt:load-project! "emacs-config")))





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

;; (define-key scheme-mode-map "[" 'skeleton-pair-insert-maybe)

;; (add-hook 'scheme-mode-hook (lambda ()
;;                               (define-key scheme-mode-map "[" 'skeleton-pair-insert-maybe)))


(define-key lisp-mode-map "'" 'self-insert-command)
(define-key emacs-lisp-mode-map "'" 'self-insert-command)
(define-key racket-mode-map "'" 'self-insert-command)
(define-key lisp-interaction-mode-map "'" 'self-insert-command)

(define-key emacs-lisp-mode-map (kbd "RET") #'newline-and-indent)
(define-key lisp-mode-map (kbd "RET") #'newline-and-indent)

(provide 'bindings)
