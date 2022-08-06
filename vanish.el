;;; vanish.el --- Hide different parts of a buffer. -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Alex Balgavy

;; Author: Alex Balgavy
;; Homepage: https://github.com/thezeroalpha/vanish.el
;; Keywords: convenience

;; Package-Version: 0.1
;; Package-Requires: ((emacs "24.1"))

;;; Commentary:

;; This package lets you define regions of text to hide, such as TBLFM
;; lines and drawers, using regular expressions. It provides a minor
;; mode, `vanish-mode`, and creates key bindings to toggle hiding
;; the predefined parts of the buffer based on a configurable prefix
;; (by default `C-c v`).

(defgroup vanish nil
  "Customization group for `vanish'."
  :group 'convenience)

;; Specifications of regions of text to hide.
(defcustom vanish-exprs
  `((tblfm . (:key ?f
                   :start ,(rx bol (* blank) "#+TBLFM:")
                   :end ,(rx eol)
                   :name "TBLFM"))
    (drawer . (:key ?d
                    :start ,(rx bol (* blank) ":" (* (not ":")) ":" eol)
                    :end ,(rx bol (* blank) ":END:" eol)
                    :name "drawer")))
  "Expressions to hide. Each expression has a :key (the key to press to show/hide), :start and :end as regular expressions to determine the start/end of the element, and a :name."
  :type '(alist :key-type symbol :value-type (plist :key-type symbol :value-type (choice (character :tag "Keybinding") (regexp :tag "Regex"))))
  :group 'vanish)

;; Prefix key for vanish minor mode keymap
(defcustom vanish-prefix (kbd "C-c v")
  "Prefix for vanish keymap"
  :type 'key-sequence
  :group 'vanish)

;; Keymap for vanish mode, initially empty, filled on activation
;; of minor mode.
(defvar vanish-mode-map (make-sparse-keymap))

;; Minor mode for vanish.
(define-minor-mode vanish-mode
  "Turn on Vanish mode"
  :lighter " Vanish"
  :group 'vanish
  :keymap vanish-mode-map
  (vanish--initialize vanish-prefix)
  ;; When vanish mode disabled, show everything that was hidden.
  (unless vanish-mode (vanish-show-all)))

;; Buffer-local list of elements that are currently hidden.
(defvar-local vanish--hidden-elements
  (list)
  "Elements currently hidden")

(defun vanish-set-hide (element hidden)
  "Hide ELEMENT (symbol) if HIDDEN is t, show if it's nil,"
    (let* ((element-vanish-expr (cdr (assoc element vanish-exprs)))
           (vanish-start-re (plist-get element-vanish-expr :start))
           (vanish-end-re (plist-get element-vanish-expr :end)))
      (save-excursion
        (let* ((beg (point-min))
               (end (point-max)))
          (goto-char beg)
          (while (re-search-forward vanish-start-re end t)
            (save-excursion
              (beginning-of-line 1)
              (when (looking-at vanish-start-re)
                (let* ((start (1- (match-beginning 0)))
                       (limit (save-excursion
                                (outline-next-heading)
                                (point))))
                  (if (re-search-forward vanish-end-re limit t)
                      (outline-flag-region start (point-at-eol) hidden)
                    (user-error "Error"))))))))
      ;; Set org-cycle-hook
      (if hidden
          (add-hook 'org-cycle-hook #'vanish--cycle-hook nil 'local)
        (remove-hook 'org-cycle-hook #'vanish-cycle-hook 'local))

      ;; Set buffer-local list of hidden elements
      (if hidden
          (unless (memq element vanish--hidden-elements) (push element vanish--hidden-elements))
        (setq vanish--hidden-elements (remove element vanish--hidden-elements)))

      ;; Let the user know what happened
      (message "%s now %s." (plist-get element-vanish-expr :name) (if (memq element vanish--hidden-elements) "hidden" "shown"))))

(defun vanish--cycle-hook (&rest _)
  "Re-hide all currently hidden elements when Org visibility is cycled."
  (mapc (lambda (e) (vanish-set-hide e t))
        vanish--hidden-elements))

(defun vanish-show-all ()
  "Show all currenltly hidden elements."
  (mapc (lambda (e) (vanish-set-hide e nil))
        vanish--hidden-elements))

(defun vanish-toggle-hide (elem)
  "Toggle hiding of ELEM."
  (let ((elem-is-hidden (memq elem vanish--hidden-elements)))
    (vanish-set-hide elem (not elem-is-hidden))))

(defun vanish-hide (prefix elem)
  "Toggle hiding of ELEM if PREFIX is 1, hide if PREFIX <= 0, show if PREFIX > 1."
  (cond ((= prefix 1)
         (vanish-toggle-hide elem))
        ((<= prefix 0)
         (vanish-set-hide elem nil))
        ((> prefix 1)
         (vanish-set-hide elem t))))

(defun vanish--create-binding (kbd-prefix elem)
  "Create a binding for ELEM based on its corresponding `:key` in `vanish-exprs`, starting with KBD-PREFIX."
  (let* ((element-vanish-exp (cdr (assoc elem vanish-exprs)))
         (element-key (plist-get element-vanish-exp :key))
         (element-name (plist-get element-vanish-exp :name)))
    (define-key vanish-mode-map (concat kbd-prefix (char-to-string element-key))
      `(lambda (prefix)
         ,(format "Toggle hiding of %s if PREFIX is 1, hide if PREFIX <= 0, show if PREFIX > 1." element-name)
         (interactive "p") (vanish-hide prefix (quote ,elem))))))

(defun vanish--initialize (kbd-prefix)
  "Set up all key bindings starting with KBD-PREFIX"
  (mapc (lambda (e) (vanish--create-binding kbd-prefix (car e)))
        vanish-exprs))

(provide 'vanish)
;;; vanish.el ends here
