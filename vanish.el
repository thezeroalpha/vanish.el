;;; vanish.el --- Hide different parts of a buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Alex Balgavy

;; Author: Alex Balgavy
;; Homepage: https://github.com/thezeroalpha/vanish.el
;; Keywords: convenience

;; Package-Version: 0.1
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; This package lets you define regions of text to hide, such as TBLFM
;; lines and drawers, using regular expressions. It provides a minor
;; mode, `vanish-mode`, and three functions. `vanish-hide` to hide an
;; element, `vanish-unhide` to show an element, and `vanish-toggle` to
;; toggle visibility (and hide/show depending on the universal
;; argument; see its docstring). The suggested use is to bind a key to
;; `vanish-toggle` in `vanish-mode`.

;; FIXME: (add-to-list 'vanish-exprs `(comment . (:key ?c :start ,(rx bol (* blank) ";") :end ,(rx eol) :name "Comment" :include-end t)))
;;; Code:
(defgroup vanish nil
  "Customization group for `vanish'."
  :group 'convenience)

;; Specifications of regions of text to hide.
;; TODO: document include-end keyword
(defcustom vanish-exprs
  `((tblfm . (:key ?f
                   :start ,(rx bol (* blank) "#+TBLFM:")
                   :end ,(rx eol)
                   :name "TBLFM"
                   :include-end t))
    (drawer . (:key ?d
                    :start ,(rx bol (* blank) ":" (* (not ":")) ":" eol)
                    :end ,(rx bol (* blank) ":END:" eol)
                    :name "drawer"
                    :include-end nil)))
  "Expressions to hide.
Each expression has a :key (the key to press to show/hide),
:start and :end as regular expressions to determine the start/end
of the element, and a :name."
  :type '(alist :key-type symbol :value-type (plist :key-type symbol :value-type (choice (character :tag "Keybinding") (regexp :tag "Regex"))))
  :group 'vanish)

;; Keymap for vanish mode, initially empty, filled on activation
;; of minor mode.
(defvar vanish-mode-map (make-sparse-keymap))

;; Minor mode for vanish.
(define-minor-mode vanish-mode
  "Turn on Vanish mode."
  :lighter " Vanish"
  :group 'vanish
  :keymap vanish-mode-map
  ;; When vanish mode disabled, show everything that was hidden.
  (unless vanish-mode (vanish-show-all)))

;; Buffer-local list of elements that are currently hidden.
(defvar-local vanish--hidden-elements
  (list)
  "Elements currently hidden.")

(defun vanish-set-hide (element hidden)
  "Hide ELEMENT (symbol) if HIDDEN is t, show if it's nil."
    (let* ((element-vanish-expr (cdr (assoc element vanish-exprs)))
           (vanish-start-re (plist-get element-vanish-expr :start))
           (vanish-end-re (plist-get element-vanish-expr :end))
           (include-end (plist-get element-vanish-expr :include-end)))
      (save-excursion
        (let* ((beg (point-min))
               (end (point-max)))
          (goto-char beg)
          (while (re-search-forward vanish-start-re end t)
            (save-excursion
              (beginning-of-line 1)
              (when (looking-at vanish-start-re)
                (let* ((start (1- (match-beginning 0)))
                       (limit (point-max)))
                  (if (re-search-forward vanish-end-re limit t)
                      ;; `with-silent-modifications` necessary here to ensure font lock doesn't override this
                      (with-silent-modifications (put-text-property (1+ start) (if include-end (1+ (point-at-eol)) (point-at-eol)) 'invisible hidden nil))
                    (user-error "Error"))))))))

      ;; Set buffer-local list of hidden elements
      (if hidden
          (unless (memq element vanish--hidden-elements) (push element vanish--hidden-elements))
        (setq vanish--hidden-elements (remove element vanish--hidden-elements)))

      ;; Let the user know what happened
      (message "%s now %s." (plist-get element-vanish-expr :name) (if (memq element vanish--hidden-elements) "hidden" "shown"))))

(defun vanish-show-all ()
  "Show all currenltly hidden elements."
  (mapc (lambda (e) (vanish-set-hide e nil))
        vanish--hidden-elements))

(defun vanish-toggle-hide (elem)
  "Toggle hiding of ELEM."
  (let ((elem-is-hidden (memq elem vanish--hidden-elements)))
    (vanish-set-hide elem (not elem-is-hidden))))

(defun vanish--choose-elt (prompt)
  "Prompt the user to choose an element, with PROMPT."
  (let* ((opts (mapcar (lambda (e) (list (plist-get (cdr e) :key)
                                         (symbol-name (car e))))
                       vanish-exprs))
         (choice (read-multiple-choice prompt opts)))
    (intern-soft (nth 1 choice))))

(defun vanish-hide ()
  "Interactively choose an element to hide."
  (interactive)
  (vanish-set-hide (vanish--choose-elt "Hide: ") t))

(defun vanish-unhide ()
  "Interactively choose an element to show."
  (interactive)
  (vanish-set-hide (vanish--choose-elt "Show: ") nil))

(defun vanish-toggle (prefix)
  "Interactively choose an element to toggle visibility.
With PREFIX > 0, show. With a PREFIX <= 0, hide."
  (interactive "P")
  (let ((prefnum (prefix-numeric-value prefix)))
    (cond ((not prefix)
           (vanish-toggle-hide (vanish--choose-elt "Toggle visibility: ")))
          ((<= prefnum 0)
           (vanish-unhide))
          ((> prefnum 0)
           (vanish-hide)))))

(provide 'vanish)
;;; vanish.el ends here
