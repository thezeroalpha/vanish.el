;;; vanish.el --- Hide different parts of a buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Alex Balgavy

;; Author: Alex Balgavy
;; Homepage: https://github.com/thezeroalpha/vanish.el
;; Keywords: convenience

;; Package-Version: 0.1
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; TODO: update commentary.

;; FIXME: (add-to-list 'vanish-exprs `(comment . (:key ?c :start ,(rx bol (* blank) ";") :end ,(rx eol) :name "Comment" :include-end t)))
;;; Code:
(defgroup vanish nil
  "Customization group for `vanish'."
  :group 'convenience)

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

(defun vanish--set-hidden (start end ishidden)
  "Set the `invisible` property to ISHIDDEN for text from START to END."
  ;; `with-silent-modifications` necessary here to ensure font lock doesn't override this
  (with-silent-modifications (put-text-property start end 'invisible ishidden)))

;; Hide elements
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


;; Buffer-local list of elements that are currently hidden.
(defvar-local vanish--hidden-elts
  (list)
  "Elements currently hidden.")

(defvar-local vanish--hidden-regions
  (list)
  "Regions currently hidden.")

(defun vanish--choose-elt (prompt)
  "Prompt the user to choose an element from `vanish-exprs`, with PROMPT."
  (let* ((opts (mapcar (lambda (e) (list (plist-get (cdr e) :key)
                                         (symbol-name (car e))))
                       vanish-exprs))
         (choice (read-multiple-choice prompt opts)))
    (intern-soft (nth 1 choice))))

(defun vanish--elt-set-hide (elt hidden)
  "Hide ELT (symbol in `vanish-exprs`) if HIDDEN is t, show if it's nil."
  (let* ((elt-vanish-expr (cdr (assoc elt vanish-exprs)))
         (elt-vanish-start-re (plist-get elt-vanish-expr :start))
         (elt-vanish-end-re (plist-get elt-vanish-expr :end))
         (elt-include-end (plist-get elt-vanish-expr :include-end))
         (elt-name (plist-get elt-vanish-expr :name)))
    (save-excursion
      (let* ((beg (point-min))
             (end (point-max)))
        (goto-char beg)
        (while (re-search-forward elt-vanish-start-re end t)
          (save-excursion
            (beginning-of-line 1)
            (when (looking-at elt-vanish-start-re)
              (let* ((start-of-re (1- (match-beginning 0)))
                     (limit (point-max)))
                (if (re-search-forward elt-vanish-end-re limit t)
                    (let ((elt-beg (1+ start-of-re))
                          (elt-end (if elt-include-end
                                       (1+ (point-at-eol))
                                     (point-at-eol))))
                      (vanish--set-hidden elt-beg elt-end hidden))
                  (user-error "Did not find end of element"))))))))

    ;; Update buffer-local list of hidden elements
    (if hidden
        (unless (memq elt vanish--hidden-elts)
          (push elt vanish--hidden-elts))
      (setq vanish--hidden-elts (remove elt vanish--hidden-elts)))

    ;; Let the user know what happened
    (let ((elt-hidden-or-shown (if (memq elt vanish--hidden-elts) "hidden" "shown")))
      (message "%s now %s." elt-name elt-hidden-or-shown))))

(defun vanish--region-set-hide (region hidden)
  "Hide REGION (list of min max) if HIDDEN is t, show if it's nil."
  (let ((beg (car region))
        (end (cadr region)))
    (vanish--set-hidden beg end hidden))

  (if hidden
      (unless (memq region vanish--hidden-regions)
        (push region vanish--hidden-regions))
    (setq vanish--hidden-regions (remove region vanish--hidden-regions)))

  (deactivate-mark)

  (let ((region-hidden-or-shown (if (memq region vanish--hidden-regions) "hidden" "shown")))
    (message "Region now %s." region-hidden-or-shown)))

(defun vanish-elt-hide ()
  "Interactively choose an element to hide."
  (interactive)
  (vanish--elt-set-hide (vanish--choose-elt "Hide: ") t))

(defun vanish-elt-unhide ()
  "Interactively choose an element to show."
  (interactive)
  (vanish--elt-set-hide (vanish--choose-elt "Show: ") nil))

(defun vanish-elt-toggle (prefix)
  "Interactively choose an element to toggle visibility.
With PREFIX > 0, show. With a PREFIX <= 0, hide."
  (interactive "P")
  (let ((prefnum (prefix-numeric-value prefix)))
    (cond ((not prefix)
           (let* ((elt (vanish--choose-elt "Toggle visibility: "))
                  (elt-is-hidden (memq elt vanish--hidden-elts)))
             (vanish--elt-set-hide elt (not elt-is-hidden))))
          ((<= prefnum 0)
           (vanish-elt-unhide))
          ((> prefnum 0)
           (vanish-elt-hide)))))

(defun vanish-show-all-elts ()
  "Show all currently hidden elements."
  (interactive)
  (dolist (e vanish--hidden-elts)
    (vanish--elt-set-hide e nil)))

(defun vanish-show-all-regions ()
  "Show all currently hidden regions."
  (interactive)
  (dolist (r vanish--hidden-regions) (vanish--set-hidden (car r) (cadr r) nil)))

(defun vanish-show-all ()
  "Show all currently hidden elements and regions."
  (interactive)
  (vanish-show-all-elts)
  (vanish-show-all-regions))

(defun vanish-region-hide (min max)
  "If a region is active (from MIN to MAX), hide it.
If none is active, unhide all regions."
  (interactive "r")
  (cond ((region-active-p)
         (vanish--region-set-hide (list min max) t))
        (t
         (dolist (mp vanish--hidden-regions)
           (vanish--region-set-hide mp nil)))))

(defun vanish-hide-dwim (prefix)
  "If a region is selected, hide it.
If none is selected, prompt for an element to hide.
With a PREFIX, prompt for an element to hide even if a region is active."
  (interactive "P")
  (cond (prefix
         (vanish-elt-hide))

        ((region-active-p)
         (if (region-noncontiguous-p)
             (user-error "Non-contiguous regions not yet supported"))
         (vanish--region-set-hide (list (region-beginning) (region-end)) t))

        (t (vanish-elt-hide))))


(provide 'vanish)
;;; vanish.el ends here
