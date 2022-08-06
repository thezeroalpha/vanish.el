# vanish.el: hide different parts of a buffer
This package lets you define regions of text to hide, such as TBLFM lines and drawers, using regular expressions. It provides a minor mode, `vanish-mode`, and creates key bindings to toggle hiding the predefined parts of the buffer based on a configurable prefix (by default `C-c v`).

## Demo

https://user-images.githubusercontent.com/8124851/183263651-0ec06d41-53c1-426f-967b-b82d66a99d38.mp4

## Installation
Several options:

- Download it directly and put it in your `load-path`
- Use quelpa:

    ```emacs-lisp
    (quelpa '(vanish :repo "thezeroalpha/vanish.el" :fetcher github))
    (require 'vanish)
    ```
- Use `use-package` with `quelpa-use-package`:

    ```emacs-lisp
    (use-package vanish
        :quelpa (vanish :repo "thezeroalpha/vanish.el" :fetcher github)
        :ensure nil)
    ```
- Use whatever package manager you prefer, as long as you can install from a Github source

## Configuration
After changing variables, disable and re-enable `vanish-mode` to update the keymap.

To change the vanish prefix from `C-c v` to e.g. `C-c q h`:

``` emacs-lisp
(custom-set-variables '(vanish-prefix (kbd "C-c q h") 'now))
```

To add the option to hide Elisp comments:

```emacs-lisp
(add-to-list 'vanish-exprs `(elispcomment . (:key ?c :name "Elisp comment" :start ,(rx bol (* blank) ";") :end ,(rx eol))))
```

## Usage
Enable `vanish-mode`, then you can hide various parts of the buffer using (by default) `C-c v KEY` where `KEY` is a key defined in `vanish-exprs`.
For example, hide `#+TBLFM` lines in Org mode with `C-c v f`.
