# svg-thing
An interactive demo exploring emacs vector capablities.

![screenshot](https://github.com/sabof/svg-thing/raw/master/screenshot.png)

## (Not really) Installation:

- Copy this code

```lisp
    (progn
      (setq package-user-dir
            (concat temporary-file-directory "tempelpa"))
      (setq package-archives
            '(("melpa" . "https://melpa.org/packages/")))
      (package-initialize)
      (package-refresh-contents)
      (package-install 'es-lib)
      (require 'es-lib)
      (with-current-buffer
      (url-retrieve-synchronously
           "https://raw.githubusercontent.com/sabof/svg-thing/master/svg-thing.el")
        (goto-char (point-min))
        (search-forward "\n\n")
        (delete-region (point-min) (point))
        (eval-buffer))
      (svg-thing)
      (setq-default
       mode-line-format nil)
      (tool-bar-mode -1)
      (menu-bar-mode -1)
      (scroll-bar-mode -1)
      (delete-other-windows))
```

- Run "emacs -Q"
- M-: C-y RET

## Instructions:

- Start: M-x svg-thing
- Add node: middle-click or Ctrl + left-click
- Move node: left-click and drag
- Delete node: right click
- Reset: g
- Save: M-x write-file
