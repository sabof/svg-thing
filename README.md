# svg-thing
An interactive demo exploring emacs vector capablities.

![screenshot](https://github.com/sabof/svg-thing/raw/master/screenshot.png)

## Installation:

- Get es-lib (from here, or melpa)
- Copy svg-thing.el to an empty buffer
- M-x eval-buffer

## (Not really) installation

- Copy this code

```lisp
    (progn
      (setq package-user-dir "/tmp/tempelpa")
      (setq package-archives
            '(("melpa" . "http://melpa.milkbox.net/packages/")))
      (package-initialize)
      (package-refresh-contents)
      (package-install 'es-lib)
      (require 'es-lib)
      (save-window-excursion
        (switch-to-buffer
         (url-retrieve-synchronously
          "https://raw.github.com/sabof/svg-thing/master/svg-thing.el" ))
        (goto-char (point-min))
        (search-forward "\n\n")
        (delete-region (point-min) (point))
        (eval-buffer))
      (svg-thing)
      (delete-other-windows))
```

- Run "emacs -Q"
- M-: C-y RET

## Instructions:

- Start: M-x svg-thing
- Add node: middle-click
- Move node: left-click and drag
- Delete node: right click
- Reset: g
- Save: M-x write-file
