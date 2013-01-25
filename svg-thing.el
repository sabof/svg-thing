(defvar is-st nil)
(make-variable-buffer-local 'is-st)
(defvar st-timer nil)
(defvar st-mouse-down-pos nil)
(defvar st-objects nil)

(defun st-mouse-position ()
  (let ((wpe (window-inside-pixel-edges))
        (mp (mouse-pixel-position)))
    (cons (- (cadr mp) (first wpe))
          (- (cddr mp) (second wpe)))))

(defun st-object-at (pos)
  (find-if
   (lambda (opos)
     (let ((radius 5))
       (and (and (<= (- (car opos) radius)
                     (car pos))
                 (>= (+ (car opos) radius)
                     (car pos)))
            (and (<= (- (cdr opos) radius)
                     (cdr pos))
                 (>= (+ (cdr opos) radius)
                     (cdr pos))))))
   st-objects))

(defun* st-line (positions &optional extra-props)
  (let (( line-positions
          (concat "M"
                  (mapconcat
                   (lambda (pos)
                     (format "%s %s" (car pos) (cdr pos)))
                   positions
                   " L"))))
    (format "<path d=\"%sZ\" fill=\"none\" %s />"
            line-positions (or extra-props ""))))

(defun* st-curve (positions &optional extra-props)
  (when (< (length positions) 3)
    (return-from st-curve ""))
  (let* ((init-pos (first positions))
         ( line-positions
           (format "M %s %s Q %s %s,%s %s "
                   (car (first positions))
                   (cdr (first positions))
                   (car (second positions))
                   (cdr (second positions))
                   (car (third positions))
                   (cdr (third positions))))
         ;; (positions (append positions (list init-pos)))
         )
    (setq positions (cdddr positions))
    (while positions
      (let* ((p1 (pop positions))
             (new-coords
              (format "T %s %s"
                      (car p1) (cdr p1))))
        (setq line-positions (concat line-positions new-coords))))
    (format "<path d=\"%s\" fill=\"none\" %s />"
            line-positions (or extra-props ""))))

(defun st-shift (list)
  (append (cdr (butlast list)) (list (car list))))

(defun st-even (list)
  (let (result
        switch)
    (dolist (elem list)
      (when switch
        (push elem result))
      (setq switch (not switch)))
    (nreverse result)))

(defun st-odd (list)
  (let (result
        switch)
    (dolist (elem list)
      (unless switch
        (push elem result))
      (setq switch (not switch)))
    (nreverse result)))

(defun st-set-keys ()
  (es-buffer-local-set-keys
    (kbd "<mouse-1>") 'st-on-click
    (kbd "<down-mouse-1>") 'st-mouse-down
    (kbd "<drag-mouse-1>") 'st-mouse-drag
    (kbd "g") 'st-reset))

(defun* st-redraw ()
  (let* (( wpe (window-inside-pixel-edges))
         ( content
           (format "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%s\" height=\"%s\" version=\"1.1\">\n"
                   (third wpe) (fourth wpe)))
         (circles "")
         (line1 (st-curve st-objects "stroke=\"white\""))
         (line2 (st-curve (reverse st-objects) "stroke=\"black\""))
         (objects st-objects)
         (total-objects (length st-objects)))

    (dotimes (iter total-objects)
      (let* (( obj (pop objects))
             ( color
               (cond ((or (zerop iter)
                          (= iter (1- total-objects)))
                      "green")
                     ((or (= iter 1)
                          (= iter (- total-objects 2)))
                      "blue")
                     (t "red"))))
        (setq circles
              (concat
               (format
                "<circle cx=\"%s\" cy=\"%s\" r=\"%s\" stroke=\"black\" stroke-width=\"2\" fill=\"%s\"/>\n"
                (car obj) (cdr obj) 5 color)
               circles))))
    (setq content (concat content line1 line2 circles "</svg>"))
    (fundamental-mode)
    (erase-buffer)
    (insert content)
    (es-silence-messages
     (image-mode))))

(defun st-reset-buffer ()
  (st-redraw)
  (es-disable-local-scrolling)
  (st-set-keys))

(defun st-reset ()
  (interactive)
  (setq st-objects nil)
  (st-reset-buffer))

(defun st-mouse-down (&rest ignore)
  (interactive)
  (setq st-mouse-down-pos (st-mouse-position)))

(defun st-mouse-drag (&rest ignore)
  (interactive)
  (let ((found (st-object-at st-mouse-down-pos))
        pos)
    (when found
      (setq pos (position found st-objects))
      (setf (nth pos st-objects) (st-mouse-position))
      (st-reset-buffer))))

(defun st-on-click (&rest ignore)
  (interactive)
  (es-back-push (st-mouse-position) st-objects)
  (st-reset-buffer))

(defun svg-thing-start ()
  (interactive)
  (switch-to-buffer "*svg-thing*")
  (st-reset))


(provide 'svg-thing)
;; svg-thing.el ends here