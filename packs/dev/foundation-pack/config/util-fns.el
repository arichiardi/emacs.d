;;; util-fns.el --- Utils  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Utility functions originally coming from Emacs Live.
;;
;;; Code:

(defun insert-date ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

(defun delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun what-face (pos)
  "Return the name of the face at point"
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond
   ((not (> (count-windows) 1)) (message "You can't rotate a single window!"))
   (t
    (let ((i 1)
          (num-windows (count-windows)))
      (while  (< i num-windows)
        (let* (
               (w1 (elt (window-list) i))
               (w2 (elt (window-list) (+ (% i num-windows) 1)))
               (b1 (window-buffer w1))
               (b2 (window-buffer w2))
               (s1 (window-start w1))
               (s2 (window-start w2))
               )
          (set-window-buffer w1  b2)
          (set-window-buffer w2 b1)
          (set-window-start w1 s2)
          (set-window-start w2 s1)
          (setq i (1+ i))))))))

(defun live-delete-whitespace-except-one ()
  (interactive)
  (just-one-space -1))


(defun live-backwards-kill-line ()
  "Kill all characters on current line before point. Same as
  passing 0 as an argument to kill-line"
  (interactive)
  (kill-line 0))

(defun live-end-of-buffer-p ()
  "Predicate fn to determine whether point is at the end of the
   buffer"
  (<= (buffer-size) (point)))

(defun live-indent-defun ()
  "Indent the current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (region-beginning) (region-end))))

(defun live-delete-and-extract-sexp ()
  "Delete the sexp and return it."
  (interactive)
  (let* ((begin (point)))
    (forward-sexp)
    (let* ((result (buffer-substring-no-properties begin (point))))
      (delete-region begin (point))
      result)))

;; Golden-ratio frame sizing on Wayland sessions only.
(defconst live-golden-ratio
  (/ (+ 1 (sqrt 5.0)) 2.0)
  "The golden ratio φ.")

(defconst live-golden-frame-scale
  0.5
  "Fraction of the monitor to fill with the golden‑ratio frame.")

(defun live-golden-frame-size (&optional frame)
  "Return a cons (WIDTH . HEIGHT) that obeys the golden‑ratio.
Computes dimensions in pixel space first, then converts to cells,
so the visual aspect ratio is correct even though chars are
rectangular.

Uses FRAME if supplied, otherwise the selected frame."
  (let* ((fm        (condition-case nil
                        (frame-monitor-attributes frame)
                      (error nil)))
         (geom      (and fm (alist-get 'geometry fm)))
         (disp-w    (or (and geom (nth 2 geom))
                        (and frame (frame-pixel-width frame))
                        (display-pixel-width)))
         (disp-h    (or (and geom (nth 3 geom))
                        (and frame (frame-pixel-height frame))
                        (display-pixel-height)))
         (char-w    (frame-char-width frame))
         (char-h    (frame-char-height frame))
         ;; scaled available area
         (avail-w   (floor (* disp-w live-golden-frame-scale)))
         (avail-h   (floor (* disp-h live-golden-frame-scale)))
         ;; golden-ratio candidates in pixels
         (cand1-w   avail-w)
         (cand1-h   (floor (/ cand1-w live-golden-ratio)))
         (cand2-h   avail-h)
         (cand2-w   (floor (* cand2-h live-golden-ratio)))
         ;; pick the one that fits
         (px-w      (if (<= cand1-h avail-h) cand1-w cand2-w))
         (px-h      (if (<= cand1-h avail-h) cand1-h cand2-h))
         ;; convert to cells
         (cells-w   (/ px-w char-w))
         (cells-h   (/ px-h char-h)))
    (cons cells-w cells-h)))

(defun live-resize-to-golden (frame)
  "Resize FRAME to golden‑ratio dimensions after it is created."
  (let* ((size      (live-golden-frame-size frame))
         (w         (car size))
         (h         (cdr size)))
    (message "live-resize-to-golden: %d×%d cells" w h)
    (set-frame-size frame w h)))

(when (string-equal "wayland" (getenv "XDG_SESSION_TYPE"))
  (add-hook 'after-make-frame-functions #'live-resize-to-golden)
  (run-at-time 0.2 nil #'live-resize-to-golden (selected-frame)))

;;; util-fns.el ends here
