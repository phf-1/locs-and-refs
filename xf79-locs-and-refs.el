;;; xf79-locs-and-refs.el --- Define locations and references for text buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Pierre-Henry FRÖHRING

;; Author: Pierre-Henry FRÖHRING <contact@phfrohring.com>
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/your-username/your-package

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is NOT part of GNU Emacs.

;; Commentary:

;; This mode makes both references and locations "active" in all TextBuffers. Clicking an active
;; reference or location opens a buffer listing all buffers and files in the home directory
;; containing matching references or locations.

;; - A **TextBuffer** is a buffer derived from `text-mode` or `prog-mode`.
;; - A **Reference** is a string matching the regex `(rx "(ref +" uuid ")")`.
;; - A **Location** is a string matching the regex `(rx "(loc +" uuid ")")`.

;; Code:

(cl-defstruct (xf79--Error
               (:constructor xf79--make-error)
               (:copier nil))
  "Structure representing an error with a message."
  message)

(defun xf79--Error-mk (msg)
  "Create an error structure with the given message MSG."
  (xf79--make-error :message (or msg "")))

(cl-defstruct (xf79--Interval
               (:constructor xf79--make-interval)
               (:copier nil))
  "Structure representing an interval in a buffer."
  buffer start end)

(defun xf79--Interval-mk (b i j)
  "Create an interval from BUFFER B, START position I, and END position J.
Return the interval if valid, or an error if not."
  (let (err)
    (setq err
          (condition-case err
              (progn
                (or (bufferp b) (error "b is not a buffer"))
                (with-current-buffer b
                  (let ((min (point-min))
                        (max (point-max)))
                    (or (and (<= min i) (<= i j)) (error "i is out of range"))
                    (or (<= j max) (error "j is out of range")))))
            (error
             (xf79--Error-mk (cadr err)))))

    (if (xf79--Error-p err)
        err
      (xf79--make-interval :buffer b :start i :end j))))

(defun xf79--Interval-use (func)
  "Apply FUNC to the buffer, start, and end of an INTERVAL."
  (lambda (interval)
    (pcase interval
      ((cl-struct xf79--Interval buffer start end)
       (funcall func buffer start end)))))

(defun xf79--Interval-buffer (interval)
  "Return the buffer associated with INTERVAL."
  (funcall (xf79--Interval-use (lambda (b i j) b)) interval))

(defun xf79--Interval-start (interval)
  "Return the start position of INTERVAL."
  (funcall (xf79--Interval-use (lambda (b i j) i)) interval))

(defun xf79--Interval-end (interval)
  "Return the end position of INTERVAL."
  (funcall (xf79--Interval-use (lambda (b i j) j)) interval))

(defun xf79--Interval-string (interval)
  "Return the string content of INTERVAL in its buffer."
  (funcall (xf79--Interval-use (lambda (b i j) (with-current-buffer b (buffer-substring-no-properties i j)))) interval))

(defun xf79--Interval-make-clickable-private (callback buffer start end)
  "Make the interval from START to END in BUFFER clickable, calling CALLBACK when clicked.
The overlay will evaporate after use, and previous overlays are removed."
  (with-current-buffer buffer
    ;; Remove any existing overlays at this location
    (dolist (ov (overlays-in start end))
      (when (overlay-get ov 'clickable)
        (delete-overlay ov)))

    ;; Create a new overlay
    (let ((overlay (make-overlay start end)))
      (overlay-put overlay 'face '(:underline t))
      (overlay-put overlay 'mouse-face 'highlight)
      (overlay-put overlay 'help-echo "Click to activate")
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'keymap
                   (let ((map (make-sparse-keymap)))
                     (define-key map [mouse-1]
                                 (lambda (event)
                                   (interactive "e")
                                   (let ((ov (car (overlays-at (posn-point (event-start event)))))
                                         (win (posn-window (event-start event))))
                                     (when (and ov (overlay-get ov 'clickable))
                                       (with-selected-window win
                                         (save-excursion
                                           (goto-char (overlay-start ov))
                                           (funcall callback)))))))
                     map))
      (overlay-put overlay 'clickable t)
      overlay)))

(defun xf79--Interval-make-clickable (interval callback)
  "Make INTERVAL clickable by applying CALLBACK when clicked."
  (let (add-overlay)
    (setq add-overlay
          (xf79--Interval-use
           (lambda (buffer start end)
             (xf79--Interval-make-clickable-private callback buffer start end))))
    (funcall add-overlay interval)))

;; Define a regular expression for matching UUIDs.
(rx-define xf79--uuid-rx
  (seq
   (and (= 8 (any hex))
        "-"
        (= 4 (any hex))
        "-"
        (= 4 (any hex))
        "-"
        (= 4 (any hex))
        "-"
        (= 12 (any hex)))))

(cl-defstruct (xf79--TextBuffer
               (:constructor xf79--make-text-buffer)
               (:copier nil))
  "Structure representing a text buffer."
  buffer)

(defun xf79--TextBuffer-mk (b)
  "Create a TextBuffer from BUFFER B if its mode derives from text-mode or prog-mode."
  (with-current-buffer b
    (if (or (derived-mode-p 'text-mode)
            (derived-mode-p 'prog-mode))
        (xf79--make-text-buffer :buffer b)
      (xf79--Error-mk "Buffer's major mode does not derive from text-mode or prog-mode"))))

(defun xf79--TextBuffer-use (func)
  "Apply FUNC to the buffer of a TextBuffer."
  (lambda (textbuffer)
    (pcase textbuffer
      ((cl-struct xf79--TextBuffer buffer)
       (funcall func buffer)))))

(defun xf79--TextBuffer-buffer (textbuffer)
  "Return the buffer associated with TEXTBUFFER."
  (funcall (xf79--TextBuffer-use (lambda (b) b)) textbuffer))

(defun xf79--search (regex)
  "Search for REGEX in buffers and files, presenting results in a new buffer."
  (let* ((buffer-matches (xf79--search-buffers regex))
         (file-matches (xf79--search-files regex))
         (matches (append buffer-matches file-matches)))
    (xf79--search-display regex matches)))

(defun xf79--search-display (regex matches)
  "Display search results for REGEX in a new buffer with clickable lines."
  (let ((results-buffer (get-buffer-create "*xf79 Search Results*")))
    (with-current-buffer results-buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert "Search Results for: " regex "\n\n")
      (dolist (match matches)
        (let* ((name (car match))
               (pos (cadr match))
               (line (if (numberp pos) (format "%s:%d" name pos) name)))
          (insert (format "%s\n" line))))

      ;; Make each line clickable
      (goto-char (point-min))
      (forward-line 1)
      (while (re-search-forward "^.+$" nil t)
        (let* ((start (match-beginning 0))
               (end (match-end 0))
               (ov (make-overlay start end))
               (line (buffer-substring-no-properties start end))
               (info (split-string line ":")))
          (overlay-put ov 'face 'link)
          (overlay-put ov 'mouse-face 'highlight)
          (overlay-put ov 'help-echo "Click to open")
          (overlay-put ov 'keymap
                       (let ((map (make-sparse-keymap)))
                         (define-key map [mouse-1]
                                     (lambda (event)
                                       (interactive "e")
                                       (let* ((pos (posn-point (event-start event)))
                                              (line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                                              (info (split-string line ":")))
                                         (if (and (> (length info) 1) (string-match-p "^/.*$" (car info))) ; Check if it's a file path with line number
                                             (progn
                                               (find-file (car info))
                                               (goto-char (point-min))
                                               (forward-line (1- (string-to-number (cadr info)))))
                                           (progn
                                             (switch-to-buffer (car info))
                                             (goto-char (string-to-number (cadr info))))))))
                         map))))

      (buffer-disable-undo results-buffer)
      (setq buffer-read-only t)
      (goto-char (point-min))
      (display-buffer results-buffer))))

(defun xf79--search-buffers (regex)
  "Search for REGEX in all buffers, returning buffer names and match positions."
  (let (matches)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward regex nil t)
            (push (list (buffer-name buffer) (match-beginning 0)) matches)))))
    matches))

(defun xf79--search-files (regex)
  "Search for REGEX in files under $HOME using ripgrep, returning file names and line numbers."
  (let* ((home-directory (expand-file-name "~"))
         (command (format "rg --line-number -H '%s' %s" (regexp-quote regex) home-directory))
         matches)
    (with-temp-buffer
      (call-process-shell-command command nil t nil)
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
               (match (split-string line ":")))
          (push (list (nth 0 match) (string-to-number (nth 1 match))) matches))
        (forward-line 1)))
    matches))

;; Define regex for matching references.
(rx-define xf79--ref-rx
  (seq "(ref" (1+ " ") (group xf79--uuid-rx) ")"))

(cl-defstruct (xf79--Reference
               (:constructor xf79--make-reference)
               (:copier nil))
  "Structure representing a reference in a buffer."
  interval uuid)

(defun xf79--Reference-mk (interval uuid)
  "Create a reference from INTERVAL and UUID. Make the interval clickable to search for matching locations."
  (xf79--Interval-make-clickable interval
                                 (lambda ()
                                   (xf79--search (rx "(loc" (+ " ") (literal uuid) ")"))))

  (let* ((string (xf79--Interval-string interval))
         (match (string-match (rx-to-string 'xf79--ref-rx) string)))
    (if match
        (let ((matched-uuid (match-string 1 string)))
          (xf79--make-reference :interval interval :uuid matched-uuid))
      (xf79--Error-mk "String does not match reference regex"))))

(defun xf79--Reference-use (use)
  "Apply USE to the interval and UUID of a Reference."
  (lambda (reference)
    (pcase reference
      ((cl-struct xf79--Reference interval uuid)
       (funcall use interval uuid)))))

(defun xf79--Reference-interval (reference)
  "Return the interval of REFERENCE."
  (funcall (xf79--Reference-use (lambda (interval uuid) interval)) reference))

(defun xf79--Reference-uuid (reference)
  "Return the UUID of REFERENCE."
  (funcall (xf79--Reference-use (lambda (interval uuid) uuid)) reference))

(defun xf79--Reference-rx ()
  "Return the regex used for matching references."
  xf79--ref-rx)

;; Define regex for matching locations.
(rx-define xf79--loc-rx
  (seq "(loc" (1+ " ") (group xf79--uuid-rx) ")"))

(cl-defstruct (xf79--Location
               (:constructor xf79--make-location)
               (:copier nil))
  "Structure representing a location in a buffer."
  interval uuid)

(defun xf79--Location-mk (interval uuid)
  "Create a location from INTERVAL and UUID. Make the interval clickable to search for matching references."
  (xf79--Interval-make-clickable interval
                                 (lambda ()
                                   (xf79--search (rx "(ref" (+ " ") (literal uuid) ")"))))

  (let* ((string (xf79--Interval-string interval))
         (match (string-match (rx-to-string 'xf79--loc-rx) string)))
    (if match
        (let ((matched-uuid (match-string 1 string)))
          (xf79--make-location :interval interval :uuid matched-uuid))
      (xf79--Error-mk "String does not match location regex"))))

(defun xf79--Location-use (use)
  "Apply USE to the interval and UUID of a Location."
  (lambda (location)
    (pcase location
      ((cl-struct xf79--Location interval uuid)
       (funcall use interval uuid)))))

(defun xf79--Location-interval (location)
  "Return the interval of LOCATION."
  (funcall (xf79--Location-use (lambda (interval uuid) interval)) location))

(defun xf79--Location-uuid (location)
  "Return the UUID of LOCATION."
  (funcall (xf79--Location-use (lambda (interval uuid) uuid)) location))

(defun xf79--Location-rx ()
  "Return the regex used for matching locations."
  xf79--loc-rx)

(cl-defstruct (xf79--LiveTextBuffer
               (:constructor xf79--make-live-text-buffer)
               (:copier nil))
  "Structure representing a live text buffer with locations and references."
  text-buffer locations references)

(defun xf79--LiveTextBuffer-mk (text-buffer locs refs)
  "Create a LiveTextBuffer from TEXT-BUFFER, LOCATIONS, and REFERENCES."
  (if (not (xf79--TextBuffer-p text-buffer))
      (xf79--Error-mk "text-buffer is not a TextBuffer.")
    (let ((buffer (xf79--TextBuffer-buffer text-buffer))
          (locations (copy-sequence locs))
          (references (copy-sequence refs)))
      (with-current-buffer buffer
        (save-excursion
          (save-match-data
            (goto-char (point-min))
            (while (re-search-forward (rx-to-string 'xf79--loc-rx) nil t)
              (let* ((start (match-beginning 0))
                     (end (match-end 0))
                     (interval (xf79--Interval-mk buffer start end))
                     (uuid (match-string 1)))
                (push (xf79--Location-mk interval uuid) locations)))
            (goto-char (point-min))
            (while (re-search-forward (rx-to-string 'xf79--ref-rx) nil t)
              (let* ((start (match-beginning 0))
                     (end (match-end 0))
                     (interval (xf79--Interval-mk buffer start end))
                     (uuid (match-string 1)))
                (push (xf79--Reference-mk interval uuid) references))))))
      (xf79--make-live-text-buffer :text-buffer text-buffer
                                 :locations (nreverse locations)
                                 :references (nreverse references)))))

(defun xf79--LiveTextBuffer-use (func)
  "Apply FUNC to the components of a LiveTextBuffer."
  (lambda (livetextbuffer)
    (pcase livetextbuffer
      ((cl-struct xf79--LiveTextBuffer text-buffer locations references)
       (funcall func text-buffer locations references)))))

(defun xf79--LiveTextBuffer-text-buffer (livetextbuffer)
  "Return the TextBuffer associated with LIVETEXTBUFFER."
  (funcall (xf79--LiveTextBuffer-use (lambda (tb locs refs) tb)) livetextbuffer))

(defun xf79--activate ()
  "Activate the xf79 mechanism by initializing and setting up event handlers."
  (xf79--check-ripgrep)
  (xf79--membrane 'init)
  (add-hook 'after-change-major-mode-hook #'xf79--created)
  (add-hook 'after-change-functions #'xf79--mutated))

(defun xf79--created ()
  "Handle the creation of a new buffer."
  (xf79--membrane `(created ,(current-buffer))))

(defun xf79--mutated (a b c)
  "Handle buffer mutation."
  (xf79--membrane `(mutated ,(current-buffer))))

(defun xf79--deactivate ()
  "Deactivate the xf79 mechanism by removing hooks and cleaning up timers."
  (remove-hook 'after-change-major-mode-hook #'xf79--created)
  (remove-hook 'after-change-functions #'xf79--mutated)
  (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when xf79--timer
            (cancel-timer xf79--timer)
            (kill-local-variable 'xf79--timer)))))

(defvar xf79--timer nil
  "Record the last time the buffer has been modified.")
(put 'xf79--timer 'permanent-local t)

(defun xf79--membrane (message)
  "Handle messages for buffer activation or mutation."
  (pcase message
    (`(,symbol ,buffer)
     (let ((text-buffer (xf79--TextBuffer-mk buffer)))
       (when (xf79--TextBuffer-p text-buffer)
         (with-current-buffer buffer
           (pcase symbol
             ('mutated
              (when xf79--timer (cancel-timer xf79--timer))
              (setq-local xf79--timer (run-with-idle-timer 1 nil (lambda () (xf79--membrane `(created ,buffer))))))
             ('created
              (xf79--LiveTextBuffer-mk text-buffer '() '())
              (setq-local xf79--timer nil)))))))
    ('init
     (dolist (buffer (buffer-list))
       (xf79--membrane `(created ,buffer))))))

(defun xf79--check-ripgrep ()
  (unless (executable-find "rg")
    (user-error "Ripgrep (rg) is not installed. Please install it to use this package.")))

(define-minor-mode locs-and-refs-mode
  "Minor mode for locs-and-refs package."
  :init-value nil
  :lighter " L&R"
  :keymap nil
  :global t
  (if locs-and-refs-mode
      (xf79--activate)
    (xf79--deactivate)))

(provide 'xf79-locs-and-refs)

;;; xf79-locs-and-refs.el ends here

;; Local Variables:
;; coding: utf-8
;; fill-column: 100
;; require-final-newline: t
;; sentence-end-double-space: nil
;; indent-tabs-mode: nil
;; End:
