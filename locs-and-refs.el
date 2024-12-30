;;; locs-and-refs.el --- Define locations and references for text buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Pierre-Henry FRÖHRING

;; Author: Pierre-Henry FRÖHRING <contact@phfrohring.com>
;; Package-Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/your-username/your-package

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This mode makes both references and locations "active" in all TextBuffers. Clicking an active
;; reference or location opens a buffer listing all buffers and files in the home directory
;; containing matching references or locations.

;; - A **TextBuffer** is a buffer derived from `text-mode` or `prog-mode`.
;; - A **Reference** is a string matching the regex `(rx "(ref +" uuid ")")`.
;; - A **Location** is a string matching the regex `(rx "(loc +" uuid ")")`.

;;; Code:

(require 'cl-lib)

(cl-defstruct (lar--Error
               (:constructor lar--make-error)
               (:copier nil))
  "Structure representing an error with a message."
  message)

(defun lar--Error-mk (msg)
  "Create an error structure with the given message MSG."
  (lar--make-error :message (or msg "")))

(cl-defstruct (lar--Interval
               (:constructor lar--make-interval)
               (:copier nil))
  "Structure representing an interval in a buffer."
  buffer start end)

(defun lar--Interval-mk (b i j)
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
             (lar--Error-mk (cadr err)))))

    (if (lar--Error-p err)
        err
      (lar--make-interval :buffer b :start i :end j))))

(defun lar--Interval-use (func)
  "Apply FUNC to the buffer, start, and end of an INTERVAL."
  (lambda (interval)
    (pcase interval
      ((cl-struct lar--Interval buffer start end)
       (funcall func buffer start end)))))

(defun lar--Interval-string (interval)
  "Return the string content of INTERVAL in its buffer."
  (funcall (lar--Interval-use (lambda (b i j) (with-current-buffer b (buffer-substring-no-properties i j)))) interval))

(defun lar--Interval-make-clickable-private (callback buffer start end)
  "Make the interval from START to END in BUFFER clickable.

When clicked, it calls CALLBACK. The overlay will evaporate after
use, and previous overlays are removed."
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

(defun lar--Interval-make-clickable (interval callback)
  "Make INTERVAL clickable by applying CALLBACK when clicked."
  (let (add-overlay)
    (setq add-overlay
          (lar--Interval-use
           (lambda (buffer start end)
             (lar--Interval-make-clickable-private callback buffer start end))))
    (funcall add-overlay interval)))

;; Define a regular expression for matching UUIDs.
(rx-define lar--uuid-rx
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

(cl-defstruct (lar--TextBuffer
               (:constructor lar--make-text-buffer)
               (:copier nil))
  "Structure representing a text buffer."
  buffer)

(defun lar--TextBuffer-mk (b)
  "Create a TextBuffer from BUFFER B if its mode derives from `text-mode' or `prog-mode'."
  (with-current-buffer b
    (if (or (derived-mode-p 'text-mode)
            (derived-mode-p 'prog-mode))
        (lar--make-text-buffer :buffer b)
      (lar--Error-mk "Buffer's major mode does not derive from text-mode or prog-mode"))))

(defun lar--TextBuffer-use (func)
  "Apply FUNC to the buffer of a TextBuffer."
  (lambda (textbuffer)
    (pcase textbuffer
      ((cl-struct lar--TextBuffer buffer)
       (funcall func buffer)))))

(defun lar--search (regex)
  "Search for REGEX in buffers and files, presenting results in a new buffer."
  (let* ((buffer-matches (lar--search-buffers regex))
         (file-matches (lar--search-files regex))
         (matches (append buffer-matches file-matches)))
    (lar--search-display regex matches)))

(defun lar--search-display (regex matches)
  "Display MATCHES for REGEX in a new buffer with clickable lines."
  (let ((results-buffer (get-buffer-create "*locs-and-refs Search Results*")))
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
               (ov (make-overlay start end)))
          (overlay-put ov 'face 'link)
          (overlay-put ov 'mouse-face 'highlight)
          (overlay-put ov 'help-echo "Click to open")
          (overlay-put ov 'keymap
                       (let ((map (make-sparse-keymap)))
                         (define-key map [mouse-1]
                                     (lambda (_event)
                                       (interactive "e")
                                       (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
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

(defun lar--search-buffers (regex)
  "Search for REGEX in all buffers, returning buffer names and match positions."
  (let (matches)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward regex nil t)
            (push (list (buffer-name buffer) (match-beginning 0)) matches)))))
    matches))

(defun lar--search-files (regex)
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
(rx-define lar--ref-rx
  (seq "(ref" (1+ " ") (group lar--uuid-rx) ")"))

(cl-defstruct (lar--Reference
               (:constructor lar--make-reference)
               (:copier nil))
  "Structure representing a reference in a buffer."
  interval uuid)

(defun lar--Reference-mk (interval uuid)
  "Create a reference from INTERVAL and UUID.
Make the interval clickable to search for matching locations."
  (lar--Interval-make-clickable interval
                                 (lambda ()
                                   (lar--search (rx "(loc" (+ " ") (literal uuid) ")"))))

  (let* ((string (lar--Interval-string interval))
         (match (string-match (rx-to-string 'lar--ref-rx) string)))
    (if match
        (let ((matched-uuid (match-string 1 string)))
          (lar--make-reference :interval interval :uuid matched-uuid))
      (lar--Error-mk "String does not match reference regex"))))

(defun lar--Reference-use (use)
  "Apply USE to the interval and UUID of a Reference."
  (lambda (reference)
    (pcase reference
      ((cl-struct lar--Reference interval uuid)
       (funcall use interval uuid)))))

;; Define regex for matching locations.
(rx-define lar--loc-rx
  (seq "(loc" (1+ " ") (group lar--uuid-rx) ")"))

(cl-defstruct (lar--Location
               (:constructor lar--make-location)
               (:copier nil))
  "Structure representing a location in a buffer."
  interval uuid)

(defun lar--Location-mk (interval uuid)
  "Create a location from INTERVAL and UUID.
Make the interval clickable to search for matching references."
  (lar--Interval-make-clickable interval
                                 (lambda ()
                                   (lar--search (rx "(ref" (+ " ") (literal uuid) ")"))))

  (let* ((string (lar--Interval-string interval))
         (match (string-match (rx-to-string 'lar--loc-rx) string)))
    (if match
        (let ((matched-uuid (match-string 1 string)))
          (lar--make-location :interval interval :uuid matched-uuid))
      (lar--Error-mk "String does not match location regex"))))

(defun lar--Location-use (use)
  "Apply USE to the interval and UUID of a Location."
  (lambda (location)
    (pcase location
      ((cl-struct lar--Location interval uuid)
       (funcall use interval uuid)))))

(cl-defstruct (lar--LiveTextBuffer
               (:constructor lar--make-live-text-buffer)
               (:copier nil))
  "Structure representing a live text buffer with locations and references."
  text-buffer locations references)

(defun lar--LiveTextBuffer-mk (text-buffer locs refs)
  "Create a LiveTextBuffer from TEXT-BUFFER, locations LOCS, and references REFS."
  (if (not (lar--TextBuffer-p text-buffer))
      (lar--Error-mk "text-buffer is not a TextBuffer.")
    (let ((buffer (lar--TextBuffer-buffer text-buffer))
          (locations (copy-sequence locs))
          (references (copy-sequence refs)))
      (with-current-buffer buffer
        (save-excursion
          (save-match-data
            (goto-char (point-min))
            (while (re-search-forward (rx-to-string 'lar--loc-rx) nil t)
              (let* ((start (match-beginning 0))
                     (end (match-end 0))
                     (interval (lar--Interval-mk buffer start end))
                     (uuid (match-string 1)))
                (push (lar--Location-mk interval uuid) locations)))
            (goto-char (point-min))
            (while (re-search-forward (rx-to-string 'lar--ref-rx) nil t)
              (let* ((start (match-beginning 0))
                     (end (match-end 0))
                     (interval (lar--Interval-mk buffer start end))
                     (uuid (match-string 1)))
                (push (lar--Reference-mk interval uuid) references))))))
      (lar--make-live-text-buffer :text-buffer text-buffer
                                 :locations (nreverse locations)
                                 :references (nreverse references)))))

(defun lar--LiveTextBuffer-use (func)
  "Apply FUNC to the components of a LiveTextBuffer."
  (lambda (livetextbuffer)
    (pcase livetextbuffer
      ((cl-struct lar--LiveTextBuffer text-buffer locations references)
       (funcall func text-buffer locations references)))))

(defun lar--activate ()
  "Activate the locs-and-refs mechanism by initializing and setting up event handlers."
  (lar--check-ripgrep)
  (lar--membrane 'init)
  (add-hook 'after-change-major-mode-hook #'lar--created)
  (add-hook 'after-change-functions #'lar--mutated))

(defun lar--created ()
  "Handle the creation of a new buffer."
  (lar--membrane `(created ,(current-buffer))))

(defun lar--mutated (_a _b _c)
  "Handle buffer mutation."
  (lar--membrane `(mutated ,(current-buffer))))

(defun lar--deactivate ()
  "Deactivate the locs-and-refs mechanism by removing hooks and cleaning up timers."
  (remove-hook 'after-change-major-mode-hook #'lar--created)
  (remove-hook 'after-change-functions #'lar--mutated)
  (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when lar--timer
            (cancel-timer lar--timer)
            (kill-local-variable 'lar--timer)))))

(defvar lar--timer nil
  "Record the last time the buffer has been modified.")
(put 'lar--timer 'permanent-local t)

(defun lar--membrane (message)
  "Handle MESSAGE for buffer activation or mutation."
  (pcase message
    (`(,symbol ,buffer)
     (let ((text-buffer (lar--TextBuffer-mk buffer)))
       (when (lar--TextBuffer-p text-buffer)
         (with-current-buffer buffer
           (pcase symbol
             ('mutated
              (when lar--timer (cancel-timer lar--timer))
              (setq-local lar--timer (run-with-idle-timer 1 nil (lambda () (lar--membrane `(created ,buffer))))))
             ('created
              (lar--LiveTextBuffer-mk text-buffer '() '())
              (setq-local lar--timer nil)))))))
    ('init
     (dolist (buffer (buffer-list))
       (lar--membrane `(created ,buffer))))))

(defun lar--check-ripgrep ()
  "Check for the presence of ripgrep, else: signal the user."
  (unless (executable-find "rg")
    (user-error "Ripgrep (rg) is not installed. Please install it to use this package")))

(define-minor-mode locs-and-refs-mode
  "Minor mode for locs-and-refs package."
  :init-value nil
  :lighter " L&R"
  :keymap nil
  :group 'convenience
  :global t
  (if locs-and-refs-mode
      (lar--activate)
    (lar--deactivate)))

(provide 'locs-and-refs)

;;; locs-and-refs.el ends here

;; Local Variables:
;; coding: utf-8
;; fill-column: 100
;; require-final-newline: t
;; sentence-end-double-space: nil
;; indent-tabs-mode: nil
;; End:
