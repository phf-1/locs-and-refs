;;; locs-and-refs.el --- Define locations and references for text buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Pierre-Henry FRÖHRING

;; Author: Pierre-Henry FRÖHRING <contact@phfrohring.com>
;; Package-Version: 0.5
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/phf-1/locs-and-refs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This mode makes both references and locations "active" in all TextBuffers. Clicking an active
;; reference or location opens a buffer listing all buffers and files in the home directory
;; containing matching references or locations.

;; - A TextBuffer is a buffer derived from `text-mode` or `prog-mode`.
;; - A Reference is a string matching the regex `(rx "(ref +" (literal uuid) ")")`.
;; - A Location is a string matching the regex `(rx "(loc +" (literal uuid) ")")`.

;;; Code:

(defun lar--is-a (x symbol)
  "Given any object X, test if X has been built using a constructor of the type with tag SYMBOL."
  (unless (atom symbol) (error "Variable symbol is not an Atom"))
  (eq (car-safe x) symbol))

(defconst lar--error-tag :error
  "The tag associated with the type Error.")

(defun lar--error (msg)
  "An error has a message MSG."
  (unless (stringp msg) (error "Variable msg is not a String"))
  (list lar--error-tag msg))

(defun lar--error-p (x)
  "Test if X is an error."
  (lar--is-a x lar--error-tag))

(defun lar--error-use (func)
  "Define how to build function defined on errors.
FUNC is a function that takes the error message as an input."
  (lambda (err)
    (pcase err
      ((and `(,tag ,msg) (guard (eq tag lar--error-tag)))
       (funcall func msg))
      (:core func)
      (_ (error "Variable err is not an Error")))))

(defun lar--error-message (err)
  "Return the message associated with the error ERR."
  (funcall (lar--error-use (lambda (msg) msg)) err))

(defconst lar--interval-tag :interval
  "The tag associated with the type Interval.")

(defun lar--interval (buffer start end)
  "An interval represents a contiguous sequence of characters from START to END in BUFFER."
  (unless (bufferp buffer) (error "Variable buffer is not a Buffer"))
  (unless (integerp start) (error "Variable start is not an Integer"))
  (unless (integerp end) (error "Variable end is not an Integer"))
  (let (err)
    (setq err
          (condition-case err
              (progn
                (with-current-buffer buffer
                  (let ((min (point-min))
                        (max (point-max)))
                    (unless (and (<= min start) (<= start end)) (error "Variable start is out of range"))
                    (unless (<= end max) (error "Variable end is out of range")))))
            (error
             (lar--error (cadr err)))))
    (if (lar--error-p err)
        err
      (list lar--interval-tag buffer start end))))

(defun lar--interval-p (interval)
  "Test if INTERVAL is an Interval structure."
  (lar--is-a interval lar--interval-tag))

(defun lar--interval-use (func)
  "Define how to build function defined on intervals.
FUNC takes the message of the arror as input."
  (lambda (interval)
    (pcase interval
      ((and `(,tag ,buffer ,start ,end) (guard (eq tag lar--interval-tag)))
       (funcall func buffer start end))
      (:core func)
      (_ (error "Variable interval is not an Interval")))))

(defun lar--interval-buffer (interval)
  "Return the buffer associated with INTERVAL."
  (funcall (lar--interval-use (lambda (buffer _start _end) buffer)) interval))

(defun lar--interval-start (interval)
  "Return the start position associated with INTERVAL."
  (funcall (lar--interval-use (lambda (_buffer start _end) start)) interval))

(defun lar--interval-end (interval)
  "Return the end position associated with INTERVAL."
  (funcall (lar--interval-use (lambda (_buffer _start end) end)) interval))

(defun lar--interval-string (interval)
  "Return the string associated with INTERVAL."
  (funcall (lar--interval-use (lambda (buffer start end) (with-current-buffer buffer (buffer-substring-no-properties start end)))) interval))

(defun lar--interval-make-clickable-private (callback buffer start end)
  "Make the region inside START and END of BUFFER clickable.
After a click, CALLBACK is called without arguments."
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
      (overlay-put overlay 'clickable t)
      (overlay-put overlay 'keymap
                   (let ((map (make-sparse-keymap)))
                     (define-key map [mouse-1]
                                 (lambda (_event)
                                   (interactive "e")
                                   (funcall callback)))
                     map))
      overlay)))

(defun lar--interval-make-clickable (interval callback)
  "Make INTERVAL clickable.
After a click on INTERVAL, CALLBACK is called without arguments."
  (let (add-overlay)
    (setq add-overlay
          (lar--interval-use
           (lambda (buffer start end)
             (lar--interval-make-clickable-private callback buffer start end))))
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

(defconst lar--text-buffer-tag :text-buffer
  "The tag associated with the type TextBuffer.")

(defun lar--text-buffer (buffer)
  "A TextBuffer represents a BUFFER that is not killed and derives from `text-mode' or `prog-mode'."
  (unless (bufferp buffer) (error "Variable buffer is not a Buffer"))
  (if (buffer-live-p buffer)
      (with-current-buffer buffer
        (if (or (derived-mode-p 'text-mode)
                (derived-mode-p 'prog-mode))
            (list lar--text-buffer-tag buffer)
          (lar--error "Buffer's major mode does not derive from text-mode or prog-mode")))
    (lar--error "Buffer has been killed")))

(defun lar--text-buffer-p (x)
  "Test if X is a TextBuffer."
  (lar--is-a x lar--text-buffer-tag))

(defun lar--text-buffer-use (func)
  "Define how to build function defined on text buffers.
FUNC is a function that takes the text-buffer's buffer as input."
  (lambda (text-buffer)
    (pcase text-buffer
      ((and `(,tag ,buffer) (guard (eq tag lar--text-buffer-tag)))
       (funcall func buffer))
      (:core func)
      (_ (error "Variable text-buffer is not a TextBuffer")))))

(defun lar--text-buffer-buffer (text-buffer)
  "Return the buffer associated with TEXT-BUFFER."
  (funcall (lar--text-buffer-use (lambda (buffer) buffer)) text-buffer))

(defun lar--search (regex)
  "Search for REGEX in buffers and files, presenting results in a new buffer."
  (let* ((buffer-matches (lar--search-buffers regex))
         (file-matches (lar--search-files regex))
         (matches (append buffer-matches file-matches)))
    (lar--search-display regex matches)))

(defun lar--search-display (regex matches)
  "Display MATCHES for REGEX in a new buffer with clickable lines."
  (unless (listp matches) (error "Variable matches is not a List"))
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

;; TODO (loc 23b15a13-5c26-4af6-ad57-117723fe03c7)
;;
;;   lar--bookmark* are macro definitions used to define locations and references.
;;   These macros capture what is common about locations and references since
;;   structures of locations and references are almost identical. There might be a
;;   way to build locations; then references from locations, re-using common
;;   structures. For instance:
;;
;;   given:
;;     f : Location → C
;;
;;   we have:
;;     f ≡ loc-use φ, where φ : Interval UUID → C
;;     φ ≡ f :core
;;
;;   so:
;;     f' :≡ ref-use f :core
;;     f' : Reference → C
;;     f' :core ≡ f :core
;;
;;   then:
;;     For all f : Location → C, we may build f' : Reference → C using identical core
;;     functions. In other words, references may be defined re-using locations for
;;     the most part without having to use macros.

(defmacro lar--bookmark-rx (bookmark-name)
  "Define a `rx' named lar--BOOKMARK-TYPE-rx."
  `(rx-define ,(intern (concat "lar--" bookmark-name "-rx"))
     (seq ,(concat "(" bookmark-name) (1+ " ") (group lar--uuid-rx) ")")))

(defmacro lar--bookmark-tag (bookmark-name)
  "Define the tag associated with the type named BOOKMARK-TYPE."
  `(defconst ,(intern (concat "lar--" bookmark-name "-tag"))
     ,(intern (concat ":" bookmark-name))))

(defmacro lar--bookmark (name matching-name)
  "Define a constructor for a bookmark type named NAME that matches MATCHING-NAME."
  `(defun ,(intern (concat "lar--"  name)) (interval)
     (unless (lar--interval-p interval) (error "Variable interval is not an Interval"))
     (let (struct search)
       (let* ((string (lar--interval-string interval))
              (match (string-match (rx-to-string ',(intern (concat "lar--" name "-rx"))) string)))
         (if match
             (progn
               (setq struct (list ,(intern (concat "lar--" name "-tag")) interval (match-string 1 string)))
               (setq search (lambda ()
                              (lar--search (rx ,(concat "(" matching-name) (+ " ") (literal (,(intern (concat "lar--"  name "-uuid")) struct)) ")"))))
               (lar--interval-make-clickable interval search)
               struct)
           (lar--error "The string of the interval does not match the regex"))))))

(defmacro lar--bookmark-use (name)
  "Define how to build a function defined on bookmarks named NAME."
`(defun ,(intern (concat "lar--" name "-use")) (use)
  (lambda (struct)
    (pcase struct
      ((and `(,tag ,interval ,uuid) (guard (eq tag ,(intern (concat "lar--" name "-tag")))))
       (funcall use interval uuid))
      (:core func)
      (_ (error "Unexpected input"))))))

(defmacro lar--bookmark-interval (name)
  "Return the interval associated with an instance of bookmark named NAME."
  `(defun ,(intern (concat "lar--" name "-interval")) (bookmark)
  (funcall (,(intern (concat "lar--" name "-use")) (lambda (interval _uuid) interval)) bookmark)))

(defmacro lar--bookmark-uuid (name)
  "Return the uuid associated with an instance of bookmark named NAME."
  `(defun ,(intern (concat "lar--" name "-uuid")) (bookmark)
  (funcall (,(intern (concat "lar--" name "-use")) (lambda (_interval uuid) uuid)) bookmark)))

;; Defines Location.
;; A location may be refered to by a Reference.
(lar--bookmark-rx "loc")
(lar--bookmark-tag "loc")
(lar--bookmark "loc" "ref")
(lar--bookmark-use "loc")
(lar--bookmark-interval "loc")
(lar--bookmark-uuid "loc")

;; Defines Reference.
;; A reference may refer to a Location.
(lar--bookmark-rx "ref")
(lar--bookmark-tag "ref")
(lar--bookmark "ref" "loc")
(lar--bookmark-use "ref")
(lar--bookmark-interval "ref")
(lar--bookmark-uuid "ref")

(defconst lar--live-text-buffer-tag :live-text-buffer
  "The tag associated with the type LiveTextBuffer.")

(defun lar--live-text-buffer (text-buffer)
  "The LiveTextBuffer built from TEXT-BUFFER has all its locations and references clickable."
  (unless (lar--text-buffer-p text-buffer) (error "Variable text-buffer is not a TextBuffer"))
  (if (not (lar--text-buffer-p text-buffer))
      (lar--error "Variable text-buffer is not a TextBuffer")
    (let ((buffer (lar--text-buffer-buffer text-buffer))
          (locations '())
          (references '()))
      (with-current-buffer buffer
        (save-excursion
          (save-match-data
            (goto-char (point-min))
            (while (re-search-forward (rx-to-string 'lar--loc-rx) nil t)
              (let* ((start (match-beginning 0))
                     (end (match-end 0))
                     (interval (lar--interval buffer start end)))
                (push (lar--loc interval) locations)))
            (goto-char (point-min))
            (while (re-search-forward (rx-to-string 'lar--ref-rx) nil t)
              (let* ((start (match-beginning 0))
                     (end (match-end 0))
                     (interval (lar--interval buffer start end)))
                (push (lar--ref interval) references))))))
      (list lar--live-text-buffer-tag text-buffer (nreverse locations) (nreverse references)))))

(defun lar--live-text-buffer-use (func)
  "Define how to build function defined on LiveTextBuffer.
FUNC takes the live-text-buffer's text-buffer, locations and references as input."
  (lambda (live-text-buffer)
    (pcase live-text-buffer
      ((and `(,tag ,text-buffer ,locations ,references) (guard (eq tag lar--live-text-buffer-tag)))
       (funcall func text-buffer locations references))
      (:core func)
      (_ (error "Variable live-text-buffer is not a LiveTextBuffer")))))

(defvar lar--timer nil
  "Record the last time the buffer has been modified.")
(put 'lar--timer 'permanent-local t)

(defun lar--membrane (message)
  "Handle MESSAGE sent from Emacs.

If MESSAGE matches:
  `init, then: all buffers of Emacs are made LiveTextBuffer if possible.
  (list `created buffer), then: buffer is made a LiveTextBuffer.
  (list `mutated buffer), then: buffer is made a LiveTextBuffer."

  (pcase message
    (`(,symbol ,buffer)
     (let ((text-buffer (lar--text-buffer buffer)))
       (when (lar--text-buffer-p text-buffer)
         (with-current-buffer buffer
           (pcase symbol
             ('mutated
              (when lar--timer (cancel-timer lar--timer))
              (setq-local lar--timer (run-with-idle-timer 1 nil (lambda () (lar--membrane `(created ,buffer))))))
             ('created
              (lar--live-text-buffer text-buffer)
              (setq-local lar--timer nil)))))))
    ('init
     (dolist (buffer (buffer-list))
       (lar--membrane `(created ,buffer))))))

(defun lar--activate ()
  "Make Emacs turn all its buffers into LiveTextBuffer, if possible."
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
  "Deactivate the locs-and-refs mechanism."
  (remove-hook 'after-change-major-mode-hook #'lar--created)
  (remove-hook 'after-change-functions #'lar--mutated)
  (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when lar--timer
            (cancel-timer lar--timer)
            (kill-local-variable 'lar--timer)))))

(defun lar--check-ripgrep ()
  "Check for the presence of ripgrep, else: signal the user."
  (unless (executable-find "rg")
    (user-error "Ripgrep (rg) is not installed. Please install it to use this package")))

;;;###autoload
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
