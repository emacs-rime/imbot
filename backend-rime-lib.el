;;; backend-rime-lib.el *- lexical-binding: t; -*-

(defvar imbot-backend-menu-keys `(("M-n" . 65366) ; Next PageDown
                                  ("M-p" . 65365) ; Prior PageUp
                                  ("C-n" . 65364) ; C-n Down
                                  ("C-p" . 65362) ; Up
                                  ("SPC" . 32)    ; Space
                                  ,@(mapcar (lambda (x) `(,(char-to-string x) . ,x))
                                            (number-sequence ?0 ?9))))

(defvar imbot-backend-composition-keys '(("C-d" . 65535)
                                       ("<deletechar>" . 65535)
                                       ("C-k" . (65505 65535)) ; Shift+Delete
                                       ("DEL" . 65288) ; BackSpace
                                       ("<backspace>" . 65288)
                                       ("<delete>" . 65288)
                                       ("C-b" . 65361) ; Left
                                       ("C-f" . 65363) ; Right
                                       ("C-a" . 65360) ; Home
                                       ("C-e" . 65367))); End

(defun imbot-backend-process-key (keycode mask)
  (rime-lib-process-key keycode mask))

(defun imbot-backend-update-tooltip ()
  "Build candidate menu tooltip from imbot context."
  (let* ((context (rime-lib-get-context))
         (composition (alist-get 'composition context))
         (composition-length (alist-get 'length composition))
         (preedit (alist-get 'preedit composition))
         tooltip)
    (when preedit
      ;; format preedit
      (let* ((cursor-pos (alist-get 'cursor-pos composition))
             (cursor-distance-to-end (- composition-length cursor-pos))
             ;; (select-labels (alist-get 'select-labels context))
             ;; (commit-text-preview (alist-get 'commit-text-preview context))
             ;; (sel-start (alist-get 'sel-start composition))
             ;; (sel-end (alist-get 'sel-end composition))
             ;; (input (imbot-backend-get-input))
             (menu (alist-get 'menu context))
             prompt-str page-str candidate-str-list candidate-str-draft)
        (setq prompt-str (with-temp-buffer
                           (insert preedit)
                           (backward-char cursor-distance-to-end)
                           (insert "˰")
                           (buffer-string)))
        (when menu
          (let* ((highlighted-candidate-index (alist-get 'highlighted-candidate-index menu))
                 (last-page-p (alist-get 'last-page-p menu))
                 (num-candidates (alist-get 'num-candidates menu))
                 (page-no (alist-get 'page-no menu))
                 (candidates (alist-get 'candidates menu)))
            (setq page-str (if last-page-p (format "(%s<)" (1+ page-no))
                             (format "(%s)" (1+ page-no))))
            (dolist (i (number-sequence 0 (1- num-candidates)))
              (push (if (= i highlighted-candidate-index)
                        (format "[%d%s]" (1+ i) (car (nth i candidates)))
                      (format "%d%s" (1+ i) (car (nth i candidates))))
                    candidate-str-list))))
        (setq candidate-str-list (reverse candidate-str-list))
        (setq candidate-str-draft (mapconcat #'identity candidate-str-list " "))
        (when (> (string-width candidate-str-draft) imbot-tooltip-max-width)
          (setq candidate-str-draft (mapconcat #'identity candidate-str-list "\n")))
        (setq tooltip (concat prompt-str page-str "\n" candidate-str-draft))))
    (cons tooltip (rime-lib-get-commit))))

(defun imbot-backend-get-commit ()
  (rime-lib-get-commit))

(defun imbot-backend-clear-composition ()
  (rime-lib-clear-composition))

(defun imbot-backend-get-input ()
  (rime-lib-get-input))

(defun imbot-backend-return ()
  (when-let ((input (imbot-backend-get-input)))
    (insert input)
    (imbot-backend-clear-composition)
    (imbot--update)))

(defun imbot-backend-cleanup ())

(provide 'backend-rime-lib)
