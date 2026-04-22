;;; imbot.el --- Emacs input method  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qiang Fang

;; Author: Qiang Fang
;; Keywords: convenience, input method, dbus
;; Homepage: https://github.com/QiangF/imbot
;; Created: July 24th, 2020
;; Package-Requires: ((emacs "29.1"))
;; Package-Version: 4.0

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; imbot provide an emacs input method frontend for fcitx5 via dbus
;; usage:
;; (require 'imbot)
;; (setq default-input-method "imbot")

;;; Code:

(require 'seq)
(require 'dash)
(require 'posframe)

(defgroup imbot nil
  "imbot is a smart input method"
  :group 'imbot)

(defvar imbot-backend 'backend-fcitx-dbus
  "definition for imbot-backend functions")

(require `,imbot-backend)

(defun imbot--predicate-program-mode-p ()
  (when (derived-mode-p 'prog-mode 'conf-mode)
    ;; point in comment or string
    (not (or (nth 3 (syntax-ppss))
             (nth 4 (syntax-ppss))))))

;; use shape only, eink display theme friendly
(defvar imbot--active-cursor '(hbar . 4))
(defvar imbot--inactive-cursor '(bar . 4))
(defvar imbot--inline-cursor '(hollow . 4)
  "Inline english cursor.")
(setq-default cursor-type imbot--inactive-cursor)
;; (setq-default cursor-in-non-selected-windows 'hollow)

(defvar imbot--overlay nil
  "Inline english overlay.")

(defun imbot--delete-overlay ()
  (delete-overlay imbot--overlay)
  (setq imbot--overlay nil))

(defface imbot--inline-face '((t (:weight bold :box nil :inverse-video nil)))
  "Face to show inline english (input method temperarily disabled) is active.")

(defun imbot--predicate-english-context-p ()
  "Return t if English should be inputed at cursor point."
  ;; (message "real this command %s" real-this-command)
  (unless (or (eq real-this-command 'imbot--english-inline-deactivate)
              (eq real-this-command 'imbot--english-inline-quit)
              (eq real-this-command 'toggle-input-method)
              (eq major-mode 'mistty-mode))
    (let* ((visual-line-beginning (line-beginning-position))
           (point (point))
           (overlay-active (overlayp imbot--overlay))
           (english-context
            (or
             ;; switch to English when one space follows Chinse character
             ;; \cC represents any character of category “C”, according to “M-x describe-categories”
             (looking-back "\\cC " (max visual-line-beginning (- point 2)))
             (string-match "^\\s-*[0-9]+$" (buffer-substring-no-properties visual-line-beginning point))
             ;; (looking-at-p "^\\*")    ; org heading
             (looking-back "[a-zA-Z\\_-]" (max visual-line-beginning (1- point))))))
      (if overlay-active
          (if english-context
              (progn (move-overlay imbot--overlay visual-line-beginning (line-end-position))
                     (message "Activate input method with [return]. Quit with [C-g]"))
            (imbot--delete-overlay))
        (when english-context
          (setq imbot--overlay (make-overlay visual-line-beginning (line-end-position) nil t t))
          (overlay-put imbot--overlay 'priority 900)
          (overlay-put imbot--overlay
                       'face 'imbot--inline-face)
          (overlay-put imbot--overlay
                       'keymap (let ((keymap (make-sparse-keymap)))
                                 (define-key keymap (kbd "C-g")
                                             #'imbot--english-inline-quit)
                                 (define-key keymap (kbd "RET")
                                             #'imbot--english-inline-deactivate)
                                 (define-key keymap (kbd "<return>")
                                             #'imbot--english-inline-deactivate)
                                 (define-key keymap (kbd "C-\\")
                                             #'imbot--english-inline-deactivate)
                                 keymap))))
      english-context)))

(defun imbot--english-inline-deactivate ()
  "Deactivate the inline english overlay."
  (interactive)
  (when (overlayp imbot--overlay)
    (imbot--delete-overlay)
    (setq cursor-type imbot--active-cursor))
  (setq imbot--suppressed nil))

(defun imbot--english-inline-quit ()
  "Quit the inline english overlay."
  (interactive)
  (when imbot--overlay
    (imbot--delete-overlay)
    (imbot--deactivate)))

(defvar imbot--disable-predicates
  '(imbot--predicate-english-context-p
    imbot--predicate-program-mode-p))

(defvar imbot--suppressed nil)
(make-variable-buffer-local 'imbot--suppressed)

(defun imbot--suppress-check ()
  (when (equal input-method-function 'imbot-input-method)
    (let ((suppressed (or (string-match " *temp*" (buffer-name))
                          (seq-find 'funcall imbot--disable-predicates nil))))
      ;; only run on value change
      (unless (equal imbot--suppressed suppressed)
        (if suppressed
            (progn (set-cursor-color "red")
                   (setq cursor-type imbot--inline-cursor))
          (if input-method-function
              (setq cursor-type imbot--active-cursor)
            (setq cursor-type imbot--inactive-cursor))
          ;; Another special face is the cursor face.
          ;; On graphical displays, the background color of this face is used to draw the text cursor.
          ;; None of the other attributes of this face have any effect.
          ;; As the foreground color for text under the cursor is taken from the background color of the underlying text.
          ;; On text terminals, the appearance of the text cursor is determined by the terminal, not by the cursor face.
          (custom-set-faces
           '(cursor ((t (:inherit font-lock-keyword-face))))))
        (redisplay t))
      (setq imbot--suppressed suppressed))))

(defun imbot--text-read-only-p ()
  "Return t if the text at point is read-only."
  ;; EWW: when a readonly buffer is readonly，it may still have modifiable text input field
  (and (or buffer-read-only
           ;; (get-pos-property (point) 'read-only)
           (and (get-char-property (point) 'read-only)
                (get-char-property (point) 'front-sticky)))
       (not (or inhibit-read-only
                (get-char-property (point) 'inhibit-read-only)))))

(defface imbot--tooltip-face
  '((((background light)) :background "#bfffff") (t :background "#400000"))
  "Face with a (presumably) dimmed background for popup.")

(defvar imbot--posframe-buffer " *imbot-posframe*"
  "The buffer name for candidate posframe.")

;; https://github.com/tumashu/vertico-posframe
(require 'eieio)
(defun imbot-posframe-refposhandler (&optional frame)
  "The default posframe refposhandler used by vertico-posframe.
Optional argument FRAME ."
  (cond
   ;; EXWM environment
   ((bound-and-true-p exwm--connection)
    (or (ignore-errors
          (let ((info (elt exwm-workspace--workareas
                           exwm-workspace-current-index)))
            (cons (oref info x)
                  (oref info y))))
        ;; Need user install xwininfo.
        (ignore-errors
          (posframe-refposhandler-xwininfo frame))
        ;; Fallback, this value will incorrect sometime, for example: user
        ;; have panel.
        (cons 0 0)))
   (t nil)))

;; when imbot--map-exit-function is non nil, the transitive-map is active, keys in the map not handled by input method
(defvar imbot--map-exit-function nil)
(defvar imbot--tooltip nil)

(defun imbot--map-unset ()
  (when imbot--map-exit-function
    (funcall imbot--map-exit-function)
    (setq imbot--map-exit-function nil))
  (and imbot--posframe-buffer
       (posframe-hide imbot--posframe-buffer))
  (setq imbot--tooltip nil))

(defun imbot--process-key (key state)
  (let ((handled (imbot-backend-process-key key state)))
    ;; commit is still nil when composition is active
    (if handled
        ;; commit will be inserted directly without going through the input method, has issues with isearch
        (if imbot--tooltip
            (let* ((formated-tooltip (imbot-backend-format-tooltip)))
              (posframe-show imbot--posframe-buffer
                             :refposhandler 'imbot-posframe-refposhandler
                             :background-color 'unspecified
                             :foreground-color (face-attribute 'default :foreground)
                             :border-width 1
                             :border-color (face-attribute 'default :foreground)
                             :left-fringe 5
                             :right-fringe 5
                             :y-pixel-offset 5
                             :string formated-tooltip)
              ;; set-transient-map uses overriding-terminal-local-map
              ;; save the source of overriding in a variable
              ;; when transient map is active, key binding in this map not handled by input method
              (setq imbot--map-exit-function (set-transient-map imbot--map t))
              nil)
          (imbot--map-unset))
      ;; return non-handled event
      (list key))))

(defun imbot--activate (&optional _name)
  (unless buffer-read-only
    (setq-local input-method-function 'imbot-input-method)
    (setq-local deactivate-current-input-method-function #'imbot--deactivate)
    (add-hook 'post-command-hook 'imbot--suppress-check)
    (advice-add 'keyboard-quit :after 'imbot--map-unset)
    (imbot-backend-activate)
    (add-hook 'kill-emacs-hook 'imbot-backend-cleanup)
    (setq cursor-type imbot--active-cursor)
    (redisplay t)))

(defun imbot--deactivate ()
  (kill-local-variable 'input-method-function)
  (remove-hook 'post-command-hook 'imbot--suppress-check)
  (advice-remove 'keyboard-quit 'imbot--map-unset)
  (imbot-backend-escape)
  (setq cursor-type imbot--inactive-cursor)
  (redisplay t))

(defun imbot--send-functional-key ()
  (interactive)
  (let* ((keyseq (vector last-input-event))
         (keyseq-name (key-description keyseq))
         (state 0)
         (keysym (cdr (or (assoc keyseq-name imbot-backend-menu-keys)
                          (assoc keyseq-name imbot-backend-composition-keys))))
         key)
    (if (listp keysym)
        (setq state (cadr keysym)
              key (car keysym))
      (setq key keysym))
    (imbot--process-key key state)))

(defvar imbot--map
  (let ((map (make-sparse-keymap)))
    (dolist (i (append imbot-backend-menu-keys imbot-backend-composition-keys))
      (define-key map (kbd (car i)) 'imbot--send-functional-key))
    (define-key map (kbd "C-g") 'imbot-backend-escape)
    map))

(defun imbot--special-p ()
  (if (bound-and-true-p worf-mode)
      (worf--special-p)
    (or (region-active-p)
        (looking-at outline-regexp))))

;; ref quail-input-method, some keys are not handled by input method, like the return key
(defun imbot-input-method (key)
  "Process character KEY with input method, other keys not handled."
  (if (or imbot--suppressed

           ;; When an overriding keymap is active (e.g., `set-transient-map'
           ;; used by spatial-window, avy, etc.), pass the key through if
           ;; it has a binding there.  This matches quail's behavior per
           ;; Emacs bug#68338.
           (and overriding-terminal-local-map
                (lookup-key overriding-terminal-local-map (vector key)))
           overriding-local-map
           ;; (eq (cadr overriding-terminal-local-map) universal-argument-map)
           ;; (and overriding-terminal-local-map
           ;;      (not (equal (cadr overriding-terminal-local-map) imbot--map)))

           ;; upper case letter
           ;; (and (> key 64) (< key 91))
           ;; (not (alpha-char-p key))

           (imbot--special-p)
           (imbot--text-read-only-p))
      (list key)
    (imbot--process-key key 0)))

(register-input-method "imbot" "euc-cn" 'imbot--activate "ㄓ" "smart input method")

(advice-add 'toggle-input-method :after 'posframe-hide-all)

(provide 'imbot)

;;; imbot.el ends here
