;;; imbot.el --- Emacs input method  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Qiang Fang

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

;; imbot provide an emacs input method using fcitx5 through dbus or librime with a dynamic module.
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
  "当前为`prog-mode'或`conf-mode'，且光标在注释或字符串当中。"
  (when (derived-mode-p 'prog-mode 'conf-mode)
    (not (or (nth 3 (syntax-ppss))
             (nth 4 (syntax-ppss))))))

(defvar imbot--default-cursor '(bar . 4))
(setq-default cursor-type imbot--default-cursor)
(setq-default cursor-in-non-selected-windows 'hollow)

(defvar imbot--overlay nil
  "Inline english overlay.")

(defun imbot--delete-overlay ()
  (delete-overlay imbot--overlay)
  (setq cursor-type imbot--default-cursor)
  (setq imbot--overlay nil))

(defvar imbot--inline-cursor '(hbar . 4)
  "Inline english cursor.")

(defface imbot--inline-face '((t (:weight bold :box nil :inverse-video nil)))
  "Face to show inline english (input method temperarily disabled) is active.")

(defun imbot--predicate-english-context-p ()
  "Return t if English should be inputed at cursor point."
  ;; (message "real this command %s" real-this-command)
  (unless (or (eq real-this-command 'imbot--english-inline-deactivate)
              (eq real-this-command 'imbot--english-inline-quit)
              (eq real-this-command 'toggle-input-method))
    (let* ((visual-line-beginning (line-beginning-position))
           (point (point))
           (overlay-active (overlayp imbot--overlay))
           (english-context
            (or
             ;; 中文后面紧接1个空格切换到英文输入
             ;; \cC represents any character of category “C”, according to “M-x describe-categories”
             (looking-back "\\cC " (max visual-line-beginning (- point 2)))
             (string-match "^\\s-*[0-9]+$" (buffer-substring-no-properties visual-line-beginning point))
             ;; (looking-at-p "^\\*")    ; org heading
             (looking-back "[a-zA-Z\\-]" (max visual-line-beginning (1- point))))))
      (if overlay-active
          (if english-context
              (progn (move-overlay imbot--overlay visual-line-beginning (line-end-position))
                     (message "Activate input method with [return]. Quit with [C-g]"))
            (imbot--delete-overlay))
        (when english-context
          (setq imbot--overlay (make-overlay visual-line-beginning (line-end-position) nil t t))
          (setq cursor-type imbot--inline-cursor)
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
    (imbot--delete-overlay))
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
  (if (equal input-method-function 'imbot-input-method)
      (let ((suppressed (or (string-match " *temp*" (buffer-name))
                            (seq-find 'funcall imbot--disable-predicates nil))))
        (set-cursor-color "red")
        (if suppressed
            (setq cursor-type imbot--inline-cursor)
          (setq cursor-type imbot--default-cursor))
        (unless (equal imbot--suppressed suppressed)
          (redisplay t)
          (setq imbot--suppressed suppressed)))
    (imbot--restore-cursor)))

(defun imbot--text-read-only-p ()
  "Return t if the text at point is read-only."
  ;; NOTE: 在 widget 输入框存在的情况下，即使 buffer 是只读的，widget 输入
  ;; 框也有可能要输入文本，EWW 就存在类似情况。
  (and (get-pos-property (point) 'read-only)
       (not (or inhibit-read-only
                (get-pos-property (point) 'inhibit-read-only)))))

(defface imbot--tooltip-face
  '((((background light)) :background "#bfffff") (t :background "#400000"))
  "Face with a (presumably) dimmed background for popup.")

(defvar imbot--posframe-buffer " *imbot-posframe*"
  "The buffer name for candidate posframe.")

(defun imbot--tooltip-posframe (tooltip)
  (if tooltip
      (posframe-show imbot--posframe-buffer
                   :foreground-color (face-attribute 'imbot--tooltip-face :foreground)
                   :background-color (face-attribute 'imbot--tooltip-face :background)
                   :string tooltip)
    (posframe-hide imbot--posframe-buffer)))

(defvar imbot--overriding nil)

(defun imbot--map-set ()
  (setq overriding-terminal-local-map imbot--map)
  (setq imbot--overriding t))

(defun imbot--map-unset ()
  (setq overriding-terminal-local-map nil)
  (setq imbot--overriding nil))

(defun imbot--update (key state)
  (let ((handled (imbot-backend-process-key key state)))
    ;; with-silent-modifications
    (unwind-protect
        ;; commit is still nil when composition is active
        (if handled
            (let* ((output (imbot-backend-update-tooltip))
                   (tooltip (car output))
                   (commit (cdr output)))
              (if tooltip
                  (imbot--map-set)
                (imbot--map-unset))
              (imbot--tooltip-posframe tooltip)
              (when commit
                (setq fcitx-ic-commit-string nil)
                (insert commit)))
          (list key)))))

(defun imbot--activate (&optional _name)
  (unless buffer-read-only
    (setq-local input-method-function 'imbot-input-method)
    (setq-local deactivate-current-input-method-function #'imbot--deactivate)
    (add-hook 'post-command-hook 'imbot--suppress-check)
    (advice-add 'keyboard-quit :after 'imbot--map-unset)
    (imbot-backend-activate)
    (add-hook 'kill-emacs-hook 'imbot-backend-cleanup)
    (redisplay t)))

;; Another special face is the cursor face.
;; On graphical displays, the background color of this face is used to draw the text cursor.
;; None of the other attributes of this face have any effect.
;; As the foreground color for text under the cursor is taken from the background color of the underlying text.
;; On text terminals, the appearance of the text cursor is determined by the terminal, not by the cursor face.
(defun imbot--restore-cursor ()
  (custom-set-faces
   '(cursor ((t (:inherit font-lock-keyword-face)))))
  (setq cursor-type imbot--default-cursor))

(defun imbot--deactivate ()
  (kill-local-variable 'input-method-function)
  (remove-hook 'post-command-hook 'imbot--suppress-check)
  (advice-remove 'keyboard-quit 'imbot--map-unset)
  (imbot-backend-escape)
  (imbot-backend-focusout)
  (imbot--restore-cursor)
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
    ;; (message "function key is %s" keysym)
    (imbot--update key state)))

(defvar imbot--map
  (let ((map (make-sparse-keymap)))
    (dolist (i (append imbot-backend-menu-keys imbot-backend-composition-keys))
      (define-key map (kbd (car i)) 'imbot--send-functional-key))
    (define-key map (kbd "C-g") 'imbot-backend-escape)
    map))

(defun imbot-input-method (key)
  "Process character KEY with input method, other keys not handled."
  (if (or imbot--suppressed
          ;; (lookup-key overriding-terminal-local-map (vector key))
          ;; (eq (cadr overriding-terminal-local-map) universal-argument-map)
          ;; (and overriding-terminal-local-map
          ;;      (not (equal (cadr overriding-terminal-local-map) imbot--map)))
          (and (or overriding-local-map overriding-terminal-local-map)
               (not imbot--overriding))
          ;; upper case letter
          ;; (and (> key 64) (< key 91))
          ;; (not (alpha-char-p key))
          (imbot--text-read-only-p))
      (list key)
    (imbot--update key 0)))

(register-input-method "imbot" "euc-cn" 'imbot--activate "ㄓ" "smart input method")

(provide 'imbot)

;;; imbot.el ends here
