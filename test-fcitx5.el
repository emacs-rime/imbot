(require 'dbus)

(defvar fcitx-ic-path nil)
(defvar fcitx-preedit-string nil)
(defvar fcitx-commit-string nil)
(defvar fcitx-candidates nil)
(defvar fcitx-ui-result nil)

;; 1. Create Context
(setq fcitx-ic-path
      (car (dbus-call-method :session
                             "org.fcitx.Fcitx5"
                             "/org/freedesktop/portal/inputmethod"
                             "org.fcitx.Fcitx.InputMethod1"
                             "CreateInputContext"
                             '((:struct "program" "emacs")
                               (:struct "display" "emacs")))))

;; 2. Setup Capabilities (Client Side UI)
(let* ((ClientSideUI (ash 1 0))
       (Preedit (ash 1 1))
       (ClientSideControlState (ash 1 2))
       (Password (ash 1 3))
       (KeyEventOrderFix (ash 1 37))
       (ClientSideInputPanel (ash 1 39))
       ;; Combine them. Note: We are EXCLUDING bits related to shared state
       (cap-flag (logior
                  ;; ClientSideUI
                  ;; Preedit
                  ClientSideControlState
                  ;; Password
                  ;; KeyEventOrderFix
                  ClientSideInputPanel)))
  (dbus-call-method :session "org.fcitx.Fcitx5" fcitx-ic-path
                    "org.fcitx.Fcitx.InputContext1" "SetCapability"
                    :uint64 cap-flag))

;; 3. Register Signal Handler
;; (dbus-register-signal :session
;;                       "org.fcitx.Fcitx5"
;;                       fcitx-ic-path
;;                       "org.fcitx.Fcitx.InputContext1"
;;                       "UpdateFormattedPreedit"
;;                       (lambda (preedit cursorpos)
;;                         (setq fcitx-preedit-string preedit)
;;                         (message "Preedit: %s" preedit)))

(dbus-register-signal :session
                      "org.fcitx.Fcitx5"
                      fcitx-ic-path
                      "org.fcitx.Fcitx.InputContext1"
                      "UpdateClientSideUI"
                      (lambda (&rest args)
                        (message "UI: %s" args)
                        (setq fcitx-ui-result args)))

;; (dbus-register-signal :session
;;                       "org.fcitx.Fcitx5"
;;                       fcitx-ic-path
;;                       "org.fcitx.Fcitx.InputContext1"
;;                       "UpdateClientSideUI"
;;                       ;; preedit here is nil
;;                       (lambda (preedit cursorpos auxUp auxDown candidates
;;                                        candidateIndex layoutHint hasPrev hasNext)
;;                         (setq fcitx-candidates candidates)
;;                         (message "Candidates: %s preedit %s" candidates preedit)))

(dbus-register-signal :session
                      "org.fcitx.Fcitx5"
                      fcitx-ic-path
                      "org.fcitx.Fcitx.InputContext1"
                      "CommitString"
                      (lambda (str)
                        ;; (insert str)
                        ;; Clear preedit since the cycle is finished
                        ;; (setq fcitx-preedit-string "")
                        (setq fcitx-commit-string str)))

;; 4. Focus and Test
(dbus-call-method :session "org.fcitx.Fcitx5" fcitx-ic-path
                  "org.fcitx.Fcitx.InputContext1" "FocusIn")

;; (dbus-call-method :session
;;                   "org.fcitx.Fcitx5"
;;                   "/controller"
;;                   "org.fcitx.Fcitx.Controller1"
;;                   "Activate"
;;                   :timeout 600)

(dbus-call-method :session
                  "org.fcitx.Fcitx5"
                  "/controller"
                  "org.fcitx.Fcitx.Controller1"
                  "SetCurrentIM"
                  :string "rime")

(defun my-fcitx-send-key (keysym)
  "Send a key press to Fcitx."
  (dbus-call-method :session "org.fcitx.Fcitx5" fcitx-ic-path
                    "org.fcitx.Fcitx.InputContext1" "ProcessKeyEvent"
                    :uint32 keysym :uint32 0 :uint32 0 :boolean nil :uint32 0))

(defun fcitx-send-key (keysym)
  "Sends keysym to Fcitx and returns t if Fcitx handled it."
  (let ((handled (dbus-call-method :session "org.fcitx.Fcitx5" fcitx-ic-path
                                   "org.fcitx.Fcitx.InputContext1" "ProcessKeyEvent"
                                   :uint32 keysym :uint32 0 :uint32 0 :boolean nil :uint32 0)))
    (sleep-for 0.1)
    ;; Fcitx returns a boolean-like integer.
    ;; If handled is non-zero, Fcitx is using it for the IM logic.
    (message "Fcitx handled key %s: %s, candidates %s"
             keysym
             (if handled "YES" "NO")
             fcitx-candidates)))

;; Test: Send 'n'
(fcitx-send-key 110)
;; i
(fcitx-send-key 105)
;; h
(fcitx-send-key 104)
;; c
(fcitx-send-key 99)
;; space
;; (fcitx-send-key 32)
;; return
(fcitx-send-key #xFF0D)
;; now fcitx-commit-string is 你
