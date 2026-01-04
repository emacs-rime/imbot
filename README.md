#+TITLE: README

# About
**imbot** removes the hassle of switching input method back and forth. Imbot provide an emacs input method using fcitx5 through dbus or librime with a dynamic module. It is recommended to use the fcitx-dbus backend for linux, and to use the rime-lib backend for Windows and Macos.

# Install
Put the files on load-path, or install __imbot__ from **melpa**.

# Config
An example using use-package:

```
    (use-package imbot
      :custom (default-input-method "imbot")
      :config
      ;; This function is used to ensure the input method is disabled.
      (defun my-cleanup-input-method (&rest args)
        (when current-input-method
          (deactivate-input-method)))
    
      ;; Add advice to run the cleanup function after aborting wdired.
      (advice-add 'wdired-change-to-dired-mode :after #'my-cleanup-input-method))
```

To prevent conflict with the system input method panel, run emacs with:

```
    GTK_IM_MODULE="" QT_IM_MODULE="" SDL_IM_MODULE="" INPUT_METHOD="" XMODIFIERS='@im=""' emacs
```

