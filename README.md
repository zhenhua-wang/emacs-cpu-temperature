# Install
```elisp
(use-package emacs-cpu-temperature
  :straight (:host github :repo "zhenhua-wang/emacs-cpu-temperature")
  :config
  (setq cpu-temperature-update-interval 1)
  (cpu-temperature-mode 1))
```

# Usage
I use this package to show CPU temperature on tab-bar. Here is an example. You can simply add this function to `tab-bar-format`. For more details, check [this](https://github.com/zhenhua-wang/emacs.d/blob/f7b174c414192392967c7499228f0a43ac0241d5/lisp/zw-exwm.el#L117-L133)
```elisp
  (defun zw/tab-bar-format-cpu-temp ()
    "Produce menu that shows cpu temperature."
    `((global menu-item ,cpu-temperature-string
              nil :help ,(format "CPU temperature: %s" cpu-temperature-string))))
```

![example](example/example.png)
