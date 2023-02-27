# Install
Currently, it only supports linux system

```elisp
(use-package emacs-cpu-temperature
  :straight (:host github :repo "zhenhua-wang/emacs-cpu-temperature")
  :config
  (setq cpu-temperature-update-interval 1
        cpu-temperature-termal-zone-type "x86_pkg_temp"
        cpu-temperature-termal-zone-path "/sys/class/thermal/")
  (cpu-temperature-mode 1))
```

# Usage
To show it on mode-line or tab-bar, you can simple append the variable `cpu-temperature-string` to `global-mode-string`.
```elisp
(add-to-list 'global-mode-string 'cpu-temperature-string t)
```

![example](example/example.png)
