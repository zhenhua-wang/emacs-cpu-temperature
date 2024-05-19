# Install
Currently, it only supports linux system

```elisp
(use-package emacs-cpu-temperature
  :straight (:host github :repo "zhenhua-wang/emacs-cpu-temperature")
  :init
  (setq cpu-temperature-update-interval 1
        cpu-temperature-method 'thermal-zone
        cpu-temperature-thermal-zone-type "x86_pkg_temp"
        cpu-temperature-thermal-zone-path "/sys/class/thermal/")
  :config
  (cpu-temperature-mode 1))
```

# Update
## 2024-05-19 Add support for HWMON
Reading temperature from HWMON is supported and the default temperature to monitor is `Composite`, which can be customized.

# Usage
To show it on mode-line or tab-bar, you can simple append the variable `cpu-temperature-string` to `global-mode-string`.
```elisp
(add-to-list 'global-mode-string 'cpu-temperature-string t)
```

If you want to change config on the fly, make sure to re-run `(cpu-temperature-set-path)` to take effect.

![example](example/example.png)
