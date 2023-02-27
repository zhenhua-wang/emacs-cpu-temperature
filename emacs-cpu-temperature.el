;;; emacs-cpu-temperature.el --- emacs cpu temperature  -*- lexical-binding: t; -*-

(defcustom cpu-temperature-termal-zone-type "x86_pkg_temp"
  "CPU thermal zone type."
  :type 'string)

(defcustom cpu-temperature-termal-zone-path "/sys/class/thermal/"
  "CPU thermal zone path."
  :type 'string)

(defvar cpu-temperature--termal-zone nil
  "CPU thermal zone.")

(defun cpu-temperature-set-termal-zone ()
  "Set thermal zone based on CPU type."
  (let* ((termal-zones (seq-filter
                        (lambda (s) (string-match "thermal_zone" s))
                        (directory-files cpu-temperature-termal-zone-path))))
    (dolist (zone termal-zones)
      (when (string-match cpu-temperature-termal-zone-type
                          (f-read-text (concat cpu-temperature-termal-zone-path
                                               zone "/type")))
        (setq cpu-temperature--termal-zone zone)))))

(defun cpu-temperature-termal-zone-temp ()
  "Get CPU temperature for the current thermal zone."
  (/ (string-to-number (f-read-text (concat cpu-temperature-termal-zone-path cpu-temperature--termal-zone "/temp")))
     1000))

;; initialize
(cpu-temperature-set-termal-zone)

(provide 'emacs-cpu-temperature)

;;; emacs-cpu-temperature.el ends here
