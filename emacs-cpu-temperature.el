;;; emacs-cpu-temperature.el --- emacs cpu temperature  -*- lexical-binding: t; -*-

(defcustom cpu-temperature-termal-zone-type "x86_pkg_temp"
  "CPU thermal zone type."
  :type 'string)

(defcustom cpu-temperature-termal-zone-path "/sys/class/thermal/"
  "CPU thermal zone path."
  :type 'string)

(defcustom cpu-temperature-update-interval 3
  "CPU temperature updating interval in seconds."
  :type 'natnum)

(defvar cpu-temperature-string nil
  "String that holds the current CPU temperature.")

(defvar cpu-temperature--termal-zone nil
  "CPU thermal zone.")

(defvar cpu-temperature--timer nil)

(defun cpu-temperature-set-termal-zone ()
  "Set thermal zone based on CPU type."
  (let* ((termal-zones (seq-filter
                        (lambda (s) (string-match "thermal_zone" s))
                        (directory-files cpu-temperature-termal-zone-path))))
    (dolist (zone termal-zones)
      (when (string-match cpu-temperature-termal-zone-type
                          (with-temp-buffer
                            (insert-file-contents
                             (concat cpu-temperature-termal-zone-path zone "/type"))
                            (buffer-string)))
        (setq cpu-temperature--termal-zone zone)))))

(defun cpu-temperature-update ()
  "Update CPU temperature for the current thermal zone."
  (setq cpu-temperature-string
        (format "%d°C "
                (/ (string-to-number (with-temp-buffer
                                       (insert-file-contents
                                        (concat cpu-temperature-termal-zone-path cpu-temperature--termal-zone "/temp"))
                                       (buffer-string)))
                   1000))))

;;;###autoload
(define-minor-mode cpu-temperature-mode
  "Toggle update of CPU temperature."
  :global t
  (if cpu-temperature-mode
      (progn
        (cpu-temperature-set-termal-zone)
        (cpu-temperature-update)
        (setq cpu-temperature--timer (run-at-time t cpu-temperature-update-interval
		                                  'cpu-temperature-update)))
    (cancel-timer cpu-temperature--timer)))

(provide 'emacs-cpu-temperature)

;;; emacs-cpu-temperature.el ends here
