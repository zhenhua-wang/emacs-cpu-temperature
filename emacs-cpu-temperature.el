;;; emacs-cpu-temperature.el --- emacs cpu temperature  -*- lexical-binding: t; -*-

(defcustom cpu-temperature-thermal-zone-type "x86_pkg_temp"
  "CPU thermal zone type."
  :type 'string)

(defcustom cpu-temperature-thermal-zone-path "/sys/class/thermal/"
  "CPU thermal zone path."
  :type 'string)

(defcustom cpu-temperature-update-interval 3
  "CPU temperature updating interval in seconds."
  :type 'natnum)

(defvar cpu-temperature-string nil
  "String that holds the current CPU temperature.")

(defvar cpu-temperature--thermal-zone nil
  "CPU thermal zone.")

(defvar cpu-temperature--temp-path nil
  "CPU thermal zone temperature.")

(defvar cpu-temperature--timer nil)

(defun cpu-temperature-set-thermal-zone ()
  "Set thermal zone based on CPU type."
  (let* ((thermal-zones (seq-filter
                         (lambda (s) (string-match "thermal_zone" s))
                         (ignore-errors
                           (directory-files cpu-temperature-thermal-zone-path)))))
    (dolist (zone thermal-zones)
      (when (ignore-errors
              (string-match cpu-temperature-thermal-zone-type
                            (with-temp-buffer
                              (insert-file-contents
                               (concat cpu-temperature-thermal-zone-path zone "/type"))
                              (buffer-string))))
        (setq cpu-temperature--thermal-zone zone
              cpu-temperature--temp-path (concat cpu-temperature-thermal-zone-path
                                                 cpu-temperature--thermal-zone "/temp"))))))

(defun cpu-temperature-update ()
  "Update CPU temperature for the current thermal zone."
  (setq cpu-temperature-string
        (or (ignore-errors
              (format "%d°C "
                      (/ (string-to-number
                          (with-temp-buffer
                            (insert-file-contents cpu-temperature--temp-path)
                            (buffer-string)))
                         1000)))
            "")))

;;;###autoload
(define-minor-mode cpu-temperature-mode
  "Toggle update of CPU temperature."
  :global t
  (if cpu-temperature-mode
      (progn
        (cpu-temperature-set-thermal-zone)
        (cpu-temperature-update)
        (setq cpu-temperature--timer (run-at-time t cpu-temperature-update-interval
		                                  'cpu-temperature-update)))
    (cancel-timer cpu-temperature--timer)))

(provide 'emacs-cpu-temperature)

;;; emacs-cpu-temperature.el ends here
