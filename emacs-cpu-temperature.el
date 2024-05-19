;;; emacs-cpu-temperature.el --- emacs cpu temperature  -*- lexical-binding: t; -*-

(defgroup cpu-temperature nil
  "cpu temperature group"
  :group 'hardware)

(defgroup cpu-temperature-hwmon nil
  "cpu temperature hwmon group"
  :group 'cpu-temperature)

(defgroup cpu-temperature-thermal-zone nil
  "cpu temperature thermal zone group"
  :group 'cpu-temperature)

(defcustom cpu-temperature-update-interval 3
  "CPU temperature updating interval in seconds."
  :type 'natnum
  :group 'cpu-temperature)

(defcustom cpu-temperature-method 'thermal-zone
  "Method to monitor CPU temperature.

To change cpu-temperature-method, you also need to re-run `cpu-temperature-set-path' after change this variable."
  :type '(choice (const :tag "hwmon" hwmon)
                 (const :tag "thermal zone" thermal-zone))
  :group 'cpu-temperature)

(defcustom cpu-temperature-hwmon-type "CPU"
  "CPU hwmon type."
  :type 'string
  :group 'cpu-temperature-hwmon)

(defcustom cpu-temperature-hwmon-path "/sys/class/hwmon/"
  "CPU hwmon path."
  :type 'string
  :group 'cpu-temperature-hwmon)

(defcustom cpu-temperature-thermal-zone-type "x86_pkg_temp"
  "CPU thermal zone type."
  :type 'string
  :group 'cpu-temperature-thermal-zone)

(defcustom cpu-temperature-thermal-zone-path "/sys/class/thermal/"
  "CPU thermal zone path."
  :type 'string
  :group 'cpu-temperature-thermal-zone)

(defvar cpu-temperature-string nil
  "String that holds the current CPU temperature.")

(defvar cpu-temperature--temp-path nil
  "CPU thermal zone temperature.")

(defvar cpu-temperature--timer nil)

(defun cpu-temperature--read-file-content (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun cpu-temperature-update ()
  "Update CPU temperature."
  (setq cpu-temperature-string
        (or (ignore-errors
              (format "%d°C "
                      (/ (string-to-number
                          (cpu-temperature--read-file-content cpu-temperature--temp-path))
                         1000)))
            "")))

(defun cpu-temperature-set-path ()
  "Set CPU temperature file path."
  (pcase cpu-temperature-method
    ('hwmon (cpu-temperature--hwmon-set-path))
    ('thermal-zone (cpu-temperature--thermal-zone-set-path))))

(defun cpu-temperature--hwmon-set-path ()
  "Set CPU temperature file path using hwmon."
  (let* ((hwmon-paths (seq-filter
                       (lambda (s) (string-match "hwmon" s))
                       (ignore-errors
                         (directory-files cpu-temperature-hwmon-path 'full)))))
    (dolist (hwmon-path hwmon-paths)
      (when-let* ((hwmon-label (cl-find-if
                                (lambda (s) (and (string-match "temp[0-9]_label" s)
                                                 (string-match cpu-temperature-hwmon-type
                                                               (cpu-temperature--read-file-content s))))
                                (ignore-errors
                                  (directory-files hwmon-path 'full))))
                  (hwmon-file (concat (car (split-string hwmon-label "_"))
                                      "_input")))
        (setq cpu-temperature--temp-path hwmon-file)))))

(defun cpu-temperature--thermal-zone-set-path ()
  "Set CPU temperature file path using thermal zone."
  (let* ((zone-paths (seq-filter
                      (lambda (s) (string-match "thermal_zone" s))
                      (ignore-errors
                        (directory-files cpu-temperature-thermal-zone-path 'full)))))
    (dolist (zone-path zone-paths)
      (when (string-match cpu-temperature-thermal-zone-type
                          (cpu-temperature--read-file-content
                           (expand-file-name "type" zone-path)))
        (setq cpu-temperature--temp-path (expand-file-name "temp" zone-path))))))

;;;###autoload
(define-minor-mode cpu-temperature-mode
  "Toggle update of CPU temperature."
  :global t
  (if cpu-temperature-mode
      (progn
        (cpu-temperature-set-path)
        (cpu-temperature-update)
        (setq cpu-temperature--timer
              (run-at-time t cpu-temperature-update-interval
		           'cpu-temperature-update)))
    (cancel-timer cpu-temperature--timer)))

(provide 'emacs-cpu-temperature)

;;; emacs-cpu-temperature.el ends here
