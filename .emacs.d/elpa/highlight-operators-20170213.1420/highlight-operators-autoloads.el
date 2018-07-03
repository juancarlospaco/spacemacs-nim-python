;;; highlight-operators-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "highlight-operators" "highlight-operators.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from highlight-operators.el

(autoload 'highlight-operators-mode "highlight-operators" "\
Extra highlighting for operators in programming modes.

\(fn &optional ARG)" t nil)

(defvar global-highlight-operators-mode nil "\
Non-nil if Global Highlight-Operators mode is enabled.
See the `global-highlight-operators-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-highlight-operators-mode'.")

(custom-autoload 'global-highlight-operators-mode "highlight-operators" nil)

(autoload 'global-highlight-operators-mode "highlight-operators" "\
Toggle Highlight-Operators mode in all buffers.
With prefix ARG, enable Global Highlight-Operators mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Highlight-Operators mode is enabled in all buffers where
`(lambda nil (highlight-operators-mode 1))' would do it.
See `highlight-operators-mode' for more information on Highlight-Operators mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "highlight-operators" '("highlight-operators-regexp")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; highlight-operators-autoloads.el ends here
