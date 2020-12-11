;;; wsl.el --- Integrate with MS Windows Windows Subsystem for Linux  -*- lexical-binding:t -*-

;; Copyright (C) 2020 Free Software
;; Foundation, Inc.

;; Author: Matthew O. Smith <matt@m0smith.com>
;; Version: 1.0
;; Package-Requires: ((emacs "24.1"))
;; Keywords: tools
;; URL: https://github.com/m0smith/wsl-el

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides an integration between a natvie Windows installation of EMACS
;; which also has the Window Subsystem for Linux (wsl) installed.  It
;; configures various packages to use the provided linus utilites.

;;; Code:

(require 'grep)

(defgroup wsl nil
  "Integration with the Window Subsystem for Linux"
  :group 'tools)

;;;###autoload
(defcustom wsl-command "c:/windows/sysnative/wsl.exe"
  "The wsl.exe command that Windows uses to access the linux commands.
If this is present then 2 things are known: This is a
windows box and it has WSL installed."
  :type 'file
  :group 'wsl)

(defcustom wsl-jka-compra-compress-programs '("gzip")
  "The list of programs that jka-compra-compression uses to do its work."
  :type '(repeat string)
  :group 'wsl)

(defcustom wsl-on-hook '(wsl-update-jka-compra-on
			 wsl-grep-on
			 wsl-shell-on
			 wsl-archive-on
			 wsl-subr-on)
  "List of functions to call when WSL mode is enabled"
  :group 'wsl
  :type 'hook)

(defcustom wsl-off-hook '(wsl-update-jka-compra-off
			  wsl-grep-off
			  wsl-shell-off
			  wsl-archive-off
			  wsl-subr-off)
  "List of functions to call when WSL mode is disabled"
  :group 'wsl
  :type 'hook)

(defvar wsl-jka-compr-compression-info-list jka-compr-compression-info-list
  "Hold onto the original value of `jka-compr-compression-info-list'")

(defun wsl-p ()
  "Return the expanded `wsl-command' if EMACS is running in Windows with WSL installed."
  (interactive)
  (let ((wsl-exe (expand-file-name wsl-command)))
    (when (and wsl-exe (file-exists-p wsl-exe))
      wsl-exe)))

;;;
;; jka-compra
;;;

(defun wsl-update-jka-compra-el (program args)
  (if (member program wsl-jka-compra-compress-programs)
      (vector (wsl-p) (cons program args))
    (vector program args)))

(defun wsl-update-jka-compra (v)
  (seq-let [regexp compress-msg compress-program compress-args
		   uncompress-msg uncompress-program uncompress-args
		   append-flag strip-extension-flag file-magic-chars] v
    (seq-let [cp ca] (wsl-update-jka-compra-el compress-program compress-args)
      (seq-let [ucp uca] (wsl-update-jka-compra-el uncompress-program uncompress-args)
	(vector regexp compress-msg cp ca
		uncompress-msg ucp uca
		append-flag strip-extension-flag file-magic-chars)))))

(defun wsl-update-jka-compra-on (wsl-exe)
  (setq jka-compr-compression-info-list (mapcar 'wsl-update-jka-compra jka-compr-compression-info-list))
  (jka-compr-update))

(defun wsl-update-jka-compra-off ()
  (setq jka-compr-compression-info-list wsl-jka-compr-compression-info-list)
  (jka-compr-update))


;;;;
;; subr
;;;;

(defun wsl-shell-quote-argument (arg)
  (message "wsl-shell-quote-argument : %s" arg)
  (when-let ((pre (string-match "^\\([a-zA-Z]\\):[/\\]\\(.*\\)$" arg)))
    (format "/mnt/%s/%s" (downcase (match-string 1 arg)) (match-string 2 arg))
  ))

(defun wsl-subr-on (wsl-exe)
  (advice-add 'shell-quote-argument :before-until #'wsl-shell-quote-argument))

(defun wsl-subr-off ()
  (advice-remove 'shell-quote-argument #'wsl-shell-quote-argument))

;;;
;; archive
;;;

(defvar wsl-archive-zip-extract archive-zip-extract)

(defun wsl-archive-extract-by-stdout (old-fn archive &rest args)
  (apply old-fn (wsl-shell-quote-argument archive) args))

(defun wsl-archive-on (wsl-exe)
  (advice-add 'archive-extract-by-stdout :around #'wsl-archive-extract-by-stdout))
  (setq archive-zip-extract (cons wsl-exe archive-zip-extract)))

(defun wsl-archive-off ()
  (advice-remove 'archive-extract-by-stdout #'wsl-archive-extract-by-stdout))
  (setq archive-zip-extract wsl-archive-zip-extract))

  

;;
;; SHELL
;;

(defvar wsl-shell-file-name shell-file-name)
(defvar wsl-shell-command-switch shell-command-switch)

(defun wsl-shell-on (wsl-exe)
  (setq shell-command-switch "-c"
	explicit-wsl.exe-args (list wsl-exe)
	explicit-cmdproxy.exe-args (list wsl-exe)
	;;shell-file-name wsl-exe
	))

(defun wsl-shell-off ()
  (setq shell-command-switch wsl-shell-command-switch
	shell-file-name wsl-shell-file-name
	))

;;
;; TRAMP
;;

(defvar wsl-tramp-methods tramp-methods)

(defun wsl-tramp-on (wsl-exe)
  (push
   (cons
    "wsl"
    (list (list 'tramp-login-program wsl-exe)
	  '(tramp-login-args (("-e") ("/bin/bash" "-il")))
	  (list 'tramp-remote-shell "/bin/sh -i")
	  '(tramp-remote-shell-login ("-il"))
	  '(tramp-remote-shell-args ("-i" "-c"))
      ))
   tramp-methods)
  )

(defun wsl-tramp-off ()
  
  (setq tramp-methods wsl-tramp-methods))

;;
;; grep
;;
(defvar wsl-null-device null-device)

(defun wsl-grep-on (wsl-exe)
  (setq null-device "/dev/null")
  (grep-apply-setting 'grep-command (format "%s grep -n " wsl-exe))
  (grep-apply-setting 'grep-template (format "%s grep <X> <C> -n <R> <F>" wsl-exe))
  (grep-apply-setting 'grep-find-command
		      (cons (format "%s find . -type f -exec grep  -n  \"{}\" /dev/null \";\"" wsl-exe) 60))
  (grep-apply-setting 'grep-find-template
		      (format "%s find <D> <X> -type f <F> -exec grep <C> -n <R> \"{}\" /dev/null \";\"" wsl-exe))
  (grep-compute-defaults)
  )

(defun wsl-grep-off ()
  (setq null-device wsl-null-device
	grep-host-defaults-alist nil
	grep-command nil
	grep-template nil
	grep-find-command nil
	grep-find-template nil)
  (grep-compute-defaults) )

;;
;; minor mode
;;

(defun wsl-mode-on ()
  "What to do when function `wsl-mode' is enabled."
  (let ((wsl-exe (wsl-p)))
    (when wsl-exe
      (run-hook-with-args 'wsl-on-hook wsl-exe)
      (setq cider-clojure-cli-command wsl-exe))))

;; "c:/windows/sysnative/wsl.exe" clojure -Sdeps '{:deps {nrepl {:mvn/version "0.8.2"} refactor-nrepl {:mvn/version "2.5.0"} cider/cider-nrepl {:mvn/version "0.25.4"}}}' -m nrepl.cmdline --middleware '["refactor-nrepl.middleware/wrap-refactor", "cider.nrepl/cider-middleware"]'

(defun wsl-mode-off ()
  "What to do when function `wsl-mode' is disabled."
  (run-hooks 'wsl-off-hook))


;;;###autoload
(define-minor-mode wsl-mode
  "Turn in integration with the Windows Subsystem for Linux integration."
  :init-value nil
  :lighter " œ"
  :global t

  (if wsl-mode
      (wsl-mode-on)
    (wsl-mode-off)))

(defun wsl-copy-region-to-clipboard (start end)
  "Copy region (START END) to Windows clipboard."
  (interactive "r")
  (call-process-region start end "clip.exe" nil 0))

(defun wsl-clipboard-to-string ()
  "Return Windows clipboard as a string."
  (let ((coding-system-for-read 'dos))
    (substring
     (shell-command-to-string
      "powershell.exe -Command Get-Clipboard" ) 0  -1)))

(defun wsl-paste-from-clipboard (arg)
  "Insert Windows clipboard at point.
With prefix ARG, also add to `kill-ring'."
  (interactive "P")
  (let ((clip (wsl-clipboard-to-string)))
    (insert clip)
    (if arg (kill-new clip))))

(provide 'wsl)

;;; wsl.el ends here


