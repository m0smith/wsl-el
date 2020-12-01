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

(defvar wsl-jka-compr-compression-info-list jka-compr-compression-info-list
  "Hold onto the original value of `jka-compr-compression-info-list'")

(defun wsl-p ()
  "Return the expanded `wsl-command' if EMACS is running in Windows with WSL installed."
  (interactive)
  (let ((wsl-exe (expand-file-name wsl-command)))
    (when (and wsl-exe (file-exists-p wsl-exe))
      wsl-exe)))

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

(defun wsl-update-jka-compra-on ()
  (setq jka-compr-compression-info-list (mapcar 'wsl-update-jka-compra jka-compr-compression-info-list))
  (jka-compr-update))

(defun wsl-update-jka-compra-off ()
  (setq jka-compr-compression-info-list wsl-jka-compr-compression-info-list)
  (jka-compr-update))

(defun wsl-mode-on ()
  "What to do when function `wsl-mode' is enabled."
  (let ((wsl-exe (wsl-p)))
    (when wsl-exe
      (wsl-update-jka-compra-on)
      (grep-apply-setting 'grep-command (format "%s grep  -n " wsl-exe))
      (grep-apply-setting 'grep-template (format "%s grep <X> <C> -n <R> <F>" wsl-exe))
      (grep-apply-setting 'grep-find-command
			  (cons (format "%s find . -type f -exec grep  -n  \"{}\" /dev/null \";\"" wsl-exe) 60))
      (grep-apply-setting 'grep-find-template
			  (format "%s find <D> <X> -type f <F> -exec grep <C> -n <R> \"{}\" /dev/null \";\"" wsl-exe))
      
      (setq cider-clojure-cli-command wsl-exe))))

;; "c:/windows/sysnative/wsl.exe" clojure -Sdeps '{:deps {nrepl {:mvn/version "0.8.2"} refactor-nrepl {:mvn/version "2.5.0"} cider/cider-nrepl {:mvn/version "0.25.4"}}}' -m nrepl.cmdline --middleware '["refactor-nrepl.middleware/wrap-refactor", "cider.nrepl/cider-middleware"]'

(defun wsl-mode-off ()
  "What to do when function `wsl-mode' is disabled."
  (wsl-update-jka-compra-off)
  (setq grep-host-defaults-alist nil
	grep-command nil
	grep-template nil
	grep-find-command nil
	grep-find-template nil)
  (grep-compute-defaults))

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


