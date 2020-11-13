;;; wsl.el --- Integrate with MS Windows Windows Subsystem for Linux  -*- lexical-binding:t -*-

;; Copyright (C) 2020 Free Software
;; Foundation, Inc.

;; Author: Matthew O. Smith <matt@m0smith.com>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: tools

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

(defgroup wsl nil
  "Integration with the Window Subsystem for Linux"
  :group 'tools)

;;;###autoload
(defcustom wsl-command "c:\\windows\\sysnative\\wsl.exe"
  "The wsl.exe command that Windows uses to access the linux
commands.  If this is present then 2 things are known: This is a
windows box and it has WSL installed."
  :type 'file
  :group 'wsl)

(defun wsl-p ()
  "Return the expanded `wsl-command' if EMACS is running in Windows with WSL installed"
  (interactive)
  (when-let ((wsl-exe (expand-file-name wsl-command)))
    (when (file-exists-p wsl-exe)
      wsl-exe)))

(defun wsl-mode-on ()
  (let ((wsl-exe (wsl-p)))
    (when wsl-exe
      (grep-apply-setting 'grep-command (format "%s grep  -n " wsl-exe))
      (grep-apply-setting 'grep-template (concat wsl-exe " " "grep <X> <C> -n <R> <F>"))
      (grep-apply-setting 'grep-find-command
			  (cons (format "%s find . -type f -exec grep  -n  \"{}\" NUL \";\"" wsl-exe) 60))
      (grep-apply-setting 'grep-find-template
			  (format "%s find <D> <X> -type f <F> -exec grep <C> -n <R> \"{}\" NUL \";\"" wsl-exe)))))

(defun wsl-mode-off ()
  (setq grep-host-defaults-alist nil
	grep-command nil
	grep-template nil
	grep-find-command nil
	grep-find-template nil)
  (grep-compute-defaults))

(define-minor-mode wsl-mode
  "Turn in integration with the Windows Subsystem for Linux integration."
  :init-value nil
  :lighter " œ"
  :global t

  (if wsl-mode
      (wsl-mode-on)
    (wsl-mode-off)))

(defun wsl-copy-region-to-clipboard (start end)
  "Copy region to Windows clipboard."
  (interactive "r")
  (call-process-region start end "clip.exe" nil 0))

(defun wsl-clipboard-to-string ()
  "Return Windows clipboard as a string."
  (let ((coding-system-for-read 'dos))
    (substring
     (shell-command-to-string
      "powershell.exe -Command Get-Clipboard" ) 0  -1)))

(defun wsl-paste-from-clipboard (arg)
  "Insert Windows clipboard at point. With prefix ARG, also add to kill-ring"
  (interactive "P")
  (let ((clip (wsl-clipboard-to-string)))
    (insert clip)
    (if arc (kill-new clip))))

(provide 'wsl)
