;;; ~/.emacs.d/lisp/y-init-company.el --- Company Config

;; Copyright (C) 2017-2019 yonggang.yyg<yygcode@gmail.com>

;; Author: yonggang.yyg<yygcode@gmail.com>
;; Maintainer: yonggang.yyg<yygcode@gmail.com>
;; Keyword: Emacs Company Configuration
;; Homepage: https://ycode.org; http://ycode.org;
;; URL: http://github.com/yygcode/.emacs.d

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING, if not see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Company config for different major-mode.

;;; Code:

;; https://company-mode.github.io/
;; https://github.com/company-mode/company-mode
(use-package company
  :diminish
  :init
  (setq company-show-numbers t
        company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above nil
        company-tooltip-limit 10
        company-selection-changed t
        company-selection-wrap-around t
        company-require-match nil)
  :config
  (bind-key [remap completion-at-point] #'company-complete)
  (bind-key [remap complete-symbol] #'company-complete)
  (bind-key [remap complete-tag] #'company-complete)
  (bind-key [remap completion-at-point-functions] #'company-complete)
  :hook
  (after-init . global-company-mode)
  (company-mode . y/company-hook)
  ;; I don't know how to config face by company-frontends-set
  :custom-face
  (company-tooltip ((t (:foreground "orange1"))))
  (company-tooltip-selection ((t (:foreground "orange1"
                                  :background "DarkOliveGreen4"))))
  :bind
  (:map company-active-map
          ("<tab>"   . company-select-next)
          ("S-<tab>" . company-select-previous)
          ("C-n"     . company-select-next)
          ("C-p"     . company-select-previous)
          ("C-k"     . company-complete-selection)
          ;; company-quickhelp has no map, used map here.
          ("C-h" . company-quickhelp-manual-begin)
          ))

;; https://github.com/expez/company-quickhelp
(use-package company-quickhelp
  :init
  (setq company-quickhelp-delay nil
        company-quickhelp-max-lines 30)
  :hook
  (company-mode . company-quickhelp-mode))

(defun y/company-elisp-hook()
  "Config company hook for elisp."
  (local-set-key "\t" #'company-indent-or-complete-common)
  (setq company-backends '((company-elisp
                            company-yasnippet
                            company-files
                            company-ispell))
        company-idle-delay 0))

(defun y/company-text-hook()
  "Config company hook for text."
  (setq company-backends '((company-abbrev
                            company-dabbrev
                            company-files
                            company-bbdb
                            company-yasnippet
                            company-ispell))
        company-idle-delay 0.1))

(defun y/company-hook()
  "Comelete anything hook."
  (make-local-variable 'company-backends)
  (make-local-variable 'company-idle-delay)
  (and company-mode ;; do nothing if nil
       (cond ((or (equal major-mode 'emacs-lisp-mode)
                  (equal major-mode 'lisp-interaction-mode))
              (y/company-elisp-hook))
             ;; default to text mode, so add special above here.
             (t
              (y/company-text-hook)))))

(provide 'y-init-company)

;;; y-init-company.el ends here
