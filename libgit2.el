;;; libgit2.el --- Thin bindings to libgit2. -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2022 TheBB and the Authors

;; Authors: Eivind Fonn <evfonn@gmail.com>
;;          dick        <dickie.smalls@commandlinesystems.com>
;; URL: https://github.com/commercial-emacs/libgit2-el
;; Version: 0.0.1
;; Keywords: git vc
;; Package-Requires: ((emacs "27.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Thin bindings to libgit2.

;;; Code:

(defconst libgit2-buffer-name "*libgit build*")

(defvar libgit2-build-dir
  (expand-file-name "build" (file-name-directory (or (locate-library "libgit2")
                                                     load-file-name))))

(defvar libgit2-module-file
  (expand-file-name "libgit2-el.so" libgit2-build-dir))

;;;###autoload
(defun libgit2-load ()
  (interactive)
  (make-directory libgit2-build-dir 'parents)
  (unless (featurep 'libgit2)
    (if (file-exists-p libgit2-module-file)
        (load libgit2-module-file nil t t)
      (let ((default-directory libgit2-build-dir))
        (set-process-sentinel
         (start-process "libgit2-build" libgit2-buffer-name "cmake" "..")
         (lambda (proc _event)
           (when (eq 'exit (process-status proc))
             (if (not (zerop (process-exit-status proc)))
                 (progn
                   (pop-to-buffer libgit2-buffer-name)
                   (error "libgit: configuring failed with code %d"
                          (process-exit-status proc)))
               (let ((default-directory libgit2-build-dir))
                 (set-process-sentinel
                  (apply #'start-process "libgit2-build" libgit2-buffer-name "cmake"
                         (split-string "--build . --target install"))
                  (lambda (proc _event)
                    (when (eq 'exit (process-status proc))
                      (if (not (zerop (process-exit-status proc)))
                          (progn
                            (pop-to-buffer libgit2-buffer-name)
                            (error "libgit: building failed with exit code %d"
                                   (process-exit-status proc)))
                        (load libgit2-module-file nil t t))))))))))))))

(libgit2-load)

(provide 'libgit2)

;;; libgit2.el ends here
