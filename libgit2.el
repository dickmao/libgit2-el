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

(defvar libgit2--build-dir
  (expand-file-name "build" (file-name-directory (or (locate-library "libgit2")
                                                     load-file-name)))
  "Directory where the libgit2-el dynamic module file should be built.")

(defvar libgit2--module-file
  (expand-file-name "libgit2-el.so" libgit2--build-dir)
  "Path to the libgit2-el dynamic module file.")

(defun libgit2--configure ()
  "Run the configure step of libgit2-el asynchronously.

On successful exit, pass control on to the build step."
  (make-directory libgit2--build-dir 'parents)
  (let ((default-directory libgit2--build-dir))
    (set-process-sentinel
     (start-process "libgit2-cmake" "*libgit build*" "cmake" "..")
     (lambda (proc _event)
       (when (eq 'exit (process-status proc))
         (if (zerop (process-exit-status proc))
             (libgit2--build)
           (pop-to-buffer "*libgit build*")
           (error "libgit: configuring failed with exit code %d"
                  (process-exit-status proc))))))))

(defun libgit2--build ()
  "Run the build step of libgit2-el asynchronously.

On successful exit, pass control on to the load step."
  (let ((default-directory libgit2--build-dir))
    (set-process-sentinel
     (apply #'start-process "libgit2-cmake" "*libgit build*" "cmake"
             (split-string "--build .. --target install"))
     (lambda (proc _event)
       (when (eq 'exit (process-status proc))
         (if (zerop (process-exit-status proc))
             (libgit2--load)
           (pop-to-buffer "*libgit build*")
           (error "libgit: building failed with exit code %d" (process-exit-status proc))))))))

(defun libgit2--load ()
  "Load the `libgit2-el' dynamic module.
If that fails, then raise an error."
  (unless (featurep 'libgit2)
    (load libgit2--module-file nil t t))
  (unless (featurep 'libgit2)
    (error "libgit: unable to load the libgit2-el dynamic module")))

;;;###autoload
(defun libgit2-load ()
  "Load the `libgit2-el' dynamic module."
  (interactive)
  (if (file-exists-p libgit2--module-file)
      (libgit2--load)
    (libgit2--configure)))

(libgit2-load)

(provide 'libgit2)

;;; libgit2.el ends here
