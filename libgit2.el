;;; libgit2.el --- Thin bindings to libgit2. -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2022 TheBB and the Authors

;; Authors: Eivind Fonn <evfonn@gmail.com>
;;          dick        <dickie.smalls@commandlinesystems.com>
;; URL: https://github.com/commercial-emacs/libgit
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

(defvar libgit--build-dir
  (expand-file-name "build" libgit--root)
  "Directory where the libgit2-el dynamic module file should be built.")

(defvar libgit--module-file
  (expand-file-name "libgit2-el.so" libgit--build-dir)
  "Path to the libgit2-el dynamic module file.")

(defun libgit--configure ()
  "Run the configure step of libgit2-el asynchronously.

On successful exit, pass control on to the build step."
  (make-directory libgit--build-dir 'parents)
  (let ((default-directory libgit--build-dir))
    (set-process-sentinel
     (start-process "libgit-cmake" "*libgit build*" "cmake" "..")
     (lambda (proc _event)
       (when (eq 'exit (process-status proc))
         (if (= 0 (process-exit-status proc))
             (libgit--build)
           (pop-to-buffer "*libgit build*")
           (error "libgit: configuring failed with exit code %d" (process-exit-status proc))))))))

(defun libgit--build ()
  "Run the build step of libgit2-el asynchronously.

On successful exit, pass control on to the load step."
  (let ((default-directory libgit--build-dir))
    (set-process-sentinel
     (start-process "libgit-cmake" "*libgit build*" "make")
     (lambda (proc _event)
       (when (eq 'exit (process-status proc))
         (if (= 0 (process-exit-status proc))
             (libgit--load)
           (pop-to-buffer "*libgit build*")
           (error "libgit: building failed with exit code %d" (process-exit-status proc))))))))

(defun libgit--load ()
  "Load the `libgit2-el' dynamic module.
If that fails, then raise an error."
  (unless (featurep 'libgit2)
    (load libgit--module-file nil t t))
  (unless (featurep 'libgit2)
    (error "libgit: unable to load the libgit2-el dynamic module")))

;;;###autoload
(defun libgit-load ()
  "Load the `libgit2-el' dynamic module."
  (interactive)
  (if (file-exists-p libgit--module-file)
      (libgit--load)
    (libgit--configure)))

(libgit-load)

(provide 'libgit2)

;;; libgit2.el ends here
