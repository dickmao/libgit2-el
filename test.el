(require 'libgit2)

(defun my-sideband-progress (msg)
  (message "Received a message on the sideband"))

(defun my-certificate-check (cert validp hostname)
  (message "Checking certificate: %S" (if (listp cert) (car cert) cert)))

(defun my-credentials (url username allowed-types)
  (message "Supplying credentials...")
  (libgit2-cred-ssh-key-from-agent username))

(defun my-transfer-progress (obj-total
                             obj-indexed
                             obj-received
                             obj-local
                             delta-total
                             delta-indexed
                             bytes-received)
  (message "Received: %s/%s (%s bytes)" obj-received obj-total bytes-received))

(let* ((path "~/test")
       (repo (libgit2-repository-init path))
       (remote (libgit2-remote-create repo "origin" "git@github.com:TheBB/dotemacs")))
  (libgit2-remote-fetch
   remote nil
   '((callbacks
      (sideband-progress . my-sideband-progress)
      (certificate-check . my-certificate-check)
      (credentials . my-credentials)
      (transfer-progress . my-transfer-progress)))))
