(ert-deftest repository-init ()
  (with-temp-dir path
    (should (libgit2-repository-p (libgit2-repository-init path)))
    (should (file-directory-p (concat path ".git"))))
  (with-temp-dir path
    (should (libgit2-repository-p (libgit2-repository-init path t)))
    (should (file-exists-p (concat path "HEAD")))))

(ert-deftest repository-open ()
  (with-temp-dir path
    (shell-command-to-string "git init")
    (should (libgit2-repository-p (libgit2-repository-open path))))
 (with-temp-dir path
    (shell-command-to-string "git init --bare")
    (should (libgit2-repository-p (libgit2-repository-open path)))))

(ert-deftest repository-open-bare ()
  (with-temp-dir path
    (shell-command-to-string "git init")
    (should-error (libgit2-repository-open-bare path) :type 'giterr-repository))
 (with-temp-dir path
    (shell-command-to-string "git init --bare")
    (should (libgit2-repository-open-bare path))))

(ert-deftest repository-commondir ()
  (with-temp-dir path
    (init)
    (commit-change "test" "content")
    (run "git" "worktree" "add" "subdir" "HEAD")
    (should (path= (concat path ".git/")
                   (libgit2-repository-commondir
                    (libgit2-repository-open path))))
    (should (path= (concat path ".git/")
                   (libgit2-repository-commondir
                    (libgit2-repository-open (concat path "subdir/")))))))

(ert-deftest repository-head ()
  (with-temp-dir path
    (init)
    (commit-change "test" "content")
    (let ((repo (libgit2-repository-open path)))
      (should (string= (libgit2-reference-name (libgit2-repository-head repo))
                       "refs/heads/master"))
      (run "git" "checkout" "-b" "zing")
      (should (string= (libgit2-reference-name (libgit2-repository-head repo))
                       "refs/heads/zing")))))

(ert-deftest repository-head-for-worktree ()
  (with-temp-dir path
    (init)
    (commit-change "test" "content")
    (run "git" "branch" "zing")
    (run "git" "worktree" "add" "subdir" "zing")
    (let ((repo (libgit2-repository-open path)))
      (should (string= (libgit2-reference-name (libgit2-repository-head-for-worktree repo "subdir"))
                       "refs/heads/zing")))))

(ert-deftest repository-head-for-worktree ()
  (with-temp-dir path
    (init)
    (commit-change "test" "content")
    (run "git" "branch" "zing")
    (run "git" "worktree" "add" "subdir" "zing")
    (let ((repo (libgit2-repository-open path)))
      (should (string= (libgit2-reference-name (libgit2-repository-head-for-worktree repo "subdir"))
                       "refs/heads/zing")))))

(ert-deftest repository-ident ()
  (with-temp-dir path
    (init)
    (let ((repo (libgit2-repository-open path)))
      (should (equal '(nil . nil) (libgit2-repository-ident repo)))
      (libgit2-repository-set-ident repo "John Doe" "john@example.com")
      (should (equal '("John Doe" . "john@example.com")
                     (libgit2-repository-ident repo))))))

(ert-deftest repository-message ()
  (with-temp-dir path
    (init)
    (write ".git/MERGE_MSG" "something\n")
    (should (string= (libgit2-repository-message (libgit2-repository-open path))
                     "something\n"))))

(ert-deftest repository-path ()
  (with-temp-dir path
    (init)
    (should (path= (libgit2-repository-path (libgit2-repository-open path))
                   (concat path ".git/")))))

(ert-deftest repository-state ()
  (with-temp-dir path
    (init)
    (let ((repo (libgit2-repository-open path)))
      (should (not (libgit2-repository-state repo)))

      (write ".git/rebase-merge/interactive" "")
      (should (eq 'rebase-interactive (libgit2-repository-state repo)))
      (delete-directory ".git/rebase-merge" 'recursive)

      (make-directory ".git/rebase-merge")
      (should (eq 'rebase-merge (libgit2-repository-state repo)))
      (delete-directory ".git/rebase-merge")

      (write ".git/rebase-apply/rebasing" "")
      (should (eq 'rebase (libgit2-repository-state repo)))
      (delete-directory ".git/rebase-apply" 'recursive)

      (write ".git/rebase-apply/applying" "")
      (should (eq 'apply-mailbox (libgit2-repository-state repo)))
      (delete-directory ".git/rebase-apply" 'recursive)

      (make-directory ".git/rebase-apply")
      (should (eq 'apply-mailbox-or-rebase (libgit2-repository-state repo)))
      (delete-directory ".git/rebase-apply")

      (write ".git/MERGE_HEAD" "")
      (should (eq 'merge (libgit2-repository-state repo)))
      (delete-file ".git/MERGE_HEAD")

      (write ".git/REVERT_HEAD" "")
      (should (eq 'revert (libgit2-repository-state repo)))
      (write ".git/sequencer/todo" "")
      (should (eq 'revert-sequence (libgit2-repository-state repo)))
      (delete-file ".git/REVERT_HEAD")
      (delete-directory ".git/sequencer" 'recursive)

      (write ".git/CHERRY_PICK_HEAD" "")
      (should (eq 'cherrypick (libgit2-repository-state repo)))
      (write ".git/sequencer/todo" "")
      (should (eq 'cherrypick-sequence (libgit2-repository-state repo)))
      (delete-file ".git/CHERRY_PICK_HEAD")
      (delete-directory ".git/sequencer" 'recursive)

      (write ".git/BISECT_LOG" "")
      (should (eq 'bisect (libgit2-repository-state repo))))))

(ert-deftest repository-workdir ()
  (with-temp-dir path
    (init)
    (should (path= (libgit2-repository-workdir (libgit2-repository-open path)) path)))
  (with-temp-dir path
    (init "--bare")
    (should (not (libgit2-repository-workdir (libgit2-repository-open path))))))

(ert-deftest repository-detach-head ()
  (with-temp-dir path
    (init)
    (commit-change "test" "content")
    (let ((repo (libgit2-repository-open path))
          (id (read-file-nnl ".git/refs/heads/master")))
      (should (string= (read-file-nnl ".git/HEAD") "ref: refs/heads/master"))
      (libgit2-repository-detach-head repo)
      (should (string= (read-file-nnl ".git/HEAD") id)))))

(ert-deftest repository-message-remove ()
  (with-temp-dir path
    (init)
    (write ".git/MERGE_MSG" "something\n")
    (libgit2-repository-message-remove (libgit2-repository-open path))
    (should (not (file-exists-p ".git/MERGE_MSG")))))

(ert-deftest repository-set-head ()
  (with-temp-dir path
    (init)
    (commit-change "test" "content")
    (run "git" "branch" "zing")
    (should (string= (read-file-nnl ".git/HEAD") "ref: refs/heads/master"))
    (libgit2-repository-set-head (libgit2-repository-open path) "refs/heads/zing")
    (should (string= (read-file-nnl ".git/HEAD") "ref: refs/heads/zing"))))

(ert-deftest repository-set-head-detached ()
  (with-temp-dir path
    (init)
    (commit-change "test" "content")
    (let ((repo (libgit2-repository-open path))
          (id (read-file-nnl ".git/refs/heads/master")))
      (should (string= (read-file-nnl ".git/HEAD") "ref: refs/heads/master"))
      (libgit2-repository-set-head-detached repo id)
      (should (string= (read-file-nnl ".git/HEAD") id)))))

(ert-deftest repository-set-namespace ()
  ;; TODO
  (skip-unless nil))

(ert-deftest repository-set-workdir ()
  ;; TODO
  (skip-unless nil))

(ert-deftest repository-state-cleanup ()
  (with-temp-dir path
    (init)
    (let ((repo (libgit2-repository-open path)))
      (should (not (libgit2-repository-state repo)))

      (write ".git/rebase-merge/interactive" "")
      (write ".git/rebase-apply/rebasing" "")
      (write ".git/rebase-apply/applying" "")
      (write ".git/sequencer/todo" "")
      (write ".git/MERGE_HEAD" "")
      (write ".git/REVERT_HEAD" "")
      (write ".git/CHERRY_PICK_HEAD" "")
      (write ".git/BISECT_LOG" "")

      (should (libgit2-repository-state repo))
      (libgit2-repository-state-cleanup repo)
      (should (not (libgit2-repository-state repo))))))

(ert-deftest repository-bare-p ()
  (with-temp-dir path
    (init)
    (should (not (libgit2-repository-bare-p (libgit2-repository-open path)))))
  (with-temp-dir path
    (init "--bare")
    (should (libgit2-repository-bare-p (libgit2-repository-open path)))))

(ert-deftest repository-empty-p ()
  (with-temp-dir path
    (init)
    (should (libgit2-repository-empty-p (libgit2-repository-open path)))
    (commit-change "test" "content")
    (should (not (libgit2-repository-empty-p (libgit2-repository-open path))))))

(ert-deftest repository-head-detached-p ()
  ;; TODO
  (skip-unless nil))

(ert-deftest repository-shallow-p ()
  (with-temp-dir (src tgt)
    (in-dir src
      (init)
      (commit-change "test" "content")
      (commit-change "test" "content2"))
    (run "git" "clone" "--depth" "1" (concat "file://" src) tgt)
    (let ((repo (libgit2-repository-open src)))
      (should (not (libgit2-repository-shallow-p repo))))
    (let ((repo (libgit2-repository-open tgt)))
      (should (libgit2-repository-shallow-p repo)))))

(ert-deftest repository-worktree-p ()
  (with-temp-dir (src tgt)
    (in-dir src
      (init)
      (commit-change "test" "content")
      (run "git" "worktree" "add" tgt "HEAD"))
    (let ((repo (libgit2-repository-open src)))
      (should (not (libgit2-repository-worktree-p repo))))
    (let ((repo (libgit2-repository-open tgt)))
      (should (libgit2-repository-worktree-p repo)))))

(ert-deftest repository-discover ()
  (with-temp-dir (a b)
    (in-dir a
      (init))
    (should (path= (libgit2-repository-discover a) (concat a ".git/")))
    (should-error (libgit2-repository-discover b) :type 'giterr-repository)))
