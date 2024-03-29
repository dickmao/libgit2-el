(ert-deftest branch-create ()
  (with-temp-dir path
    (init)
    (commit-change "test" "content")
    (let ((repo (libgit2-repository-open path)))
      (should (libgit2-branch-create repo "new-branch" (libgit2-revparse-single repo "HEAD")))
      (should-error (libgit2-branch-create repo "new-branch" (libgit2-revparse-single repo "HEAD")))))
  (with-temp-dir path
    (init)
    (commit-change "test" "content")
    (run "git" "branch" "second")
    (run "git" "checkout" "second")
    (commit-change "test2" "content2")
    (let ((repo (libgit2-repository-open path)))
      (should-error (libgit2-branch-create repo "master" (libgit2-revparse-single repo "second")))
      (should (libgit2-branch-create repo "master" (libgit2-revparse-single repo "second") t)))))

(ert-deftest branch-create-from-annotated ()
  (with-temp-dir path
    (init)
    (commit-change "test" "content")
    (let ((repo (libgit2-repository-open path)))
      (should (libgit2-branch-create-from-annotated repo "new-branch" "HEAD"))
      (should-error (libgit2-branch-create-from-annotated repo "new-branch" "HEAD"))))
  (with-temp-dir path
    (init)
    (commit-change "test" "content")
    (run "git" "branch" "second")
    (run "git" "checkout" "second")
    (commit-change "test2" "content2")
    (let ((repo (libgit2-repository-open path)))
      (should-error (libgit2-branch-create-from-annotated repo "master" "second"))
      (should (libgit2-branch-create-from-annotated repo "master" "second" t)))))

(ert-deftest branch-lookup ()
  (with-temp-dir path
    (init)
    (commit-change "test" "content")
    (run "git" "branch" "second")
    (let ((repo (libgit2-repository-open path)))
      (should (libgit2-branch-lookup repo "master"))
      (should (libgit2-branch-lookup repo "second"))
      (should (libgit2-branch-lookup repo "second"))
      (should-error (libgit2-branch-lookup repo "third"))
      (should-error (libgit2-branch-lookup repo "master" t)))))

(ert-deftest branch-delete ()
  (with-temp-dir path
    (init)
    (commit-change "test" "content")
    (run "git" "branch" "second")
    (let* ((repo (libgit2-repository-open path))
           (masterref (libgit2-branch-lookup repo "master"))
           (secondref (libgit2-branch-lookup repo "second")))
      (should-error (libgit2-branch-delete masterref))
      (libgit2-branch-delete secondref))))

(ert-deftest branch-checked-out-p ()
  (with-temp-dir path
    (init)
    (commit-change "test" "content")
    (run "git" "branch" "second")
    (let* ((repo (libgit2-repository-open path))
           (masterref (libgit2-branch-lookup repo "master"))
           (secondref (libgit2-branch-lookup repo "second")))
      (should (libgit2-branch-checked-out-p masterref))
      (should-not (libgit2-branch-checked-out-p secondref)))))

(ert-deftest branch-head-p ()
  (with-temp-dir path
    (init)
    (commit-change "test" "content")
    (run "git" "branch" "second")
    (let* ((repo (libgit2-repository-open path))
           (masterref (libgit2-branch-lookup repo "master"))
           (secondref (libgit2-branch-lookup repo "second")))
      (should (libgit2-branch-head-p masterref))
      (should-not (libgit2-branch-head-p secondref)))))

(ert-deftest branch-name ()
  (with-temp-dir path
    (init)
    (commit-change "test" "content")
    (run "git" "branch" "second")
    (let* ((repo (libgit2-repository-open path))
           (ref (libgit2-branch-lookup repo "second")))
      (should-error (libgit2-branch-name nil))
      (should (string= "second" (libgit2-branch-name ref))))))

(ert-deftest branch-remote-name ()
  (with-temp-dir (path path-upstream)
    (in-dir path-upstream
      (init)
      (commit-change "test" "content"))
    (in-dir path
      (init)
      (commit-change "test" "content")
      (run "git" "remote" "add" "-f" "upstream" (concat "file://" path-upstream)))
    (let ((repo (libgit2-repository-open path)))
      (should-error (libgit2-branch-remote-name nil "refs/remotes/upstream/master"))
      (should-error (libgit2-branch-remote-name repo nil))
      (should-error (libgit2-branch-remote-name repo "master"))
      (should (string= "upstream" (libgit2-branch-remote-name repo "refs/remotes/upstream/master"))))))

(ert-deftest branch-upstream ()
  (with-temp-dir (path path-upstream)
    (in-dir path-upstream
      (init)
      (commit-change "test" "content"))
    (in-dir path
      (init)
      (commit-change "test" "content")
      (run "git" "remote" "add" "-f" "upstream" (concat "file://" path-upstream))
      (run "git" "branch" "second" "upstream/master"))
    (let* ((repo (libgit2-repository-open path))
           (ref (libgit2-reference-lookup repo "refs/heads/second")))
      (should-error (libgit2-branch-upstream-name nil "refs/heads/second"))
      (should-error (libgit2-branch-upstream-name repo nil))
      (should-error (libgit2-branch-upstream-name repo "refs/heads/master"))
      (should (string= "refs/remotes/upstream/master" (libgit2-branch-upstream-name repo "refs/heads/second")))
      (should (string= "refs/remotes/upstream/master" (libgit2-reference-name (libgit2-branch-upstream ref))))
      (should (string= "upstream" (libgit2-branch-upstream-remote repo "refs/heads/second"))))))

(ert-deftest branch-set-upstream ()
  (with-temp-dir path
    (init)
    (commit-change "test" "content")
    (run "git" "branch" "other")
    (let* ((repo (libgit2-repository-open path))
           (master (libgit2-reference-dwim repo "master")))
      (libgit2-branch-set-upstream master "other")
      (should (string= "refs/heads/other" (libgit2-reference-name (libgit2-branch-upstream master))))
      (should-error (libgit2-branch-set-upstream master "zomg") :type 'giterr-reference))))

(ert-deftest branch-move ()
  (with-temp-dir path
    (init)
    (commit-change "test" "content")
    (let* ((repo (libgit2-repository-open path))
           (master (libgit2-reference-dwim repo "master"))
           (moved (libgit2-branch-move master "zing")))
      (should (string= "refs/heads/master" (libgit2-reference-name master)))
      (should (string= "refs/heads/zing" (libgit2-reference-name moved)))
      (should-error (libgit2-reference-dwim repo "master") :type 'giterr-reference))))

(ert-deftest branch-foreach ()
  (with-temp-dir path
    (init)
    (commit-change "test" "content")
    (run "git" "branch" "alpha")
    (run "git" "branch" "bravo")
    (run "git" "branch" "charlie")
    (let* ((repo (libgit2-repository-open path))
           data)
      (libgit2-branch-foreach repo 'local (lambda (ref) (push (libgit2-reference-name ref) data)))
      (should (equal (reverse data)
                     '("refs/heads/alpha"
                       "refs/heads/bravo"
                       "refs/heads/charlie"
                       "refs/heads/master")))

      (setq data nil)
      (libgit2-branch-foreach repo 'remote (lambda (ref) (push (libgit2-reference-name ref) data)))
      (should-not data))))
