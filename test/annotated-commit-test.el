(ert-deftest annotated-commit-from-ref ()
  (with-temp-dir path
    (init)
    (commit-change "test" "foo")
    (let* ((repo (libgit2-repository-open path))
           (ref  (libgit2-reference-lookup repo "HEAD"))
           (ann  (libgit2-annotated-commit-from-ref repo ref)))
      (should (libgit2-annotated-commit-p ann)))))

(ert-deftest annotated-commit-from-fetchhead ()
  (with-temp-dir remote-path
    (init)
    (commit-change "test" "aaa" "Commit A")
    (let* ((remote-repo (libgit2-repository-open remote-path))
	   (remote-id (libgit2-reference-name-to-id remote-repo "HEAD")))

      (with-temp-dir local-path
        (init)
	(commit-change "test" "bbb" "Commit B")
	(run "git" "fetch" remote-path)

	(let* ((local-repo (libgit2-repository-open local-path))
               (ann  (libgit2-annotated-commit-from-fetchhead
		      local-repo
		      "refs/heads/master"
		      remote-path
		      remote-id)))
	  (should (libgit2-annotated-commit-p ann)))))))

(ert-deftest annotated-commit-from-revspec ()
  (with-temp-dir path
    (init)
    (commit-change "test1" "foo")
    (commit-change "test2" "bar")
    (let* ((repo (libgit2-repository-open path))
           (ann  (libgit2-annotated-commit-from-revspec repo "HEAD^")))
      (should (libgit2-annotated-commit-p ann)))))

(ert-deftest annotated-commit-lookup ()
  (with-temp-dir path
    (init)
    (commit-change "test" "foo")
    (let* ((repo (libgit2-repository-open path))
	   (id (libgit2-reference-name-to-id repo "HEAD"))
           (ann  (libgit2-annotated-commit-from-revspec repo id)))
      (should (libgit2-annotated-commit-p ann)))))

(ert-deftest annotated-commit-id ()
  (with-temp-dir path
    (init)
    (commit-change "test1" "foo")
    (let* ((repo (libgit2-repository-open path))
	   (revspec-id (libgit2-reference-name-to-id repo "HEAD")))
      (commit-change "test2" "bar")
      (let ((ann  (libgit2-annotated-commit-from-revspec repo "HEAD^")))
	(should (string= revspec-id
			 (libgit2-annotated-commit-id ann)))))))
