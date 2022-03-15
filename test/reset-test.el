(ert-deftest reset ()
  ;; Soft reset
  (with-temp-dir path
    (init)
    (commit-change "a" "content-a1")
    (let* ((repo (libgit2-repository-open path))
	   (reset-to
	    (libgit2-object-lookup
	     repo (libgit2-reference-name-to-id repo "HEAD"))))

      (commit-change "a" "content-a2")
      (libgit2-reset repo reset-to 'soft)

      ;; Head should reset, content of a file is preserved, file is
      ;; staged.
      (should (string= (libgit2-reference-name-to-id repo "HEAD")
		       (libgit2-object-id reset-to)))
      (should (string= (read-file "a") "content-a2"))
      (should (member 'index-modified (libgit2-status-file repo "a")))))

  ;; Mixed reset
  (with-temp-dir path
    (init)
    (commit-change "a" "content-a1")
    (let* ((repo (libgit2-repository-open path))
  	   (reset-to
  	    (libgit2-object-lookup
  	     repo (libgit2-reference-name-to-id repo "HEAD"))))

      (commit-change "a" "content-a2")
      (libgit2-reset repo reset-to 'mixed)

      ;; Head should reset, content of a file is preserved, file is
      ;; unstaged.
      (should (string= (libgit2-reference-name-to-id repo "HEAD")
  		       (libgit2-object-id reset-to)))
      (should (string= (read-file "a") "content-a2"))
      (should (member 'wt-modified (libgit2-status-file repo "a")))))

  ;; Hard reset
  (with-temp-dir path
    (init)
    (commit-change "a" "content-a1")
    (let* ((repo (libgit2-repository-open path))
  	   (reset-to
  	    (libgit2-object-lookup
  	     repo (libgit2-reference-name-to-id repo "HEAD"))))

      (commit-change "a" "content-a2")
      (libgit2-reset repo reset-to 'hard)

      ;; Head should reset, content of a file is lost.
      (should (string= (libgit2-reference-name-to-id repo "HEAD")
  		       (libgit2-object-id reset-to)))
      (should (string= (read-file "a") "content-a1"))
      (should (eq nil (libgit2-status-file repo "a"))))))

(ert-deftest reset-from-annotated ()
  ;; Soft reset
  (with-temp-dir path
    (init)
    (commit-change "a" "content-a1")
    (let* ((repo (libgit2-repository-open path))
	   (reset-to
	    (libgit2-annotated-commit-from-revspec repo "HEAD")))

      (commit-change "a" "content-a2")
      (libgit2-reset-from-annotated repo reset-to 'soft)

      ;; Head should reset, content of a file is preserved, file is
      ;; staged.
      (should (string= (libgit2-reference-name-to-id repo "HEAD")
		       (libgit2-annotated-commit-id reset-to)))
      (should (string= (read-file "a") "content-a2"))
      (should (member 'index-modified (libgit2-status-file repo "a")))))

  ;; Mixed reset
  (with-temp-dir path
    (init)
    (commit-change "a" "content-a1")
    (let* ((repo (libgit2-repository-open path))
  	   (reset-to
  	    (libgit2-annotated-commit-from-revspec repo "HEAD")))

      (commit-change "a" "content-a2")
      (libgit2-reset-from-annotated repo reset-to 'mixed)

      ;; Head should reset, content of a file is preserved, file is
      ;; unstaged.
      (should (string= (libgit2-reference-name-to-id repo "HEAD")
  		       (libgit2-annotated-commit-id reset-to)))
      (should (string= (read-file "a") "content-a2"))
      (should (member 'wt-modified (libgit2-status-file repo "a")))))

  ;; Hard reset
  (with-temp-dir path
    (init)
    (commit-change "a" "content-a1")
    (let* ((repo (libgit2-repository-open path))
  	   (reset-to
  	    (libgit2-annotated-commit-from-revspec repo "HEAD")))

      (commit-change "a" "content-a2")
      (libgit2-reset-from-annotated repo reset-to 'hard)

      ;; Head should reset, content of a file is lost.
      (should (string= (libgit2-reference-name-to-id repo "HEAD")
  		       (libgit2-annotated-commit-id reset-to)))
      (should (string= (read-file "a") "content-a1"))
      (should (eq nil (libgit2-status-file repo "a"))))))

(ert-deftest reset-default ()
  (with-temp-dir path
    (init)
    (commit-change "a" "content-a1")
    (commit-change "b" "content-b1")
    (let* ((repo (libgit2-repository-open path))
	   (reset-to
	    (libgit2-object-lookup
	     repo (libgit2-reference-name-to-id repo "HEAD"))))

      (write "a" "content-a2")
      (write "b" "content-b2")
      (add "a" "b")
      (libgit2-reset-default repo reset-to '("a"))

      ;; Head should not move, files matching pathspec should become
      ;; unstaged.
      (should (string= (libgit2-reference-name-to-id repo "HEAD")
		       (libgit2-object-id reset-to)))
      (should (string= (read-file "a") "content-a2"))
      (should (string= (read-file "b") "content-b2"))
      (should (member 'wt-modified (libgit2-status-file repo "a")))
      (should (member 'index-modified (libgit2-status-file repo "b"))))))
