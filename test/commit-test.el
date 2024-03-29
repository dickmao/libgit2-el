(ert-deftest commit-lookup ()
  (with-temp-dir path
    (init)
    (commit-change "test" "content")
    (let* ((repo (libgit2-repository-open path))
           (id (libgit2-reference-name-to-id repo "HEAD"))
           (commit (libgit2-commit-lookup repo id))
           (commit-short (libgit2-commit-lookup-prefix repo (substring id 0 7))))
      (should (libgit2-commit-p commit))
      (should (string= id (libgit2-commit-id commit)))
      (should (string= id (libgit2-commit-id commit-short)))
      (should-error (libgit2-commit-lookup repo "test") :type 'giterr-invalid))))

(ert-deftest commit-parentcount ()
  (with-temp-dir path
    (init)
    (commit-change "test" "content")
    (let* ((repo (libgit2-repository-open path))
           (id (libgit2-reference-name-to-id repo "HEAD"))
           (commit (libgit2-commit-lookup repo id)))
      (should (= 0 (libgit2-commit-parentcount commit))))
    (commit-change "test" "more content")
    (let* ((repo (libgit2-repository-open path))
           (id (libgit2-reference-name-to-id repo "HEAD"))
           (commit (libgit2-commit-lookup repo id)))
      (should (= 1 (libgit2-commit-parentcount commit))))))

(ert-deftest commit-parent-id ()
  (with-temp-dir path
    (init)
    (commit-change "test" "content")
    (let* ((repo (libgit2-repository-open path))
           (parent-id (libgit2-reference-name-to-id repo "HEAD")))
      (commit-change "test" "more content")
      (let* ((this-id (libgit2-reference-name-to-id repo "HEAD"))
             (commit (libgit2-commit-lookup repo this-id)))
        (should (string= parent-id (libgit2-commit-parent-id commit)))
        (should (string= parent-id (libgit2-commit-parent-id commit 0)))
        (should (string= parent-id (libgit2-commit-id (libgit2-commit-parent commit))))
        (should (string= parent-id (libgit2-commit-id (libgit2-commit-parent commit 0))))
        (should-error (libgit2-commit-parent-id commit 1) :type 'args-out-of-range)
        (should-error (libgit2-commit-parent commit 1) :type 'giterr-invalid)))))

(ert-deftest commit-ancestor ()
  (with-temp-dir path
    (init)
    (commit-change "test" "content")
    (let* ((repo (libgit2-repository-open path))
           (id-1 (libgit2-reference-name-to-id repo "HEAD")))
      (commit-change "test" "more content")
      (let* ((id-2 (libgit2-reference-name-to-id repo "HEAD")))
        (commit-change "test" "so much content wow")
        (let* ((id-3 (libgit2-reference-name-to-id repo "HEAD"))
               (commit (libgit2-commit-lookup repo id-3)))
          (should (string= id-3 (libgit2-commit-id (libgit2-commit-nth-gen-ancestor commit 0))))
          (should (string= id-2 (libgit2-commit-id (libgit2-commit-nth-gen-ancestor commit 1))))
          (should (string= id-1 (libgit2-commit-id (libgit2-commit-nth-gen-ancestor commit 2))))
          (should-error (libgit2-commit-nth-gen-ancestor commit 3) :type 'giterr-invalid)
          (should-error (libgit2-commit-nth-gen-ancestor commit -1) :type 'giterr-invalid))))))

(ert-deftest commit-author-committer ()
  (with-temp-dir path
    (init)
    (commit-change "test" "content")
    (let* ((repo (libgit2-repository-open path))
           (id (libgit2-reference-name-to-id repo "HEAD"))
           (commit (libgit2-commit-lookup repo id))
           (author (libgit2-commit-author commit))
           (committer (libgit2-commit-committer commit)))
      (should (string= "A U Thor" (libgit2-signature-name author)))
      (should (string= "author@example.com" (libgit2-signature-email author)))
      (should (string= "A U Thor" (libgit2-signature-name committer)))
      (should (string= "author@example.com" (libgit2-signature-email committer))))))

(ert-deftest commit-message ()
  (with-temp-dir path
    (init)
    (commit-change "test" "content" "here is a message!")
    (let* ((repo (libgit2-repository-open path))
           (id (libgit2-reference-name-to-id repo "HEAD"))
           (commit (libgit2-commit-lookup repo id)))
      (should (string= "here is a message!\n" (libgit2-commit-message commit))))))

(ert-deftest commit-summary ()
  (with-temp-dir path
    (init)
    (commit-change "test" "content" "here is a message!\n\nhere is some more info")
    (let* ((repo (libgit2-repository-open path))
           (id (libgit2-reference-name-to-id repo "HEAD"))
           (commit (libgit2-commit-lookup repo id)))
      (should (string= "here is a message!" (libgit2-commit-summary commit)))
      (should (string= "here is some more info" (libgit2-commit-body commit))))))

(ert-deftest commit-create ()
  (with-temp-dir path
    (init)
    (commit-change "b" "contents")
    (write "a" "abcdef")
    (write "b" "ghijkl")
    (write "somedir/c" "mnopqr")
    (let* ((repo (libgit2-repository-open path))
           (index (libgit2-repository-index repo))
           (parent (libgit2-revparse-single repo "HEAD"))
           tree commit)
      (libgit2-index-add-bypath index "a")
      (libgit2-index-add-bypath index "b")
      (libgit2-index-add-bypath index "somedir/c")
      (setq tree (libgit2-tree-lookup repo (libgit2-index-write-tree index)))
      (setq commit (libgit2-commit-lookup
                    repo
                    (libgit2-commit-create
                     repo "HEAD"
                     (libgit2-signature-now "Zeus" "zeus@olympus.gr")
                     (libgit2-signature-now "Hades" "hades@underworld.gr")
                     "This is a commit message"
                     tree (list parent))))
      (should (= 1 (libgit2-commit-parentcount commit)))
      (should (string= (libgit2-commit-id parent) (libgit2-commit-parent-id commit)))
      (should (string= "Zeus" (libgit2-signature-name (libgit2-commit-author commit))))
      (should (string= "zeus@olympus.gr" (libgit2-signature-email (libgit2-commit-author commit))))
      (should (string= "Hades" (libgit2-signature-name (libgit2-commit-committer commit))))
      (should (string= "hades@underworld.gr" (libgit2-signature-email (libgit2-commit-committer commit))))
      (should (string= "This is a commit message" (libgit2-commit-message commit)))
      (should (string= (libgit2-tree-id tree) (libgit2-commit-tree-id commit)))
      (should (string= (libgit2-commit-id commit) (libgit2-reference-name-to-id repo "HEAD")))
      (should (string= (libgit2-commit-id commit) (libgit2-reference-name-to-id repo "refs/heads/master"))))))
