(ert-deftest remote-urls ()
  (with-temp-dir (path remote-a remote-b)
    (in-dir remote-a
      (init)
      (commit-change "test-a" "content"))
    (in-dir remote-b
      (init)
      (commit-change "test-b" "content"))
    (in-dir path
      (init)
      (run "git" "remote" "add" "a" (concat file-prefix remote-a))
      (run "git" "remote" "add" "b" (concat file-prefix remote-b))
      (run "git" "remote" "set-url" "--push" "b" "some-url")
      (let* ((repo (libgit2-repository-open path))
             (ra (libgit2-remote-lookup repo "a"))
             (rb (libgit2-remote-lookup repo "b")))
        (should (equal '("a" "b") (libgit2-remote-list repo)))
        (should (libgit2-remote-p (libgit2-remote-lookup repo "a")))
        (should-error (libgit2-remote-lookup repo "c") :type 'giterr-config)
        (should (string-prefix-p file-prefix (libgit2-remote-url ra)))
        (should (path= remote-a (substring (libgit2-remote-url ra) (length file-prefix))))
        (should-not (libgit2-remote-pushurl ra))
        (should (string-prefix-p file-prefix (libgit2-remote-url rb)))
        (should (path= remote-b (substring (libgit2-remote-url rb) (length file-prefix))))
        (should (string= "some-url" (libgit2-remote-pushurl rb)))))))

(ert-deftest remote-refspecs ()
  (with-temp-dir (path rpath-a rpath-b)
    (in-dir rpath-a
      (init)
      (commit-change "test-a" "content"))
    (in-dir rpath-b
      (init)
      (commit-change "test-b" "content")
      (run "git" "checkout" "-b" "branchname"))
    (in-dir path
      (init)
      (run "git" "remote" "add" "rema" (concat file-prefix rpath-a))
      (run "git" "remote" "add" "-t" "branchname" "-t" "otherbranch" "remb" (concat file-prefix rpath-a))
      (let* ((repo (libgit2-repository-open path))
             (remote-a (libgit2-remote-lookup repo "rema"))
             (remote-b (libgit2-remote-lookup repo "remb")))
        (should (= 1 (libgit2-remote-refspec-count remote-a)))
        (should (= 2 (libgit2-remote-refspec-count remote-b)))
        (should (equal '("+refs/heads/*:refs/remotes/rema/*")
                       (libgit2-remote-get-refspecs remote-a)))
        (should (equal '("+refs/heads/branchname:refs/remotes/remb/branchname"
                         "+refs/heads/otherbranch:refs/remotes/remb/otherbranch")
                       (libgit2-remote-get-refspecs remote-b)))
        (should-not (libgit2-remote-get-refspecs remote-a 'push))
        (should-not (libgit2-remote-get-refspecs remote-b 'push))
        (let ((spec (libgit2-remote-get-refspec remote-b 0)))
          (should (eq 'fetch (libgit2-refspec-direction spec)))
          (should (string= "refs/remotes/remb/branchname" (libgit2-refspec-dst spec)))
          (should (string= "refs/heads/branchname" (libgit2-refspec-src spec)))
          (should (string= "+refs/heads/branchname:refs/remotes/remb/branchname" (libgit2-refspec-string spec)))
          (should (libgit2-refspec-force-p spec)))))))

(ert-deftest remote-fetch-simple ()
  (let (id)
    (with-temp-dir (path rpath)
      (in-dir rpath
        (init)
        (commit-change "test" "content")
        (setq id (rev-parse)))
      (in-dir path
        (init)
        (run "git" "remote" "add" "origin" (concat file-prefix rpath))
        (let* ((repo (libgit2-repository-open path))
               (remote (libgit2-remote-lookup repo "origin")))
          (should-not (libgit2-reference-list repo))
          (libgit2-remote-fetch remote)
          (should (equal (libgit2-reference-list repo) '("refs/remotes/origin/master")))
          (should (string= id (libgit2-reference-name-to-id repo "refs/remotes/origin/master"))))))))

(ert-deftest remote-fetch-nonexistent ()
  (let (id)
    (with-temp-dir path
      (init)
      (run "git" "remote" "add" "origin" (concat file-prefix "nonexistent"))
      (let* ((repo (libgit2-repository-open path))
             (remote (libgit2-remote-lookup repo "origin")))
        (should-error (libgit2-remote-fetch remote) :type 'giterr-os)))))

(ert-deftest remote-push-simple ()
  (let (id)
    (with-temp-dir (path rpath)
      (in-dir rpath
        (init "--bare"))
      (in-dir path
        (init)
        (commit-change "test" "content")
        (setq id (rev-parse))
        (run "git" "remote" "add" "origin" (concat file-prefix rpath))
        (let* ((repo (libgit2-repository-open path))
               (remote (libgit2-remote-lookup repo "origin"))
               (remote-repo (libgit2-repository-open rpath)))

          (let ((remote (libgit2-remote-lookup repo "origin")))
            (should-not (libgit2-remote-get-refspecs remote 'push))
            (libgit2-remote-push remote))

          (should-not (libgit2-reference-list remote-repo))
          (should-error (libgit2-commit-lookup remote-repo id) :type 'giterr-odb)

          (libgit2-remote-add-refspec repo "origin" "refs/heads/master:refs/heads/otherbranch" 'push)
          (let ((remote (libgit2-remote-lookup repo "origin")))
            (should (equal '("refs/heads/master:refs/heads/otherbranch")
                           (libgit2-remote-get-refspecs remote 'push)))
            (libgit2-remote-push remote))

          (should (equal (libgit2-reference-list remote-repo) '("refs/heads/otherbranch")))
          (should (string= id (libgit2-reference-name-to-id remote-repo "refs/heads/otherbranch"))))))))

(ert-deftest remote-push-nonexistent ()
  (let (id)
    (with-temp-dir path
      (init)
      (run "git" "remote" "add" "origin" (concat file-prefix "nonexistent"))
      (let* ((repo (libgit2-repository-open path))
             (remote (libgit2-remote-lookup repo "origin")))
        (should-error (libgit2-remote-push remote) :type 'giterr-os)))))
