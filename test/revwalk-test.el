(ert-deftest revwalk-linear ()
  (with-temp-dir path
    (init)
    (commit-change "a" "abc")
    (commit-change "b" "abc")
    (commit-change "c" "abc")
    (commit-change "d" "abc")
    (commit-change "e" "abc")
    (commit-change "f" "abc")
    (let* ((repo (libgit2-repository-open path))
           (walk (libgit2-revwalk-new repo))
           (head (libgit2-revparse-single repo "HEAD"))
           seen)
      (libgit2-revwalk-push-head walk)
      (libgit2-revwalk-foreach walk (lambda (id) (push id seen)))
      (should (equal (reverse seen)
                     (cl-loop for i below 6
                              collect (libgit2-commit-id
                                       (libgit2-commit-nth-gen-ancestor head i))))))))

(ert-deftest revwalk-hidden ()
  (with-temp-dir path
    (init)
    (commit-change "a" "abc")
    (commit-change "b" "abc")
    (commit-change "c" "abc")
    (commit-change "d" "abc")
    (commit-change "e" "abc")
    (commit-change "f" "abc")
    (let* ((repo (libgit2-repository-open path))
           (walk (libgit2-revwalk-new repo))
           (head (libgit2-revparse-single repo "HEAD"))
           seen)
      (libgit2-revwalk-push-head walk)
      (libgit2-revwalk-hide walk (libgit2-commit-id
                                 (libgit2-commit-nth-gen-ancestor head 3)))
      (libgit2-revwalk-foreach walk (lambda (id) (push id seen)))
      (should (equal (reverse seen)
                     (cl-loop for i below 3
                              collect (libgit2-commit-id
                                       (libgit2-commit-nth-gen-ancestor head i))))))))

(ert-deftest revwalk-range ()
  (with-temp-dir path
    (init)
    (commit-change "a" "abc")
    (commit-change "b" "abc")
    (commit-change "c" "abc")
    (commit-change "d" "abc")
    (commit-change "e" "abc")
    (commit-change "f" "abc")
    (let* ((repo (libgit2-repository-open path))
           (walk (libgit2-revwalk-new repo))
           (head (libgit2-revparse-single repo "HEAD"))
           seen)
      (libgit2-revwalk-push-range
       walk
       (concat (libgit2-commit-id (libgit2-commit-nth-gen-ancestor head 3))
               ".." (libgit2-commit-id head)))
      (libgit2-revwalk-foreach walk (lambda (id) (push id seen)))
      (should (equal (reverse seen)
                     (cl-loop for i below 3
                              collect (libgit2-commit-id
                                       (libgit2-commit-nth-gen-ancestor head i))))))))

(ert-deftest revwalk-hide-callback ()
  (with-temp-dir path
    (init)
    (commit-change "a" "abc")
    (commit-change "b" "abc")
    (commit-change "c" "abc")
    (commit-change "d" "abc")
    (commit-change "e" "abc")
    (commit-change "f" "abc")
    (let* ((repo (libgit2-repository-open path))
           (walk (libgit2-revwalk-new repo))
           (head (libgit2-revparse-single repo "HEAD"))
           seen)
      (libgit2-revwalk-push-head walk)
      (libgit2-revwalk-foreach
       walk
       (lambda (id) (push id seen))
       (lambda (id)
         (string= id (libgit2-commit-id
                      (libgit2-commit-nth-gen-ancestor head 3)))))
      (should (equal (reverse seen)
                     (cl-loop for i below 3
                              collect (libgit2-commit-id
                                       (libgit2-commit-nth-gen-ancestor head i))))))))
