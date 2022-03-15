(ert-deftest ignore ()
  (with-temp-dir path
    (init)
    (let ((repo (libgit2-repository-open path)))
      (libgit2-ignore-add-rule repo "testrule")
      (should (eql t (libgit2-ignore-path-ignored-p repo "testrule")))
      (libgit2-ignore-clear-internal-rules repo)
      (should (eql nil (libgit2-ignore-path-ignored-p repo "testrule"))))))
