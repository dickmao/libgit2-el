(ert-deftest refcount ()
  (with-temp-dir path
    (init)
    (commit-change "test" "content")
    (let ((repo (libgit2-repository-open path)))
      (should (= 1 (libgit2--refcount repo)))
      (let ((ref (libgit2-repository-head repo)))
        (should (= 2 (libgit2--refcount repo)))
        (let ((obj (libgit2-reference-peel ref)))
          (should (= 3 (libgit2--refcount repo))))))))

(ert-deftest memtest-1 ()
  (skip-unless nil)

  ;; Clean out stacks
  (garbage-collect)
  (libgit2--allocs)
  (libgit2--finalizes)
  (libgit2--frees)

  (let (repo repo-ptr ref ref-ptr obj obj-ptr new-repo)
    (with-temp-dir path
      (init)
      (commit-change "test" "content")

      ;; Allocate exactly one repository object
      (setq repo (libgit2-repository-open path))
      (setq repo-ptr (libgit2--wrapper repo))
      (should (member repo-ptr (libgit2--allocs)))
      (should-not (member repo-ptr (libgit2--finalizes)))
      (should-not (member repo-ptr (libgit2--frees)))
      (should (= 0 (libgit2--parent-wrapper repo)))

      ;; Allocate exactly one reference object
      (setq ref (libgit2-repository-head repo))
      (setq ref-ptr (libgit2--wrapper ref))
      (should (member ref-ptr (libgit2--allocs)))
      (let ((fin (libgit2--finalizes)))
        (should-not (member repo-ptr fin))
        (should-not (member ref-ptr fin)))
      (let ((free (libgit2--frees)))
        (should-not (member repo-ptr free))
        (should-not (member ref-ptr free)))
      (should (= 2 (libgit2--refcount repo)))
      (should (= repo-ptr (libgit2--parent-wrapper ref)))

      ;; Allocate exactly one commit
      (setq obj (libgit2-reference-peel ref))
      (setq obj-ptr (libgit2--wrapper obj))
      (should (member obj-ptr (libgit2--allocs)))
      (let ((fin (libgit2--finalizes)))
        (should-not (member repo-ptr fin))
        (should-not (member ref-ptr fin)))
      (let ((free (libgit2--frees)))
        (should-not (member repo-ptr free))
        (should-not (member ref-ptr free)))
      (should (= 3 (libgit2--refcount repo)))
      (should (= repo-ptr (libgit2--parent-wrapper obj)))

      ;; Delete the reference object and run GC
      ;; finalize and free the reference ptr
      ;; finalize but don't free the repo ptr
      (setq ref nil)
      (garbage-collect)
      (let ((fin (libgit2--finalizes)))
        (should (member ref-ptr fin))
        (should (member repo-ptr fin)))
      (let ((free (libgit2--frees)))
        (should (member ref-ptr free))
        (should-not (member repo-ptr free)))
      (should (= 2 (libgit2--refcount repo)))

      ;; Get a new user-ptr to the repo
      ;; it's different in Emacs eq-sense but points
      ;; not only to the same git object but the same wrapper
      (setq new-repo (libgit2-object-owner obj))
      (should-not (eq new-repo repo))
      (should (= repo-ptr (libgit2--wrapper new-repo)))
      (should (= 3 (libgit2--refcount repo))))))

(ert-deftest memtest-2 ()
  (skip-unless nil)

  ;; Clean out stacks
  (garbage-collect)
  (libgit2--allocs)
  (libgit2--finalizes)
  (libgit2--frees)

  (let (repo repo-ptr ref ref-ptr)
    (with-temp-dir path
      (init)
      (commit-change "test" "content")

      ;; Allocate exactly one repository object
      (setq repo (libgit2-repository-open path))
      (setq repo-ptr (libgit2--wrapper repo))
      (should (member repo-ptr (libgit2--allocs)))
      (should-not (member repo-ptr (libgit2--finalizes)))
      (should-not (member repo-ptr (libgit2--frees)))
      (should (= 0 (libgit2--parent-wrapper repo)))

      ;; Allocate exactly one reference object
      (setq ref (libgit2-repository-head repo))
      (setq ref-ptr (libgit2--wrapper ref))
      (should (member ref-ptr (libgit2--allocs)))
      (let ((fin (libgit2--finalizes)))
        (should-not (member repo-ptr fin))
        (should-not (member ref-ptr fin)))
      (let ((free (libgit2--frees)))
        (should-not (member repo-ptr free))
        (should-not (member ref-ptr free)))
      (should (= 2 (libgit2--refcount repo)))
      (should (= repo-ptr (libgit2--parent-wrapper ref)))

      ;; Delete the repository object and run GC
      (setq repo nil)
      (garbage-collect)
      (let ((fin (libgit2--finalizes)))
        (should (member repo-ptr fin))
        (should-not (member ref-ptr fin)))
      (let ((free (libgit2--frees)))
        (should-not (member repo-ptr free))
        (should-not (member ref-ptr free)))

      ;; Delete the reference object and run GC
      (setq ref nil)
      (garbage-collect)
      (let ((fin (libgit2--finalizes)))
        (should (member repo-ptr fin))
        (should (member ref-ptr fin)))
      (let ((free (libgit2--frees)))
        (should (member repo-ptr free))
        (should (member ref-ptr free))))))
