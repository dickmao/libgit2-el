(ert-deftest checkout-simple ()
  (with-temp-dir path
    (init)
    (write "a" "abcdef")
    (write "b" "ghijkl")
    (write "c" "mnopqr")
    (add "a" "b" "c")
    (commit)
    (commit-change "a" "changed")
    (write "a" "conflicting")
    (let* ((repo (libgit2-repository-open path))
           (head (libgit2-revparse-single repo "HEAD")))

      ;; Dry run shouldn't change anything
      (libgit2-checkout-tree repo head '((strategy . none)))
      (should (string= "conflicting" (read-file-nnl "a")))

      ;; But it should notify
      (let (data)
        (libgit2-checkout-tree
         repo head
         '((strategy . none)
           (notify-when . all)
           (notify . (lambda (&rest args) (push args data)))))
        (should (equal data '((dirty "a"))))
        (should (string= "conflicting" (read-file-nnl "a"))))

      ;; Same with safe
      (libgit2-checkout-tree repo head '((strategy . safe)))
      (should (string= "conflicting" (read-file-nnl "a")))

      (let (data)
        (libgit2-checkout-tree
         repo head
         '((strategy . safe)
           (notify-when . all)
           (notify . (lambda (&rest args) (push args data)))))
        (should (equal data '((dirty "a"))))
        (should (string= "conflicting" (read-file-nnl "a"))))

      ;; Force will change it
      (libgit2-checkout-tree repo head '((strategy . force)))
      (should (string= "changed" (read-file-nnl "a")))

      ;; Even with no conflicts, dry-run won't change anything
      (libgit2-checkout-tree repo (libgit2-commit-parent head) '((strategy . none)))
      (should (string= "changed" (read-file-nnl "a")))

      ;; But safe will
      (libgit2-checkout-tree repo (libgit2-commit-parent head) '((strategy . safe)))
      (should (string= "abcdef" (read-file-nnl "a"))))))

(ert-deftest checkout-error-on-conflict ()
  (with-temp-dir path
    (init)
    (write "a" "abcdef")
    (write "b" "ghijkl")
    (write "c" "mnopqr")
    (add "a" "b" "c")
    (commit)
    (write "a" "ABCDEF")
    (write "b" "GHIJKL")
    (write "c" "MNOPQR")
    (add "a" "b" "c")
    (commit)
    (write "b" "conflicting")
    (let* ((repo (libgit2-repository-open path))
           (head (libgit2-revparse-single repo "HEAD")))
      (should-error
       (libgit2-checkout-tree repo (libgit2-commit-parent head) '((strategy . none)))
       :type 'giterr-checkout)
      (should-error
       (libgit2-checkout-tree repo (libgit2-commit-parent head) '((strategy . safe)))
       :type 'giterr-checkout)
      (libgit2-checkout-tree repo (libgit2-commit-parent head) '((strategy . force)))
      (should (string= "abcdef" (read-file-nnl "a")))
      (should (string= "ghijkl" (read-file-nnl "b")))
      (should (string= "mnopqr" (read-file-nnl "c"))))))

(ert-deftest checkout-notify-abort ()
  (with-temp-dir path
    (init)
    (commit-change "a" "abcdef")
    (commit-change "a" "ghijkl")
    (let* ((repo (libgit2-repository-open path))
           (head (libgit2-revparse-single repo "HEAD")))
      (libgit2-checkout-tree
       repo (libgit2-commit-parent head)
       '((strategy . force)
         (notify-when . all)
         (notify . (lambda (reason path)
                     (should (eq reason 'updated))
                     (should (string= path "a"))
                     'abort))))
      (should (string= "ghijkl" (read-file-nnl "a"))))))

(ert-deftest checkout-progress ()
  (with-temp-dir path
    (init)
    (write "a" "abc")
    (write "b" "def")
    (write "c" "ghi")
    (write "d" "jkl")
    (write "e" "mno")
    (add "a" "b" "c" "d" "e")
    (commit)
    (write "a" "ABC")
    (write "b" "DEF")
    (write "c" "GHI")
    (write "d" "JKL")
    (write "e" "MNO")
    (add "a" "b" "c" "d" "e")
    (commit)
    (let* ((repo (libgit2-repository-open path))
           (head (libgit2-revparse-single repo "HEAD"))
           data)

      (libgit2-checkout-tree
       repo (libgit2-commit-parent head)
       '((progress . (lambda (&rest args) (push args data)))))
      (should (equal (reverse data)
                     '((nil 0 5) ("a" 1 5) ("b" 2 5) ("c" 3 5) ("d" 4 5) ("e" 5 5))))
      (should (string= "abc" (read-file-nnl "a")))

      ;; Check that signals in progress are ignored
      ;; Use force here because we haven't updated HEAD, so the repo is dirty
      (setq data nil)
      (libgit2-checkout-tree
       repo head
       '((strategy . force)
         (progress . (lambda (&rest args) (push args data) (error "no!")))))
      (should (equal (reverse data)
                     '((nil 0 5) ("a" 1 5) ("b" 2 5) ("c" 3 5) ("d" 4 5) ("e" 5 5))))
      (should (string= "ABC" (read-file-nnl "a"))))))

(ert-deftest checkout-baseline ()
  (with-temp-dir path
    (init)
    (write "a" "abcdef")
    (write "b" "ghijkl")
    (write "c" "mnopqr")
    (add "a" "b" "c")
    (commit)
    (commit-change "a" "changed")
    (write "a" "conflicting")
    (let* ((repo (libgit2-repository-open path))
           (head (libgit2-revparse-single repo "HEAD")))

      (should-error
       (libgit2-checkout-tree
        repo (libgit2-commit-parent head)
        `((strategy . none)
          (baseline . ,(libgit2-commit-tree head))))
       :type 'giterr-checkout)
      (should (string= "conflicting" (read-file-nnl "a")))

      (should-error
       (libgit2-checkout-tree
        repo (libgit2-commit-parent head)
        `((strategy . safe)
          (baseline . ,(libgit2-commit-tree head))))
       :type 'giterr-checkout)
      (should (string= "conflicting" (read-file-nnl "a")))

      ;; Force does not check baseline
      (libgit2-checkout-tree
       repo (libgit2-commit-parent head)
       `((strategy . force)
         (baseline . ,(libgit2-commit-tree head))))
      (should (string= "abcdef" (read-file-nnl "a"))))))
