(ert-deftest blob-text ()
  (with-temp-dir path
    (init)
    (commit-change "filename" "line1\nline2\nline3\n")
    (let* ((repo (libgit2-repository-open path))
           (blob (libgit2-revparse-single repo "HEAD:filename")))
      (should (libgit2-blob-p blob))
      (should-not (libgit2-blob-binary-p blob))
      (should (= 18 (libgit2-blob-rawsize blob)))
      (should (string= "line1\nline2\nline3\n" (libgit2-blob-rawcontent blob)))
      (should (string= "line1\nline2\nline3\n" (libgit2-blob-filtered-content blob "filename"))))))

(ert-deftest blob-binary ()
  ;; Extracted from an ELF header
  (let* ((str (unibyte-string ?\x7f ?\x45 ?\x4c ?\x46 ?\x02 ?\x01 ?\x01 ?\x00
                              ?\x00 ?\x00 ?\x00 ?\x00 ?\x00 ?\x00 ?\x00 ?\x00
                              ?\x03 ?\x00 ?\x3e ?\x00 ?\x01 ?\x00 ?\x00 ?\x00
                              ?\xb0 ?\x07 ?\x00 ?\x00 ?\x00 ?\x00 ?\x00 ?\x00
                              ?\x40 ?\x00 ?\x00 ?\x00 ?\x00 ?\x00 ?\x00 ?\x00
                              ?\xb8 ?\x1b ?\x00 ?\x00 ?\x00 ?\x00 ?\x00 ?\x00
                              ?\x00 ?\x00 ?\x00 ?\x00 ?\x40 ?\x00 ?\x38 ?\x00
                              ?\x09 ?\x00 ?\x40 ?\x00 ?\x1d ?\x00 ?\x1c ?\x00
                              ?\x06 ?\x00 ?\x00 ?\x00 ?\x05 ?\x00 ?\x00 ?\x00
                              ?\x40 ?\x00 ?\x00 ?\x00 ?\x00 ?\x00 ?\x00 ?\x00)))
    (with-temp-dir path
      (init)
      (commit-change "filename" str)
      (let* ((repo (libgit2-repository-open path))
             (blob (libgit2-revparse-single repo "HEAD:filename")))
        (should (libgit2-blob-p blob))
        (should (libgit2-blob-binary-p blob))
        (should (= 80 (libgit2-blob-rawsize blob)))
        (should (string= str (libgit2-blob-rawcontent blob)))
        (should (string= "" (libgit2-blob-filtered-content blob "filename")))
        (should (string= str (libgit2-blob-filtered-content blob "filename" t)))))))

(ert-deftest blob-create-fromdisk ()
  (with-temp-dir path
    (init)
    (write "file" "here's some text\n")
    (let* ((repo (libgit2-repository-open path))
           (id (libgit2-blob-create-fromdisk repo (concat path "file"))))
      (should (string= id "e11288449233d84c7dd74759fb6b20b06642b7c5"))
      (let ((blob (libgit2-blob-lookup repo id)))
        (should (libgit2-blob-p blob))
        (should-not (multibyte-string-p (libgit2-blob-rawcontent blob)))
        (should (string= "here's some text\n" (libgit2-blob-rawcontent blob)))))))

(ert-deftest blob-create-fromstring ()
  (with-temp-dir path
    (init)
    (let* ((repo (libgit2-repository-open path))
           ;; Valid utf-8 multibyte sequences, to check that we do not
           ;; get a multibyte string back
           (cont (unibyte-string ?\x24 ?\xc2 ?\xa2 ?\xe0 ?\xa4 ?\xb9
                                 ?\xe2 ?\x82 ?\xac ?\xf0 ?\x90 ?\x8d ?\x88))
           (id (libgit2-blob-create-fromstring repo cont)))
      (should (string= id "86ace23e965f5c8bb77f31ce58ed0d2e34d1152e"))
      (let ((blob (libgit2-blob-lookup repo id)))
        (should (libgit2-blob-p blob))
        (should-not (multibyte-string-p (libgit2-blob-rawcontent blob)))
        (should (equal cont (libgit2-blob-rawcontent blob)))))))
