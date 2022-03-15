(ert-deftest repository-config ()
  (with-temp-dir path
    (init)
    (should (libgit2-config-p (libgit2-repository-config (libgit2-repository-open path))))))

(ert-deftest config-get ()
  (with-temp-dir path
    (init)
    (write ".git/config" "\
[user]
  email = someone@somewhere.com
  name = John Doe
  age = 7
  money = 1k
  hairs = 1m
  atoms = 1g
  male = yes
  intelligent = off
  handsome = 1
  rich = false
")
    (let* ((repo (libgit2-repository-open path))
           (config (libgit2-repository-config repo))
           (snap (libgit2-config-snapshot config)))
      ;; Can't read from a live config object
      (should-error (libgit2-config-get-string config "something") :type 'giterr-config)
      (should (string= "someone@somewhere.com" (libgit2-config-get-string snap "user.email")))
      (should (string= "John Doe" (libgit2-config-get-string snap "user.name")))
      (should (= 7 (libgit2-config-get-int snap "user.age")))
      (should (= 1024 (libgit2-config-get-int snap "user.money")))
      (should (= (* 1024 1024) (libgit2-config-get-int snap "user.hairs")))
      (should (= (* 1024 1024 1024) (libgit2-config-get-int snap "user.atoms")))
      (should (libgit2-config-get-bool snap "user.male"))
      (should-not (libgit2-config-get-bool snap "user.intelligent"))
      (should (libgit2-config-get-bool snap "user.handsome"))
      (should-not (libgit2-config-get-bool snap "user.rich")))))

(ert-deftest config-set ()
  (with-temp-dir path
    (init)
    (let* ((repo (libgit2-repository-open path))
           (config (libgit2-repository-config repo))
           trans)

      (libgit2-config-set-string config "user.name" "Captain Spiff")
      (let ((snap (libgit2-config-snapshot config)))
        (should (string= "Captain Spiff" (libgit2-config-get-string snap "user.name")))
        ;; Can't write to a config snapshot
        (should-error (libgit2-config-set-string snap "user.name" "Zorg") :type 'giterr-config))

      ;; If the config is locked, writes aren't visible immediately
      (setq trans (libgit2-config-lock config))
      (libgit2-config-set-string config "user.name" "Wolfgang Amadeus")
      (let ((snap (libgit2-config-snapshot config)))
        (should (string= "Captain Spiff" (libgit2-config-get-string snap "user.name"))))
      (libgit2-transaction-commit trans)
      (let ((snap (libgit2-config-snapshot config)))
        (should (string= "Wolfgang Amadeus" (libgit2-config-get-string snap "user.name"))))

      (libgit2-config-set-int config "user.age" 7)
      (let ((snap (libgit2-config-snapshot config)))
        (should (= 7 (libgit2-config-get-int snap "user.age"))))

      (libgit2-config-set-bool config "user.male" t)
      (let ((snap (libgit2-config-snapshot config)))
        (should (libgit2-config-get-bool snap "user.male")))

      (libgit2-config-set-bool config "user.intelligent" nil)
      (let ((snap (libgit2-config-snapshot config)))
        (should-not (libgit2-config-get-bool snap "user.intelligent"))))))

(ert-deftest config-levels ()
  (with-temp-dir path
    (write "cfg-a" "\
[user]
  email = someone@somewhere.com
  name = John Doe
  age = 7
  money = 1k
  hairs = 1m
  atoms = 1g
  male = yes
  intelligent = off
  handsome = 1
  rich = false
")
    (write "cfg-b" "\
[user]
  email = someone-else@somewhere-else.com
  name = Jane Doe
  age = 18
  money = 1g
  hairs = 1k
  atoms = 1
  male = no
  intelligent = on
  beautiful = 1
  rich = true
")
    (let ((config (libgit2-config-new)))
      (libgit2-config-add-file-ondisk config "cfg-a" 'global)
      (libgit2-config-add-file-ondisk config "cfg-b" 'local)
      (let ((snap (libgit2-config-snapshot config)))
        (should (string= "someone-else@somewhere-else.com" (libgit2-config-get-string snap "user.email")))
        (should (string= "Jane Doe" (libgit2-config-get-string snap "user.name")))
        (should (= 18 (libgit2-config-get-int snap "user.age")))
        (should (= (* 1024 1024 1024) (libgit2-config-get-int snap "user.money")))
        (should (= 1024 (libgit2-config-get-int snap "user.hairs")))
        (should (= 1 (libgit2-config-get-int snap "user.atoms")))
        (should-not (libgit2-config-get-bool snap "user.male"))
        (should (libgit2-config-get-bool snap "user.handsome"))
        (should (libgit2-config-get-bool snap "user.beautiful"))
        (should (libgit2-config-get-bool snap "user.rich")))
      (let* ((subconfig (libgit2-config-open-level config 'global))
             (snap (libgit2-config-snapshot subconfig)))
        (should (string= "someone@somewhere.com" (libgit2-config-get-string snap "user.email")))
        (should (string= "John Doe" (libgit2-config-get-string snap "user.name")))
        (should (= 7 (libgit2-config-get-int snap "user.age")))
        (should (= 1024 (libgit2-config-get-int snap "user.money")))
        (should (= (* 1024 1024) (libgit2-config-get-int snap "user.hairs")))
        (should (= (* 1024 1024 1024) (libgit2-config-get-int snap "user.atoms")))
        (should (libgit2-config-get-bool snap "user.male"))
        (should (libgit2-config-get-bool snap "user.handsome"))
        (should-error (libgit2-config-get-bool snap "user.beautiful") :type 'giterr-config)
        (should-not (libgit2-config-get-bool snap "user.rich")))

      ;; Delete only deletes on the highest level
      (libgit2-config-delete-entry config "user.rich")
      (let ((snap (libgit2-config-snapshot config)))
        (should-not (libgit2-config-get-bool snap "user.rich")))

      ;; Deleting again won't work, the lower levels are immutable
      (should-error (libgit2-config-delete-entry config "user.rich") :type 'giterr-config))))
