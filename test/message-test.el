(ert-deftest message-prettify ()
  (should (string= "Just header\n"
                   (libgit2-message-prettify "Just header    ")))
  (should (string= "  Leading space\n"
                   (libgit2-message-prettify "  Leading space")))
  (should (string= "Title\n\nExtra lines\n"
                   (libgit2-message-prettify "Title  \n \nExtra lines")))
  (should (string= "Comments\n# Here's one\n"
                   (libgit2-message-prettify "Comments\n# Here's one")))
  (should (string= "Comments\n"
                   (libgit2-message-prettify "Comments\n# Here's one" ?#)))
  (should (string= "Comments\n# Here's one\n"
                   (libgit2-message-prettify "Comments\n# Here's one" ?.)))
  (should (string= "Comments\n"
                   (libgit2-message-prettify "Comments\n. Here's one" ?.))))

(ert-deftest message-trailers ()
  (should (equal '(("Planet" . "Earth") ("Species" . "Humanity"))
                 (libgit2-message-trailers
                  "Title\n\nPlanet: Earth\nSpecies: Humanity\n")))
  (should (equal nil (libgit2-message-trailers "Nothing here"))))
