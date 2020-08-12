
(ert-deftest team//test-relative-path ()
  (should (equal
           (team/relative-path
            "/a/b/c/d/e.dll"
            "/a/b/f/d/hehe.csproj")
           "../../c/d/e.dll")))
