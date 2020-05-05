









(defconst githubbel-github-api-root "https://api.github.com/")

(defun githubbel-simple-api-get (arg)
  "Make simple gitbub api request. ARG is the request url."
  (shell-command-to-string (concat "curl " githubbel-github-api-root githubbel-github-api-root arg)))




(githubbel-simple-api-get "OmniSharp/omnisharp-roslyn/releases")


(shell-command-to-string (format "curl -s %s/%s" githubbel-github-api-root "repos/OmniSharp/omnisharp-roslyn/releases"))



;; to be continued


;;curl -s https://api.github.com/repos/KhronosGroup/WebGL | grep 'created_at' | cut -d: -f2-
