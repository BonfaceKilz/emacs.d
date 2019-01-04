;; Set up the env

(setenv "PATH" (concat "/usr/local/bin:/opt/local/bin:/usr/bin:/bin" (getenv "PATH")))
(setenv "GOPATH" (concat (getenv "HOME") "/go"))
(setq exec-path (append exec-path '("/usr/local/bin")))
(add-to-list 'exec-path (concat (getenv "GOPATH") "/bin"))

(provide 'setup-envs)
