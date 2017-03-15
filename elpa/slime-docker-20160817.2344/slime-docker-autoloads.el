;;; slime-docker-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "slime-docker" "slime-docker.el" (22485 22133
;;;;;;  776230 686000))
;;; Generated autoloads from slime-docker.el

(autoload 'slime-docker-start "slime-docker" "\
Start a Docker container and Lisp process in the container then connect to it.

If the slime-tramp contrib is also loaded (highly recommended),
this will also set up the appropriate tramp translations to view
and edit files in the spawned container.

PROGRAM and PROGRAM-ARGS are the filename and argument strings
  for the Lisp process.
IMAGE-NAME is a string naming the image that should be used to
  start the container.
IMAGE-TAG is a string nameing the tag to use. Defaults to
  \"latest\".
INIT is a function that should return a string to load and start
  Swank. The function will be called with a plist of all
  arguments passed to `slime-docker-start'
CODING-SYSTEM is ignored.
ENV an alist of environment variables to set in the docker
  container.
BUFFER the name of the buffer to use for the subprocess.
NAME a symbol to describe the Lisp implementation.
DIRECTORY set this as the working directory in the container.
RM if true, the container is removed when the process closes.
MOUNTS a list describing the voluments to mount into the
  container. It is of the form:
  (((HOST-PATH . CONTAINER-PATH) &key READ-ONLY) ... )
UID if specified, sets the UID of the Lisp process in the
  container.
SLIME-MOUNT-PATH the location where to mount SLIME into the
  container defaults to
  /usr/local/share/common-lisp/source/slime/
SLIME-MOUNT-READ-ONLY if non-NIL, SLIME is mounted into the
  container as read-only. Defaults to T.
DOCKER-MACHINE if non-NIL, must be a string naming a machine name
  known to docker-machine. If provided, used to set appropriate
  environment variables for the docker process to communicate
  with the desired machine. Does not start the machine if it is
  currently not running.
DOCKER-COMMAND is the command to use when interacting with
  docker. Defaults to \"docker\". See
  `slime-docker-machine-ssh-agent-helper-path' if you are using
  docker-machine and would like to share your SSH Agent with the
  container.
DOCKER-MACHINE-SETENV if non-NIL, uses `setenv' to set Emacs
  environment with the necessary variables from
  docker-machine. Should be non-NIL if you expect tramp to work
  with images running in docker machine.
SECURITY-OPTS specifies --security-opt options when running
  'docker run'. Must be an alist where keys and values are
  strings. See README for note on using this with SBCL.
USERNS specifies the user namespace to use when starting the
  container. See the --userns option to 'docker run' for more
  information.
DNS specifies a list of DNS servers to use in the container. If
  you're on a laptop, it's recommended to set this value as
  Docker does not update a container's DNS info while it is
  running (for example if you change networks).
PORTS is a list of port specifications to open in the docker
  container. The port specifications are plists with the
  properties :ip, :host-port, and :container-port. :ip must be a
  string. :host-port and :container-port must be a number or a
  cons cell.

\(fn &key (program inferior-lisp-program) PROGRAM-ARGS DIRECTORY NAME (buffer \"*docker-lisp*\") (image-name \"daewok/lisp-devel\") (image-tag \"latest\") (rm t) ENV (init (quote slime-docker--init-command)) MOUNTS CODING-SYSTEM (slime-mount-path \"/usr/local/share/common-lisp/source/slime/\") (slime-mount-read-only t) UID DOCKER-MACHINE (docker-command \"docker\") (docker-machine-setenv t) SECURITY-OPTS USERNS DNS PORTS)" nil nil)

(autoload 'slime-docker "slime-docker" "\
Launch a Lisp process in a Docker container and connect SLIME to it.

The normal entry point to slime-docker.el. Similar to `slime'
function. Tries to guess the correct Lisp to start based on
prefix arguments and the values of `slime-docker-implementations'
and `slime-docker-default-lisp'.

COMMAND is the command to run in the Docker container.

\(fn &optional COMMAND)" t nil)

;;;***

;;;### (autoloads nil nil ("slime-docker-pkg.el") (22485 22133 994317
;;;;;;  810000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; slime-docker-autoloads.el ends here
