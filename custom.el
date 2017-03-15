(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(bookmark-default-file "/home/bonface/.emacs.d/bookmarks")
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (adwaita)))
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "a2e7b508533d46b701ad3b055e7c708323fb110b6676a8be458a758dd8f24e27" "bb749a38c5cb7d13b60fa7fc40db7eced3d00aa93654d150b9627cabd2d9b361" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
 '(fci-rule-color "#eee8d5")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(ido-enable-flex-matching t)
 '(ido-mode (quote both) nil (ido))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (helm regex-tool dsvn gnuplot flycheck-ledger ledger-mode origami flycheck-clojure cider elein cljsbuild-mode clojure-mode hippie-expand-slime cask-mode flycheck-package highlight-quoted cl-lib-highlight aggressive-indent redshank immortal-scratch hl-sexp auto-compile ipretty lively elisp-slime-nav paredit-everywhere sql-indent projectile-rails yatex yatemplate yari yaml-mode yagist whole-line-or-region whitespace-cleanup-mode wgrep web-mode vc-darcs unfill tidy textile-mode tagedit switch-window svg solarized-theme smex smarty-mode slime-volleyball slime-theme slime-ritz slime-docker slime-company slime-annot slim-mode skewer-less session scss-mode scratch sass-mode ruby-hash-syntax rspec-mode robe rinari react-snippets rake rainbow-mode rainbow-delimiters project-local-variables processing-snippets processing-mode ppd-sr-speedbar pip-requirements phpunit php-auto-yasnippets php+-mode pastels-on-dark-theme pastelmac-theme pandoc-mode page-break-lines orgtbl-show-header orgtbl-join orgtbl-ascii-plot orgtbl-aggregate orglink orgit orgbox organic-green-theme org2jekyll org2issue org2elcomment org2blog org-wunderlist org-webpage org-toodledo org-time-budgets org-themis org-tfl org-table-comment org-sync org-rtm org-review org-repo-todo org-ref org-redmine org-readme org-random-todo org-protocol-jekyll org-projectile org-preview-html org-present org-pomodoro org-pdfview org-password-manager org-page org-outlook org-octopress org-multiple-keymap org-mobile-sync org-mac-iCal org-linkany org-link-travis org-journal org-jira org-jekyll org-iv org-if org-grep org-gnome org-gcal org-fstree org-evil org-elisp-help org-ehtml org-easy-img-insert org-dropbox org-dp org-download org-dotemacs org-doing org-dashboard org-cua-dwim org-context org-commentary org-clock-today org-clock-csv org-clock-convenience org-cliplink org-chinese-utils org-capture-pop-frame org-caldav org-bullets org-bookmark-heading org-board org-beautify-theme org-babel-eval-in-repl org-autolist org-attach-screenshot org-alert org-agenda-property org-ac ob-ipython nlinum mwe-log-commands muttrc-mode multiple-cursors multi-web-mode move-dup mmm-mode magit-gitflow magit-gh-pulls lorem-ipsum latex-unicode-math-mode latex-preview-pane latex-pretty-symbols latex-math-preview latex-extra json-mode js-comint java-snippets intero indent-guide ibuffer-vc hindent highlight-symbol highlight-escape-sequences hayoo haskell-snippets ham-mode guide-key goto-gem go-snippets go-playground-cli go-playground go-complete go-autocomplete go gitignore-mode github-issues github-clone gitconfig-mode git-timemachine git-messenger git-command git-blamed geiser fullframe flyparens flymd flymake-yaml flymake-shell flymake-sass flymake-rust flymake-ruby flymake-python-pyflakes flymake-phpcs flymake-php flymake-less flymake-json flymake-haskell-multi flymake-go flymake-elixir flymake-css flymake-cppcheck flycheck-elm flatland-theme fill-column-indicator expand-region exec-path-from-shell evil-mu4e erlang emmet-mode ember-yasnippets elm-mode elixir-yasnippets eink-theme ein-mumamo django-snippets disable-mouse dirtree-prosjekt direx-grep diredful dired-toggle-sudo dired-toggle dired-subtree dired-sort-menu+ dired-sort dired-single dired-ranger dired-rainbow dired-quick-sort dired-open dired-narrow dired-k dired-imenu dired-filter dired-filetype-face dired-fdclone dired-efap dired-dups dired-details+ dired-avfs dired-atool dired+ diminish diff-hl default-text-scale datomic-snippets darcsum csv-nav csv-mode css-eldoc crontab-mode counsel company-web company-quickhelp company-php company-math company-go company-anaconda common-lisp-snippets color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized coffee-mode clojure-snippets cdlatex buster-snippets bundler bug-reference-github browse-kill-ring browse-at-remote bookmark+ bf-mode benchmark-init basic-c-compile bash-completion auto-yasnippet auto-complete-auctex auctex-lua anzu anything-git-grep anything-git-files angular-snippets angry-police-captain android-mode ac-slime ac-php ac-math ac-html-csswatcher ac-html-bootstrap ac-html-angular ac-html)))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(session-use-package t nil (session))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
