;; Setting up avy key bindings

;; Input one char, jump to it with a tree
(global-set-key (kbd "C-:") 'avy-goto-char)

;; Input arbitrary consecutive chars, jump to the first one with a tree
(global-set-key (kbd "C-'") 'avy-goto-char-timer)

;; Input zero chars, jump to a line start with a tree
(global-set-key (kbd "M-g f") 'avy-goto-line)

;; Input one char at word start, jump to a word start with a tree
(global-set-key (kbd "M-g w") 'avy-goto-word-1)

;; Input zero chars, jump to a word start with a tree
(global-set-key (kbd "M-g e") 'avy-goto-word-0)

(avy-setup-default)
(global-set-key (kbd "C-c C-j") 'avy-resume)

(provide 'setup-avy)
