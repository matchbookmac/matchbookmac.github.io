---
layout: post
title:  "macOS, Tmux and Emacs: Copy and Paste"
date:   2017-01-15 14:40:03 -0800
categories: macOS emacs tmux
---

I've been following the tmux trail in [thoughtbot's Upcase](https://thoughtbot.com/upcase), and I got to the part where
Chris integrates tmux copy-mode with the macOS system clipboard (`pbcopy`/`pbpaste`)
The issue he was dealing with is that, of the box, tmux doesn't integrate nicely with macOS `pbcopy` and `pbpaste`.
His solution was to update his tmux.conf to use some vim key bindings and inline shell scripting to talk to
`pbcopy`/`pbpaste`. I am conversant in vim, but my main editor is emacs, so I decided to branch out from the tutorial at
that point and figure out how to do the same thing he was doing, but with emacs bindings. In addition to that, when
I was running an emacs client inside of a tmux session, the emacs copy and paste functionality via the kill-ring
was not working as expected.

I first decided to tackle copy/paste integration for just tmux and macOS. I figured that maybe lessons learned there
could be applied to my emacs client issues as well. Chris used
[`reattach-to-user-namespace`](https://github.com/ChrisJohnsen/tmux-MacOSX-pasteboard) as part of the solution to get
his vim-like bindings working, so I went ahead and did `brew install reattach-to-user-namespace`. The author of
`reattach-to-user-namespace` also did a good job of documenting why tmux and `pbcopy`/`pbpaste` don't play nicely,
so you can peruse the README in the link above instead of me trying to explain it here.

After I installed `reattach-to-user-namespace`, it was time to build my tmux copy-mode configuration. Basing it off of
Chris', I came up with this:

``` shell
# tmux.conf
# execute new window login under reattach-to-user-namespace
set-option -g default-command "reattach-to-user-namespace -l bash"

# use copy-pipe to pipe selection from tmux copy-mode to pbcopy, using reattach-to-user-namespace
unbind -t emacs-copy 'C-w'
unbind -t emacs-copy 'M-w'
unbind -t emacs-copy Enter
bind-key -t emacs-copy 'C-w' copy-pipe "reattach-to-user-namespace pbcopy"
bind-key -t emacs-copy 'M-w' copy-pipe "reattach-to-user-namespace pbcopy"
bind-key -t emacs-copy Enter copy-pipe "reattach-to-user-namespace pbcopy"

# use shell script to paste from pbpaste into tmux
bind ] run "reattach-to-user-namespace pbpaste | tmux load-buffer - && tmux paste-buffer"
```

Now when in tmux copy-mode, my copy commands work as expected, and I can paste the results into applications outside of
my terminal!

Having resolved my issues with tmux copy and paste, it was time to tackle my emacs copy and paste issues when running
an emacs client inside of tmux. The first solution I came up with was not ideal. In effect, tmux intercepted my usual emacs
paste command `C-y`, and used `pbpaste` to `cat` the result via a tmux buffer into the emacs client. This _worked_, but
it had the downside that, depending on the size of the paste performed, calling an undo (`C-/`) would only undo chunks
of the paste instead of the whole thing, because tmux was "typing" the characters from the buffer into the emacs client
instead of using emacs' kill-ring functionality:

``` shell
bind-key -n -t emacs-copy M-w copy-pipe "reattach-to-user-namespace pbcopy"
bind-key -n C-y run "pbpaste | tmux load-buffer - ; tmux paste-buffer -r"
```

I was pretty unhappy with this solution, then I remembered that I was already doing some sorcery in my init.el for emacs
to get it to integrate the kill-ring with `pbcopy`/`pbpaste`, and that I might be able to handle the integration there
instead of in my `tmux.conf`. Before making any changes, this is what I had in init.el:

``` emacs-lisp
(defun copy-from-osx ()
  "Use OSX clipboard to paste."
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  "Add kill ring entries (TEXT) to OSX clipboard.  PUSH."
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)
```

Both functions are shelling out to use the `pbcopy` and `pbpaste` commands, so what if I used the
`reattach-to-user-namespace` magic from earlier in here as well?

``` emacs-lisp
(defun copy-from-osx ()
  "Use OSX clipboard to paste."
  (shell-command-to-string "reattach-to-user-namespace pbpaste"))

(defun paste-to-osx (text &optional push)
  "Add kill ring entries (TEXT) to OSX clipboard.  PUSH."
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "reattach-to-user-namespace" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)
```

It worked! Now I can use emacs inside of a tmux session, use emacs' kill-ring to yank some text, paste that
text into an application outside of the terminal, and my undo functionality works as expected. Hopefully this helps
someone else out there looking to use the session management of tmux combined with the editing power of emacs.
