# The only true ~/.emacs.d config

This is my 4th emacs configuration, starting from scratch. This time I'm
trying to keep things minimal and only add them when I find the need for
them, instead of installing every single plugin in the universe.

To install simply clone the repo

    $ git clone https://github.com/darthdeus/emacs-rebirth ~/.emacs.d

So far this configuration is mostly setup for doing Haskell development.
There is a small amount of config for Rails, but not yet fully usable.

The main focus is on enabling `evil-mode` in order to get the same
benefits of editing as when using VIM, but to keep the interactivity of
emacs, such as the Interactive Haskell Mode.

The biggest challenge so far has been to unify the huge amount of
keybindings available in both modes. For example while `q` is very
useful in plain VIM, it becomes unusable in Emacs as many buffers use
`q` to close them, but other buffers use it to define a macro. I
couldn't yet figure out what the rule here is, as some buffers appear as
a *menu-ish* one, but still want to define a macro. For that reason I've
disabled `q` completely when in normal mode.
