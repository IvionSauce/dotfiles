# Ivion’s dotfiles

## About
These are my dotfiles, there are many like it, but these are mine.

## Installation
Use [GNU Stow](https://www.gnu.org/software/stow/) to install each _package_ (in this case, a collection of configuration files). Or you could just manually copy them, whatever fits your fancy.

### Gotchas
In the case of ZSH and Emacs it is advisable to create the configuration directory – .zsh and .emacs.d – prior to installing their packages. This because both also put files in there you might not want to include in your dotfiles management.

## Copyright and stuff
Most of these configuration options and snippets have been glanced from other people, with the understanding they are meant to be shared. Hence everything is released into the public domain – the exceptions are: the [included ZSH prompt](https://github.com/sindresorhus/pure), which is MIT licensed; and the [included Emacs theme](https://github.com/fniessen/emacs-leuven-theme), which is GPLv3 licensed.
