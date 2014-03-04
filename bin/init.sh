#!/bin/sh

# Usage
# $ ./init.sh /path/to/this/dotfiles/dir

# DOTFILES is dotfiles dir.
DOTFILES=$1

# Puts symbolic links.
## emacs
ln -s $DOTFILES/emacs ~/.emacs.d

## zsh
ln -s $DOTFILES/zsh/zshrc ~/.zshrc

## git
ln -s $DOTFILES/git/gitconfig ~/.gitconfig
ln -s $DOTFILES/git/gitexclude ~/.gitexclude

## screen
ln -s $DOTFILES/screen/screenrc ~/.screenrc
