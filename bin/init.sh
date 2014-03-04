#!/bin/sh

# Usage
# $ ./init.sh /path/to/this/dotfiles/dir

# DOTFILES is dotfiles dir.
DOTFILES=$1

# Puts symbolic links.
## emacs
if [ -e ~/.emacs.d ]; then
    ### -d, -F, --directory allow the superuser to attempt to hard link directories.
    echo "replace ~/.emacs.d?"
    read answer
    case $answer in
        y)
            rm -r ~/.emacs.d
            ln -s $DOTFILES/emacs/ ~/.emacs.d
            ;;
        n)
            ;;
        *)
            ;;
    esac
else
    ln -s $DOTFILES/emacs/ ~/.emacs.d
fi

## zsh
ln -si $DOTFILES/zsh/zshrc ~/.zshrc

## git
ln -si $DOTFILES/git/gitconfig ~/.gitconfig
ln -si $DOTFILES/git/gitexclude ~/.gitexclude

## screen
ln -si $DOTFILES/screen/screenrc ~/.screenrc

## quicklisp
if [ -e ~/.quicklisp ]; then
    echo "replace ~/.quicklisp?"
    read answer
    case $answer in
        y)
            rm -r ~/.quicklisp
            ln -s $DOTFILES/quicklisp/ ~/.quicklisp
            ;;
        n)
            ;;
        *)
            ;;
    esac
else
    ln -si $DOTFILES/quicklisp/ ~/.quicklisp
fi

## sbcl
ln -si $DOTFILES/sbcl/sbclrc ~/.sbclrc
