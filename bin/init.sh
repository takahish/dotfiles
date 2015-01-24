#!/bin/sh

# Usage
# $ ./init.sh /path/to/this/dotfiles/dir

# DOTFILES is dotfiles dir.
DOTFILES=$1

# commands
ECHO=/bin/echo
READ=read
CURL=`which curl`


##### Set directories #####
$ECHO "set directories"

### bin
if [ ! -d $HOME/bin ]; then
    mkdir $HOME/bin
fi

### src
if [ ! -d $HOME/src ]; then
    mkdir $HOME/src
fi

### git
if [ ! -d $HOME/git ]; then
    mkdir $HOME/git
fi

### tmp
if [ ! -d $HOME/tmp ]; then
    mkdir $HOME/tmp
    ln -si $DOTFILES/cron/crontab $HOME/.crontab
    crontab $HOME/.crontab
fi

### works
if [ ! -d $HOME/works ]; then
    mkdir $HOME/works
fi
##########


##### Puts symbolic links #####
### bash
$ECHO -n "set .bashrc? "
$READ answer
case $answer in
    "y" | "yes")
        ln -si $DOTFILES/bash/bash_profile $HOME/.bash_profile
        ln -si $DOTFILES/bash/bash_aliases $HOME/.bash_aliases
        ln -si $DOTFILES/bash/git-prompt.sh $HOME/.git-prompt.sh
        ln -si $DOTFILES/bash/git-completion.bash $HOME/.git-completion.bash
        $ECHO -n "main machine? "
        $READ answer
        case $answer in
            "y" | "yes")
                ln -si $DOTFILES/bash/bashrc.main $HOME/.bashrc
                ;;
            *)
                ln -si $DOTFILES/bash/bashrc.sub $HOME/.bashrc
                ;;
        esac
        ;;
    *)
        $ECHO "not setting"
        ;;
esac

### vim
### install nebobundle
$ECHO -n "set .vimrc? "
$READ answer
case $answer in
    "y" | "yes")
        if [ ! -d $HOME/.vim/bundle ]; then
            $ECHO "##### start install Neobundle #####"
            $CURL https://raw.githubusercontent.com/Shougo/neobundle.vim/master/bin/install.sh | sh
            $ECHO "##### done install Neobundle #####"
        fi
        ln -si $DOTFILES/vim/vimrc $HOME/.vimrc
        ;;
    *)
        $ECHO "not setting"
        ;;
esac

### zsh
### install oh-my-zsh
#$ECHO -n "set .zshrc? "


### zsh
### install oh-my-zsh
#$ECHO -n "set .zshrc? "
#$READ answer
#case $answer in
#    "y" | "yes")
#        if [ ! -d $HOME/.oh-my-zsh ]; then
#            $ECHO "##### start install oh-my-zsh #####"
#            $CURL -L http://install.ohmyz.sh | sh
#            $ECHO "##### done install oh-my-zsh #####"
#        fi
#        $ECHO -n "main machine? "
#        $READ answer
#        case $answer in
#            "y" | "yes")
#                ln -si $DOTFILES/zsh/zshrc.main $HOME/.zshrc
#                ;;
#            *)
#                ln -si $DOTFILES/zsh/zshrc.sub $HOME/.zshrc
#                ;;
#        esac
#        ;;
#    *)
#        $ECHO "not setting"
#        ;;
#esac

### emacs
$ECHO -n "set .emacs.d? "
$READ answer
case $answer in
    "y" | "yes")
        if [ -d $HOME/.emacs.d -o -L $HOME/.emacs.d ]; then
            ### -d, -F, --directory allow the superuser to attempt to hard link directories.
            $ECHO -n "replace $HOME/.emacs.d? "
            $READ answer
            case $answer in
                "y" | "yes")
                    rm -r $HOME/.emacs.d
                    ln -s $DOTFILES/emacs/ $HOME/.emacs.d
                    ;;
                *)
                    $ECHO "not replaced"
                    ;;
            esac
        else
            ln -s $DOTFILES/emacs/ $HOME/.emacs.d
        fi
        ;;
    *)
        $ECHO "not setting"
	;;
esac


### git
$ECHO -n "set .gitconfig and .gitexclude? "
$READ answer
case $answer in
    "y" | "yes" )
        ln -si $DOTFILES/git/gitconfig $HOME/.gitconfig
        ln -si $DOTFILES/git/gitexclude $HOME/.gitexclude
        ;;
    *)
        $ECHO "not setting"
        ;;
esac        

### screen
$ECHO -n "set .screenrc? "
$READ answer
case $answer in
    "y" | "yes" )
        ln -si $DOTFILES/screen/screenrc $HOME/.screenrc
        ;;
    *)
        $ECHO "not setting"
        ;;
esac

### quicklisp
#$ECHO -n "set .quicklisp? "
#$READ answer
#case $answer in
#    "y" | "yes" )
#        if [ -d $HOME/.quicklisp -o -L $HOME/.quicklisp ]; then
#            $ECHO -n "replace $HOME/.quicklisp? "
#            $READ answer
#            case $answer in
#                y)
#                    rm -r $HOME/.quicklisp
#                    ln -s $DOTFILES/quicklisp/ $HOME/.quicklisp
#                    ;;
#                *)
#                    $ECHO "not replaced"
#                    ;;
#            esac
#        else
#            ln -si $DOTFILES/quicklisp/ $HOME/.quicklisp
#        fi
#
#        ### lisp interpreter
#        ##### sbcl
#        ln -si $DOTFILES/sbcl/sbclrc $HOME/.sbclrc
#        ;;
#    *)
#        $ECHO "not setting"
#        ;;
#esac

### lein
#$ECHO -n "set .lein? "
#$READ answer
#case $answer in
#    "y" | "yes" )
#        if [ -d $HOME/.lein -o -L $HOME/.lein ]; then
#            $ECHO -n "replace $HOME/.lein? "
#            $READ answer
#            case $answer in
#                y)
#                    rm -r $HOME/.lein
#                    ln -s $DOTFILES/lein/ $HOME/.lein
#                    ;;
#                *)
#                    $ECHO "not replaced"
#                    ;;
#
#            esac
#        else
#            ln -si $DOTFILES/lein/ $HOME/.lein
#        fi
#        ;;
#    *)
#        $ECHO "not setting"
#        ;;
#esac
##########
