#!/bin/sh

# Usage
# $ ./init.sh /path/to/this/dotfiles/dir

# DOTFILES is dotfiles dir.
DOTFILES=$1


##### Set directories #####
ECHO "set directories"
### src
if [ ! -d ~/src ] ; then
    mkdir ~/src
fi

### git
if [ ! -d ~/git ]; then
    mkdir ~/git
fi

### tmp
if [ ! -d ~/tmp ]; then
    mkdir ~/tmp
    ln -si $DOTFILES/cron/crontab ~/.crontab
    crontab ~/.crontab
fi

### works
if [ ! -d ~/works ]; then
    mkdir ~/works
fi
##########


##### Set command that is dependent on the environment #####
### Echo is different in mac and linux.
if [ `uname` = "Darwin" ]; then
    ECHO=/bin/echo
elif [ `uname` = "Linux" ]; then
    ECHO=echo
fi
##########



##### Puts symbolic links #####
### zsh
### install oh-my-zsh
ECHO -n "set .zshrc? "
read answer
case $answer in
    "y" | "yes")
        if [ ! -d ~/.oh-my-zsh ]; then
            ECHO "##### start install oh-my-zsh #####"
            curl -L http://install.ohmyz.sh | sh
            ECHO "##### done install oh-my-zsh #####"
        fi
        ECHO -n "main machine? "
        read answer
        case $answer in
            "y" | "yes")
                ln -si $DOTFILES/zsh/zshrc.main ~/.zshrc
                ;;
            *)
                ln -si $DOTFILES/zsh/zshrc.sub ~/.zshrc
                ;;
        esac
        ;;
    *)
        ECHO "not setting"
        ;;
esac

### emacs
ECHO -n "set .emacs.d? "
read answer
case $answer in
    "y" | "yes")
        if [ -d ~/.emacs.d -o -L ~/.emacs.d ]; then
            ### -d, -F, --directory allow the superuser to attempt to hard link directories.
            ECHO -n "replace ~/.emacs.d? "
            read answer
            case $answer in
                "y" | "yes")
                    rm -r ~/.emacs.d
                    ln -s $DOTFILES/emacs/ ~/.emacs.d
                    ;;
                *)
                    ECHO "not replaced"
                    ;;
            esac
        else
            ln -s $DOTFILES/emacs/ ~/.emacs.d
        fi
        ;;
    *)
        ECHO "not setting"
	;;
esac


### git
ECHO -n "set .gitconfig and .gitexclude? "
read answer
case $answer in
    "y" | "yes" )
        ln -si $DOTFILES/git/gitconfig ~/.gitconfig
        ln -si $DOTFILES/git/gitexclude ~/.gitexclude
        ;;
    *)
        ECHO "not setting"
        ;;
esac        

### screen
ECHO -n "set .screenrc? "
read answer
case $answer in
    "y" | "yes" )
        ln -si $DOTFILES/screen/screenrc ~/.screenrc
        ;;
    *)
        ECHO "not setting"
        ;;
esac

### quicklisp
ECHO -n "set .quicklisp? "
read answer
case $answer in
    "y" | "yes" )
        if [ -d ~/.quicklisp -o -L ~/.quicklisp ]; then
            ECHO -n "replace ~/.quicklisp? "
            read answer
            case $answer in
                y)
                    rm -r ~/.quicklisp
                    ln -s $DOTFILES/quicklisp/ ~/.quicklisp
                    ;;
                *)
                    ECHO "not replaced"
                    ;;
            esac
        else
            ln -si $DOTFILES/quicklisp/ ~/.quicklisp
        fi

        ### lisp interpreter
        ##### sbcl
        ln -si $DOTFILES/sbcl/sbclrc ~/.sbclrc
        ;;
    *)
        ECHO "not setting"
        ;;
esac

### lein
ECHO -n "set .lein? "
read answer
case $answer in
    "y" | "yes" )
        if [ -d ~/.lein -o -L ~/.lein ]; then
            ECHO -n "replace ~/.lein? "
            read answer
            case $answer in
                y)
                    rm -r ~/.lein
                    ln -s $DOTFILES/lein/ ~/.lein
                    ;;
                *)
                    ECHO "not replaced"
                    ;;

            esac
        else
            ln -si $DOTFILES/lein/ ~/.lein
        fi
        ;;
    *)
        ECHO "not setting"
        ;;
esac
##########
