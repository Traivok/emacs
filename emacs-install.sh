#!/bin/bash

# Emacs installer for my college's computers

BIN="~/bin/" # where emacs bin will be located
PREFIX="~/Development/" # where emacs will be located
E_SETUP="~/.emacs.d/" # where .emacs location
E_SETUP_REMOTE="https://github.com/Traivok/emacs/" # default url of .emacs.d setup

function make-emacs { # configuration directives
    ./configure --prefix=~/emacs --bindir=$BIN
    make
}

if [ -d $PREFIX ] || [ -d $BIN ]; then
   mkdir $PREFIX && echo "${PREFIX} created."
   mkdir $BIN && echo "${BIN} created."
fi

if [ -f "${BIN}/emacs"  ]; then
    echo -n "Emacs was found, do you want update it? "
    read answear

    if echo "$answer" | grep -iq "^y" ; then 
	cd "${PREFIX}emacs/"
	git pull -v origin master
	make-emacs
    fi
	
else
    cd $PREFIX && git clone -v https://github.com/emacs-mirror/emacs && cd emacs
    make-emacs
fi
   
if [ -d ~/.emacs.d ]; then
    cd ~$E_SETUP
    git pull -v origin master
else
    cd ~/ && git clone -v $E_SETUP_REMOTE $E_SETUP
fi
