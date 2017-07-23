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

function make-dir { # check and make prefixdir, bindir
    if [ -d $PREFIX ] || [ -d $BIN ]; then
	mkdir $PREFIX && echo "${PREFIX} created."
	mkdir $BIN && echo "${BIN} created."
    fi    
}

function download-emacs { # download emacs or update if it was found
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
}

function download-dot-emacs { # download .emacs.d/ dir or update if it was found
    if [ -d ~/.emacs.d ]; then
	cd ~$E_SETUP
	git pull -v origin master
    else
	cd ~/ && git clone -v $E_SETUP_REMOTE $E_SETUP
    fi    
}

function download-rtags { # download rtags or update if it was found

    if [ -d "{$PREFIX}rtags" ]; then
	echo -n "Rtags was found, do you want update it? "
	read answear
	
	if echo "$answear" | grep -iq "^y" ; then
	    git pull -v origin master
	    cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 .
	    make
	fi

    else
	cd $PREFIX && git clone --recursive https://github.com/Andersbakken/rtags.git
	cd rtags
	cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 .
	make
    fi
}

make-dir
download-emacs
download-dot-emacs
download-rtags
