#! /bin/sh
rm -rf build
mkdir -p build
cd build
# Install ncurses
wget http://ftp.gnu.org/pub/gnu/ncurses/ncurses-5.9.tar.gz
tar xvf ncurses-5.9.tar.gz
cd ncurses-5.9
./configure --prefix=$HOME/.local --without-install-prefix --with-termlib --enable-termcap --enable-getcap --enable-tcap-names --with-shared
make && make install
cd ..

# install emacs
wget http://open-source-box.org/emacs/emacs-25.2.tar.xz
tar xvf emacs-25.2.tar.xz
cd emacs-25.2
./configure --prefix=$HOME/.local LDFLAGS=-L$HOME/.local/lib --without-pop --without-kerberos --without-mmdf --without-sound --without-wide-int --without-xpm --without-jpeg --without-tiff --without-gif --without-png --without-rsvg --without-xml2 --without-imagemagick --without-xft --without-libotf --without-m17n-flt --without-xaw3d --without-xim --without-ns --without-gpm --without-dbus --without-gconf --without-gsettings --without-selinux --without-gnutls --without-x
make && make install
cd ..
