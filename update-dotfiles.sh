# dotemacs
cd "$HOME"
git clone https://github.com/Traivok/emacs/new/master .emacs.d/

# rtags
cd $PREFIX && git clone --recursive https://github.com/Andersbakken/rtags.git
cd rtags
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 .
make
