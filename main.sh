cd ~/Dropbox/Work/Fizz
ghc Viewer.hs -O2 -rtsopts -prof -fprof-auto
cd ~/Dropbox/Work/Fizz/data
../Viewer +RTS -K100M -p -RTS
cd ~/Dropbox/Work/Fizz
