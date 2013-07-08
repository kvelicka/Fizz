cd ~/Dropbox/Work/Fizz
ghc Viewer.hs -rtsopts -prof -fprof-auto
cd ~/Dropbox/Work/Fizz/data
../Viewer +RTS -K100M -p -RTS
cd ~/Dropbox/Work/Fizz
