cd ~/Dropbox/Work/Fizz
#ghc Viewer.hs -rtsopts -prof -fprof-auto -threaded -eventlog
ghc Viewer.hs -rtsopts -threaded -eventlog
cd ~/Dropbox/Work/Fizz/data
../Viewer +RTS -N2 -s -ls -RTS
cd ~/Dropbox/Work/Fizz
