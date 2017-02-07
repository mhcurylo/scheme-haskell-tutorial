#!/bin/sh
set -eu


# assign R to 0
R=0

# find all haskells in src  # send them to entr 
# and test                  # which will
                            # 1. run 'stack test' on start
                            # 2. run 'stack test' on file contents change
                            # 3. exit with code 2 if directory contents change
find src test -name '*.hs' | entr -d -r -c stack test || R=$?

# either entr exited with code not 2 or recursively execute self
[ $R -ne 2 ] || exec sh $0