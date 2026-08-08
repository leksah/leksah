#!/bin/sh -ex
(echo .git && for a in `find . -name '.gitignore'`; do sed -e "s|^|`dirname $a`|" -e "s|^./||" < $a; done) > .dockerignore

