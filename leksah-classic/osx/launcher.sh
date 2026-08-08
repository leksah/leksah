#!/bin/bash

if test -e /etc/profile; then
    . /etc/profile
fi

if test -e ~/.bash_profile; then
    . ~/.bash_profile
elif test -e ~/.profile; then
    . ~/.profile
fi

bundle_macos=`dirname "$0"`
bundle_contents=`dirname "$bundle_macos"`
"$bundle_contents/Resources/bin/run"

