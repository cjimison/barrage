#!/bin/bash

echo Downloading Barrage
git clone --no-checkout --depth 1 git@github.com:foo/bar.gitgit@github.com:cjimison/barrage_build.git
mv barrage_build/barrage_linux_x64.tgz ./
#rm -rf barrage_build 
#tar -xzvf barrage_linux_x64.tgz

