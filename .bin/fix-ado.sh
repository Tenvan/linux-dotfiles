#!/usr/bin/env bash

git remote remove origin
git remote add origin git@ssh.dev.azure.com:v3/infoniqacustomers/time-de/$1
git fetch --all
git branch --set-upstream-to=origin/develop develop
git branch --set-upstream-to=origin/master master
git remote show origin
