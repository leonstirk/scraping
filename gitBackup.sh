#!/bin/bash

rm -f *~

git add -A
git commit -m "auto-backup"
git push origin main
