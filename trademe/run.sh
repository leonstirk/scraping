#!/usr/local/bin/bash

python tm_overview.py

Rscript overview_to_detail.R

python tm_detail.py

Rscript merge_data.R

DATE=`date +%Y-%m-%d`
mv data.Rda archive/$DATE.Rda

rm -f *.json

rm -f *~
