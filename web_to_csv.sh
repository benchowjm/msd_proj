#!/bin/sh

#  web_to_csv.sh
#  
#
#  Created by Benjamin Chow on 13/4/15.
#
# To run this script, use the next 2 lines without hashtags:
# chmod u=wrx web_to_csv.sh
# ./web_to_csv.sh

## Download file from the web
[ -f Electronics.txt.gz ] || curl -O http://snap.stanford.edu/data/amazon/Electronics.txt.gz

## Unzip file
gunzip Electronics.txt.gz > Electronics.txt

## Set permissions for shell script
chmod u=rwx txt_to_dataframe.sh

## Issue warning
echo "Old file will be deleted"

## Execute shell script
./txt_to_dataframe.sh Electronics.txt Electronics.csv

