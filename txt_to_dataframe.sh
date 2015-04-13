#!/bin/sh

#  txt_to_dataframe.sh
#  
#
#  Created by Benjamin Chow on 12/4/15.
#

## Get first header
VAR1=$(head -n 1 $1 | cut -f1 -d:)

## Get number of variables
NUM_VARS=$(($(grep -m 2 -n $VAR1 $1 | tail -n 1 | cut -f1 -d:)-2))

## Store variable names in $VARS
for i in `seq 0 $(($NUM_VARS-1))`
do
    VARS[i]=$(head -n $(($i+1)) $1 | tail -n 1 | cut -f1 -d:)
done

## Optional Checking for duplicate variable names - alas there are some
#for i in `seq 0 $(($NUM_VARS-1))`
#do
#    grep ${VARS[$i]} $1 | cut -f1 -d: | wc -l
#done

#echo "We can see that 'product/price' occurs an additional 20 times compared to the others"

#grep ${VARS[2]} $1 | grep -nv "product/price: " # Finds offending lines & line numbers
# All include either "review/summary" or "review/text"

##### Start conversion to CSV file #####

## Remove all commas
sed -i.bak 's/,//g' $1

## Create VARS3 for use in sed - insert '\' before '/' and ':' after each variable
for i in `seq 0 $(($NUM_VARS-1))`
do
    VARS2[i]=$(echo ${VARS[$i]} | sed 's/\//\\\//g')
    VARS3[i]="${VARS2[$i]}: "
done

## Convert all newlines into commas
cat $1 | tr '\n' , | cat > temp.txt

## Different observations are now separated by ',,' which we convert to newlines
sed 's/,,/\
/g' temp.txt > $1
rm temp.txt

## Remove all headers from the file
for i in `seq 0 $(($NUM_VARS-1))`
do
sed -i.bak s/"${VARS3[$i]}"//g $1
done

## Change 'unknown's to missing values
sed -i.bak 's/,unknown/,/g' $1

## Start new file with headers corresponding to original varnames (stored in VARS)
echo ${VARS[@]} | tr ' ' ',' | cat > $2

## Append output to new file
cat $1 >> $2

## Remove backup file & original file
rm $1.bak
rm $1
