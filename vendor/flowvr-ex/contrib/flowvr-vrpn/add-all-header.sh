#!/bin/bash

contact="01/06/2008 Sophie Robert <Sophie.Robert@univ-orleans.fr>"
fname=$1

current=./

header_list=$(find $current -iname *.h)
src_list=$(find $current -name *.cpp)



for fname in $header_list; do
  echo $fname
  fic=$(./add-header.pl copyright-lib.txt $fname "$contact")
  echo "$fic" > $fname
done

for fname in $src_list; do
  echo $fname
  fic=$(./add-header.pl copyright-apps.txt $fname "$contact")
  echo "$fic" > $fname
done


