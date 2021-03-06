#!/bin/bash

# ~/spy611/script/btc/sed_enhance_html.bash

# I use this script to share some sed syntax to enhance some 
# HTML generated by Postgres.

if [ $# -lt 1 ]
then
  echo You need to give an erbfile
  echo Demo:
  echo $0 ~/spy611/app/views/btc_algo/_gbm.erb
  exit 1
fi

cd ~/spy611/script/btc/

ERBFILE=$1
# Enhance HTML:
sed -i '1,$s/>model</>Model</'       $ERBFILE
sed -i '1,$s/>yr</>Year</'           $ERBFILE
sed -i '1,$s/>algo</>Algorithm</'    $ERBFILE
sed -i '1,$s/min_date/Minimum Date/' $ERBFILE
sed -i '1,$s/max_date/Maximum Date/' $ERBFILE
sed -i '1,$s/count_nup/Count of NotUp Predictions/' $ERBFILE
sed -i '1,$s/count_up/Count of Up Predictions/'     $ERBFILE
sed -i '1,$s/avg_pctgain_nup/Avg Pct Gain of NotUp Predictions/' $ERBFILE
sed -i '1,$s/avg_pctgain_up/Avg Pct Gain of Up Predictions/'     $ERBFILE
sed -i '1,$s/diff_of_avgs/Avg Pct Gain of Up Predictions minus Avg Pct Gain of NotUp Predictions/' $ERBFILE

exit
