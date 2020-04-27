#! /usr/bin/awk -f
g   BEGIN {FS="[[:blank:]._/-]|"}
   /\ts/ {$0 = FILENAME " " $0;$1=$1; 
       # print $0;
       myline = " " $8 " " $2 " " $3 " " $4 " " $5 " " $6 " " $7 " " $12"."$13 " " $NF " "; }
   /\tn/ {
       myline=myline " " $NF;
       print $0;
       print myline;
       myline=""}  
