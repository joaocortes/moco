#! /usr/bin/awk -f
   BEGIN {FS="[[:blank:]._/-]|"}
   /\ts/ {$0 = FILENAME " " $0;$1=$1; 
       # print $0;
       myline = " " $11 " " $2 " " $3 " " $4 " " $5 " " $6 " " $7 " " $8 " " $9 " " $10 " " $14"."$15 " " $18 " "; }
   /\tn/ { myline=myline " " $4;
       print myline;
       myline=""}  

