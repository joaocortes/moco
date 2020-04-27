#! /usr/bin/awk -f
  BEGIN {FS="[[:blank:]._/-]|"}
  /\ts/ {
      $0 = FILENAME " " $0;
      $1=$1; 
      myline = " " $10 " " $2 " " $3 " " $4 " " $5 " " $6 " " $7 " " $8 " " $9 " " $14"."$15 " " $17 " ";
  }
  /\tn/ { 
      myline=myline " " $4;
      print myline;
      myline=""
}  

