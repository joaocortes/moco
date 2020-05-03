#! /usr/bin/awk -f
   BEGIN {FS="[][[:blank:]_/]"}
   /\ts/ {
       myline =$NF;
   }
   /\tn/ { 
       myline=myline " " $NF " "$(NF-2);
       FS = "[._-]";
       $0 = FILENAME;
       $1=$1;
       myline = $(NF-1) " " myline ;
       if(NF>=1){
	   for(i=2;i<=NF-2;i++) myline= myline " " $i;}
       print myline;
       FS="[[:blank:]_/]|";
   }  
