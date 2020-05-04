#! /usr/bin/awk -f
   BEGIN {FS="[] \t_/]"}
   /\ts/ {
       myline =$NF;
   }
   /\tn/ { 
       myline=myline " " $NF " "$(NF-2);
       FS = "[.]";
       $0 = FILENAME;
       $1=$1;
       myline =  myline " " $(NF-1) ;
       # if(NF>=1){
       # 	   for(i=2;i<=NF-2;i++) myline= myline " " $i;}
       print myline;
       # FS="[[:blank:]_/]|";
   }  
