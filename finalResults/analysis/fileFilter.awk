#! /usr/bin/awk -f
BEGIN {FS="[] \t_/]"}

/\ts/ {
    myline =$NF;
}

/\tn/ { 
    myline=myline " " $NF " "$(NF-2)
}  

/c constraints: /{
    myline=myline " " $(NF-1)

}
/c Variables: /{
    myline=myline " " $(NF-1)
}

/f completed upper limit: /{
    i=NF
    while($i~/[0-9]*/){
	myline=myline " " $(i)
	i--
    }
}

