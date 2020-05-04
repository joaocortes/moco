#! /usr/bin/awk -f
BEGIN {
    state=0
    FS="[] \t_/]"}
/\ts/ {
    myline =$NF;
    state=1;
}

/\tn/ { 
    myline=myline " " $NF " "$(NF-2)
}  

/c constraints: /{
    myline=myline " " $(NF)

}
/c Variables: /{
    myline=myline " " $(NF)
}

/f completed upper limit: /{
    i=NF
    while($i~/[0-9]*/){
	myline=myline " " $(i)
	i--
    }
}
END {
if(state == 0)
    myline = myline " VOID"
    
id = FILENAME
where = match(id,/_S([0-2]+)/)
alg=substr(id, where+2, 1)
myline =  alg " " myline

idStart=match(id, /solver_/)
id = substr(id,idStart+RLENGTH)
idEnd=match(id, /_S([0-2]+)/)
id = substr(id,1, idEnd -1)
myline = myline " " id
print myline;
myline = ""
}

