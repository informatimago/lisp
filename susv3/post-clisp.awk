BEGIN{
    "/usr/local/bin/clisp -q -norc -ansi -x \"(princ(namestring (truename \\\"$(pwd)/\\\")))\""|getline pwd
}

/Compiling file /{
    file=$4;
    if(substr(file,0,length(pwd))==pwd){
        file=substr(file,1+length(pwd));
    }
    next;
}

/; *Loading file /{
    file=$4;
    if(substr(file,0,length(pwd))==pwd){
        file=substr(file,1+length(pwd));
    }
    next;
}

/WARNING in.*in lines/{
    start=index($0,"WARNING in ")+length("WARNING in ");
    fn=substr($0,start,index($0," in lines ")-start);
    start=index($0," in lines ")+length(" in lines ");
    lines=substr($0,start,index(substr($0,start)," ")-1);
    split(lines,n,"\\.");
    w="WARNING";
    next;
}

/ERROR.*in lines/{
    start=index($0,"ERROR in ")+length("WARNING in ");
    fn=substr($0,start,index($0," in lines ")-start);
    start=index($0," in lines ")+length(" in lines ");
    lines=substr($0,start,index(substr($0,start)," ")-1);
    split(lines,n,"\\.");
    w="ERROR";
    next;
}

{
    if(w!=""){
        printf "%s:%s:1:\n%s:%s:2:%s in %s: %s\n",file,n[1],file,n[3],w,fn,$0;
        w="";
    }else{
        print $0;
    }
}
