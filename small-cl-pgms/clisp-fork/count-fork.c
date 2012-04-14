#include <stdio.h>
#include <unistd.h>
#include <sys/time.h>
#include <signal.h>

static run=1;
static void sh_stop(int signal){run=0;}
int main(void){
    long long counter;
    struct itimerval i;
    i.it_interval.tv_sec=0;
    i.it_interval.tv_usec=0;
    i.it_value.tv_sec=0;
    i.it_value.tv_usec=10000;
    signal(SIGVTALRM,sh_stop);
    setitimer(ITIMER_VIRTUAL,&i,0);
    counter=0;
    while(run){
        if(fork()==0){
            exit(0);}
        counter++;}
    printf("forked %lld times\n",counter);
    return(0);}
