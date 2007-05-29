#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <errno.h>
#include <sys/resource.h>
#include <string.h>

int main(int argc, char* argv[]) 
{
    struct rlimit rlim;

    if (getrlimit (RLIMIT_NOFILE, &rlim) < 0)
        fprintf (stderr, "getrlimit (RLIMIT_NOFILE): %s\n", strerror (errno));
    else {
        printf("softlimit on num_files: %d\thardlimit on num_files: %d\n", (int)rlim.rlim_cur, (int)rlim.rlim_max);
    } 
    if (getrlimit (RLIMIT_NPROC, &rlim) < 0)
        fprintf (stderr, "getrlimit (RLIMIT_NPROC): %s\n", strerror (errno));
    else {
        printf("softlimit on num_child: %d\thardlimit on num_child: %d\n", (int)rlim.rlim_cur, (int)rlim.rlim_max);
    } 
    
    printf("RLIM_INFINITY: %d\n", (int)RLIM_INFINITY);
    
    return 0;
}

#if 0
int nfds_needed = calculate_nfds_needed (nprocs);
if (nfds_needed > rlim->rlim_cur) {
    if (nfds_needed <= rlim->rlim_max)
        rlim->rlim_cur = rlim->rlim_max;
    if (setrlimit (RLIMIT_NOFILE, rlim) < 0)
        fprintf (stderr, "setrlimit (RLIMIT_NOFILE, cur = %d): %m\n";
                 else
                 fprintf (stderr, "Hard limit for number of open files is too low\n");
    } 
#endif
