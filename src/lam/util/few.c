/* 
 * $HEADER$
 */

#include <stdio.h>
#include <errno.h>
#include <sys/wait.h>
#include <stdlib.h>
#include <unistd.h>

#include "lam_config.h"
#include "lam/util/few.h"

/** @file **/

/**
 *  forks, execs and waits for a subordinate program
 *
 *  @param argv Argument vector, argv[0] is the program
 *  @retval Status code or ERROR
 *  Returns:    - status code or ERROR
 */
int
lam_few(char *argv[])
{
    int status;
    int pid;
    int ret;

    if ((pid = fork()) < 0) {
        return(pid);
        }
    /* child */
    else if (0 == pid) {    
        execvp(argv[0], argv);
        exit(errno);
        }
    /* parent */
    else {  
        while ((0 != waitpid(pid, &status, 0)) &&
                (! WIFEXITED(status)) && (EINTR == errno));
    }
    
    return status;
}
