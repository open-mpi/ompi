/* 
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>
#include <errno.h>
#include <sys/wait.h>
#include <stdlib.h>
#include <unistd.h>

#include "include/constants.h"
#include "util/few.h"

/** @file **/

/**
 *  Forks, execs, and waits for a subordinate program
 *
 * @param argv Null-terminated argument vector; argv[0] is the program
 * (same as arguments to execvp())
 *
 * @param status Upon success, will be filled with the return status
 * from waitpid(2).  The WIF* macros can be used to examine the value
 * (see waitpid(2)).
 *
 * @retval LAM_SUCCESS If the child launched and exited.
 * @retval LAM_ERR_IN_ERRNO If a failure occurred, errno should be
 * examined for the specific error.
 *
 * This function forks, execs, and waits for an executable to
 * complete.  The input argv must be a NULL-terminated array (perhaps
 * built with the lam_arr_*() interface).  Upon success, LAM_SUCCESS
 * is returned.  This function will wait either until the child
 * process has exited or waitpid() returns an error other than EINTR.
 *
 * Note that a return of LAM_SUCCESS does \em not imply that the child
 * process exited successfully -- it simply indicates that the child
 * process exited.  The WIF* macros (see waitpid(2)) should be used to
 * examine the status to see hold the child exited.
 */
int
lam_few(char *argv[], int *status)
{
    pid_t pid, ret;

    if ((pid = fork()) < 0) {
      return LAM_ERR_IN_ERRNO;
    }

    /* Child execs.  If it fails to exec, exit. */

    else if (0 == pid) {    
      execvp(argv[0], argv);
      exit(errno);
    }

    /* Parent loops waiting for the child to die. */

    else {  
      do {
        /* If the child exited, return */

        if (pid == (ret = waitpid(pid, status, 0))) {
          break;
        } 

        /* If waitpid was interrupted, loop around again */

        else if (ret < 0) {
          if (EINTR == errno) {
            continue;
          }

          /* Otherwise, some bad juju happened -- need to quit */

          return LAM_ERR_IN_ERRNO;
        }
      } while (true);
    }

    /* Return the status to the caller */

    return LAM_SUCCESS;
}
