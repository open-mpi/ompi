/* 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <errno.h>
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "include/constants.h"
#include "util/few.h"

int ompi_few(char *argv[], int *status)
{
    pid_t pid, ret;

    if ((pid = fork()) < 0) {
      return OMPI_ERR_IN_ERRNO;
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

          return OMPI_ERR_IN_ERRNO;
        }
      } while (true);
    }

    /* Return the status to the caller */

    return OMPI_SUCCESS;
}
