/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>

#include "include/constants.h"
#include "runtime/runtime.h"
#include "mca/ns/base/base.h"
#include "mca/pcm/pcm.h"
#include "util/proc_info.h"
#include "util/sys_info.h"
#include "util/session_dir.h"

ompi_proc_info_t ompi_process_info = {
    /*  .init =                 */   false,
    /*  .pid =                  */   0,
    /*  .name =                 */   NULL,
    /*  .seed =                 */   false,
    /*  .my_universe =          */   "default-universe",
    /*  .tmpdir_base =          */   NULL,
    /*  .universe_session_dir = */   NULL,
    /*  .job_session_dir =      */   NULL,
    /*  .proc_session_dir =     */   NULL,
    /*  .sock_stdin =           */   NULL,
    /*  .sock_stdout =          */   NULL,
    /*  .sock_stderr =          */   NULL};


int ompi_proc_info(void)
{

    /* local variable */
    int return_code=OMPI_SUCCESS;

    if (ompi_process_info.init) {  /* already done this - don't do it again */
        return(OMPI_SUCCESS);
    }

    /* get the process id */
    ompi_process_info.pid = getpid();

    /* set process name */
    if (ompi_process_info.seed) {
	ompi_process_info.name = ompi_name_server.create_process_name(0, 0, 0);
    } else {
    ompi_process_info.name = ompi_rte_get_self();
    }

    /* set process to inited */
    ompi_process_info.init = true;

    return return_code;
}
