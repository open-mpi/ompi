/*
 * $HEADER$
 */
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>

#include "ompi_config.h"
#include "include/constants.h"

#include "util/proc_info.h"

ompi_proc_info_t ompi_process_info = {
    /*  .init =                 */   false,
    /*  .pid =                  */   0,
    /*  .name =                 */   NULL,
    /*  .universe_session_dir = */   NULL,
    /*  .job_session_dir =      */   NULL,
    /*  .proc_session_dir =     */   NULL,
    /*  .sock_stdin =           */   NULL,
    /*  .sock_stdout =          */   NULL,
    /*  .sock_stderr =          */   NULL};


int ompi_proc_info(void)
{

    if (ompi_process_info.init) {  /* already done this - don't do it again */
	return(OMPI_SUCCESS);
    }

    /* get the process id */
    ompi_process_info.pid = getpid();

    /* define process name */
    ompi_process_info.name = (ompi_process_name_t *)malloc(sizeof(ompi_process_name_t));
    ompi_process_info.name->cellid = 0;
    ompi_process_info.name->jobid = 1;
    ompi_process_info.name->procid = 2;
    sprintf(ompi_process_info.name->name, "%0x.%0x.%0x",
	    ompi_process_info.name->cellid,
	    ompi_process_info.name->jobid,
	    ompi_process_info.name->procid);


    ompi_process_info.init = true;
    return(OMPI_SUCCESS);
}
