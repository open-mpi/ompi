/*
 * $HEADER$
 */
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>

#include "ompi_config.h"
#include "include/constants.h"
#include "mca/ns/ns.h"
#include "mca/pcm/pcm.h"
#include "util/proc_info.h"
#include "util/sys_info.h"
#include "util/session_dir.h"

ompi_proc_info_t ompi_process_info = {
    /*  .init =                 */   false,
    /*  .pid =                  */   0,
    /*  .name =                 */   NULL,
    /*  .seed =                 */   false,
    /*  .universe_session_dir = */   NULL,
    /*  .job_session_dir =      */   NULL,
    /*  .proc_session_dir =     */   NULL,
    /*  .sock_stdin =           */   NULL,
    /*  .sock_stdout =          */   NULL,
    /*  .sock_stderr =          */   NULL};


int ompi_proc_info(void)
{

    /* local variable */
    char *jobid_str=NULL, *procid_str=NULL;
    int return_code=OMPI_SUCCESS;

    if (ompi_process_info.init) {  /* already done this - don't do it again */
        return(OMPI_SUCCESS);
    }

    /* get the process id */
    ompi_process_info.pid = getpid();

    /* set process name */
    ompi_process_info.name=mca_pcm.pcm_self();

    /* create the proc session directory */
    /* RLG - need to change universe name */
    jobid_str=malloc(sizeof(ompi_process_id_t)*8+1);
    if( NULL == jobid_str) {
        return_code=OMPI_ERR_OUT_OF_RESOURCE;
        goto CLEANUP;
    }
    memset(jobid_str,0,sizeof(ompi_process_id_t)*8+1);
    sprintf(jobid_str,"%-d",ompi_process_info.name->jobid);

    procid_str=malloc(sizeof(ompi_process_id_t)*8+1);
    if( NULL == procid_str) {
        return_code=OMPI_ERR_OUT_OF_RESOURCE;
        goto CLEANUP;
    }
    memset(procid_str,0,sizeof(ompi_process_id_t)*8+1);
    sprintf(procid_str,"%-d",ompi_process_info.name->vpid);

    ompi_process_info.proc_session_dir=ompi_session_dir
        (true, NULL, ompi_system_info.user, "bOb",jobid_str,procid_str);
    if( NULL == ompi_process_info.proc_session_dir ) {
        return_code=OMPI_ERROR;
        goto CLEANUP;
    }

    /* set the job session directory */
    ompi_process_info.job_session_dir=ompi_session_dir
        (false, NULL, ompi_system_info.user, "bOb",jobid_str,NULL);
    if( NULL == ompi_process_info.proc_session_dir ) {
        return_code=OMPI_ERROR;
        goto CLEANUP;
    }

    /* set the Universe session directory */
    ompi_process_info.universe_session_dir=ompi_session_dir
        (false, NULL, ompi_system_info.user, "bOb",NULL,NULL);
    if( NULL == ompi_process_info.proc_session_dir ) {
        return_code=OMPI_ERROR;
        goto CLEANUP;
    }

    /* set process to inited */
    ompi_process_info.init = true;

CLEANUP:

    /* clean up */
    if(jobid_str) {
        free(jobid_str);
    }
    if(procid_str) {
        free(procid_str);
    }

    return return_code;
}
