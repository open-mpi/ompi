/*
 * $HEADER$
 */
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <libgen.h>
#include <stdlib.h>
#include <pwd.h>
#include <sys/stat.h>

#include "ompi_config.h"

#include "include/constants.h"
#include "util/sys_info.h"
#include "util/os_path.h"
#include "util/os_create_dirpath.h"
#include "util/os_session_dir.h"
#include "util/cmd_line.h"
#include "util/common_cmd_line.h"

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
    char *universe = NULL;
    char *tmpdir = NULL;
    char *procname, *procpath, *jobname, *jobpath;
    mode_t mode = S_IRWXU;

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

    /* see if user specified universe name */
    if (ompi_cmd_line_is_taken(ompi_common_cmd_line, "universe")) {
	if (NULL == ompi_cmd_line_get_param(ompi_common_cmd_line, "universe", 0, 0)) {
	    return(OMPI_ERROR);
	}
	universe = strdup(ompi_cmd_line_get_param(ompi_common_cmd_line, "universe", 0, 0));
    }

    /* see if user specified session directory prefix */
    if (ompi_cmd_line_is_taken(ompi_common_cmd_line, "tmpdir")) {
	if (NULL == ompi_cmd_line_get_param(ompi_common_cmd_line, "tmpdir", 0, 0)) {
	    return(OMPI_ERROR);
	}
	tmpdir = strdup(ompi_cmd_line_get_param(ompi_common_cmd_line, "tmpdir", 0, 0));
    }

    /* get the universe session directory setup */
    if (OMPI_ERROR == ompi_session_tree_init(tmpdir, universe)) {
    /* this is a serious error, so return the error condition */
        return(OMPI_ERROR);
    }

    /* get the job session directory setup */
    sprintf(jobname, "%x", ompi_process_info.name->jobid);
    jobpath = ompi_os_path(false, ompi_process_info.universe_session_dir, jobname, NULL);
    if (OMPI_ERROR == ompi_os_create_dirpath(jobpath, mode)) {
    /* this is a serious error, so return the error condition */
        return(OMPI_ERROR);
    }
    ompi_process_info.job_session_dir = (char *)malloc((strlen(jobpath)+strlen(ompi_system_info.path_sep)+1)*sizeof(char));
    strcpy(ompi_process_info.job_session_dir, jobpath);
    ompi_process_info.job_session_dir = strcat(ompi_process_info.job_session_dir, ompi_system_info.path_sep);


    /* setup process session directory */
    sprintf(procname, "%x", ompi_process_info.name->procid);
    procpath = ompi_os_path(false, ompi_process_info.job_session_dir, procname, NULL);
    if (OMPI_ERROR == ompi_os_create_dirpath(procpath, mode)) {  /* error in setting up the directory - cannot proceed */
	return(OMPI_ERROR);
    }
    ompi_process_info.proc_session_dir = (char *)malloc((strlen(procpath)+strlen(ompi_system_info.path_sep)+1)*sizeof(char));
    strcpy(ompi_process_info.proc_session_dir, procpath);
    ompi_process_info.proc_session_dir = strcat(ompi_process_info.proc_session_dir, ompi_system_info.path_sep);

    ompi_process_info.init = true;
    return(OMPI_SUCCESS);
}
