/*
 * $HEADER$
 */
/** @file **/

#include "ompi_config.h"

#include <stdio.h>

#include "include/constants.h"

#include "util/sys_info.h"
#include "util/cmd_line.h"
#include "util/proc_info.h"
#include "util/pack.h"
#include "runtime/runtime.h"

#include "mca/base/base.h"
#include "mca/oob/base/base.h"
#include "mca/ns/base/base.h"

#include "tools/ompid/ompid.h"

static void ompi_console_recv(int status, ompi_process_name_t* sender,
			      ompi_buffer_t buffer, int tag,
			      void* cbdata);


int main(int argc, char *argv[])
{
    int ret, recv_tag;
    ompi_cmd_line_t *cmd_line;
    bool allow_multi_user_threads   = false;
    bool have_hidden_threads  = false;
    ompi_buffer_t cmd;
    ompi_daemon_cmd_flag_t command;
    ompi_process_name_t seed={0,0,0};

    /*
     * Intialize the Open MPI environment
     */
    if (OMPI_SUCCESS != ompi_init(argc, argv)) {
        /* BWB show_help */
        printf("show_help: ompi_init failed\n");
        return ret;
    }

    /* get the system info and setup defaults */
    ompi_sys_info();
    ompi_universe_info.host = strdup(ompi_system_info.nodename);
    ompi_universe_info.uid = strdup(ompi_system_info.user);

    /* setup to read common command line options that span all Open MPI programs */
    cmd_line = OBJ_NEW(ompi_cmd_line_t);

    ompi_cmd_line_make_opt(cmd_line, 'v', "version", 0,
			   "Show version of Open MPI and this program");

    ompi_cmd_line_make_opt(cmd_line, 'h', "help", 0,
			   "Show help for this function");

    fprintf(stderr, "setting up cmd_line\n");

    /* setup rte command line arguments */
    ompi_rte_cmd_line_setup(cmd_line);

    /*
     * setup  mca command line arguments
     */
    if (OMPI_SUCCESS != (ret = mca_base_cmd_line_setup(cmd_line))) {
	/* BWB show_help */
	printf("show_help: mca_base_cmd_line_setup failed\n");
	return ret;
    }

    if (OMPI_SUCCESS != mca_base_cmd_line_process_args(cmd_line)) {
	/* BWB show_help */
	printf("show_help: mca_base_cmd_line_process_args\n");
	return ret;
    }

    fprintf(stderr, "parse commands\n");

    /* parse the local commands */
    if (OMPI_SUCCESS != ompi_cmd_line_parse(cmd_line, true, argc, argv)) {
	exit(ret);
    }

    if (ompi_cmd_line_is_taken(cmd_line, "help") || 
        ompi_cmd_line_is_taken(cmd_line, "h")) {
        printf("...showing ompi_info help message...\n");
        exit(1);
    }

    if (ompi_cmd_line_is_taken(cmd_line, "version") ||
	ompi_cmd_line_is_taken(cmd_line, "v")) {
	printf("...showing off my version!\n");
	exit(1);
    }

    fprintf(stderr, "parse environ\n");

    /* parse the environment */
    ompi_rte_parse_environ();

    fprintf(stderr, "parse rte cmds\n");

    /* parse the cmd_line for rte options - override settings from enviro, where necessary
     * copy everything into enviro variables for passing later on
     */
    ompi_rte_parse_cmd_line(cmd_line);

    /* Open up the MCA */
    fprintf(stderr, "open mca\n");
    if (OMPI_SUCCESS != (ret = mca_base_open())) {
        /* JMS show_help */
        printf("show_help: ompid failed in mca_base_open\n");
        return ret;
    }

    fprintf(stderr, "join runtime\n");

    /* Join the run-time environment */
    allow_multi_user_threads = true;
    have_hidden_threads = false;
    if (OMPI_SUCCESS != (ret = ompi_rte_init_stage1(&allow_multi_user_threads,
						    &have_hidden_threads))) {
        /* JMS show_help */
        printf("show_help: ompid failed in ompi_rte_init\n");
        return ret;
    }

    fprintf(stderr, "check local univ\n");

    if (OMPI_SUCCESS != (ret = ompi_rte_universe_exists())) {
	fprintf(stderr, "could not contact local universe %s\n", ompi_universe_info.name);
	exit(1);
    }

    fprintf(stderr, "init stage 2\n");

    /* setup the rest of the rte */
    if (OMPI_SUCCESS != (ret = ompi_rte_init_stage2(&allow_multi_user_threads,
						    &have_hidden_threads))) {
        /* JMS show_help */
        printf("show_help: ompid failed in ompi_rte_init\n");
        return ret;
    }

    /*****    SET MY NAME   *****/
    if (NULL == ompi_process_info.name) { /* don't overwrite an existing name */
	if (ompi_process_info.seed) {
	    ompi_process_info.name = ompi_name_server.create_process_name(0, 0, 0);
	} else {
	    ompi_process_info.name = ompi_rte_get_self();
	}
    }

    /* finalize the rte startup */
    if (OMPI_SUCCESS != (ret = ompi_rte_init_finalstage(&allow_multi_user_threads,
							&have_hidden_threads))) {
	printf("failed to finalize the rte startup\n");
	return ret;
    }
 
    /* register the console callback function */
    ret = mca_oob_recv_packed_nb(MCA_OOB_NAME_ANY, MCA_OOB_TAG_DAEMON, 0, ompi_console_recv, NULL);
    if(ret != OMPI_SUCCESS && ret != OMPI_ERR_NOT_IMPLEMENTED) {
	printf("daemon callback not registered: error code %d", ret);
	return ret;
    }

    fprintf (stderr, "issuing exit cmd\n");
    ompi_buffer_init(&cmd, 0);
    command = OMPI_DAEMON_EXIT_CMD;
    recv_tag = MCA_OOB_TAG_DAEMON;
    ompi_pack(cmd, &command, 1, OMPI_DAEMON_OOB_PACK_CMD);
    mca_oob_send_packed(&seed, cmd, MCA_OOB_TAG_DAEMON, 0);

    ompi_rte_finalize();
    mca_base_close();
    ompi_finalize();
    return 0;
}

static void ompi_console_recv(int status, ompi_process_name_t* sender,
			      ompi_buffer_t buffer, int tag,
			      void* cbdata)
{
    ompi_daemon_cmd_flag_t command;
    int32_t num_bytes, i;
    uint8_t *outbytes;

    printf("console - message received from [%d,%d,%d]\n", sender->cellid,
	   sender->jobid, sender->vpid);

    if (OMPI_SUCCESS != ompi_unpack(buffer, &num_bytes, 1, OMPI_INT32)) {
	printf("\terror unpacking number of bytes\n");
	return;
    }

    outbytes = (uint8_t*)malloc(num_bytes);

    if (OMPI_SUCCESS != ompi_unpack(buffer, &outbytes, num_bytes, OMPI_BYTE)) {
	printf("\terror unpacking number of bytes\n");
	return;
    }

    for (i=0; i<num_bytes; i++) {
	printf("%c", outbytes[i]);
    }

    free(outbytes);
    ompi_buffer_free(buffer);
    return;
}
