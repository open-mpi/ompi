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
#include "mca/gpr/base/base.h"

#include "tools/ompid/ompid.h"

#define OMPI_CONSOLE_MAX_LINE_LENGTH 1024

static char *ompi_getinputline(void);

static void ompi_console_sendcmd(ompi_daemon_cmd_flag_t usercmd);


int main(int argc, char *argv[])
{
    ompi_list_t *list;
    ompi_list_item_t *item;
    ompi_registry_value_t *value;
    char *hostname, *contact_info;
    ompi_process_name_t proc_name;
    int32_t proc_slots;
    mca_ns_base_jobid_t jobid;
    mca_ns_base_vpid_t vpid;
    int ret;
    ompi_cmd_line_t *cmd_line;
    bool allow_multi_user_threads   = false;
    bool have_hidden_threads  = false;
    bool exit_cmd;
    char *usercmd, *str_response;
    ompi_buffer_t buffer;
    ompi_process_name_t seed={0,0,0};
    int recv_tag;

    /*
     * Intialize the Open MPI environment
     */
    if (OMPI_SUCCESS != ompi_init(argc, argv)) {
        /* BWB show_help */
        printf("show_help: ompi_init failed\n");
        return ret;
    }

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
    jobid = ompi_name_server.create_jobid();
    vpid = ompi_name_server.reserve_range(jobid, 1);
    ompi_process_info.name = ompi_name_server.create_process_name(0, jobid, vpid);

    fprintf(stderr, "my name: [%d,%d,%d]\n", ompi_process_info.name->cellid,
	    ompi_process_info.name->jobid, ompi_process_info.name->vpid);

    /*
     *  Register my process info with my replica.
     */
    if (OMPI_SUCCESS != (ret = ompi_rte_register())) {
	fprintf(stderr, "ompi_rte_init: failed in ompi_rte_register()\n");
	return ret;
    }

   /* finalize the rte startup */
    if (OMPI_SUCCESS != (ret = ompi_rte_init_finalstage(&allow_multi_user_threads,
							&have_hidden_threads))) {
	printf("failed to finalize the rte startup\n");
	return ret;
    }
 

    exit_cmd = false;
    while (!exit_cmd) {
	printf("ompiconsole> ");
	usercmd = ompi_getinputline();
	if (0 == strncmp(usercmd, "exit", strlen("exit"))) {
	    exit_cmd = true;
	    ompi_console_sendcmd(OMPI_DAEMON_EXIT_CMD);
	} else if (0 == strncmp(usercmd, "contactinfo", strlen("contactinfo"))) {
	    ompi_console_sendcmd(OMPI_DAEMON_CONTACT_QUERY_CMD);
	    if (0 > mca_oob_recv_packed(&seed, &buffer, &recv_tag)) {
		printf("****got a bad response\n");
	    } else {

		if (0 > ompi_unpack_string(buffer, &str_response)) {
		    printf("****couldn't decode answer\n");
		} else {
		    printf(str_response);
		    printf("\n");
		}
	    }
	} else if (0 == strncmp(usercmd, "dumpvm", strlen("dumpvm"))) {
	    fprintf(stderr, "getting vm list\n");
	    list = ompi_registry.get(OMPI_REGISTRY_OR, "ompi-vm", NULL);
	    fprintf(stderr, "got vm list: length %d\n", (int)ompi_list_get_size(list));
	    for (item = ompi_list_get_first(list);
		 item != ompi_list_get_end(list);
		 item = ompi_list_get_next(item)) {
		value = (ompi_registry_value_t*)item;
		buffer = (ompi_buffer_t)value->object;
		ompi_unpack_string(buffer, &hostname);
		ompi_unpack(buffer, &proc_name, 1, OMPI_NAME);
		ompi_unpack_string(buffer, &contact_info);
		ompi_unpack(buffer, &proc_slots, 1, OMPI_INT32);
		printf("host: %s\n", hostname);
		printf("proc: [%d,%d,%d]\n", proc_name.cellid, proc_name.jobid, proc_name.vpid);
		printf("cont: %s\n", contact_info);
		printf("slot: %d\n\n", proc_slots); 
	    }
	} else {
	    printf("huh???\n");
	}
    }

    fprintf(stderr, "finalize rte\n");
    ompi_rte_finalize();
    fprintf(stderr, "close mca\n");
    mca_base_close();
    fprintf(stderr, "finalize ompi\n");
    ompi_finalize();
    exit(0);
}


static void ompi_console_sendcmd(ompi_daemon_cmd_flag_t usercmd)
{
    ompi_buffer_t cmd;
    ompi_daemon_cmd_flag_t command;
    int recv_tag;
    ompi_process_name_t seed={0,0,0};

    ompi_buffer_init(&cmd, 0);
    command = usercmd;
    recv_tag = MCA_OOB_TAG_DAEMON;
    ompi_pack(cmd, &command, 1, OMPI_DAEMON_OOB_PACK_CMD);
    mca_oob_send_packed(&seed, cmd, MCA_OOB_TAG_DAEMON, 0);
    ompi_buffer_free(cmd);
}

char *ompi_getinputline()
{
    char *ret, *buff;
    char input[OMPI_CONSOLE_MAX_LINE_LENGTH];

    ret = fgets(input, OMPI_CONSOLE_MAX_LINE_LENGTH, stdin);
    if (NULL != ret) {
	input[strlen(input)-1] = '\0';  /* remove newline */
	buff = strdup(input);
	return buff;
    }
    return NULL;
}

