/*
 * $HEADER$
 */

/** @file **/

/* #define _GNU_SOURCE */

#include "ompi_config.h"

#include <sys/types.h>
#include <unistd.h>

#include "include/constants.h"
#include "event/event.h"
#include "util/output.h"
#include "threads/mutex.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/base/mca_base_param.h"
#include "mca/pcm/base/base.h"
#include "mca/pcmclient/base/base.h"
#include "mca/llm/base/base.h"
#include "mca/oob/oob.h"
#include "mca/ns/base/base.h"
#include "mca/gpr/base/base.h"
#include "util/proc_info.h"
#include "util/session_dir.h"
#include "util/sys_info.h"
#include "util/cmd_line.h"

#include "runtime/runtime.h"
#include "runtime/runtime_internal.h"
#include "runtime/ompi_rte_wait.h"

/**
 * Initialze and setup a process in the OMPI RTE.
 *
 * @retval OMPI_SUCCESS Upon success.
 * @retval OMPI_ERROR Upon failure.
 *
 * This function performs 
 * 
 * Just a note for developer: 

 * So there are 3 ways in which an application can be started
 * 1) rte_boot, followed by mpirun
 * 2) mpirun (alone)
 * 3) singleton (./a.out)
 * 
 * Case 1) If the rte has already been booted, then mpirun will accept
 * an optional command line parameter --universe=[rte universe name]
 * which says which universe this application wants to be a part
 * of. mpirun will then package this universe name and send it to the
 * processes it will be starting off (fork/exec) on local or remote
 * node.The packaging mechanism can be either command line parameter
 * to the a.out it forks or make it part of environment
 * (implementation dependent).  
 *
 * Case 2) When mpirun is done alone and no universe is present, then
 * the mpirun starts off the universe (using rte_boot), then
 * fork/execs the processes, passin g along the [universe_name]. 
 *
 * Case 3) For a singleton, if there is alrady an existing rte
 * universe which it wants to join, it can specify that using the
 * --universe command line. So it will do 
 *
 * $ ./a.out --universe=[universe_name]
 * 
 * In this case, MPI_Init will have to be called as MPI_Init(&argc, &argv)

 * If it does not want to join any existing rte, then it just starts
 * off as ./a.out with no command line option. In that case, MPI_Init
 * does not necesaarily needs to passed argc and argv. Infact if argc
 * and argv are not passed or just have one entry (the command name),
 * then MPI_Init would assume that new rte universe needs to be
 * started up.
 *
 *
 * MPI_Init() will look at its argc, argv. If it find the universe
 * name there, fine. Else it looks at the environment variables for
 * universe_name. If it finds there, fine again. Under such
 * conditions, it joins the existing rte universe. If no universe
 * name is found, it calls rte_boot to start off a new rte universe.
 *
 * For singleton, MPI_Init() do:
 *
 * if (I am a singleton) and (there is no universe)
 *    do rte_boot
 *
 * But if I am not a singleton, then I have been started by mpirun and
 * already provided a universe_name to join. So I wont ever start a
 * universe under such conditons. mpirun will pass me the
 * universe_name (either mpirun would have started the universe, or it
 * would have been already started by rte_boot)
 */

/* globals used by RTE */
int ompi_rte_debug_flag=0;
ompi_universe_t ompi_universe_info = {
    /* .name =                */    NULL,
    /* .host =                */    NULL,
    /* .uid =                 */    NULL,
    /* .pid =                 */    0,
    /* .persistence =         */    false,
    /* .scope =               */    NULL,
    /* .probe =               */    false,
    /* .console =             */    false,
    /* .ns_replica =          */    NULL,
    /* .gpr_replica =         */    NULL,
    /* .seed_contact_info =    */   NULL,
    /* .console_connected =   */    false,
    /* .scriptfile =          */    NULL,
    /* .hostfile =            */    NULL
};

static void printname(char *location);

int ompi_rte_init(ompi_cmd_line_t *cmd_line, bool *allow_multi_user_threads, bool *have_hidden_threads)
{
    int ret;
    bool user_threads, hidden_threads;
    char *universe, *jobid_str, *procid_str;
    pid_t pid;
    mca_ns_base_jobid_t jobid;
    mca_ns_base_vpid_t vpid;
    ompi_process_name_t illegal_name={MCA_NS_BASE_CELLID_MAX, MCA_NS_BASE_JOBID_MAX, MCA_NS_BASE_VPID_MAX};
    ompi_process_name_t *new_name;

    *allow_multi_user_threads = true;
    *have_hidden_threads = false;

    ret =  mca_base_param_register_int("ompi", "rte", "debug", NULL, 0);
    mca_base_param_lookup_int(ret, &ompi_rte_debug_flag);


    /*
     * Initialize the event library 
    */
    if (OMPI_SUCCESS != (ret = ompi_event_init())) {
	    ompi_output(0, "ompi_rte_init: ompi_event_init failed with error status: %d\n", ret);
	    return ret;
    }

    /*
     * Internal startup
     */
    if (OMPI_SUCCESS != (ret = ompi_rte_internal_init_spawn())) {
	/* JMS show_help */
	printf("show_help: ompi_rte_init failed in ompi_rte_internal_init_spawn\n");
	return ret;
    }
    if (OMPI_SUCCESS != (ret = ompi_rte_wait_init())) {
	/* JMS show_help */
	printf("show_help: ompi_rte_init failed in ompi_rte_wait_init\n");
	return ret;
    }


    /*
     * Out of Band Messaging
     */
    if (OMPI_SUCCESS != (ret = mca_oob_base_open())) {
	/* JMS show_help */
	printf("show_help: ompi_rte_init failed in oob_base_open\n");
	return ret;
    }
    user_threads = true;
    hidden_threads = false;


    /*
     * Name Server - just do the open so we can access base components
     */
    if (OMPI_SUCCESS != (ret = mca_ns_base_open())) {
	/* JMS show_help */
	printf("show_help: ompi_rte_init failed in ns_base_open\n");
	return ret;
    }

    /*
     * Process Control and Monitoring Client - just open for now
     */
    if (OMPI_SUCCESS != (ret = mca_pcmclient_base_open())) {
	/* JMS show_help */
	printf("show_help: ompi_rte_init failed in pcmclient_base_open\n");
	return ret;
    }

    printname("component open");

    /*
     * Process Control and Monitoring Client -
     * just complete selection of proper module
     */
    user_threads = true;
    hidden_threads = false;
    if (OMPI_SUCCESS != (ret = mca_pcmclient_base_select(&user_threads, 
							 &hidden_threads))) {
	printf("show_help: ompi_rte_init failed in pcmclient_base_select\n");
	/* JMS show_help */
	return ret;
    }
    *allow_multi_user_threads &= user_threads;
    *have_hidden_threads |= hidden_threads;

    printname("pcm_select");

    /* complete setup of OOB */
    if (OMPI_SUCCESS != (ret = mca_oob_base_init(&user_threads, 
						 &hidden_threads))) {
	/* JMS show_help */
	printf("show_help: ompi_rte_init failed in mca_oob_base_init()\n");
	return ret;
    }
    *allow_multi_user_threads &= user_threads;
    *have_hidden_threads |= hidden_threads;

    printname("oob_init");

    /* parse non-PCM environmental variables and fill corresponding info structures */
    ompi_rte_parse_environ();

    printname("parse_environ");

    if (NULL != cmd_line) {
	/* parse the cmd_line for rte options - override settings from enviro, where necessary
	 * copy everything into enviro variables for passing later on
	 */
	ompi_rte_parse_cmd_line(cmd_line);

	/* parse the cmd_line for daemon options - gets all the options relating
	 * specifically to seed behavior, in case i'm a seed, but also gets
	 * options about scripts and hostfiles that might be of use to me
	 * overrride enviro variables where necessary
	 */
	ompi_rte_parse_daemon_cmd_line(cmd_line);
    }

    printname("cmd_line");

    /* check for existing universe to join */
    if (OMPI_SUCCESS != (ret = ompi_rte_universe_exists())) {
	if (ompi_rte_debug_flag) {
	    ompi_output(0, "ompi_mpi_init: could not join existing universe");
	}
	if (OMPI_ERR_NOT_FOUND != ret) {
	    /* if it exists but no contact could be established,
	     * define unique name based on current one.
	     * and start new universe with me as seed
	     */
	    universe = strdup(ompi_universe_info.name);
	    free(ompi_universe_info.name);
	    ompi_universe_info.name = NULL;
	    pid = getpid();
	    if (0 > asprintf(&ompi_universe_info.name, "%s-%d", universe, pid) && ompi_rte_debug_flag) {
		ompi_output(0, "mpi_init: error creating unique universe name");
	    }
	}

	ompi_process_info.my_universe = strdup(ompi_universe_info.name);
	ompi_process_info.seed = true;
	if (NULL != ompi_universe_info.ns_replica) {
	    free(ompi_universe_info.ns_replica);
	    ompi_universe_info.ns_replica = NULL;
	}
	if (NULL != ompi_process_info.ns_replica) {
	    free(ompi_process_info.ns_replica);
	    ompi_process_info.ns_replica = NULL;
	}
	if (NULL != ompi_universe_info.gpr_replica) {
	    free(ompi_universe_info.gpr_replica);
	    ompi_universe_info.gpr_replica = NULL;
	}
	if (NULL != ompi_process_info.gpr_replica) {
	    free(ompi_process_info.gpr_replica);
	    ompi_process_info.gpr_replica = NULL;
	}
    }

    printname("univ_exists");

    /*
     * Name Server - base already opened in stage1, so just complete the selection
     * of the proper module
     */
    user_threads = true;
    hidden_threads = false;
    if (OMPI_SUCCESS != (ret = mca_ns_base_select(&user_threads,
						  &hidden_threads))) {
	/* JMS show_help */
	printf("show_help: ompi_rte_init failed in ns_base_select\n");
	return ret;
    }
    *allow_multi_user_threads &= user_threads;
    *have_hidden_threads |= hidden_threads;

    /*
     * Allocation code - open only.  pcm will init if needed
     */
    if (OMPI_SUCCESS != (ret = mca_llm_base_open())) {
	/* JMS show_help */
	printf("show_help: ompi_rte_init failed in llm_base_open\n");
	return ret;
    }

    printname("llm_open");

    /*
     * Process Control and Monitoring - lazy load
     */
    if (OMPI_SUCCESS != (ret = mca_pcm_base_open())) {
	/* JMS show_help */
	printf("show_help: ompi_rte_init failed in pcm_base_open\n");
	return ret;
    }

    printname("pcm_open");

    /*
     * Registry 
     */
    if (OMPI_SUCCESS != (ret = mca_gpr_base_open())) {
	/* JMS show_help */
	printf("show_help: ompi_rte_init failed in mca_gpr_base_open()\n");
	return ret;
    }
    user_threads = true;
    hidden_threads = false;
    if (OMPI_SUCCESS != (ret = mca_gpr_base_select(&user_threads, 
						   &hidden_threads))) {
	/* JMS show_help */
	printf("show_help: ompi_rte_init failed in mca_gpr_base_select()\n");
	return ret;
    }
    *allow_multi_user_threads &= user_threads;
    *have_hidden_threads |= hidden_threads;
 
    printname("gpr_select");

    /*****    SET MY NAME IF NOT ALREADY PROVIDED IN ENVIRONMENT   *****/
    if (0 == ompi_name_server.compare(OMPI_NS_CMP_ALL, 
                                      ompi_rte_get_self(), 
                                      &illegal_name)) {
        /* name not previously set */
	if (ompi_process_info.seed || NULL == ompi_process_info.ns_replica) {
            /* seed or singleton - couldn't join existing univ */
            new_name = ompi_name_server.create_process_name(0,0,0);
	    *ompi_rte_get_self() = *new_name;
            free(new_name);
	    printname("singleton/seed");
	} else {  
            /* not seed or singleton - name server exists elsewhere - get a name for me */
	    jobid = ompi_name_server.create_jobid();
	    vpid = ompi_name_server.reserve_range(jobid, 1);
            new_name = ompi_name_server.create_process_name(0, jobid, vpid);
	    *ompi_rte_get_self() = *new_name;
            free(new_name);
	    printname("name_server_provided");
	}
    }

    /* setup my session directory */
    jobid_str = ompi_name_server.get_jobid_string(ompi_rte_get_self());
    procid_str = ompi_name_server.get_vpid_string(ompi_rte_get_self());
 
    if (ompi_rte_debug_flag) {
	ompi_output(0, "[%d,%d,%d] setting up session dir with",
                    ompi_rte_get_self()->cellid, 
                    ompi_rte_get_self()->jobid, 
                    ompi_rte_get_self()->vpid);
	if (NULL != ompi_process_info.tmpdir_base) {
	    ompi_output(0, "\ttmpdir %s", ompi_process_info.tmpdir_base);
	}
	ompi_output(0, "\tuniverse %s", ompi_process_info.my_universe);
	ompi_output(0, "\tuser %s", ompi_system_info.user);
	ompi_output(0, "\thost %s", ompi_system_info.nodename);
	ompi_output(0, "\tjobid %s", jobid_str);
	ompi_output(0, "\tprocid %s", procid_str);
    }
    if (OMPI_ERROR == ompi_session_dir(true,
				       ompi_process_info.tmpdir_base,
				       ompi_system_info.user,
				       ompi_system_info.nodename, NULL, 
				       ompi_process_info.my_universe,
				       jobid_str, procid_str)) {
	if (jobid_str != NULL) free(jobid_str);
	if (procid_str != NULL) free(procid_str);
	exit(-1);
    }

    /*
     * Call back into OOB to allow do any final initialization
     * (e.g. put contact info in register).
     */
    if (OMPI_SUCCESS != (ret = mca_oob_base_module_init())) {
       ompi_output(0, "ompi_rte_init: failed in mca_oob_base_module_init()\n");
       return ret;
    }

     /* 
     * All done 
     */

    return OMPI_SUCCESS;
}

static void printname(char *loc)
{
    if (ompi_rte_debug_flag) {
	if (NULL == ompi_rte_get_self()) {
	    ompi_output(0, "My name after %s has NOT been set", loc);
	} else {
	    ompi_output(0, "My name after %s is [%d,%d,%d]", loc, OMPI_NAME_ARGS(*ompi_rte_get_self()));
	}
    }
}


/*
 *  interface type support
 */

/** constructor for \c ompi_rte_node_schedule_t */
static
void
ompi_rte_int_node_schedule_construct(ompi_object_t *obj)
{
    ompi_rte_node_schedule_t *sched = (ompi_rte_node_schedule_t*) obj;
    sched->nodelist = OBJ_NEW(ompi_list_t);
}


/** destructor for \c ompi_rte_node_schedule_t */
static
void
ompi_rte_int_node_schedule_destruct(ompi_object_t *obj)
{
    ompi_rte_node_schedule_t *sched = (ompi_rte_node_schedule_t*) obj;
    ompi_rte_node_allocation_t *node;
    ompi_list_item_t *item;

    if (NULL == sched->nodelist) return;

    while (NULL != (item = ompi_list_remove_first(sched->nodelist))) {
        node = (ompi_rte_node_allocation_t*) item;
        OBJ_RELEASE(node);
    }

    OBJ_RELEASE(sched->nodelist);
}


/** constructor for \c ompi_rte_node_allocation_t */
static
void
ompi_rte_int_node_allocation_construct(ompi_object_t *obj)
{
    ompi_rte_node_allocation_t *node = (ompi_rte_node_allocation_t*) obj;
    node->start = 0;
    node->nodes = 0;
    node->count = 0;
    node->data = NULL;
}


/** destructor for \c ompi_rte_node_allocation_t */
static
void
ompi_rte_int_node_allocation_destruct(ompi_object_t *obj)
{
    ompi_rte_node_allocation_t *node = (ompi_rte_node_allocation_t*) obj;

    if (NULL == node->data) return;

    OBJ_RELEASE(node->data);
}


/** constructor for \c ompi_rte_valuepair_t */
static
void
ompi_rte_int_valuepair_construct(ompi_object_t *obj)
{
    ompi_rte_valuepair_t *valpair = (ompi_rte_valuepair_t*) obj;
    valpair->key = NULL;
    valpair->value = NULL;
}


/** destructor for \c ompi_rte_valuepair_t */
static
void
ompi_rte_int_valuepair_destruct(ompi_object_t *obj)
{
    ompi_rte_valuepair_t *valpair = (ompi_rte_valuepair_t*) obj;
    if (NULL != valpair->key) free(valpair->key);
    if (NULL != valpair->value) free(valpair->value);
}

/** constructor for \c ompi_startup_shutdown_message_t */
static
void
ompi_startup_shutdown_message_construct(ompi_startup_shutdown_message_t *msg)
{
    msg->msg = NULL;
}


/** destructor for \c ompi_startup_shutdown_message_t */
static
void
ompi_startup_shutdown_message_destruct(ompi_startup_shutdown_message_t *msg)
{
    if (NULL != msg->msg) {
	OBJ_RELEASE(msg->msg);
    }
}


/** create instance information for \c ompi_rte_node_schedule_t */
OBJ_CLASS_INSTANCE(ompi_rte_node_schedule_t, ompi_list_item_t,
                   ompi_rte_int_node_schedule_construct,
                   ompi_rte_int_node_schedule_destruct);
/** create instance information for \c ompi_rte_node_allocation_t */
OBJ_CLASS_INSTANCE(ompi_rte_node_allocation_t, ompi_list_item_t, 
                   ompi_rte_int_node_allocation_construct, 
                   ompi_rte_int_node_allocation_destruct);
/** create instance information for \c ompi_rte_valuepair_t */
OBJ_CLASS_INSTANCE(ompi_rte_valuepair_t, ompi_list_item_t, 
                   ompi_rte_int_valuepair_construct,
                   ompi_rte_int_valuepair_destruct);
/** create instance information for \c ompi_rte_node_allocation_data_t */
OBJ_CLASS_INSTANCE(ompi_rte_node_allocation_data_t, ompi_object_t, 
                   NULL, NULL);
/** create instance information for \c ompi_startup_shutdown_message_t */
OBJ_CLASS_INSTANCE(ompi_startup_shutdown_message_t, ompi_list_item_t,
                   ompi_startup_shutdown_message_construct,
                   ompi_startup_shutdown_message_destruct);
