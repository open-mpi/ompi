/*
 * $HEADER$
 */

/** @file **/

/* #define _GNU_SOURCE */

#include "ompi_config.h"

#include "include/constants.h"
#include "runtime/runtime.h"
#include "util/output.h"
#include "threads/mutex.h"
#include "mca/pcm/base/base.h"
#include "mca/pcmclient/base/base.h"
#include "mca/llm/base/base.h"
#include "mca/oob/oob.h"
#include "mca/ns/base/base.h"
#include "mca/gpr/base/base.h"
#include "util/proc_info.h"
#include "util/session_dir.h"
#include "util/sys_info.h"

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
int ompi_rte_init(bool *allow_multi_user_threads, bool *have_hidden_threads)
{
    int ret;
    bool user_threads, hidden_threads;
    char *jobid_str=NULL, *procid_str=NULL;

    *allow_multi_user_threads = true;
    *have_hidden_threads = false;

    ompi_output(0, "entered rte_init - starting name server");
    /*
     * Name Server
     */
    if (OMPI_SUCCESS != (ret = mca_ns_base_open())) {
	/* JMS show_help */
	printf("show_help: ompi_rte_init failed in ns_base_open\n");
	return ret;
    }
    if (OMPI_SUCCESS != (ret = mca_ns_base_select(&user_threads,
						  &hidden_threads))) {
	/* JMS show_help */
	printf("show_help: ompi_rte_init failed in ns_base_select\n");
	return ret;
    }
    *allow_multi_user_threads &= user_threads;
    *have_hidden_threads |= hidden_threads;

    ompi_output(0, "starting pcm-client");
    /*
     * Process Control and Monitoring Client
     */
    if (OMPI_SUCCESS != (ret = mca_pcmclient_base_open())) {
	/* JMS show_help */
	printf("show_help: ompi_rte_init failed in pcmclient_base_open\n");
	return ret;
    }
    if (OMPI_SUCCESS != (ret = mca_pcmclient_base_select(&user_threads, 
							 &hidden_threads))) {
	printf("show_help: ompi_rte_init failed in pcmclient_base_select\n");
	/* JMS show_help */
	return ret;
    }
    *allow_multi_user_threads &= user_threads;
    *have_hidden_threads |= hidden_threads;

    ompi_output(0, "starting llm");

    /*
     * Allocation code - open only.  pcm will init if needed
     */
    if (OMPI_SUCCESS != (ret = mca_llm_base_open())) {
	/* JMS show_help */
	printf("show_help: ompi_rte_init failed in llm_base_open\n");
	return ret;
    }

    ompi_output(0, "starting pcm");

    /*
     * Process Control and Monitoring
     */
    if (OMPI_SUCCESS != (ret = mca_pcm_base_open())) {
	/* JMS show_help */
	printf("show_help: ompi_rte_init failed in pcm_base_open\n");
	return ret;
    }
    if (OMPI_SUCCESS != (ret = mca_pcm_base_select(&user_threads, 
						   &hidden_threads))) {
	printf("show_help: ompi_rte_init failed in pcm_base_select\n");
	/* JMS show_help */
	return ret;
    }
    *allow_multi_user_threads &= user_threads;
    *have_hidden_threads |= hidden_threads;

    ompi_output(0, "starting oob");

    /*
     * Out of Band Messaging
     */
    if (OMPI_SUCCESS != (ret = mca_oob_base_open())) {
	/* JMS show_help */
	printf("show_help: ompi_rte_init failed in oob_base_open\n");
	return ret;
    }
    if (OMPI_SUCCESS != (ret = mca_oob_base_init(&user_threads, 
						 &hidden_threads))) {
	/* JMS show_help */
	printf("show_help: ompi_rte_init failed in mca_oob_base_init()\n");
	return ret;
    }
    *allow_multi_user_threads &= user_threads;
    *have_hidden_threads |= hidden_threads;

    ompi_output(0, "starting gpr");

    /*
     * Registry 
     */
    if (OMPI_SUCCESS != (ret = mca_gpr_base_open())) {
	/* JMS show_help */
	printf("show_help: ompi_rte_init failed in mca_gpr_base_open()\n");
	return ret;
    }
    if (OMPI_SUCCESS != (ret = mca_gpr_base_select(&user_threads, 
						   &hidden_threads))) {
	/* JMS show_help */
	printf("show_help: ompi_rte_init failed in mca_gpr_base_select()\n");
	return ret;
    }
    *allow_multi_user_threads &= user_threads;
    *have_hidden_threads |= hidden_threads;

    ompi_output(0, "calling proc_info");

    /*
     * Fill in the various important structures
     */
    /* proc structure startup */
    ompi_proc_info();  

    ompi_output(0, "doing session_dir");

    /* session directory */
    jobid_str = ompi_name_server.get_jobid_string(ompi_process_info.name);
    procid_str = ompi_name_server.get_vpid_string(ompi_process_info.name);
    if (OMPI_ERROR == ompi_session_dir(true,
				       ompi_process_info.tmpdir_base,
				       ompi_system_info.user,
				       ompi_system_info.nodename, NULL, 
				       ompi_process_info.my_universe,
				       jobid_str, procid_str)) {
	if (jobid_str != NULL) free(jobid_str);
	if (procid_str != NULL) free(procid_str);
	return OMPI_ERROR;
    }


    /*
     * Call back into NS/GPR/OOB to allow them to do any final initialization
     * (e.g. register callbacks w/ OOB, put contact info in register).
     */
 /*    if (OMPI_SUCCESS != (ret = ompi_name_server.init())) { */
/* 	printf("show_help: ompi_rte_init failed in ompi_name_server.init()\n"); */
/* 	return ret; */
/*     } */

/*     if (OMPI_SUCCESS != (ret = mca_oob_base_register())) { */
/* 	printf("show_help: ompi_rte_init failed in mca_oob_base_register()\n"); */
/* 	return ret; */
/*     } */


    /* 
     * All done 
     */
    return OMPI_SUCCESS;
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
    node->info = OBJ_NEW(ompi_list_t);
}


/** destructor for \c ompi_rte_node_allocation_t */
static
void
ompi_rte_int_node_allocation_destruct(ompi_object_t *obj)
{
    ompi_rte_node_allocation_t *node = (ompi_rte_node_allocation_t*) obj;
    ompi_rte_valuepair_t *valpair;
    ompi_list_item_t *item;

    while (NULL != (item = ompi_list_remove_first(node->info))) {
        valpair = (ompi_rte_valuepair_t*) item;
        OBJ_RELEASE(valpair);
    }

    OBJ_RELEASE(node->info);
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
