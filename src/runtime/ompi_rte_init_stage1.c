/*
 * $HEADER$
 */

/** @file **/

/* #define _GNU_SOURCE */

#include "ompi_config.h"

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

#include "runtime/runtime.h"

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
    /* .name =                */    "default-universe",
    /* .host =                */    "localhost",
    /* .uid =                 */    NULL,
    /* .pid =                 */    0,
    /* .persistence =         */    false,
    /* .scope =               */    "local",
    /* .silent_mode =         */    true,
    /* .web_server =          */    false,
    /* .socket_contact_info = */    NULL,
    /* .oob_contact_info =    */    NULL,
    /* .console_connected =   */    false,
    /* .scriptfile =          */    NULL,
    /* .hostfile =            */    NULL
};


int ompi_rte_init_stage1(bool *allow_multi_user_threads, bool *have_hidden_threads)
{
    int ret;
    bool user_threads, hidden_threads;

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
     * Out of Band Messaging
     */
    if (OMPI_SUCCESS != (ret = mca_oob_base_open())) {
	/* JMS show_help */
	printf("show_help: ompi_rte_init failed in oob_base_open\n");
	return ret;
    }
    user_threads = true;
    hidden_threads = false;
    if (OMPI_SUCCESS != (ret = mca_oob_base_init(&user_threads, 
						 &hidden_threads))) {
	/* JMS show_help */
	printf("show_help: ompi_rte_init failed in mca_oob_base_init()\n");
	return ret;
    }
    *allow_multi_user_threads &= user_threads;
    *have_hidden_threads |= hidden_threads;

    return OMPI_SUCCESS;
}
