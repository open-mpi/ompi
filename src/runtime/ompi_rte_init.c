/*
 * $HEADER$
 */

/** @file **/

#include "ompi_config.h"

#include "include/constants.h"
#include "runtime/runtime.h"
#include "util/output.h"
#include "threads/mutex.h"
#include "mca/pcm/base/base.h"
#include "mca/oob/oob.h"
#include "mca/ns/base/base.h"
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
 * an optional command line parameter --universe=<rte universe name>
 * which says which universe this application wants to be a part
 * of. mpirun will then package this universe name and send it to the
 * processes it will be starting off (fork/exec) on local or remote
 * node.The packaging mechanism can be either command line parameter
 * to the a.out it forks or make it part of environment
 * (implementation dependent).  
 *
 * Case 2) When mpirun is done alone and no universe is present, then
 * the mpirun starts off the universe (using rte_boot), then
 * fork/execs the processes, passin g along the <universe_name>. 
 *
 * Case 3) For a singleton, if there is alrady an existing rte
 * universe which it wants to join, it can specify that using the
 * --universe command line. So it will do 
 *
 * $ ./a.out --universe=<universe_name>
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
    printf("show_help: ompi_rte_init failed in oob_base_init\n");
    return ret;
  }
  *allow_multi_user_threads &= user_threads;
  *have_hidden_threads |= hidden_threads;


  /*
   * Fill in the various important structures
   */
  /* proc structure startup */
  ompi_proc_info();  

  /* universe name */
  /* BWB - fix me fix me fix me */

  /* session directory */
  if(0 > asprintf(&jobid_str, "%-d", ompi_process_info.name->jobid)) {
      return OMPI_ERROR;
  }

  if(0 > asprintf(&procid_str, "%-d", ompi_process_info.name->vpid)) {
      if (jobid_str != NULL) free(jobid_str);
      return OMPI_ERROR;
  }

  if (OMPI_ERROR == ompi_session_dir(true, NULL, ompi_system_info.user, 
                                     ompi_system_info.nodename, NULL, 
                                     "bOb", jobid_str, procid_str)) {
      if (jobid_str != NULL) free(jobid_str);
      if (procid_str != NULL) free(procid_str);
      return OMPI_ERROR;
  }


  /* 
   * All done 
   */
  return OMPI_SUCCESS;
}
