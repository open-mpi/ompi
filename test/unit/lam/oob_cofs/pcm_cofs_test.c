#include "lam_config.h"

#include "lam/constants.h"
#include "mca/lam/oob/oob.h"
#include "mca/lam/oob/cofs/src/oob_cofs.h"
#include "mca/lam/pcm/pcm.h"
#include "mca/lam/pcm/cofs/src/pcm_cofs.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


int
main(int argc, char *argv[])
{
  int ret;
  struct mca_pcm_1_0_0_t* modret;
  mca_pcm_proc_t *procs;
  size_t nprocs;
  mca_pcm_proc_t *my_proc;
  int priority = 0;
  bool allow_threads = false;
  bool have_hidden_threads = false;

  modret = mca_pcm_cofs_init(&priority, &allow_threads, &have_hidden_threads);
  if (modret == NULL) {
    printf("failed to init PCM module.  Aborting.\n");
    exit(1);
  }

  /*
   * Do all the stuff that a pcm module user would do
   */
  ret = mca_pcm_cofs_proc_startup();
  if (ret != MPI_SUCCESS) {
    printf("Failed in cofs_startup() with retcode: %d\n", ret);
    exit(1);
  }

  ret = mca_pcm_cofs_proc_get_peers(&procs, &nprocs);
  if (ret != MPI_SUCCESS) {
    printf("Failed in cofs_proc_get_peers() with retcode %d\n", ret);
    exit(1);
  }

  my_proc = mca_pcm_cofs_proc_get_me();
  if (my_proc == NULL) {
    printf("Failed to get my proc entry\n");
    exit(1);
  }

  printf("Hello, World, I am vpid %d of job %s\n", my_proc->vpid, my_proc->job_handle);

  /*
   * from here, we can bring up the OOB interface (if it isn't already) and be
   * ready to run...
   */

  /*
   * Clean up after ourselves - normall that mca interface would do that for us...
   */
  mca_pcm_cofs_close();

  return 0;
}
