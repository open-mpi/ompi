/*
 * $HEADER$
 */

#include <stdlib.h>
#include <string.h>

static void env_init_for_elan()
{
    char  *rms_rank;

    setenv("OMPI_MCA_oob_cofs_dir", "/home/1/yuw/tmp", 1);
    setenv("OMPI_MCA_pcm_cofs_cellid", "1", 1);
    setenv("OMPI_MCA_pcm_cofs_jobid", "1", 1);
    setenv("OMPI_MCA_pcm_cofs_num_procs", "2", 1);
    setenv("OMPI_MCA_ptl_base_exclude", "tcp", 1);
    setenv("OMPI_MCA_oob_base_include", "cofs", 1);
    /*setenv("OMPI_MCA_oob_base_exclude", "tcp", 1);*/

    if (NULL != (rms_rank = getenv("RMS_RANK"))) {
	/* RMS_JOBID:RMS_NNODES:RMS_NPROCS:RMS_NODEID:RMS_RESOURCEID */
	setenv("OMPI_MCA_pcm_cofs_procid", rms_rank, 1);
    } else {
	fprintf(stderr, "Hi, please test elan4 from RMS for now\n");
    }
}
