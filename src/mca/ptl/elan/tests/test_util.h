#include <string.h>

static void env_init_for_elan()
{
    char  hostname[32];

    setenv("OMPI_MCA_oob_cofs_dir", "/home/1/yuw/tmp", 1);
    /*setenv("OMPI_MCA_oob_cofs_dir", "/tmp/COFS", 1);*/
    setenv("OMPI_MCA_pcm_cofs_cellid", "1", 1);
    setenv("OMPI_MCA_pcm_cofs_jobid", "1", 1);
    setenv("OMPI_MCA_pcm_cofs_num_procs", "2", 1);

    gethostname(hostname, 32);

    if ( strcmp("quad0", hostname) == 0) {
	fprintf(stdout, "I am %s rank %d\n", hostname, 0);
	fflush(stdout);
	setenv("OMPI_MCA_pcm_cofs_procid", "1", 0);
    } else {
	fprintf(stdout, "I am %s rank %d\n", hostname, 1);
	setenv("OMPI_MCA_pcm_cofs_procid", "1", 1);
    }
}
