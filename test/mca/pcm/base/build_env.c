/*
 * $HEADER$
 */


#include "ompi_config.h"
#include "support.h"

#include <stdio.h>

#include "mca/pcm/base/base.h"
#include "util/argv.h"

char *env[] = {
    "ENV0=",
    "OMPI_MCA_CONFIG_FOO=blah",
    "ENV1=blah blah blah",
    "FOO_OMPI_MCA_BLAH=hi there",
    "ENV2=foo bar is fun",
    "ENV3=123",
    NULL
};

int
main(int argc, char *argv[])
{
    int ret;
    int len = 0;
    char ** out = NULL;
    int i;

    test_init("sched_comm_t");

    len = ompi_argv_count(env);
    if (len != 6) {
        test_failure( "ompi_argv_count(env) failed" );
    } else {
        test_success();
    }

    ret = mca_pcm_base_build_base_env(env, &out);
    if (OMPI_SUCCESS != ret) {
        test_failure("mca_pcm_base_build_base_env");
    } else {
        test_success();
    }

    len = ompi_argv_count(out);
    if (len != 1) {
        printf("out:\n");
        for (i = 0 ; out[i] != NULL ; ++i) {
            printf("\t%s\n", out[i]);
        }
        test_failure("ompi_argv_count(out)");
    } else {
        test_success();
    }    
    
    test_finalize();
    return 0;
}
