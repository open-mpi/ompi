/*
 * $HEADER$
 */


#include "ompi_config.h"
#include "support.h"

#include <stdio.h>

#include "runtime/runtime_types.h"
#include "mca/pcm/base/base.h"

static char *cmd1_str="diff ./test1_out ./test1_out_std";
static char *cmd2_str="diff ./test2_out ./test2_out_std";

char *env[] = {
    "ENV1=blah blah blah",
    "ENV2=foo bar is fun",
    "ENV3=123",
    NULL
};

int
main(int argc, char *argv[])
{
    ompi_rte_node_schedule_t *schedout, *schedin;
    FILE *test1_out=NULL; /* output file for first test */
    FILE *test2_out=NULL; /* output file for second test */
    FILE *test2_in = NULL;
    int result;           /* result of system call */
    int jobid = 123;

    test_init("sched_comm_t");

    /* Open output files for the tests */
    test1_out = fopen("./test1_out", "w+" );
    if( test1_out == NULL ) {
      printf("can't open test 1 output file\n");
      exit(-1);
    }
    test2_out = fopen("./test2_out", "w+" );
    if( test2_out == NULL ) {
      printf("can't open test 2 output file\n");
      exit(-1);
    }

    schedout = OBJ_NEW(ompi_rte_node_schedule_t);
    schedout->argv = argv;
    schedout->argc = argc;
    schedout->env = env;
    schedout->cwd = "/foo/bar/baz";

    result = mca_pcm_base_send_schedule(test1_out, jobid, schedout, 
                                        schedout->nodelist);
    if (result != OMPI_SUCCESS) {
        test_failure("send_schedule failed");
        exit(1);
    }

    fclose( test1_out );

    /* See if the file matches the test standard */
    result = system( cmd1_str );
    if( result == 0 ) {
        test_success();
    }
    else {
      test_failure( "sched_comm test1 failed" );
    }

    /* test 2 */
    schedin = OBJ_NEW(ompi_rte_node_schedule_t);

    test2_in = fopen("./test1_out", "r");

    result = mca_pcm_base_recv_schedule(test2_in, &jobid, schedin,
                                        schedin->nodelist);
    if (result != OMPI_SUCCESS) {
        test_failure("recv_schedule failed");
        exit(1);
    }
    mca_pcm_base_send_schedule(test2_out, jobid, schedin, schedin->nodelist);
    if (result != OMPI_SUCCESS) {
        test_failure("send_schedule (2) failed");
        exit(1);
    }

    fclose( test2_out );

    /* See if the file matches the test standard */
    result = system( cmd2_str );
    if( result == 0 ) {
        test_success();
    }
    else {
      test_failure( "sched_comm test2 failed" );
    }

    OBJ_RELEASE(schedin);
    OBJ_RELEASE(schedout);
    
    test_finalize();
    return 0;
}
