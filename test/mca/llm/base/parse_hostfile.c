/* -*- C -*-
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "support.h"

#include "class/ompi_object.h"
#include "class/ompi_list.h"
#include "mca/llm/base/base.h"
#include "mca/llm/base/base_internal.h"

static char *cmd1_str="diff ./test1_out ./test1_out_std";
static char *cmd2_str="diff ./test2_out ./test2_out_std";

int
main(int argc, char *argv[])
{
    ompi_list_t *hostlist;
    ompi_rte_node_allocation_t *node;
    ompi_rte_valuepair_t *valpair;
    ompi_list_item_t *nodeitem, *valpairitem; 
    FILE *test1_out=NULL; /* output file for first test */
    FILE *test2_out=NULL; /* output file for second test */
    int result;           /* result of system call */

    test_init("parse_hostfile_t");

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

    /* test 1 - test base_parse_hostfile */
    hostlist = mca_llm_base_parse_hostfile("testfile");
    assert(hostlist != NULL);

    fprintf(test1_out,"Original hostfile\n");

    for (nodeitem = ompi_list_get_first(hostlist);
         nodeitem != ompi_list_get_end(hostlist);
         nodeitem = ompi_list_get_next(nodeitem)) {

        node = (ompi_rte_node_allocation_t*) nodeitem;
        fprintf(test1_out, "\t%s %d\n", node->hostname, node->count);

        for (valpairitem = ompi_list_get_first(&(node->info));
             valpairitem != ompi_list_get_end(&(node->info));
             valpairitem = ompi_list_get_next(valpairitem)) {
            valpair = (ompi_rte_valuepair_t*) valpairitem;
            fprintf(test1_out, "\t\t%s = %s\n", valpair->key, valpair->value);
        }
    }    
    fclose( test1_out );

    /* See if the file matches the test standard */
    result = system( cmd1_str );
    if( result == 0 ) {
        test_success();
    }
    else {
      test_failure( "parse_hostfile test1 failed" );
    }

    /* test 2 */
    mca_llm_base_collapse_resources(hostlist);
    fprintf(test2_out, "Compressed hostfile\n");

    for (nodeitem = ompi_list_get_first(hostlist);
         nodeitem != ompi_list_get_end(hostlist);
         nodeitem = ompi_list_get_next(nodeitem)) {

        node = (ompi_rte_node_allocation_t*) nodeitem;
        fprintf(test2_out, "\t%s %d\n", node->hostname, node->count);

        for (valpairitem = ompi_list_get_first(&(node->info));
             valpairitem != ompi_list_get_end(&(node->info));
             valpairitem = ompi_list_get_next(valpairitem)) {
            valpair = (ompi_rte_valuepair_t*) valpairitem;
            fprintf(test2_out, "\t\t%s = %s\n", valpair->key, valpair->value);
        }
    }   
    fclose( test2_out );

    /* See if the file matches the test standard */
    result = system( cmd2_str );
    if( result == 0 ) {
        test_success();
    }
    else {
      test_failure( "parse_hostfile test2 failed" );
    }

    OBJ_RELEASE(hostlist);
    
    test_finalize();
    return 0;
}
