/* -*- C -*-
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "mca/llm/base/base.h"
#include "mca/llm/base/base_internal.h"

int
main(int argc, char *argv[])
{
    ompi_list_t *hostlist;
    mca_llm_base_node_t *node;
    mca_llm_base_valuepair_t *valpair;
    ompi_list_item_t *nodeitem, *valpairitem; 

    hostlist = mca_llm_base_parse_hostfile("testfile");
    assert(hostlist != NULL);

    printf("Original hostfile\n");

    for (nodeitem = ompi_list_get_first(hostlist);
         nodeitem != ompi_list_get_end(hostlist);
         nodeitem = ompi_list_get_next(nodeitem)) {

        node = (mca_llm_base_node_t*) nodeitem;
        printf("\t%s %d\n", node->hostname, node->count);

        for (valpairitem = ompi_list_get_first(&(node->info));
             valpairitem != ompi_list_get_end(&(node->info));
             valpairitem = ompi_list_get_next(valpairitem)) {
            valpair = (mca_llm_base_valuepair_t*) valpairitem;
            printf("\t\t%s = %s\n", valpair->key, valpair->value);
        }
    }    

    mca_llm_base_collapse_resources(hostlist);
    printf("Compressed hostfile\n");

    for (nodeitem = ompi_list_get_first(hostlist);
         nodeitem != ompi_list_get_end(hostlist);
         nodeitem = ompi_list_get_next(nodeitem)) {

        node = (mca_llm_base_node_t*) nodeitem;
        printf("\t%s %d\n", node->hostname, node->count);

        for (valpairitem = ompi_list_get_first(&(node->info));
             valpairitem != ompi_list_get_end(&(node->info));
             valpairitem = ompi_list_get_next(valpairitem)) {
            valpair = (mca_llm_base_valuepair_t*) valpairitem;
            printf("\t\t%s = %s\n", valpair->key, valpair->value);
        }
    }    

    OBJ_RELEASE(hostlist);
    
    return 0;
}
