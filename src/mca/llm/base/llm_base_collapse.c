/* -*- C -*-
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "mca/llm/llm.h"
#include "mca/llm/base/base.h"
#include "mca/llm/base/base_internal.h"

#include <stdio.h>

static
bool
has_conflicts(mca_llm_base_node_t *a, mca_llm_base_node_t *b)
{
    mca_llm_base_valuepair_t *a_val, *b_val;
    ompi_list_item_t *a_item, *b_item;

    for (a_item = ompi_list_get_first(&(a->info)) ;
         a_item != ompi_list_get_end(&(a->info)) ;
         a_item = ompi_list_get_next(a_item)) {
        a_val = (mca_llm_base_valuepair_t*) a_item;

        for (b_item = ompi_list_get_first(&(b->info)) ;
             b_item != ompi_list_get_end(&(b->info)) ;
             b_item = ompi_list_get_next(b_item)) {
            b_val = (mca_llm_base_valuepair_t*) b_item;

            /* if both a_val and b_val have the same key but different
               values, we have a conflict */
            if ((strcmp(a_val->key, b_val->key) == 0) &&
                (strcmp(a_val->value, b_val->value) != 0)) {
                return true;
            }
        }
    }

    return false;
}


static
void
keyval_merge(mca_llm_base_node_t *new, mca_llm_base_node_t *old)
{
    ompi_list_item_t *old_item;

    while (NULL != (old_item = ompi_list_remove_first(&(old->info)))) {
        ompi_list_append(&(new->info), old_item);
    }
}


int
mca_llm_base_collapse_resources(ompi_list_t *hostlist)
{
    mca_llm_base_node_t *curr_node, *check_node;
    ompi_list_item_t *curr_nodeitem, *check_nodeitem, *tmp;

    for (curr_nodeitem = ompi_list_get_first(hostlist) ;
         curr_nodeitem != ompi_list_get_end(hostlist) ;
         curr_nodeitem = ompi_list_get_next(curr_nodeitem)) {

        curr_node = (mca_llm_base_node_t*) curr_nodeitem;
        for (check_nodeitem = ompi_list_get_next(curr_nodeitem) ;
             check_nodeitem != ompi_list_get_end(hostlist) ;
             check_nodeitem = ompi_list_get_next(check_nodeitem)) {

            check_node = (mca_llm_base_node_t*) check_nodeitem;

            if ((strcmp(curr_node->hostname, check_node->hostname) == 0) &&
                (!has_conflicts(curr_node, check_node))) {
                /* they are mergeable */
                curr_node->count += check_node->count;
                keyval_merge(curr_node, check_node);

                /* delete from the list */
                tmp = ompi_list_remove_item(hostlist, 
                                            check_nodeitem);
                OBJ_RELEASE(check_nodeitem);
                check_nodeitem = tmp;
            }
        }
    }

    return OMPI_SUCCESS;
}
