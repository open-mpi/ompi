/*
 * Copyright (c) 2021-2024 Computer Architecture and VLSI Systems (CARV)
 *                         Laboratory, ICS Forth. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "mpi.h"

#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"
#include "opal/util/show_help.h"
#include "opal/util/minmax.h"

#include "coll_xhc.h"

// ------------------------------------------------

static int xhc_hierarchy_create(xhc_module_t *module, ompi_communicator_t *comm,
    opal_list_t *level_defs, int nlevel_defs, xhc_loc_t **hierarchy_dst,
    int *hierarchy_len_dst);

static int xhc_hierarchy_sort(mca_coll_xhc_module_t *module,
    ompi_communicator_t *comm, xhc_loc_t **hierarchy_dst,
    int *hierarchy_len_dst);

// ------------------------------------------------

int mca_coll_xhc_hierarchy_make(xhc_module_t *module,
        ompi_communicator_t *comm, const char *hierarchy_string,
        xhc_loc_t **hierarchy_dst, int *hierarchy_len_dst) {

    xhc_loc_t *hierarchy = NULL;
    int hierarchy_len;

    opal_list_t *level_defs = NULL;
    int nlevel_defs = 0;

    int err, return_code = OMPI_SUCCESS;

    // ---

    err = xhc_component_parse_hierarchy(hierarchy_string,
        &level_defs, &nlevel_defs);
    if(OMPI_SUCCESS != err) {RETURN_WITH_ERROR(return_code, err, end);}

    err = xhc_hierarchy_create(module, comm, level_defs,
        nlevel_defs, &hierarchy, &hierarchy_len);
    if(OMPI_SUCCESS != err) {RETURN_WITH_ERROR(return_code, err, end);}

    err = xhc_hierarchy_sort(module, comm, &hierarchy, &hierarchy_len);
    if(OMPI_SUCCESS != err) {RETURN_WITH_ERROR(return_code, err, end);}

    // ---

    *hierarchy_dst = hierarchy;
    *hierarchy_len_dst = hierarchy_len;

    end:

    for(int i = 0; i < nlevel_defs; i++) {
        OPAL_LIST_DESTRUCT(&level_defs[i]);
    }
    free(level_defs);

    if(OMPI_SUCCESS != err) {
        free(hierarchy);
    }

    return return_code;
}

// ------------------------------------------------

static int xhc_hierarchy_create(xhc_module_t *module, ompi_communicator_t *comm,
        opal_list_t *level_defs, int nlevel_defs, xhc_loc_t **hierarchy_dst,
        int *hierarchy_len_dst) {

    xhc_peer_info_t *peer_info = module->peer_info;
    xhc_coll_fns_t xhc_fns;

    int comm_size = ompi_comm_size(comm);
    int rank = ompi_comm_rank(comm);

    xhc_loc_t *hierarchy = NULL;
    int nvirt_hiers = 0;

    int *rank_list;

    opal_hwloc_locality_t *loc_list;
    ompi_datatype_t *hwloc_locality_type = NULL;

    int err, return_code = OMPI_SUCCESS;

    xhc_module_set_coll_fns(comm, &module->prev_colls, &xhc_fns);

    hierarchy = malloc(nlevel_defs * sizeof(xhc_loc_t));
    rank_list = malloc(comm_size * sizeof(int));
    loc_list = malloc(comm_size * sizeof(opal_hwloc_locality_t));

    if(!hierarchy || !rank_list || !loc_list) {
        RETURN_WITH_ERROR(return_code, OMPI_ERR_OUT_OF_RESOURCE, end);
    }

    switch(sizeof(opal_hwloc_locality_t)) {
        case 1: hwloc_locality_type = MPI_UINT8_T; break;
        case 2: hwloc_locality_type = MPI_UINT16_T; break;
        case 4: hwloc_locality_type = MPI_UINT32_T; break;
        case 8: hwloc_locality_type = MPI_UINT64_T; break;
    }
    assert(hwloc_locality_type);

    for(int h = 0; h < nlevel_defs; h++) {
        opal_list_t *defs = &level_defs[h];

        xhc_loc_def_t *my_def = NULL;
        xhc_loc_t locality;

        xhc_loc_def_t *def_0 = (xhc_loc_def_t *) opal_list_get_first(defs);

        bool is_virtual = (opal_list_get_size(defs) > 1 || def_0->rank_list
            || def_0->split > 1 || def_0->max_ranks > 0);

        if(is_virtual) {
            if(XHC_LOC_EXT_BITS == nvirt_hiers) {
                opal_show_help("help-coll-xhc.txt", "too-many-virt-hiers", true);
                RETURN_WITH_ERROR(return_code, OMPI_ERR_NOT_SUPPORTED, end);
            }

            locality = 1 << (XHC_LOC_EXT_START + nvirt_hiers);
            nvirt_hiers++;
        } else {
            locality = def_0->named_loc;
        }

        hierarchy[h] = locality;
        def_0 = NULL;

        xhc_loc_def_t *def, *def_next;

        /* Handle rank lists; take note if I belong
         * in one, and remove them from the mix */
        OPAL_LIST_FOREACH_SAFE(def, def_next, defs, xhc_loc_def_t) {
            if(def->rank_list) {
                if(!my_def) {
                    for(int rl = 0; rl < def->rank_list_len; rl++) {
                        if(rank >= def->rank_list[rl].start_rank
                                && rank <= def->rank_list[rl].end_rank) {
                            my_def = def;
                            break;
                        }
                    }
                }

                opal_list_remove_item(defs, (opal_list_item_t *) def);
                if(def != my_def) {OBJ_RELEASE(def);}
            }
        }

        bool dir_fwd = true;

        /* When multiple locality defitions are present, they are assigned
         * to groups in a left-to-right fashion. At every turn, the first
         * rank (determined by the minimum ID) that's still not part of
         * a locality, as well as the other ranks that are local with it,
         * claim/consume the next locality from the list. The direction
         * serves to implement the repeat modifier. When it is located,
         * the process starts taking place right-to-left following the max
         * ID. At the end and after the loop, the repeated locality will
         * be the only one left and all remaining ranks will follow it. */
        while(opal_list_get_size(defs) > 1) {
            def = (xhc_loc_def_t *) (dir_fwd ? opal_list_get_first(defs)
                : opal_list_get_last(defs));

            if(dir_fwd && def->repeat) {
                dir_fwd = false;
                continue;
            }

            int ticket = (NULL == my_def ? rank : (dir_fwd ? comm_size : -1));
            int chosen;

            err = comm->c_coll->coll_allreduce(&ticket, &chosen, 1,
                MPI_INT, (dir_fwd ? MPI_MIN : MPI_MAX), comm,
                comm->c_coll->coll_allreduce_module);
            if(OMPI_SUCCESS != err) {
                RETURN_WITH_ERROR(return_code, err, end);
            }

            if(chosen >= 0 && chosen < comm_size
                    && PEER_IS_LOCAL(peer_info, chosen, def->named_loc)) {
                my_def = def;
            }

            opal_list_remove_item(defs, (opal_list_item_t *) def);
            if(def != my_def) {OBJ_RELEASE(def);}
        }

        if(opal_list_get_size(defs) > 0 && !my_def) {
            my_def = (xhc_loc_def_t *) opal_list_get_first(defs);
            opal_list_remove_item(defs, (opal_list_item_t *) my_def);
        }

        /* Share which named locality each rank follows; ranks that
         * follow different localities shouldn't be grouped together */ 
        opal_hwloc_locality_t follow_loc = (my_def ? my_def->named_loc : 0);
        err = comm->c_coll->coll_allgather(&follow_loc, 1,
            hwloc_locality_type, loc_list, 1, hwloc_locality_type,
            comm, comm->c_coll->coll_allgather_module);
        if(OMPI_SUCCESS != err) {
            RETURN_WITH_ERROR(return_code, err, end);
        }

        if(NULL == my_def) {
            continue;
        }

        int my_id = -1;
        int members = 0;

        // If working with rank list, set the ranks from the list as "local"
        if(my_def->rank_list) {
            for(int i = 0; i < my_def->rank_list_len; i++) {
                for(int r = my_def->rank_list[i].start_rank;
                        r <= my_def->rank_list[i].end_rank && r < comm_size; r++) {
                    if(r == rank) {
                        my_id = members;
                    }

                    peer_info[r].locality |= locality;
                    rank_list[members++] = r;
                }
            }
        } else if(is_virtual) {
            /* We might have a named locality instead of a rank list, but if
             * we still needed to create a virtual one, we need to apply it. */
            for(int r = 0; r < comm_size; r++) {
                if(loc_list[r] != my_def->named_loc) {
                    continue;
                }

                if(!PEER_IS_LOCAL(peer_info, r, my_def->named_loc)) {
                    continue;
                }

                if(r == rank) {
                    my_id = members;
                }

                peer_info[r].locality |= locality;
                rank_list[members++] = r;
            }
        }

        /* If split or max ranks was specified, must partition the locality
         * and remove the previously added locality mapping to some ranks. */
        if(my_def->split > 1) {
            int piece_size = members / my_def->split;
            int leftover = members % my_def->split;

            for(int m = 0, next_border = 0; m < members; m++) {
                if(m == next_border) {
                    next_border += piece_size + (leftover > 0 ? 1 : 0);
                    if(leftover > 0) {leftover--;}

                    if(my_id >= m && my_id < next_border) {
                        m = next_border - 1;
                        continue;
                    }
                }

                peer_info[rank_list[m]].locality &= ~locality;
            }
        } else if(my_def->max_ranks > 1) {
            for(int m = 0; m < members; m++) {
                if(m % my_def->max_ranks == 0) {
                    if(my_id >= m && my_id - m < my_def->max_ranks) {
                        m += my_def->max_ranks - 1;
                        continue;
                    }
                }

                peer_info[rank_list[m]].locality &= ~locality;
            }
        }

        OBJ_RELEASE_IF_NOT_NULL(my_def);
    }

    *hierarchy_dst = hierarchy;
    *hierarchy_len_dst = nlevel_defs;

end:

    xhc_module_set_coll_fns(comm, &xhc_fns, NULL);

    free(rank_list);
    free(loc_list);

    if(OMPI_SUCCESS != return_code) {
        free(hierarchy);
    }

    return return_code;
}

static int xhc_hierarchy_sort(mca_coll_xhc_module_t *module,
        ompi_communicator_t *comm, xhc_loc_t **hierarchy_dst,
        int *hierarchy_len_dst) {

    xhc_peer_info_t *peer_info = module->peer_info;
    int comm_size = ompi_comm_size(comm);

    xhc_loc_t *old_hier = *hierarchy_dst;
    int hier_len = *hierarchy_len_dst;

    xhc_loc_t *new_hier = NULL;
    bool *hier_done = NULL;

    int return_code = OMPI_SUCCESS;

    new_hier = malloc((hier_len + 1) * sizeof(xhc_loc_t));
    hier_done = calloc(hier_len, sizeof(bool));

    if(NULL == new_hier || NULL == hier_done) {
        RETURN_WITH_ERROR(return_code, OMPI_ERR_OUT_OF_RESOURCE, end);
    }

    bool has_virtual = false;
    for(int i = 0; i < hier_len; i++) {
        if(old_hier[i] >= (1 << XHC_LOC_EXT_START)) {
            has_virtual = true;
            break;
        }
    }

    /* If any virtual hierarchy is involved, attempting to sort it is likely
     * asking for trouble. Skip the sorting, and only consider adding a top
     * common locality. There is a chance it wasn't actually necessary, but
     * it never hurts. */

    if(has_virtual) {
        memcpy(new_hier, old_hier, hier_len * sizeof(xhc_loc_t));
    } else {
        for(int new_idx = hier_len - 1; new_idx >= 0; new_idx--) {
            int max_matches_count = -1;
            int max_matches_hier_idx = -1;

            for(int i = 0; i < hier_len; i++) {
                if(hier_done[i]) {
                    continue;
                }

                int matches = 0;

                for(int r = 0; r < comm_size; r++) {
                    if(PEER_IS_LOCAL(peer_info, r, old_hier[i])) {
                        matches++;
                    }
                }

                if(matches > max_matches_count) {
                    max_matches_count = matches;
                    max_matches_hier_idx = i;
                }
            }

            assert(-1 != max_matches_count);

            new_hier[new_idx] = old_hier[max_matches_hier_idx];
            hier_done[max_matches_hier_idx] = true;
        }
    }

    xhc_loc_t common_locality = (xhc_loc_t) -1;

    for(int r = 0; r < comm_size; r++) {
        ompi_proc_t *proc = ompi_comm_peer_lookup(comm, r);
        common_locality &= proc->super.proc_flags;
    }

    if(0 == common_locality) {
        opal_output_verbose(MCA_BASE_VERBOSE_COMPONENT,
            ompi_coll_base_framework.framework_output,
            "coll:xhc: Error: There is no locality common "
            "to all ranks in the communicator");

        RETURN_WITH_ERROR(return_code, OMPI_ERR_NOT_SUPPORTED, end);
    }

    if(0 == hier_len || (common_locality & new_hier[hier_len - 1])
            != new_hier[hier_len - 1]) {

        new_hier[hier_len] = common_locality;
        hier_len++;
    }

    REALLOC(new_hier, hier_len, xhc_loc_t);

    free(old_hier);

    *hierarchy_dst = new_hier;
    *hierarchy_len_dst = hier_len;

end:

    free(hier_done);

    if(OMPI_SUCCESS != return_code) {
        free(new_hier);
    }

    return return_code;
}
