/*
 * Copyright (c) 2021-2026 Computer Architecture and VLSI Systems (CARV)
 *                         Laboratory, ICS Forth. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
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

static int xhc_hierarchy_create(xhc_module_t *module,
    ompi_communicator_t *comm, opal_list_t *level_defs, int nlevel_defs,
    xhc_hierarchy_t **hierarchy_dst);

static int xhc_hierarchy_sort(mca_coll_xhc_module_t *module,
    ompi_communicator_t *comm, xhc_hierarchy_t *hierarchy);

// ------------------------------------------------

int mca_coll_xhc_hierarchy_make(xhc_module_t *module, XHC_COLLTYPE_T colltype,
    const char *hierarchy_string, xhc_hierarchy_t **hierarchy_dst)
{
    opal_list_t *level_defs = NULL;
    int nlevel_defs = 0;

    xhc_hierarchy_t *hierarchy = NULL;

    int err, return_code = OMPI_SUCCESS;

    // ---

    char *err_param_name = xhc_op_config_source_param("hierarchy",
        colltype, module->op_config[colltype].hierarchy_source);

    err = xhc_component_parse_hierarchy(hierarchy_string,
        &level_defs, &nlevel_defs, err_param_name);
    if(OMPI_SUCCESS != err) {RETURN_WITH_ERROR(return_code, err, end);}

    err = xhc_hierarchy_create(module, module->comm,
        level_defs, nlevel_defs, &hierarchy);
    if(OMPI_SUCCESS != err) {RETURN_WITH_ERROR(return_code, err, end);}

    err = xhc_hierarchy_sort(module, module->comm, hierarchy);
    if(OMPI_SUCCESS != err) {RETURN_WITH_ERROR(return_code, err, end);}

    // ---

    *hierarchy_dst = hierarchy;

    end:

    free(err_param_name);

    for(int i = 0; i < nlevel_defs; i++) {
        OPAL_LIST_DESTRUCT(&level_defs[i]);
    }
    free(level_defs);

    if(OMPI_SUCCESS != return_code) {
        if(hierarchy) {
            free(hierarchy->levels);
        }

        free(hierarchy);
    }

    return return_code;
}

// ------------------------------------------------

static int xhc_hierarchy_create(xhc_module_t *module,
    ompi_communicator_t *comm, opal_list_t *level_defs, int nlevel_defs,
    xhc_hierarchy_t **hierarchy_dst)
{
    xhc_peer_info_t *peer_info = module->peer_info;
    xhc_coll_fns_t xhc_fns;

    int rank = module->rank;
    int n_ranks = module->n_ranks;

    xhc_hierarchy_t *hierarchy = NULL;

    opal_hwloc_locality_t *loc_list = NULL;
    ompi_datatype_t *hwloc_locality_type = NULL;

    int err, return_code = OMPI_SUCCESS;

    xhc_module_set_coll_fns(comm, &module->prev_colls, &xhc_fns);

    hierarchy = calloc(1, sizeof(xhc_hierarchy_t));
    if(!hierarchy) {RETURN_WITH_ERROR(return_code, OMPI_ERR_OUT_OF_RESOURCE, end);}

    hierarchy->levels = malloc(nlevel_defs * sizeof(xhc_hierarchy_level_t));
    hierarchy->n_levels = nlevel_defs;

    loc_list = malloc(n_ranks * sizeof(opal_hwloc_locality_t));

    if(!hierarchy->levels || !loc_list) {
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
            || def_0->max_members > 0 || def_0->approx_members || def_0->split > 1);

        if(is_virtual) {
            if(XHC_LOC_EXT_BITS == module->n_virt_localities) {
                opal_show_help("help-coll-xhc.txt", "too-many-virt-hiers", true);
                RETURN_WITH_ERROR(return_code, OMPI_ERR_NOT_SUPPORTED, end);
            }

            locality = 1u << (XHC_LOC_EXT_START + module->n_virt_localities);
            module->n_virt_localities++;
        } else {
            locality = def_0->named_loc;
        }

        hierarchy->levels[h] = (xhc_hierarchy_level_t) {.loc = locality};

        xhc_loc_def_t *def, *def_next;

        /* Handle rank lists; take note if I belong
         * in one, and remove them from the mix. */
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

            int ticket = (NULL == my_def ? rank : (dir_fwd ? n_ranks : -1));
            int chosen;

            err = comm->c_coll->coll_allreduce(&ticket, &chosen, 1,
                MPI_INT, (dir_fwd ? MPI_MIN : MPI_MAX), comm,
                comm->c_coll->coll_allreduce_module);
            if(OMPI_SUCCESS != err) {
                RETURN_WITH_ERROR(return_code, err, end);
            }

            if(chosen >= 0 && chosen < n_ranks
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

        // If working with rank list, set the ranks from the list as "local"
        if(my_def->rank_list) {
            for(int i = 0; i < my_def->rank_list_len; i++) {
                for(int r = my_def->rank_list[i].start_rank;
                        r <= my_def->rank_list[i].end_rank && r < n_ranks; r++) {
                    peer_info[r].locality |= locality;
                }
            }
        } else if(is_virtual) {
            /* We might have a named locality instead of a rank list, but if
             * we still needed to create a virtual one, we need to apply it. */
            for(int r = 0; r < n_ranks; r++) {
                if(loc_list[r] != my_def->named_loc) {
                    continue;
                }

                if(!PEER_IS_LOCAL(peer_info, r, my_def->named_loc)) {
                    continue;
                }

                peer_info[r].locality |= locality;
            }
        }

        /* The 'split' and 'max members' modifiers used to be handled here,
         * but the issue is we don't know which ranks to work with, since we
         * don't know which will be candidates. The candidates on each level
         * are those ranks that were leaders in the previous one. This process
         * takes place in xhc_comms_make(); instead of duplicating it, the
         * modifiers are now handled there. */

        hierarchy->levels[h].max_members = my_def->max_members;
        hierarchy->levels[h].approx_members = my_def->approx_members;
        hierarchy->levels[h].split = my_def->split;

        OBJ_RELEASE_IF_NOT_NULL(my_def);
    }

    *hierarchy_dst = hierarchy;

end:

    xhc_module_set_coll_fns(comm, &xhc_fns, NULL);

    free(loc_list);

    if(OMPI_SUCCESS != return_code) {
        if(hierarchy) {
            free(hierarchy->levels);
        }

        free(hierarchy);
    }

    return return_code;
}

static int xhc_hierarchy_sort(mca_coll_xhc_module_t *module,
    ompi_communicator_t *comm, xhc_hierarchy_t *hierarchy)
{
    xhc_peer_info_t *peer_info = module->peer_info;
    int n_ranks = module->n_ranks;

    xhc_hierarchy_level_t *old_levels = hierarchy->levels;
    int n_levels = hierarchy->n_levels;

    xhc_hierarchy_level_t *new_levels = NULL;
    bool *levels_done = NULL;

    int return_code = OMPI_SUCCESS;

    new_levels = malloc((n_levels + 1) * sizeof(xhc_hierarchy_level_t));
    levels_done = calloc(n_levels, sizeof(bool));

    if(NULL == new_levels || NULL == levels_done) {
        RETURN_WITH_ERROR(return_code, OMPI_ERR_OUT_OF_RESOURCE, end);
    }

    bool has_virtual = false;
    for(int l = 0; l < n_levels; l++) {
        if(old_levels[l].loc >= (1 << XHC_LOC_EXT_START)) {
            has_virtual = true;
            break;
        }
    }

    /* If any virtual hierarchy is involved, attempting to sort it is likely
     * asking for trouble. Skip the sorting, and only consider adding a top
     * common locality. There is a chance it wasn't actually necessary, but
     * it never hurts. */

    if(has_virtual) {
        memcpy(new_levels, old_levels, n_levels * sizeof(xhc_hierarchy_level_t));
    } else {
        for(int new_idx = n_levels - 1; new_idx >= 0; new_idx--) {
            int max_matches_count = -1;
            int max_matches_hier_idx = -1;

            for(int l = 0; l < n_levels; l++) {
                if(levels_done[l]) {
                    continue;
                }

                int matches = 0;

                for(int r = 0; r < n_ranks; r++) {
                    if(PEER_IS_LOCAL(peer_info, r, old_levels[l].loc)) {
                        matches++;
                    }
                }

                if(matches > max_matches_count) {
                    max_matches_count = matches;
                    max_matches_hier_idx = l;
                }
            }

            assert(-1 != max_matches_count);

            new_levels[new_idx] = old_levels[max_matches_hier_idx];
            levels_done[max_matches_hier_idx] = true;
        }
    }

    xhc_loc_t common_locality = (xhc_loc_t) -1;

    for(int r = 0; r < n_ranks; r++) {
        ompi_proc_t *proc = ompi_comm_peer_lookup(comm, r);
        common_locality &= proc->super.proc_flags;
    }

    if(0 == common_locality) {
        opal_output_verbose(MCA_BASE_VERBOSE_ERROR,
            ompi_coll_base_framework.framework_output,
            "coll:xhc: Error: There is no locality common "
            "to all ranks in the communicator");

        RETURN_WITH_ERROR(return_code, OMPI_ERR_NOT_SUPPORTED, end);
    }

    if(0 == n_levels || (common_locality & new_levels[n_levels - 1].loc)
            != new_levels[n_levels - 1].loc) {

        new_levels[n_levels] = (xhc_hierarchy_level_t) {.loc = common_locality};
        n_levels++;
    }

    REALLOC(new_levels, n_levels, xhc_hierarchy_level_t);

    free(hierarchy->levels);

    hierarchy->levels = new_levels;
    hierarchy->n_levels = n_levels;

end:

    free(levels_done);

    if(OMPI_SUCCESS != return_code) {
        free(new_levels);
    }

    return return_code;
}
