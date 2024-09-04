/*
 * Copyright (c) 2021-2023 Computer Architecture and VLSI Systems (CARV)
 *                         Laboratory, ICS Forth. All rights reserved.
 * Copyright (c) 2024      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <string.h>

#include "mpi.h"

#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"
#include "opal/mca/smsc/smsc.h"

#include "opal/util/arch.h"
#include "opal/util/show_help.h"
#include "opal/util/minmax.h"

#include "coll_xhc.h"

static int xhc_module_save_fallback_fns(
    xhc_module_t *module, ompi_communicator_t *comm);

static int xhc_module_create_hierarchy(mca_coll_xhc_module_t *module,
    ompi_communicator_t *comm, opal_list_t *level_defs, int nlevel_defs,
    xhc_loc_t **hierarchy_dst, int *hierarchy_len_dst);

static int xhc_module_sort_hierarchy(mca_coll_xhc_module_t *module,
    ompi_communicator_t *comm, xhc_loc_t **hierarchy_dst, int *hierarchy_len_dst);

// -----------------------------

static void xhc_module_clear(xhc_module_t *module) {
    memset(&module->prev_colls, 0, sizeof(module->prev_colls));

    module->comm_size = 0;
    module->rank = -1;

    module->hierarchy_string = NULL;
    module->hierarchy = NULL;
    module->hierarchy_len = 0;

    module->chunks = NULL;
    module->chunks_len = 0;

    module->rbuf = NULL;
    module->rbuf_size = 0;

    module->peer_info = NULL;
    module->data = NULL;
    module->init = false;
}

static void mca_coll_xhc_module_construct(mca_coll_xhc_module_t *module) {
    xhc_module_clear(module);
}

static void mca_coll_xhc_module_destruct(mca_coll_xhc_module_t *module) {
    xhc_fini(module);

    free(module->hierarchy_string);
    free(module->hierarchy);
    free(module->chunks);
    free(module->rbuf);
    free(module->peer_info);

    xhc_module_clear(module);
}

OBJ_CLASS_INSTANCE(mca_coll_xhc_module_t, mca_coll_base_module_t,
    mca_coll_xhc_module_construct, mca_coll_xhc_module_destruct);

// -----------------------------

mca_coll_base_module_t *mca_coll_xhc_module_comm_query(ompi_communicator_t *comm,
        int *priority) {

    if((*priority = mca_coll_xhc_component.priority) < 0) {
        return NULL;
    }

    if(OMPI_COMM_IS_INTER(comm) || ompi_comm_size(comm) == 1
            || ompi_group_have_remote_peers (comm->c_local_group)) {

        opal_output_verbose(MCA_BASE_VERBOSE_COMPONENT,
            ompi_coll_base_framework.framework_output,
            "coll:xhc:comm_query (%s/%s): intercomm, self-comm, "
            "or not all ranks local; disqualifying myself",
            ompi_comm_print_cid(comm), comm->c_name);

        return NULL;
    }

    int comm_size = ompi_comm_size(comm);
    for(int r = 0; r < comm_size; r++) {
        ompi_proc_t *proc = ompi_comm_peer_lookup(comm, r);

        if(proc->super.proc_arch != opal_local_arch) {
            opal_output_verbose(MCA_BASE_VERBOSE_COMPONENT,
                ompi_coll_base_framework.framework_output,
                "coll:xhc:comm_query (%s/%s): All ranks not of the same arch; "
                "disabling myself", ompi_comm_print_cid(comm), comm->c_name);

            return NULL;
        }
    }

    mca_coll_base_module_t *module =
        (mca_coll_base_module_t *) OBJ_NEW(mca_coll_xhc_module_t);

    if(module == NULL) {
        return NULL;
    }

    module->coll_module_enable = mca_coll_xhc_module_enable;
    module->coll_module_disable = mca_coll_xhc_module_disable;

    module->coll_barrier = mca_coll_xhc_barrier;

    if(mca_smsc == NULL) {
        opal_output_verbose(MCA_BASE_VERBOSE_COMPONENT,
            ompi_coll_base_framework.framework_output,
            "coll:xhc: Warning: No opal/smsc support found; "
            "only barrier will be enabled");

        return module;
    }

    module->coll_bcast = mca_coll_xhc_bcast;

    if(!mca_smsc_base_has_feature(MCA_SMSC_FEATURE_CAN_MAP)) {
        opal_output_verbose(MCA_BASE_VERBOSE_COMPONENT,
            ompi_coll_base_framework.framework_output,
            "coll:xhc: Warning: opal/smsc module is not CAN_MAP capable; "
            "(all)reduce will be disabled, bcast might see reduced performance");

        return module;
    }

    module->coll_allreduce = mca_coll_xhc_allreduce;
    module->coll_reduce = mca_coll_xhc_reduce;

    return module;
}

#define COLL_FN_HELPER(_m, _api) .coll_ ## _api = (_m)->coll_ ## _api, \
    .coll_ ## _api ## _module = (_m)

int mca_coll_xhc_module_enable(mca_coll_base_module_t *ompi_module,
        ompi_communicator_t *comm) {

    xhc_module_t *module = (xhc_module_t *) ompi_module;

    int ret;

    // ---

    ret = xhc_module_save_fallback_fns(module, comm);

    /* This can/will happen often (see #9885), but theoretically
     * isn't a problem, as in these cases the component wouldn't
     * end up getting used anyway. */
    if(ret != OMPI_SUCCESS) {
        opal_output_verbose(MCA_BASE_VERBOSE_COMPONENT,
            ompi_coll_base_framework.framework_output,
            "coll:xhc:module_enable (%s/%s): No previous fallback component "
            "found; disabling myself", ompi_comm_print_cid(comm), comm->c_name);

        return ret;
    }

    // ---

    module->comm_size = ompi_comm_size(comm);
    module->rank = ompi_comm_rank(comm);

    module->peer_info = calloc(module->comm_size, sizeof(xhc_peer_info_t));

    for(int r = 0; r < module->comm_size; r++) {
        ompi_proc_t *peer_proc = ompi_comm_peer_lookup(comm, r);

        module->peer_info[r].proc = peer_proc;
        module->peer_info[r].locality = peer_proc->super.proc_flags;
    }

    module->peer_info[module->rank].locality |=
        ((1 << OMPI_XHC_LOC_EXT_BITS) - 1) << OMPI_XHC_LOC_EXT_START;

    // ---

    /* This needs to happen here, and we need to save the hierarchy string,
     * because the info value will have been gone by the time lazy_init is
     * called. Furthermore, we can't prepeare the hierarchy here, as it might
     * required communication (allgather) with the other ranks. */

    const char *hier_mca = mca_coll_xhc_component.hierarchy_mca;

    opal_cstring_t *hier_info;
    int hier_info_flag = 0;

    if(comm->super.s_info != NULL) {
        opal_info_get(comm->super.s_info, "ompi_comm_coll_xhc_hierarchy",
            &hier_info, &hier_info_flag);

        if(hier_info_flag) {
            hier_mca = hier_info->string;
        }
    }

    module->hierarchy_string = strdup(hier_mca);

    if(hier_info_flag) {
        OBJ_RELEASE(hier_info);
    }

    if(!module->hierarchy_string) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    // ---

    ret = xhc_component_parse_chunk_sizes(mca_coll_xhc_component.chunk_size_mca,
        &module->chunks, &module->chunks_len);
    if(ret != OMPI_SUCCESS) {
        return ret;
    }

    // ---

    xhc_coll_fns_t xhc_fns = (xhc_coll_fns_t) {
        COLL_FN_HELPER(ompi_module, allreduce),
        COLL_FN_HELPER(ompi_module, barrier),
        COLL_FN_HELPER(ompi_module, bcast),
        COLL_FN_HELPER(ompi_module, reduce)
    };

    xhc_module_install_fns(module, comm, xhc_fns);

    return OMPI_SUCCESS;
}

int mca_coll_xhc_module_disable(mca_coll_base_module_t *ompi_module,
        ompi_communicator_t *comm) {

    xhc_module_t *module = (xhc_module_t *) ompi_module;

    xhc_module_install_fallback_fns(module, comm, NULL);
    mca_coll_xhc_module_destruct(module);

    return OMPI_SUCCESS;
}

// -----------------------------

#define SAVE_FALLBACK_COLL(_comm, _m, _dst, _api) do { \
    if((_m)->coll_ ## _api) { \
        MCA_COLL_SAVE_API(_comm, _api, (_dst).coll_ ## _api, \
            (_dst).coll_ ## _api ## _module, "xhc"); \
        \
        if(!(_dst).coll_ ## _api || !(_dst).coll_ ## _api ## _module) { \
            _save_status = OMPI_ERR_NOT_FOUND; \
        } \
    } \
} while(0)

#define INSTALL_FALLBACK_COLL(_comm, _m, _saved, _new, _api) do { \
    if((_comm)->c_coll->coll_ ## _api ## _module == (_m)) { \
        MCA_COLL_SAVE_API(_comm, _api, (_saved).coll_ ## _api, \
            (_saved).coll_ ## _api ## _module, "xhc"); \
        MCA_COLL_INSTALL_API(_comm, _api, (_new).coll_ ## _api, \
            (_new).coll_ ## _api ## _module, "xhc"); \
    } \
} while(0)

#define INSTALL_COLL(_comm, _src, _api) do { \
    if((_src).coll_ ## _api) { \
        MCA_COLL_INSTALL_API(_comm, _api, (_src).coll_ ## _api, \
            (_src).coll_ ## _api ## _module, "xhc"); \
    } \
} while(0)

/* Save the function pointers of the previous module, in XHC's
 * struct. Only the functions that XHC will provide are saved. */
static int xhc_module_save_fallback_fns(
        xhc_module_t *module, ompi_communicator_t *comm) {

    mca_coll_base_module_t *ompi_module = (mca_coll_base_module_t *) module;

    xhc_coll_fns_t colls = {0};
    int _save_status = OMPI_SUCCESS;

    SAVE_FALLBACK_COLL(comm, ompi_module, colls, allreduce);
    SAVE_FALLBACK_COLL(comm, ompi_module, colls, barrier);
    SAVE_FALLBACK_COLL(comm, ompi_module, colls, bcast);
    SAVE_FALLBACK_COLL(comm, ompi_module, colls, reduce);

    if(_save_status == OMPI_SUCCESS) {
        module->prev_colls = colls;
    }

    return _save_status;
}

/* Replace XHC's pointers in c_coll with those from the fallback
 * component saved earlier. XHC's pointers are conveniently returned
 * in prev_fns_dst, to later pass to xhc_module_install_fns. */
void mca_coll_xhc_module_install_fallback_fns(xhc_module_t *module,
        ompi_communicator_t *comm, xhc_coll_fns_t *prev_fns_dst) {

    mca_coll_base_module_t *ompi_module = (mca_coll_base_module_t *) module;

    xhc_coll_fns_t saved = {0};

    INSTALL_FALLBACK_COLL(comm, ompi_module, saved, module->prev_colls, allreduce);
    INSTALL_FALLBACK_COLL(comm, ompi_module, saved, module->prev_colls, barrier);
    INSTALL_FALLBACK_COLL(comm, ompi_module, saved, module->prev_colls, bcast);
    INSTALL_FALLBACK_COLL(comm, ompi_module, saved, module->prev_colls, reduce);

    if(prev_fns_dst) {
        *prev_fns_dst = saved;
    }
}

/*  */
void mca_coll_xhc_module_install_fns(xhc_module_t *module,
        ompi_communicator_t *comm, xhc_coll_fns_t fns) {

    (void) module;

    INSTALL_COLL(comm, fns, allreduce);
    INSTALL_COLL(comm, fns, barrier);
    INSTALL_COLL(comm, fns, bcast);
    INSTALL_COLL(comm, fns, reduce);
}

// -----------------------------

int mca_coll_xhc_module_prepare_hierarchy(
        xhc_module_t *module, ompi_communicator_t *comm) {

    int ret;

    opal_list_t *level_defs;
    int nlevel_defs;

    ret = xhc_component_parse_hierarchy(module->hierarchy_string,
        &level_defs, &nlevel_defs);
    if(ret != OMPI_SUCCESS) {
        return ret;
    }

    ret = xhc_module_create_hierarchy(module, comm, level_defs,
        nlevel_defs, &module->hierarchy, &module->hierarchy_len);
    if(ret != OMPI_SUCCESS) {
        return ret;
    }

    for(int i = 0; i < nlevel_defs; i++)
        OPAL_LIST_DESTRUCT(&level_defs[i]);
    free(level_defs);

    ret = xhc_module_sort_hierarchy(module, comm,
        &module->hierarchy, &module->hierarchy_len);
    if(ret != OMPI_SUCCESS) {
        return ret;
    }

    return OMPI_SUCCESS;
}

static int xhc_module_create_hierarchy(xhc_module_t *module,
        ompi_communicator_t *comm, opal_list_t *level_defs, int nlevel_defs,
        xhc_loc_t **hierarchy_dst, int *hierarchy_len_dst) {

    xhc_peer_info_t *peer_info = module->peer_info;

    int comm_size = ompi_comm_size(comm);
    int rank = ompi_comm_rank(comm);

    xhc_loc_t *hierarchy = NULL;
    int nvirt_hiers = 0;

    int *rank_list;

    opal_hwloc_locality_t *loc_list;
    ompi_datatype_t *hwloc_locality_type = NULL;

    int ret, return_code = OMPI_SUCCESS;

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
            if(nvirt_hiers == OMPI_XHC_LOC_EXT_BITS) {
                opal_output_verbose(MCA_BASE_VERBOSE_COMPONENT,
                    ompi_coll_base_framework.framework_output,
                    "coll:xhc: Error: Too many virtual hierarchies");

                RETURN_WITH_ERROR(return_code, OMPI_ERR_NOT_SUPPORTED, end);
            }

            locality = 1 << (OMPI_XHC_LOC_EXT_START + nvirt_hiers);
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
                if(def != my_def) {
                    OBJ_RELEASE(def);
                }
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

            int ticket = (my_def == NULL ? rank : (dir_fwd ? comm_size : -1));
            int chosen;

            ret = comm->c_coll->coll_allreduce(&ticket, &chosen, 1,
                MPI_INT, (dir_fwd ? MPI_MIN : MPI_MAX), comm,
                comm->c_coll->coll_allreduce_module);
            if(ret != OMPI_SUCCESS) {
                RETURN_WITH_ERROR(return_code, ret, end);
            }

            if(chosen >= 0 && chosen < comm_size
                    && PEER_IS_LOCAL(peer_info, chosen, def->named_loc)) {

                my_def = def;
            }

            opal_list_remove_item(defs, (opal_list_item_t *) def);
            if(def != my_def) {
                OBJ_RELEASE(def);
            }
        }

        if(opal_list_get_size(defs) > 0 && !my_def) {
            my_def = (xhc_loc_def_t *) opal_list_get_first(defs);
            opal_list_remove_item(defs, (opal_list_item_t *) my_def);
        }

        /* Share which named locality each rank follows; ranks that
         * follow different localities shouldn't be grouped together */
        opal_hwloc_locality_t follow_loc = (my_def ? my_def->named_loc : 0);
        ret = comm->c_coll->coll_allgather(&follow_loc, 1,
            hwloc_locality_type, loc_list, 1, hwloc_locality_type,
            comm, comm->c_coll->coll_allgather_module);
        if(ret != OMPI_SUCCESS) {
            RETURN_WITH_ERROR(return_code, ret, end);
        }

        if(my_def == NULL) {
            continue;
        }

        int member_id = -1;
        int members = 0;

        // If working with rank list, set the ranks from the list as "local"
        if(my_def->rank_list) {
            for(int i = 0; i < my_def->rank_list_len; i++) {
                for(int r = my_def->rank_list[i].start_rank;
                        r <= my_def->rank_list[i].end_rank && r < comm_size; r++) {
                    if(r == rank) {
                        member_id = members;
                    }

                    peer_info[r].locality |= locality;
                    rank_list[members++] = r;
                }
            }
        } else if(is_virtual) {
            /* We might have a named locality instead of a rank list, but if
             * we still needed to create a virtual one, we need to apply it */
            for(int r = 0; r < comm_size; r++) {
                if(loc_list[r] != my_def->named_loc) {
                    continue;
                }

                if(!PEER_IS_LOCAL(peer_info, r, my_def->named_loc)) {
                    continue;
                }

                if(r == rank) {
                    member_id = members;
                }

                peer_info[r].locality |= locality;
                rank_list[members++] = r;
            }
        }

        assert(member_id != -1);

        /* If split or max ranks was specified, math partition the locality
         * and remove the previously added locality mapping to some ranks */
        if(my_def->split > 1) {
            int piece_size = members / my_def->split;
            int leftover = members % my_def->split;

            for(int m = 0, next_border = 0; m < members; m++) {
                if(m == next_border) {
                    next_border += piece_size + (leftover > 0 ? 1 : 0);
                    if(leftover > 0) {
                        leftover--;
                    }

                    if(member_id >= m && member_id < next_border) {
                        m = next_border - 1;
                        continue;
                    }
                }

                peer_info[rank_list[m]].locality &= ~locality;
            }
        } else if(my_def->max_ranks > 1) {
            for(int m = 0; m < members; m++) {
                if(m % my_def->max_ranks == 0) {
                    if(member_id >= m && member_id - m < my_def->max_ranks) {
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

    free(rank_list);

    if(return_code != OMPI_SUCCESS) {
        free(hierarchy);
    }

    return return_code;
}

static int xhc_module_sort_hierarchy(xhc_module_t *module,
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

    if(new_hier == NULL || hier_done == NULL) {
        RETURN_WITH_ERROR(return_code, OMPI_ERR_OUT_OF_RESOURCE, end);
    }

    bool has_virtual = false;
    for(int i = 0; i < hier_len; i++) {
        if(old_hier[i] >= (1 << OMPI_XHC_LOC_EXT_START)) {
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

            assert(max_matches_count != -1);

            new_hier[new_idx] = old_hier[max_matches_hier_idx];
            hier_done[max_matches_hier_idx] = true;
        }
    }

    xhc_loc_t common_locality = (xhc_loc_t) -1;

    for(int r = 0; r < comm_size; r++) {
        ompi_proc_t *proc = ompi_comm_peer_lookup(comm, r);
        common_locality &= proc->super.proc_flags;
    }

    if(common_locality == 0) {
        opal_output_verbose(MCA_BASE_VERBOSE_COMPONENT,
            ompi_coll_base_framework.framework_output,
            "coll:xhc: Error: There is no locality common "
            "to all ranks in the communicator");

        RETURN_WITH_ERROR(return_code, OMPI_ERR_NOT_SUPPORTED, end);
    }

    if(hier_len == 0 || (common_locality & new_hier[hier_len - 1])
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

    if(return_code != OMPI_SUCCESS) {
        free(new_hier);
    }

    return return_code;
}
