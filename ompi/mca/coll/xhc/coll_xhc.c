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

#include "opal/class/opal_hash_table.h"
#include "opal/mca/rcache/rcache.h"
#include "opal/mca/shmem/base/base.h"
#include "opal/mca/smsc/smsc.h"

#include "opal/util/show_help.h"
#include "opal/util/minmax.h"
#include "opal/util/string_copy.h"

#include "coll_xhc.h"

// ------------------------------------------------

#define XHC_AUTOTUNE_MAX_N 4

// ------------------------------------------------

static int xhc_print_config_info(xhc_module_t *module, ompi_communicator_t *comm);
static int xhc_print_op_info(xhc_module_t *module, xhc_op_data_t *data);
static int xhc_print_op_hierarchy_dot(xhc_module_t *module, xhc_op_data_t *data);

// ------------------------------------------------

static int xhc_lazy_init(xhc_module_t *module) {
    ompi_communicator_t *comm = module->comm;

    int rank = module->rank;
    int n_ranks = module->n_ranks;

    xhc_peer_info_t *peer_info = NULL;

    int err, return_code = OMPI_SUCCESS;
    errno = 0;

    // ---

    peer_info = calloc(n_ranks, sizeof(xhc_peer_info_t));
    if(!peer_info) {RETURN_WITH_ERROR(return_code, OMPI_ERR_OUT_OF_RESOURCE, end);}

    for(int r = 0; r < n_ranks; r++) {
        peer_info[r].proc = ompi_comm_peer_lookup(comm, r);
        peer_info[r].locality = peer_info[r].proc->super.proc_flags;
    }

    peer_info[rank].locality |= ((1 << XHC_LOC_EXT_BITS) - 1) << XHC_LOC_EXT_START;

    module->peer_info = peer_info;

    // ---

    OBJ_CONSTRUCT(&module->hierarchy_cache, opal_hash_table_t);
    err = opal_hash_table_init(&module->hierarchy_cache, XHC_COLLCOUNT);
    if(OPAL_SUCCESS != err) {RETURN_WITH_ERROR(return_code, err, end);}

    // ---

    module->barrier_root = mca_coll_xhc_component.barrier_root;
    module->allreduce_root = mca_coll_xhc_component.allreduce_root;

    if(module->barrier_root >= (uint) n_ranks) {
        opal_show_help("help-coll-xhc.txt", "bad-internal-root", true,
            "Barrier", comm->c_name, ompi_comm_print_cid(comm),
            module->barrier_root, n_ranks, 0);

        module->barrier_root = 0;
    }

    if(module->allreduce_root >= (uint) n_ranks) {
        opal_show_help("help-coll-xhc.txt", "bad-internal-root", true,
            "Allreduce", comm->c_name, ompi_comm_print_cid(comm),
            module->allreduce_root, n_ranks, 0);

        module->allreduce_root = 0;
    }

    // ---

    module->init = true;

    if(0 == rank && (mca_coll_xhc_component.print_info & XHC_PRINT_INFO_CONFIG)) {
        err = xhc_print_config_info(module, comm);

        if(OMPI_SUCCESS != err) {
            opal_output_verbose(MCA_BASE_VERBOSE_WARN,
                ompi_coll_base_framework.framework_output,
                "coll:xhc: Warning: xhc_print_config_info() "
                "failed with error code %d", err);
        }
    }

    // ---

    end:

    if(OMPI_SUCCESS != return_code) {
        free(peer_info);

        opal_show_help("help-coll-xhc.txt", "xhc-init-failed", true,
            return_code, errno, strerror(errno));

        module->error = true;
    }

    return return_code;
}

static int xhc_alloc_cico_buffer(xhc_module_t *module,
    xhc_op_data_t *data, size_t size)
{
    xhc_shmem_ds_t shmem_ds;
    xhc_shmem_ds_t *ds_list = NULL;
    void *buffer = NULL;

    uint op_data_idx = (data - module->op_data[data->colltype]);

    int err, return_code = OMPI_SUCCESS;

    xhc_coll_fns_t xhc_fns;
    xhc_module_set_coll_fns(module->comm, &module->prev_colls, &xhc_fns);

    // --

    ds_list = malloc(module->n_ranks * sizeof(xhc_shmem_ds_t));
    data->cico = calloc(module->n_ranks, sizeof(*data->cico));

    if(!ds_list || !data->cico) {
        RETURN_WITH_ERROR(return_code, OMPI_ERR_OUT_OF_RESOURCE, end);
    }

    buffer = xhc_shmem_create(&shmem_ds, size, module->comm,
        "cico", (uint[4]) {data->colltype, op_data_idx});
    if(!buffer) {RETURN_WITH_ERROR(return_code, OMPI_ERROR, end);}

    /* Manually "touch" to assert allocation in local NUMA node
     * (assuming linux's default first-touch-alloc policy). */
    memset(buffer, 0, size);

    err = module->comm->c_coll->coll_allgather(&shmem_ds, sizeof(xhc_shmem_ds_t),
        MPI_BYTE, ds_list, sizeof(xhc_shmem_ds_t), MPI_BYTE, module->comm,
        module->comm->c_coll->coll_allgather_module);
    if(OMPI_SUCCESS != err) {RETURN_WITH_ERROR(return_code, err, end);}

    for(int r = 0; r < module->n_ranks; r++) {
        data->cico[r].shmem_ds = ds_list[r];
    }

    data->cico[module->rank].buffer = buffer;

    // --

    end:

    xhc_module_set_coll_fns(module->comm, &xhc_fns, NULL);

    free(ds_list);

    if(OMPI_SUCCESS != return_code) {
        if(buffer) {
            xhc_shmem_unlink(&shmem_ds);
            xhc_shmem_detach(&shmem_ds);
        }

        free(data->cico);
        data->cico = NULL;
    }

    return return_code;
}

static int xhc_init_data(xhc_op_data_t *data, xhc_module_t *module,
    XHC_COLLTYPE_T colltype, const char *hierarchy_string)
{

    int err, return_code = OMPI_SUCCESS;
    errno = 0;

    // ---

    data->module = module;
    data->config = &module->op_config[colltype];
    data->colltype = colltype;

    // MPI_Reduce implementation is 'multi-sliced'!
    data->n_slices = (XHC_REDUCE == colltype ? 2 : 1);

    if(data->config->cico_max > 0) {
        err = xhc_alloc_cico_buffer(module, data,
            data->n_slices * data->config->cico_max);
        if(OMPI_SUCCESS != err) {
            RETURN_WITH_ERROR(return_code, err, end);
        }
    }

    // ---

    /* Check if a hierarchy for this hierarchy string has already been
     * prepared, and use (borrow) it instead of re-creating it. */

    bool hierarchy_is_new = false;

    err = opal_hash_table_get_value_ptr(&module->hierarchy_cache,
        hierarchy_string, strlen(hierarchy_string),
        (void **) &data->hierarchy);

    if(OPAL_ERR_NOT_FOUND == err) {
        err = xhc_hierarchy_make(module, colltype,
            hierarchy_string, &data->hierarchy);
        if(OMPI_SUCCESS != err) {RETURN_WITH_ERROR(return_code, err, end);}

        hierarchy_is_new = true;
    } else if(OPAL_SUCCESS != err) {
        /* Unfortunately we can't allow this to fail, and it's not that we
         * have to re-create the hierarchy. It's that hierarchy_make() is
         * a collective operation; every rank would have to know if another
         * failed here. Same with set_value_ptr() below. */
        RETURN_WITH_ERROR(return_code, err, end);
    }

    /* Recall that if we have virtual hierarchy specifiers, the hierarchy
     * is not fully prepared until those specifiers are applied inside
     * xhc_comms_make(); only save the hierarchy in the cache after it. */

    err = xhc_comms_make(module, data);
    if(OMPI_SUCCESS != err) {RETURN_WITH_ERROR(return_code, err, end);}

    if(hierarchy_is_new) {
        err = opal_hash_table_set_value_ptr(&module->hierarchy_cache,
            hierarchy_string, strlen(hierarchy_string),
            (void *) data->hierarchy);

        if(OPAL_SUCCESS != err) {
            RETURN_WITH_ERROR(return_code, err, end);
        }
    }

    data->init = true;

    // ---

    if(mca_coll_xhc_component.print_info & (1 << colltype)) {
        err = xhc_print_op_info(module, data);

        if(OMPI_SUCCESS != err) {
            opal_output_verbose(MCA_BASE_VERBOSE_WARN,
                ompi_coll_base_framework.framework_output,
                "coll:xhc: Warning: xhc_print_op_info() "
                "failed with error code %d", err);
        }

        if(mca_coll_xhc_component.print_info & XHC_PRINT_INFO_HIER_DOT) {
            err = xhc_print_op_hierarchy_dot(module, data);

            if(OMPI_SUCCESS != err) {
                opal_output_verbose(MCA_BASE_VERBOSE_WARN,
                    ompi_coll_base_framework.framework_output,
                    "coll:xhc: Warning: xhc_print_op_hierarchy_dot() "
                    "failed with error code %d", err);
            }
        }
    }

    // ---

    end:

    if(OMPI_SUCCESS != return_code) {
        opal_show_help("help-coll-xhc.txt", "xhc-op-init-failed", true,
            xhc_colltype_to_str(colltype), return_code, errno, strerror(errno));
    }

    return return_code;
}

void mca_coll_xhc_fini(xhc_module_t *module) {
    if(module->peer_info) {
        for(int r = 0; r < module->n_ranks; r++) {
            if(module->peer_info[r].smsc_ep) {
                MCA_SMSC_CALL(return_endpoint,
                    module->peer_info[r].smsc_ep);
            }
        }

        free(module->peer_info);
    }

    xhc_hierarchy_t *ht_hierarchy;
    void *ht_key;

    OPAL_HASH_TABLE_FOREACH_PTR(ht_key,
        ht_hierarchy, &module->hierarchy_cache,
    {
        free(ht_hierarchy->levels);
        free(ht_hierarchy);
    });

    OBJ_DESTRUCT(&module->hierarchy_cache);

    for(int t = 0; t < XHC_COLLCOUNT; t++) {
        if(!module->op_data[t]) {
            continue;
        }

        int max_op_data = (module->op_config[t].hierarchy_autotune
            ? XHC_AUTOTUNE_MAX_N : 1);

        for(int i = 0; i < max_op_data; i++) {
            xhc_op_data_t *data = &module->op_data[t][i];

            if(data->init) {
                if(data->cico) {
                    for(int r = 0; r < module->n_ranks; r++) {
                        if(data->cico[r].buffer) {
                            /* if(r == module->rank) // OMPI #11123
                                xhc_shmem_unlink(&data->cico[r].shmem_ds); */

                            xhc_shmem_detach(&data->cico[r].shmem_ds);
                        }
                    }
                }

                free(data->cico);
                xhc_comms_fini(data);
            }
        }

        free(module->op_data[t]);
    }

    free(module->rbuf);
}

// ------------------------------------------------

/* Tune the bcast chunk size, so that it's:
 * - not so large that we don't have any pipelining with small messages.
 * - not so small that we lose out on performance with large messages. */
static void xhc_autotune_chunk(xhc_op_data_t *data, size_t size) {
    if(XHC_BCAST != data->colltype) {
        return;
    }

    int n_chunks = 4;
    size_t chunk_min = 4096;
    size_t chunk_max = 65536;

    size_t chunk_size = size/n_chunks;

    chunk_size = opal_max(chunk_size, chunk_min);
    chunk_size = opal_min(chunk_size, chunk_max);

    for(int i = 0; i < data->n_comms; i++) {
        data->comms[i].chunk_size = chunk_size;
    }
}

// Remember: Must always return < XHC_AUTOTUNE_MAX_N
static uint xhc_autotune_hierarchy(xhc_module_t *module,
    XHC_COLLTYPE_T colltype, size_t size, const char **hierarchy)
{
    /* Obviously this isn't the most robust check ever. But
     * assuming no weird systems, it's likely good enough. */
    bool hierarchical_caches = (module->n_cores_l3 >= 4
        && module->n_cores_numa / module->n_cores_l3 >= 2);

    /* Again not the most robust, but likely good enough to 'catch'
     * systems like Intel's, where one L3 spans the whole NUMA/socket.*/
    bool wide_shared_l3 = (module->n_cores_l3 > 0 &&
        module->n_cores_l3 >= module->n_cores_numa);

    size_t l3_per_core = (module->n_cores_l3 > 0 ?
        module->l3_cache_size / module->n_cores_l3 : 0);

    int numa_per_socket = (module->n_cores_numa > 0 ?
        module->n_cores_socket / module->n_cores_numa : 0);

    size_t cico_max = module->op_config[colltype].cico_max;
    size_t chunk_max = module->op_config[colltype].chunk_max;

    uint idx;

    switch(colltype) {
        /* In systems where we have shared caches, a flat broadcast may
         * actually be implicitly hierarchical. On systems with large LLCs,
         * like Intel's Xeon Scalables, this is true up to a larger message
         * size, whereas on systems with more & smaller L3s, like AMD's
         * Epycs, it's only the case for small messages. If we didn't find a
         * shared L3, don't assume anything and do a hierarchical broadcast.
         *
         * One notable architecture where a hierarchical broadcast is always
         * better is the Neoverse N1. Even though it does/can have a shared
         * SLC, the CMN-600's HN-Fs can't cache remotely homed cache lines.
         * Incidentally, hwloc does report the SLC at all on Ampere Altra;
         * is this intended or accidental? Might have to implement a more
         * targeted check specifically for N1/CMN-600. */
        case XHC_BCAST:
            if(hierarchical_caches && size <= cico_max) {
                *hierarchy = "flat";
                idx = 0;
            } else if(wide_shared_l3 && size <= 1048576) {
                *hierarchy = "flat";
                idx = 0;
            } else {
                *hierarchy = "numa,socket";
                idx = 1;
            }

            break;

        /* We might ideally want hierarchical gather with flat release.
         * Till then, go for flat barrier with hierarchical caches. */
        case XHC_BARRIER:
            if(hierarchical_caches) {
                *hierarchy = "flat";
                idx = 0;
            } else {
                *hierarchy = "numa,socket";
                idx = 1;
            }

            break;

        /* We also want to exploit L3 locality if there are multiple L3
         * caches per NUMA. But only up to a point. Still under investigation
         * why, but there is a correlation with cache size; make a best effort
         * to find the hotstop (using only L3 size, ideally should also consider
         * L1+L2 and op type / buffer usage). For non-hierarchical caches,
         * segment the NUMA so more will do work. And for messages large
         * enough for the whole NUMA to do work, no longer necessary. */

        /* Virtual hierarchy crash course:
         *   - topo%n splits topo to groups of approx n members
         *   - topo:n splits topo to n groups
         * Full documentation in mca_coll_xhc_component_parse_hierarchy(). */

        case XHC_REDUCE:
        case XHC_ALLREDUCE:
            if(hierarchical_caches) {
                if(size < l3_per_core) {
                    *hierarchy = "l3,numa,socket";
                    idx = 0;
                } else {
                    *hierarchy = "numa,socket";
                    idx = 1;
                }
            } else if(numa_per_socket >= 2) {
                if(size < chunk_max * module->n_cores_numa) {
                    *hierarchy = "numa%4,numa,socket";
                    idx = 0;
                } else {
                    *hierarchy = "numa,socket";
                    idx = 1;
                }
            } else {
                if(size < chunk_max * (module->n_cores_socket/4)) {
                    *hierarchy = "socket%4,socket:4,socket";
                    idx = 0;
                } else {
                    *hierarchy = "socket:4,socket";
                    idx = 1;
                }
            }

            break;

        default:
            assert(0);
            __builtin_unreachable();
    }

    return idx;
}

xhc_op_data_t *mca_coll_xhc_get_op_data(xhc_module_t *module,
    XHC_COLLTYPE_T colltype, size_t size)
{
    xhc_op_data_t *data;
    int err;

    bool h_auto = module->op_config[colltype].hierarchy_autotune;
    bool c_auto = module->op_config[colltype].chunk_autotune;

    // ---

    if(!module->init && !module->error) {
        err = xhc_lazy_init(module);
        if(OMPI_SUCCESS != err) {return NULL;}
    }

    if(!module->op_data[colltype]) {
        module->op_data[colltype] = calloc((h_auto ?
            XHC_AUTOTUNE_MAX_N : 1), sizeof(xhc_op_data_t));
        if(!module->op_data[colltype]) {return NULL;}
    }

    // ---

    uint data_idx = 0;
    const char *tuned_hierarchy;

    if(h_auto) {
        data_idx = xhc_autotune_hierarchy(module, colltype, size, &tuned_hierarchy);
        assert(data_idx < XHC_AUTOTUNE_MAX_N);
    }

    data = &module->op_data[colltype][data_idx];
    if(!data->init) {
        err = xhc_init_data(data, module, colltype, (h_auto ? tuned_hierarchy
            : module->op_config[colltype].hierarchy_string));
        if(OMPI_SUCCESS != err) {return NULL;}
    }

    // ---

    if(c_auto) {
        xhc_autotune_chunk(data, size);
    }

    // ---

    return data;
}

// ------------------------------------------------

int mca_coll_xhc_read_op_config(xhc_module_t *module, XHC_COLLTYPE_T colltype) {
    ompi_communicator_t *comm = module->comm;

    /* Assimilate the tuning parameters here, based on the various MCA params
     * and any provided info keys. We want to determine the hierarchy, chunk
     * size, and cico threshold, for each supported primitive. The info keys
     * take precedence over the MCA params, with op-specific info keys having
     * higher priority than global ones. Note that the op-specific and global
     * MCA params are mutually exclusive; this is implemented in xhc_register().
     */

    xhc_op_config_t *config = &module->op_config[colltype];

    const char *param[] = {"hierarchy", "chunk_size", "cico_max"};
    for(size_t p = 0; p < sizeof(param)/sizeof(param[0]); p++) {
        xhc_op_mca_t op_mca = mca_coll_xhc_component.op_mca[colltype];
        xhc_config_source_t config_source;

        opal_cstring_t *info_val;
        int info_flag = 0;

        int err;

        if(comm->super.s_info) {
            char *key;

            // Op-specific info key
            err = opal_asprintf(&key, "ompi_comm_coll_xhc_%s_%s",
                xhc_colltype_to_str(colltype), param[p]);
            if(err < 0) {return OMPI_ERR_OUT_OF_RESOURCE;}

            opal_info_get(comm->super.s_info,
                key, &info_val, &info_flag);
            free(key);

            if(info_flag) {
                config_source = XHC_CONFIG_SOURCE_INFO_OP;
            } else {
                // Non-specific info key
                err = opal_asprintf(&key, "ompi_comm_coll_xhc_%s",
                    param[p]);
                if(err < 0) {return OMPI_ERR_OUT_OF_RESOURCE;}

                opal_info_get(comm->super.s_info, key,
                    &info_val, &info_flag);
                free(key);

                if(info_flag) {
                    config_source = XHC_CONFIG_SOURCE_INFO_GLOBAL;
                }
            }
        }

        if(!info_flag) {
            const mca_base_var_t *var_global;

            err = mca_base_var_get(mca_base_var_find("ompi",
                "coll", "xhc", param[p]), &var_global);
            if(OPAL_SUCCESS != err) {return err;}

            config_source = (MCA_BASE_VAR_SOURCE_DEFAULT != var_global->mbv_source
                ? XHC_CONFIG_SOURCE_MCA_GLOBAL : XHC_CONFIG_SOURCE_MCA_OP);
        }

        char *err_param_name = xhc_op_config_source_param(
            param[p], colltype, config_source);

        switch(p) {
            case 0:
                config->hierarchy_string = strdup(info_flag ?
                    info_val->string : op_mca.hierarchy);

                if(info_flag) {
                    OBJ_RELEASE(info_val);
                }

                if(!config->hierarchy_string) {
                    return OMPI_ERR_OUT_OF_RESOURCE;
                }

                config->hierarchy_source = config_source;

                if(0 == strcmp(config->hierarchy_string, "auto")) {
                    config->hierarchy_autotune = true;
                }

                break;

            case 1: {
                const char *chunk_string = (info_flag ?
                    info_val->string : op_mca.chunk_size);

                if(XHC_BCAST == colltype && 0 == strcmp(chunk_string, "auto")) {
                    config->chunk_autotune = true;
                    err = OMPI_SUCCESS;
                } else {
                    err = xhc_component_parse_chunk_size(chunk_string,
                        &config->chunk_min, &config->chunk_max, err_param_name);
                    free(err_param_name);
                }

                if(info_flag) {
                    OBJ_RELEASE(info_val);
                }

                if(OMPI_SUCCESS != err) {
                    return err;
                }

                config->chunk_source = config_source;

                break;
            }

            case 2:
                if(info_flag) {
                    err = xhc_component_parse_size(info_val->string,
                        &config->cico_max, "bad-cico-max", err_param_name);

                    free(err_param_name);
                    OBJ_RELEASE(info_val);

                    if(OMPI_SUCCESS != err) {
                        return err;
                    }
                } else {
                    config->cico_max = op_mca.cico_max;
                }

                config->cico_max_source = config_source;
        }
    }

    // ---

    /* Enforce a resonable minimum chunk size,
     * and make sure min is not larger than max. */
    if(XHC_BARRIER != colltype) {
        if(config->chunk_min > config->chunk_max) {
            size_t tmp = config->chunk_min;
            config->chunk_min = config->chunk_max;
            config->chunk_max = tmp;
        }

        if(config->chunk_min < XHC_MIN_CHUNK_SIZE) {
            config->chunk_min = XHC_MIN_CHUNK_SIZE;
            config->chunk_max = opal_max(config->chunk_max, config->chunk_min);
        }
    }

    // ---

    return OMPI_SUCCESS;
}

char *mca_coll_xhc_op_config_source_param(const char *var_name,
    XHC_COLLTYPE_T colltype, xhc_config_source_t source)
{
    char *name;
    int err = -1;

    switch(source) {
        case XHC_CONFIG_SOURCE_INFO_OP:
            err = opal_asprintf(&name, "ompi_comm_coll_xhc_%s_%s",
                xhc_colltype_to_str(colltype), var_name);
            break;
        case XHC_CONFIG_SOURCE_INFO_GLOBAL:
            err = opal_asprintf(&name, "ompi_comm_coll_xhc_%s", var_name);
            break;
        case XHC_CONFIG_SOURCE_MCA_OP:
            err = opal_asprintf(&name, "coll_xhc_%s_%s",
                xhc_colltype_to_str(colltype), var_name);
            break;
        case XHC_CONFIG_SOURCE_MCA_GLOBAL:
            err = opal_asprintf(&name, "coll_xhc_%s", var_name);
            break;
        default:
            assert(0);
            __builtin_unreachable();
    }

    return (err >= 0 ? name : NULL);
}

// ------------------------------------------------

static int xhc_print_config_info(xhc_module_t *module, ompi_communicator_t *comm) {
    char *drval_str, *lb_policy_str;

    switch(mca_coll_xhc_component.dynamic_reduce) {
        case XHC_DYNAMIC_REDUCE_DISABLED:
            drval_str = "OFF"; break;
        case XHC_DYNAMIC_REDUCE_NON_FLOAT:
            drval_str = "ON (non-float)"; break;
        case XHC_DYNAMIC_REDUCE_ALL:
            drval_str = "ON (all)"; break;
        default:
            drval_str = "???";
    }

    switch((int) mca_coll_xhc_component.reduction_load_balance) {
        case 0:
            lb_policy_str = "none"; break;
        case XHC_REDUCTION_LB_LEADER_ASSIST_TOP_LEVEL:
            lb_policy_str = "top level"; break;
        case XHC_REDUCTION_LB_LEADER_ASSIST_FIRST_CHUNK:
            lb_policy_str = "first chunk"; break;
        case XHC_REDUCTION_LB_LEADER_ASSIST_TOP_LEVEL
                | XHC_REDUCTION_LB_LEADER_ASSIST_FIRST_CHUNK:
            lb_policy_str = "top level + first chunk"; break;
        case XHC_REDUCTION_LB_LEADER_ASSIST_ALL:
            lb_policy_str = "all"; break;
        default:
            lb_policy_str = "???";
    }

    printf("------------------------------------------------\n"
        "OMPI coll/xhc @ %s, priority %d\n"
        "  dynamic leader '%s', dynamic reduce '%s'\n"
        "  reduce load balance '%s'\n",
        comm->c_name, mca_coll_xhc_component.priority,
        (mca_coll_xhc_component.dynamic_leader ? "ON" : "OFF"),
        drval_str, lb_policy_str);

    for(int t = 0; t < XHC_COLLCOUNT; t++) {
        xhc_op_config_t *config = &module->op_config[t];

        if(XHC_BARRIER == t) {
            printf("\n"
                "  [%s]\n"
                "    Hierarchy: %s (source: %s)\n",
                xhc_colltype_to_str(t), config->hierarchy_string,
                xhc_config_source_to_str(config->hierarchy_source));
        } else {
            printf("\n"
                "  [%s]\n"
                "    Hierarchy: %s (source: %s)\n"
                "    Chunk size(s): %zu:%zu (source: %s)\n"
                "    CICO: Up to %zu bytes (source: %s)\n",
                xhc_colltype_to_str(t), config->hierarchy_string,
                xhc_config_source_to_str(config->hierarchy_source),
                config->chunk_min, config->chunk_max,
                xhc_config_source_to_str(config->chunk_source),
                config->cico_max,
                xhc_config_source_to_str(config->cico_max_source));
        }
    }

    printf("------------------------------------------------\n");

    return OMPI_SUCCESS;
}

static int xhc_print_op_info(xhc_module_t *module, xhc_op_data_t *data) {
    int data_idx = (data - module->op_data[data->colltype]);

    xhc_comm_t *comms = data->comms;
    int n_comms = data->n_comms;

    int rank = module->rank;

    for(int i = 0; i < n_comms; i++) {
        char *memb_list = NULL, *tmp;
        int err;

        for(int m = 0; m < comms[i].size; m++) {
            err = opal_asprintf(&tmp, "%s%s%d", (m > 0 ? memb_list : ""),
                (m > 0 ? " " : ""), comms[i].rank_list[m]);

            free(memb_list);
            memb_list = tmp;

            if(err < 0) {
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
        }

        if(XHC_BARRIER == data->colltype) {
            printf("XHC_COMM ompi_comm=%s rank=%d op=%s tune_idx=%d loc=0x%08x "
                "members=%d [%s]\n", module->comm->c_name, rank,
                xhc_colltype_to_str(data->colltype), data_idx,
                comms[i].locality, comms[i].size, memb_list);
        } else if(XHC_BCAST == data->colltype) {
            char *chunk_size_str = NULL;

            if(!data->config->chunk_autotune) {
                err = opal_asprintf(&chunk_size_str, "%zu", comms[i].chunk_size);
                if(err < 0) {
                    free(memb_list);
                    return OMPI_ERR_OUT_OF_RESOURCE;
                }
            }

            printf("XHC_COMM ompi_comm=%s rank=%d op=%s tune_idx=%d loc=0x%08x "
                "chunk_size=%s cico_size=%zu members=%d [%s]\n",
                module->comm->c_name, rank, xhc_colltype_to_str(data->colltype),
                data_idx, comms[i].locality,
                (data->config->chunk_autotune ? "auto" : chunk_size_str),
                data->config->cico_max, comms[i].size, memb_list);

            free(chunk_size_str);
        } else {
            printf("XHC_COMM ompi_comm=%s rank=%d op=%s tune_idx=%d loc=0x%08x "
                "chunk_size=%zu:%zu cico_size=%zu members=%d [%s]\n",
                module->comm->c_name, rank, xhc_colltype_to_str(data->colltype),
                data_idx, comms[i].locality, data->config->chunk_min,
                data->config->chunk_max, data->config->cico_max,
                comms[i].size, memb_list);
        }

        free(memb_list);
    }

    return OMPI_SUCCESS;
}

static int xhc_print_op_hierarchy_dot(xhc_module_t *module, xhc_op_data_t *data) {
    if(0 != module->rank) {
        return OMPI_SUCCESS;
    }

    int data_idx = (data - module->op_data[data->colltype]);

    FILE *outfile = stdout;
    char *dir;

    switch(data->colltype) {
        case XHC_BCAST:
            dir = "forward"; break;
        case XHC_REDUCE: case XHC_ALLREDUCE:
            dir = "back"; break;
        case XHC_BARRIER:
            dir = "both"; break;
        default:
            dir = "none";
    }

    fprintf(outfile, "digraph xhc_%s_%d_hierarchy {\n",
        xhc_colltype_to_str(data->colltype), data_idx);

    for(int r = 1; r < module->n_ranks; r++) {
        fprintf(outfile, "\t%d -> %d [dir=%s];\n",
            data->parents[r], r, dir);
    }

    fprintf(outfile, "}\n");

    return OMPI_SUCCESS;
}

// ------------------------------------------------

static int xhc_shmem_name(char *buffer, size_t length,
        const xhc_shmem_ds_t *xhc_ds) {

    int ret = snprintf(buffer, length,
        "%s" OPAL_PATH_SEP "xhc_shmem_seg.%u.%u:%u:%u.%s:%u:%u:%u:%u",
        mca_coll_xhc_component.shmem_backing, xhc_ds->uid,
        OPAL_PROC_MY_NAME.jobid, xhc_ds->vpid, xhc_ds->cid,
        xhc_ds->id, xhc_ds->id_ext[0], xhc_ds->id_ext[1],
        xhc_ds->id_ext[2], xhc_ds->id_ext[3]);

    assert(ret < (int) length);

    if(ret < 0) {
        opal_output_verbose(MCA_BASE_VERBOSE_ERROR,
            ompi_coll_base_framework.framework_output,
            "coll:xhc: Error in xhc_shmem_name()");
    }

    return (ret >= 0 ? OMPI_SUCCESS : OMPI_ERROR);
}

void *mca_coll_xhc_shmem_create(xhc_shmem_ds_t *xhc_ds_dst, size_t size,
        ompi_communicator_t *comm, const char *id, uint id_ext[4]) {

    char shmem_file[sizeof(((opal_shmem_ds_t *) 0)->seg_name)];
    opal_shmem_ds_t opal_ds;
    int err;

    xhc_shmem_ds_t xhc_ds = {
        .opal_ds_short = {0},
        .uid = geteuid(),
        .vpid = OPAL_PROC_MY_NAME.vpid,
        .cid = ompi_comm_get_local_cid(comm),
    };

    assert(strlen(id) <= sizeof(xhc_ds.id) - 1);

    opal_string_copy(xhc_ds.id, id, sizeof(xhc_ds.id));
    memcpy(xhc_ds.id_ext, id_ext, 4 * sizeof(uint));

    // ---

    err = xhc_shmem_name(shmem_file, sizeof(shmem_file), &xhc_ds);
    if(OMPI_SUCCESS != err) {return NULL;}

    // Ensure the backing file will be cleaned up (copied from btl/sm)
    opal_pmix_register_cleanup(shmem_file, false, false, false);

    err = opal_shmem_segment_create(&opal_ds, shmem_file, size);

    if(OPAL_SUCCESS != err) {
        opal_output_verbose(MCA_BASE_VERBOSE_ERROR,
            ompi_coll_base_framework.framework_output,
            "coll:xhc: Error: Could not create shared memory segment");

        return NULL;
    }

    memcpy(&xhc_ds.opal_ds_short, &opal_ds,
        sizeof(xhc_ds.opal_ds_short));

    // ---

    void *addr = xhc_shmem_attach(&xhc_ds);

    if(addr) {
        *xhc_ds_dst = xhc_ds;
    } else {
        opal_shmem_unlink(&opal_ds);
    }

    return addr;
}

void *mca_coll_xhc_shmem_attach(xhc_shmem_ds_t *xhc_ds) {
    opal_shmem_ds_t opal_ds;
    int err;

    memcpy(&opal_ds, &xhc_ds->opal_ds_short, sizeof(xhc_ds->opal_ds_short));
    err = xhc_shmem_name(opal_ds.seg_name, sizeof(opal_ds.seg_name), xhc_ds);
    if(OMPI_SUCCESS != err) {return NULL;}

    void *addr = opal_shmem_segment_attach(&opal_ds);

    memcpy(&xhc_ds->opal_ds_short, &opal_ds, sizeof(xhc_ds->opal_ds_short));

    if(NULL == addr) {
        opal_output_verbose(MCA_BASE_VERBOSE_ERROR,
            ompi_coll_base_framework.framework_output,
            "coll:xhc: Error: Could not attach to shared memory segment");
    }

    return addr;
}

int mca_coll_xhc_shmem_detach(xhc_shmem_ds_t *xhc_ds) {
    opal_shmem_ds_t opal_ds;
    int err, ret;

    memcpy(&opal_ds, &xhc_ds->opal_ds_short, sizeof(xhc_ds->opal_ds_short));
    err = xhc_shmem_name(opal_ds.seg_name, sizeof(opal_ds.seg_name), xhc_ds);
    if(OMPI_SUCCESS != err) {return err;}

    ret = opal_shmem_segment_detach(&opal_ds);

    memcpy(&xhc_ds->opal_ds_short, &opal_ds, sizeof(xhc_ds->opal_ds_short));

    return ret;
}

int mca_coll_xhc_shmem_unlink(xhc_shmem_ds_t *xhc_ds) {
    opal_shmem_ds_t opal_ds;

    memcpy(&opal_ds, &xhc_ds->opal_ds_short, sizeof(xhc_ds->opal_ds_short));
    xhc_shmem_name(opal_ds.seg_name, sizeof(opal_ds.seg_name), xhc_ds);

    int ret = opal_shmem_unlink(&opal_ds);

    memcpy(&xhc_ds->opal_ds_short, &opal_ds, sizeof(xhc_ds->opal_ds_short));

    return ret;
}

// ------------------------------------------------

void *mca_coll_xhc_get_cico(xhc_op_data_t *data, int rank, int slice_id) {
    assert(slice_id < data->n_slices);

    if(!data->cico[rank].buffer) {
        data->cico[rank].buffer = xhc_shmem_attach(&data->cico[rank].shmem_ds);
        assert(data->cico[rank].buffer);
    }

    return (char *) data->cico[rank].buffer + slice_id * data->config->cico_max;
}

static mca_smsc_endpoint_t *xhc_smsc_ep(xhc_op_data_t *data, int rank) {
    xhc_peer_info_t *peer_info = data->module->peer_info;

    if(!peer_info[rank].smsc_ep) {
        peer_info[rank].smsc_ep = MCA_SMSC_CALL(get_endpoint,
            &peer_info[rank].proc->super);

        if(!peer_info[rank].smsc_ep) {
            opal_output_verbose(MCA_BASE_VERBOSE_ERROR,
                ompi_coll_base_framework.framework_output,
                "coll:xhc: Error: Failed to initialize smsc endpoint");

            return NULL;
        }
    }

    return peer_info[rank].smsc_ep;
}

int mca_coll_xhc_copy_expose_region(void *base,
        size_t len, xhc_copy_data_t **region_data) {

    if(mca_smsc_base_has_feature(MCA_SMSC_FEATURE_REQUIRE_REGISTRATION)) {
        void *data = MCA_SMSC_CALL(register_region, base, len);

        if(NULL == data) {
            opal_output_verbose(MCA_BASE_VERBOSE_ERROR,
                ompi_coll_base_framework.framework_output,
                "coll:xhc: Error: Failed to register memory region with smsc");

            return OMPI_ERROR;
        }

        *region_data = data;
    }

    return OMPI_SUCCESS;
}

void mca_coll_xhc_copy_region_post(void *dst,
        xhc_copy_data_t *region_data, size_t length) {

    memcpy(dst, region_data, length);
}

int mca_coll_xhc_copy_from(xhc_op_data_t *data, int rank,
        void *dst, void *src, size_t size, void *access_token) {

    mca_smsc_endpoint_t *smsc_ep = xhc_smsc_ep(data, rank);

    if(NULL == smsc_ep) {
        return -1;
    }

    int status = MCA_SMSC_CALL(copy_from, smsc_ep,
        dst, src, size, access_token);

    return (OPAL_SUCCESS == status ? 0 : -1);
}

void mca_coll_xhc_copy_close_region(xhc_copy_data_t *region_data) {
    if(mca_smsc_base_has_feature(MCA_SMSC_FEATURE_REQUIRE_REGISTRATION)) {
        MCA_SMSC_CALL(deregister_region, region_data);
    }
}

void *mca_coll_xhc_get_registration(xhc_op_data_t *data, int rank,
        void *peer_vaddr, size_t size, xhc_reg_t **reg) {

    mca_smsc_endpoint_t *smsc_ep = xhc_smsc_ep(data, rank);

    if(NULL == smsc_ep) {
        return NULL;
    }

    void *local_ptr;

    *reg = MCA_SMSC_CALL(map_peer_region, smsc_ep,
        MCA_RCACHE_FLAGS_PERSIST, peer_vaddr, size, &local_ptr);

    if(NULL == *reg) {
        return NULL;
    }

    return local_ptr;
}

void mca_coll_xhc_return_registration(xhc_reg_t *reg) {
    /* Won't actually unmap/detach, since we've set the
     * MCA_RCACHE_FLAGS_PERSIST flag to map_peer_region */
    MCA_SMSC_CALL(unmap_peer_region, reg);
}
