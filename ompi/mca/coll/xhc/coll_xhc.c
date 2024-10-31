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

#include "opal/class/opal_hash_table.h"
#include "opal/mca/rcache/rcache.h"
#include "opal/mca/shmem/base/base.h"
#include "opal/mca/smsc/smsc.h"

#include "opal/util/show_help.h"
#include "opal/util/minmax.h"

#include "coll_xhc.h"

// ------------------------------------------------

static int xhc_alloc_bcast_cico(xhc_module_t *module,
    ompi_communicator_t *comm);

static int xhc_print_config_info(xhc_module_t *module,
    ompi_communicator_t *comm);
static int xhc_print_op_info(xhc_module_t *module,
    ompi_communicator_t *comm, XHC_COLLTYPE_T colltype);
static int xhc_print_op_hierarchy_dot(xhc_module_t *module,
    ompi_communicator_t *comm, XHC_COLLTYPE_T colltype);

static mca_smsc_endpoint_t *xhc_smsc_ep(xhc_peer_info_t *peer_info);

// ------------------------------------------------

int mca_coll_xhc_lazy_init(xhc_module_t *module, ompi_communicator_t *comm) {
    xhc_peer_info_t *peer_info = NULL;

    int comm_size = ompi_comm_size(comm);
    int rank = ompi_comm_rank(comm);

    int err, return_code = OMPI_SUCCESS;

    errno = 0;

    if(module->init) {
        return OMPI_SUCCESS;
    } else if(module->error) {
        return OMPI_ERROR;
    }

    // ---

    peer_info = calloc(comm_size, sizeof(xhc_peer_info_t));
    if(!peer_info) {RETURN_WITH_ERROR(return_code, OMPI_ERR_OUT_OF_RESOURCE, end);}

    for(int r = 0; r < comm_size; r++) {
        peer_info[r].proc = ompi_comm_peer_lookup(comm, r);
        peer_info[r].locality = peer_info[r].proc->super.proc_flags;
    }

    peer_info[rank].locality |= ((1 << XHC_LOC_EXT_BITS) - 1) << XHC_LOC_EXT_START;

    // ---

    OBJ_CONSTRUCT(&module->hierarchy_cache, opal_hash_table_t);
    err = opal_hash_table_init(&module->hierarchy_cache, XHC_COLLCOUNT);
    if(OPAL_SUCCESS != err) {RETURN_WITH_ERROR(return_code, err, end);}

    // ---

    module->comm_size = comm_size;
    module->rank = rank;

    module->peer_info = peer_info;
    module->init = true;

    if((mca_coll_xhc_component.print_info & XHC_PRINT_INFO_CONFIG) && 0 == rank) {
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

int mca_coll_xhc_init_op(xhc_module_t *module,
        ompi_communicator_t *comm, XHC_COLLTYPE_T colltype) {

    int err, return_code = OMPI_SUCCESS;

    if(!module->init && !module->error) {
        err = xhc_lazy_init(module, comm);
        if(OMPI_SUCCESS != err) {return err;}
    }

    if(module->op_data[colltype].init) {
        return OMPI_SUCCESS;
    }

    // ---

    xhc_op_config_t *config = &module->op_config[colltype];
    xhc_op_data_t *data = &module->op_data[colltype];

    data->colltype = colltype;
    data->seq = 0;

    if(XHC_BCAST == colltype) {
        err = xhc_alloc_bcast_cico(module, comm);
        if(OMPI_SUCCESS != err) {RETURN_WITH_ERROR(return_code, err, end);}
    }

    // ---

    /* Check if a hierarchy for this hierarchy string has already been
     * prepared, and use (borrow) it instead of re-creating it. */

    xhc_op_config_t *lender;

    err = opal_hash_table_get_value_ptr(&module->hierarchy_cache,
        config->hierarchy_string, strlen(config->hierarchy_string),
        (void **) &lender);

    if(OPAL_SUCCESS == err) {
        config->hierarchy = lender->hierarchy;
        config->hierarchy_len = lender->hierarchy_len;
    } else {
        if(OPAL_ERR_NOT_FOUND != err) {
            opal_output_verbose(MCA_BASE_VERBOSE_WARN,
                ompi_coll_base_framework.framework_output,
                "coll:xhc: Warning: opal_hash_table_get_value_ptr() "
                "returned %d", err);
        }

        err = xhc_hierarchy_make(module, comm, config->hierarchy_string,
            &config->hierarchy, &config->hierarchy_len);
        if(OMPI_SUCCESS != err) {RETURN_WITH_ERROR(return_code, err, end);}

        err = opal_hash_table_set_value_ptr(&module->hierarchy_cache,
            config->hierarchy_string, strlen(config->hierarchy_string),
            (void *) config);

        if(OPAL_SUCCESS != err) {
            opal_output_verbose(MCA_BASE_VERBOSE_WARN,
                ompi_coll_base_framework.framework_output,
                "coll:xhc: Warning: opal_hash_table_set_value_ptr() "
                "returned %d", err);
        }
    }

    // ---

    err = xhc_comms_make(comm, module, config, data);
    if(OMPI_SUCCESS != err) {RETURN_WITH_ERROR(return_code, err, end);}

    data->init = true;

    if(mca_coll_xhc_component.print_info & (1 << colltype)) {
        err = xhc_print_op_info(module, comm, colltype);

        if(OMPI_SUCCESS != err) {
            opal_output_verbose(MCA_BASE_VERBOSE_WARN,
                ompi_coll_base_framework.framework_output,
                "coll:xhc: Warning: xhc_print_op_info() "
                "failed with error code %d", err);
        }

        if(mca_coll_xhc_component.print_info & XHC_PRINT_INFO_HIER_DOT) {
            err = xhc_print_op_hierarchy_dot(module, comm, colltype);

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

static int xhc_alloc_bcast_cico(xhc_module_t *module, ompi_communicator_t *comm) {
    opal_shmem_ds_t *ds_list = NULL;
    opal_shmem_ds_t cico_ds;
    void *cico_buffer = NULL;
    xhc_coll_fns_t xhc_fns;

    int err, return_code = OMPI_SUCCESS;

    int comm_size = ompi_comm_size(comm);
    int rank = ompi_comm_rank(comm);

    size_t cico_size = module->op_config[XHC_BCAST].cico_max;

    if(0 == cico_size) {
        return OMPI_SUCCESS;
    }

    xhc_module_set_coll_fns(comm, &module->prev_colls, &xhc_fns);

    // --

    ds_list = malloc(comm_size * sizeof(opal_shmem_ds_t));
    if(!ds_list) {return OMPI_ERR_OUT_OF_RESOURCE;}

    cico_buffer = xhc_shmem_create(&cico_ds, cico_size, comm, "cico", 0, 0);
    if(!cico_buffer) {RETURN_WITH_ERROR(return_code, OMPI_ERR_OUT_OF_RESOURCE, end);}

    /* Manually "touch" to assert allocation in local NUMA node
     * (assuming linux's default first-touch-alloc policy). */
    memset(cico_buffer, 0, cico_size);

    err = comm->c_coll->coll_allgather(&cico_ds, sizeof(opal_shmem_ds_t),
        MPI_BYTE, ds_list, sizeof(opal_shmem_ds_t), MPI_BYTE, comm,
        comm->c_coll->coll_allgather_module);
    if(OMPI_SUCCESS != err) {RETURN_WITH_ERROR(return_code, err, end);}

    module->peer_info[rank].cico_ds = cico_ds;
    module->peer_info[rank].cico_buffer = cico_buffer;

    for(int r = 0; r < comm_size; r++) {
        if(r == rank) {continue;}
        module->peer_info[r].cico_ds = ds_list[r];
    }

    // --

    end:

    free(ds_list);
    xhc_module_set_coll_fns(comm, &xhc_fns, NULL);

    if(OMPI_SUCCESS != return_code) {
        if(cico_buffer) {
            opal_shmem_unlink(&cico_ds);
            opal_shmem_segment_detach(&cico_ds);
        }
    }

    return return_code;
}

void mca_coll_xhc_fini(mca_coll_xhc_module_t *module) {
    if(module->peer_info) {
        for(int r = 0; r < module->comm_size; r++) {
            if(module->peer_info[r].smsc_ep) {
                MCA_SMSC_CALL(return_endpoint, module->peer_info[r].smsc_ep);
            }

            if(module->peer_info[r].cico_buffer) {
                // OMPI #11123
                /* if(r == module->rank) {
                    opal_shmem_unlink(&module->peer_info[r].cico_ds);
                } */

                opal_shmem_segment_detach(&module->peer_info[r].cico_ds);
            }
        }

        free(module->peer_info);
    }

    xhc_op_config_t *ht_config_value;
    void *ht_key;

    OPAL_HASH_TABLE_FOREACH_PTR(ht_key, ht_config_value,
            &module->hierarchy_cache, {
        free(ht_config_value->hierarchy);
    });

    OBJ_DESTRUCT(&module->hierarchy_cache);

    for(int t = 0; t < XHC_COLLCOUNT; t++) {
        if(module->op_data[t].init) {
            xhc_comms_destroy(module->op_data[t].comms,
                module->op_data[t].comm_count);

            free(module->op_data[t].comms);
        }
    }

    free(module->rbuf);
}

// ------------------------------------------------

int mca_coll_xhc_read_op_config(xhc_module_t *module,
        ompi_communicator_t *comm, XHC_COLLTYPE_T colltype) {

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

                break;
            case 1:
                /* We don't *need* to keep record of the chunk string, since
                 * we parse it here. But it's good to have; we currently use
                 * it when showing information (print_info). */
                config->chunk_string = strdup(info_flag ?
                    info_val->string : op_mca.chunk_size);

                if(info_flag) {
                    OBJ_RELEASE(info_val);
                }

                if(!config->chunk_string) {
                    return OMPI_ERR_OUT_OF_RESOURCE;
                }

                err = xhc_component_parse_chunk_sizes(config->chunk_string,
                    &config->chunks, &config->chunks_len);

                if(OMPI_SUCCESS != err) {
                    return err;
                }

                config->chunk_source = config_source;

                break;
            case 2:
                if(info_flag) {
                    err = xhc_component_parse_cico_max(
                        info_val->string, &config->cico_max);
                    OBJ_RELEASE(info_val);

                    if(OMPI_SUCCESS != err) {
                        return err;
                    }
                } else {
                    config->cico_max = op_mca.cico_max;
                }

                config->cico_max_source = config_source;

                break;
        }
    }

    // ---

    /* Enforce a resonable minimum chunk size */

    if(XHC_BARRIER != colltype) {
        bool altered_chunks = false;
        for(int i = 0; i < config->chunks_len; i++) {
            if(config->chunks[i] < XHC_MIN_CHUNK_SIZE) {
                config->chunks[i] = XHC_MIN_CHUNK_SIZE;
                altered_chunks = true;
            }
        }

        if(altered_chunks) {
            char *tmp, *str = strdup("");
            for(int i = 0; i < config->chunks_len; i++) {
                int err = opal_asprintf(&tmp, "%s%s%zu", str,
                    (i > 0 ? "," : ""), config->chunks[i]);
                free(str); str = tmp;
                if(err < 0) {return OMPI_ERR_OUT_OF_RESOURCE;}
            }

            free(config->chunk_string);
            config->chunk_string = str;
        }
    }

    // ---

    return OMPI_SUCCESS;
}

// ------------------------------------------------

static int xhc_print_config_info(xhc_module_t *module, ompi_communicator_t *comm) {
    char *drval_str, *lb_policy_str, *un_min_str;
    int err;

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

    switch((int) mca_coll_xhc_component.reduce_load_balance) {
        case 0:
            lb_policy_str = "none"; break;
        case XHC_REDUCE_LB_LEADER_ASSIST_TOP_LEVEL:
            lb_policy_str = "top level"; break;
        case XHC_REDUCE_LB_LEADER_ASSIST_FIRST_CHUNK:
            lb_policy_str = "first chunk"; break;
        case XHC_REDUCE_LB_LEADER_ASSIST_TOP_LEVEL
                | XHC_REDUCE_LB_LEADER_ASSIST_FIRST_CHUNK:
            lb_policy_str = "top level + first chunk"; break;
        case XHC_REDUCE_LB_LEADER_ASSIST_ALL:
            lb_policy_str = "all"; break;
        default:
            lb_policy_str = "???";
    }

    err = opal_asprintf(&un_min_str, " (min %zu bytes)",
        mca_coll_xhc_component.uniform_chunks_min);
    if(err < 0) {return OMPI_ERR_OUT_OF_RESOURCE;}

    printf("------------------------------------------------\n"
        "OMPI coll/xhc @ %s, priority %d\n"
        "  dynamic leader '%s', dynamic reduce '%s'\n"
        "  reduce load balance '%s'\n"
        "  allreduce uniform chunks '%s'%s\n",
        comm->c_name, mca_coll_xhc_component.priority,
        (mca_coll_xhc_component.dynamic_leader ? "ON" : "OFF"),
        drval_str, lb_policy_str,
        (mca_coll_xhc_component.uniform_chunks ? "ON" : "OFF"),
        (mca_coll_xhc_component.uniform_chunks ? un_min_str : ""));

    free(un_min_str);

    for(int t = 0; t < XHC_COLLCOUNT; t++) {
        xhc_op_config_t *config = &module->op_config[t];

        if(XHC_BARRIER == t) {
            printf("\n"
                "  [%s]\n"
                "    Hierarchy: %s (source: %s)\n",
                xhc_colltype_to_str(t),
                config->hierarchy_string, xhc_config_source_to_str(config->hierarchy_source));
        } else {
            printf("\n"
                "  [%s]\n"
                "    Hierarchy: %s (source: %s)\n"
                "    Chunk size(s): %s (source: %s)\n"
                "    CICO: Up to %zu bytes (source: %s)\n",
                xhc_colltype_to_str(t),
                config->hierarchy_string, xhc_config_source_to_str(config->hierarchy_source),
                config->chunk_string, xhc_config_source_to_str(config->chunk_source),
                config->cico_max, xhc_config_source_to_str(config->cico_max_source));
        }
    }

    printf("------------------------------------------------\n");

    return OMPI_SUCCESS;
}

static int xhc_print_op_info(xhc_module_t *module,
        ompi_communicator_t *comm, XHC_COLLTYPE_T colltype) {

    xhc_comm_t *comms = module->op_data[colltype].comms;
    int comm_count = module->op_data[colltype].comm_count;

    int rank = ompi_comm_rank(comm);

    for(int i = 0; i < comm_count; i++) {
        char *memb_list = NULL, *tmp;
        int err;

        err = opal_asprintf(&memb_list, "%d", comms[i].owner_rank);
        if(err < 0) {return OMPI_ERR_OUT_OF_RESOURCE;}

        for(int m = 1; m < comms[i].size; m++) {
            if(m == comms[i].my_id) {
                if(0 == i || comms[i-1].owner_rank == rank) {
                    err = opal_asprintf(&tmp, "%s %d", memb_list, rank);
                } else {
                    err = opal_asprintf(&tmp, "%s _", memb_list);
                }
            } else {
                err = opal_asprintf(&tmp, "%s x", memb_list);
            }

            free(memb_list);
            memb_list = tmp;

            if(err < 0) {
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
        }

        if(XHC_BARRIER == colltype) {
            printf("XHC_COMM ompi_comm=%s rank=%d op=%s loc=0x%08x members=%d [%s]\n",
                comm->c_name, rank, xhc_colltype_to_str(colltype), comms[i].locality,
                comms[i].size, memb_list);
        } else {
            printf("XHC_COMM ompi_comm=%s rank=%d op=%s loc=0x%08x chunk_size=%zu "
                "cico_size=%zu members=%d [%s]\n", comm->c_name, rank,
                xhc_colltype_to_str(colltype), comms[i].locality, comms[i].chunk_size,
                comms[i].cico_size, comms[i].size, memb_list);
        }

        free(memb_list);
    }

    return OMPI_SUCCESS;
}

static int xhc_print_op_hierarchy_dot(xhc_module_t *module,
        ompi_communicator_t *comm, XHC_COLLTYPE_T colltype) {

    xhc_coll_fns_t xhc_fns;
    xhc_comm_t *comms = module->op_data[colltype].comms;

    int *src_rank = NULL;
    int my_src = -1;

    FILE *outfile = stdout;

    if(0 == module->rank) {
        src_rank = malloc(module->comm_size * sizeof(int));
        if(!src_rank) {return OMPI_ERR_OUT_OF_RESOURCE;}
    }

    for(xhc_comm_t *xc = comms; xc; xc = xc->up) {
        if(0 != xc->my_id) {
            my_src = xc->owner_rank;
            break;
        }
    }

    xhc_module_set_coll_fns(comm, &module->prev_colls, &xhc_fns);

    int err = comm->c_coll->coll_gather(&my_src,
        1, MPI_INT, src_rank, 1, MPI_INT, 0,
        comm, comm->c_coll->coll_gather_module);

    xhc_module_set_coll_fns(comm, &xhc_fns, NULL);

    if(OMPI_SUCCESS != err) {
        if(0 == module->rank) {
            free(src_rank);
        }
        return err;
    }

    if(0 == module->rank) {
        char *dir;

        switch(colltype) {
            case XHC_BCAST:
                dir = "forward"; break;
            case XHC_REDUCE: case XHC_ALLREDUCE:
                dir = "back"; break;
            case XHC_BARRIER:
                dir = "both"; break;
            default:
                dir = "none";
        }

        fprintf(outfile, "digraph xhc_%s_hierarchy {\n", xhc_colltype_to_str(colltype));

        for(int r = 1; r < module->comm_size; r++) {
            fprintf(outfile, "\t%d -> %d [dir=%s];\n", src_rank[r], r, dir);
        }

        fprintf(outfile, "}\n");

        free(src_rank);
    }

    return OMPI_SUCCESS;
}

// ------------------------------------------------

void *mca_coll_xhc_shmem_create(opal_shmem_ds_t *seg_ds, size_t size,
        ompi_communicator_t *ompi_comm, const char *name, int id1, int id2) {

    char *shmem_file;
    int err;

    // xhc_shmem_seg.<UID>@<HOST>.<JOBID>.<RANK@COMM_WORLD>:<CID>_<NAME>:<ID1>:<ID2>

    err = opal_asprintf(&shmem_file, "%s" OPAL_PATH_SEP
        "xhc_shmem_seg.%u@%s.%x.%d:%d_%s:%d:%d", mca_coll_xhc_component.shmem_backing,
        geteuid(), opal_process_info.nodename, OPAL_PROC_MY_NAME.jobid,
        ompi_comm_rank(MPI_COMM_WORLD), ompi_comm_get_local_cid(ompi_comm),
        name, id1, id2);

    if(err < 0) {
        return NULL;
    }

    // Not 100% sure what this does!, copied from btl/sm
    opal_pmix_register_cleanup(shmem_file, false, false, false);

    err = opal_shmem_segment_create(seg_ds, shmem_file, size);

    free(shmem_file);

    if(OPAL_SUCCESS != err) {
        opal_output_verbose(MCA_BASE_VERBOSE_ERROR,
            ompi_coll_base_framework.framework_output,
            "coll:xhc: Error: Could not create shared memory segment");

        return NULL;
    }

    void *addr = xhc_shmem_attach(seg_ds);

    if(NULL == addr) {
        opal_shmem_unlink(seg_ds);
    }

    return addr;
}

void *mca_coll_xhc_shmem_attach(opal_shmem_ds_t *seg_ds) {
    void *addr = opal_shmem_segment_attach(seg_ds);

    if(NULL == addr) {
        opal_output_verbose(MCA_BASE_VERBOSE_ERROR,
            ompi_coll_base_framework.framework_output,
            "coll:xhc: Error: Could not attach to shared memory segment");
    }

    return addr;
}

// ------------------------------------------------

void *mca_coll_xhc_get_cico(xhc_peer_info_t *peer_info, int rank) {
    if(NULL == peer_info[rank].cico_buffer) {
        peer_info[rank].cico_buffer = xhc_shmem_attach(&peer_info[rank].cico_ds);
    }

    return peer_info[rank].cico_buffer;
}

static mca_smsc_endpoint_t *xhc_smsc_ep(xhc_peer_info_t *peer_info) {
    if(!peer_info->smsc_ep) {
        peer_info->smsc_ep = MCA_SMSC_CALL(get_endpoint, &peer_info->proc->super);

        if(!peer_info->smsc_ep) {
            opal_output_verbose(MCA_BASE_VERBOSE_ERROR,
                ompi_coll_base_framework.framework_output,
                "coll:xhc: Error: Failed to initialize smsc endpoint");

            return NULL;
        }
    }

    return peer_info->smsc_ep;
}

int mca_coll_xhc_copy_expose_region(void *base,
        size_t len, xhc_copy_data_t **region_data) {

    if(mca_smsc_base_has_feature(MCA_SMSC_FEATURE_REQUIRE_REGISTRATION)) {
        void *data = MCA_SMSC_CALL(register_region, base, len);

        if(NULL == data) {
            opal_output_verbose(MCA_BASE_VERBOSE_ERROR,
                ompi_coll_base_framework.framework_output,
                "coll:xhc: Error: Failed to register memory region with smsc");

            return -1;
        }

        *region_data = data;
    }

    return 0;
}

void mca_coll_xhc_copy_region_post(void *dst, xhc_copy_data_t *region_data) {
    memcpy(dst, region_data, mca_smsc_base_registration_data_size());
}

int mca_coll_xhc_copy_from(xhc_peer_info_t *peer_info,
        void *dst, void *src, size_t size, void *access_token) {

    mca_smsc_endpoint_t *smsc_ep = xhc_smsc_ep(peer_info);

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

void *mca_coll_xhc_get_registration(xhc_peer_info_t *peer_info,
        void *peer_vaddr, size_t size, xhc_reg_t **reg) {

    mca_smsc_endpoint_t *smsc_ep = xhc_smsc_ep(peer_info);

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
