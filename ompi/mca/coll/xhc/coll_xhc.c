/*
 * Copyright (c) 2021-2023 Computer Architecture and VLSI Systems (CARV)
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

#include "opal/mca/rcache/rcache.h"
#include "opal/mca/shmem/base/base.h"
#include "opal/mca/smsc/smsc.h"

#include "opal/include/opal/align.h"
#include "opal/util/show_help.h"
#include "opal/util/minmax.h"

#include "coll_xhc.h"

static int xhc_comms_make(ompi_communicator_t *ompi_comm,
    xhc_peer_info_t *peer_info, xhc_comm_t **comms_dst,
    int *comm_count_dst, xhc_loc_t *hierarchy, int hierarchy_len);
static void xhc_comms_destroy(xhc_comm_t *comms, int comm_count);

static int xhc_print_info(xhc_module_t *module,
    ompi_communicator_t *comm, xhc_data_t *data);

static void *xhc_shmem_create(opal_shmem_ds_t *seg_ds, size_t size,
    ompi_communicator_t *ompi_comm, const char *name_chr_s, int name_chr_i);
static void *xhc_shmem_attach(opal_shmem_ds_t *seg_ds);
static mca_smsc_endpoint_t *xhc_smsc_ep(xhc_peer_info_t *peer_info);

// ------------------------------------------------

int mca_coll_xhc_lazy_init(xhc_module_t *module, ompi_communicator_t *comm) {

    int comm_size = ompi_comm_size(comm);
    int rank = ompi_comm_rank(comm);

    xhc_peer_info_t *peer_info = module->peer_info;

    opal_shmem_ds_t *peer_cico_ds = NULL;
    xhc_data_t *data = NULL;

    xhc_coll_fns_t xhc_fns;

    int return_code = OMPI_SUCCESS;
    int ret;

    errno = 0;

    // ----

    /* XHC requires rank communication during its initialization.
     * Temporarily apply the saved fallback collective modules,
     * and restore XHC's after initialization is done. */
    xhc_module_install_fallback_fns(module, comm, &xhc_fns);

    // ----

    ret = xhc_module_prepare_hierarchy(module, comm);
    if(ret != OMPI_SUCCESS) {
        RETURN_WITH_ERROR(return_code, ret, end);
    }

    // ----

    data = malloc(sizeof(xhc_data_t));
    peer_cico_ds = malloc(comm_size * sizeof(opal_shmem_ds_t));
    if(!data || !peer_cico_ds) {
        RETURN_WITH_ERROR(return_code, OMPI_ERR_OUT_OF_RESOURCE, end);
    }

    *data = (xhc_data_t) {
        .comms = NULL,
        .comm_count = -1,

        .pvt_coll_seq = 0
    };

    // ----

    if(OMPI_XHC_CICO_MAX > 0) {
        opal_shmem_ds_t cico_ds;

        void *my_cico = xhc_shmem_create(&cico_ds,
            OMPI_XHC_CICO_MAX, comm, "cico", 0);
        if(!my_cico) {
            RETURN_WITH_ERROR(return_code, OMPI_ERR_OUT_OF_RESOURCE, end);
        }

        /* Manually "touch" to assert allocation in local NUMA node
         * (assuming linux's default firt-touch-alloc NUMA policy) */
        memset(my_cico, 0, OMPI_XHC_CICO_MAX);

        ret = comm->c_coll->coll_allgather(&cico_ds,
            sizeof(opal_shmem_ds_t), MPI_BYTE, peer_cico_ds,
            sizeof(opal_shmem_ds_t), MPI_BYTE, comm,
            comm->c_coll->coll_allgather_module);
        if(ret != OMPI_SUCCESS) {
            RETURN_WITH_ERROR(return_code, ret, end);
        }

        for(int r = 0; r < comm_size; r++) {
            peer_info[r].cico_ds = peer_cico_ds[r];
        }

        peer_info[rank].cico_buffer = my_cico;
    }

    // ----

    /* An XHC communicator is created for each level of the hierarchy.
     * The hierachy must be in an order of most-specific to most-general. */

    ret = xhc_comms_make(comm, peer_info, &data->comms, &data->comm_count,
        module->hierarchy, module->hierarchy_len);
    if(ret != OMPI_SUCCESS) {
        RETURN_WITH_ERROR(return_code, ret, end);
    }

    for(int i = 0, c = 0; i < data->comm_count; i++) {
        data->comms[i].chunk_size = module->chunks[c];
        c = opal_min(c + 1, module->chunks_len - 1);
    }

    if(module->chunks_len < data->comm_count) {
        opal_output_verbose(MCA_BASE_VERBOSE_WARN,
            ompi_coll_base_framework.framework_output,
            "coll:xhc: Warning: The chunk sizes count is shorter than the "
            "hierarchy size; filling in with the last entry provided");
    } else if(module->chunks_len > data->comm_count) {
        opal_output_verbose(MCA_BASE_VERBOSE_WARN,
            ompi_coll_base_framework.framework_output,
            "coll:xhc: Warning: The chunk size count is larger than the "
            "hierarchy size; omitting last entries");
    }

    // ----

    if(mca_coll_xhc_component.print_info) {
        ret = xhc_print_info(module, comm, data);
        if(ret != OMPI_SUCCESS) {
            RETURN_WITH_ERROR(return_code, ret, end);
        }
    }

    // ----

    module->data = data;
    module->init = true;

    end:

    xhc_module_install_fns(module, comm, xhc_fns);

    free(peer_cico_ds);

    if(return_code != 0) {
        opal_show_help("help-coll-xhc.txt", "xhc-init-failed", true,
            return_code, errno, strerror(errno));

        xhc_fini(module);
    }

    return return_code;
}

void mca_coll_xhc_fini(mca_coll_xhc_module_t *module) {
    if(module->data) {
        xhc_data_t *data = module->data;

        if(data->comm_count >= 0) {
            xhc_comms_destroy(data->comms, data->comm_count);
        }

        free(data->comms);
        free(data);
    }

    if(module->peer_info) {
        for(int r = 0; r < module->comm_size; r++) {
            if(module->peer_info[r].cico_buffer) {
                if(r == module->rank) {
                    // OMPI issue #11123
                    // opal_shmem_unlink(&module->peer_info[r].cico_ds);
                }

                opal_shmem_segment_detach(&module->peer_info[r].cico_ds);
            }

            if(module->peer_info[r].smsc_ep) {
                MCA_SMSC_CALL(return_endpoint, module->peer_info[r].smsc_ep);
            }
        }
    }
}

// ------------------------------------------------

/* This method is where the hierarchy of XHC is constructed; it receives
 * the hierarchy specifications (hierarchy param) and groups ranks together
 * among them. The process begins with the first locality in the list. All
 * ranks that share this locality (determined via the relative peer to peer
 * distances) become siblings. The one amongst them with the lowest rank
 * number becomes the manager/leader of the group. The members don't really
 * need to keep track of the actual ranks of their siblings -- only the rank
 * of the group's leader/manager, the size of the group, and their own member
 * ID. The process continues with the next locality, only that now only the
 * ranks that became leaders in the previous level are eligible (determined
 * via comm_candidate, see inline comments). */
static int xhc_comms_make(ompi_communicator_t *ompi_comm,
        xhc_peer_info_t *peer_info, xhc_comm_t **comms_dst,
        int *comm_count_dst, xhc_loc_t *hierarchy, int hierarchy_len) {

    int ompi_rank = ompi_comm_rank(ompi_comm);
    int ompi_size = ompi_comm_size(ompi_comm);

    xhc_comm_t *comms = NULL;
    int comms_size = 0;
    int comm_count = 0;

    opal_shmem_ds_t *comm_ctrl_ds;
    bool *comm_candidate;

    size_t smsc_reg_size = 0;

    int return_code = OMPI_SUCCESS;
    int ret;

    comms = malloc((comms_size = 5) * sizeof(xhc_comm_t));
    comm_ctrl_ds = malloc(ompi_size * sizeof(opal_shmem_ds_t));
    comm_candidate = malloc(ompi_size * sizeof(bool));

    if(!comms || !comm_ctrl_ds || !comm_candidate) {
        RETURN_WITH_ERROR(return_code, OMPI_ERR_OUT_OF_RESOURCE, end);
    }

    if(mca_smsc_base_has_feature(MCA_SMSC_FEATURE_REQUIRE_REGISTRATION)) {
        smsc_reg_size = mca_smsc_base_registration_data_size();
    }

    for(int h = 0; h < hierarchy_len; h++) {
        xhc_comm_t *xc = &comms[comm_count];

        if(comm_count == comms_size) {
            void *tmp = realloc(comms, (comms_size *= 2) * sizeof(xhc_comm_t));
            if(!tmp) {
                RETURN_WITH_ERROR(return_code, OMPI_ERR_OUT_OF_RESOURCE, end);
            }
            comms = tmp;
        }

        *xc = (xhc_comm_t) {
            .locality = hierarchy[h],

            .size = 0,
            .manager_rank = -1,

            .member_info = NULL,
            .reduce_queue = NULL,

            .comm_ctrl = NULL,
            .member_ctrl = NULL,

            .ctrl_ds = (opal_shmem_ds_t) {0}
        };

        // ----

        /* Only ranks that were leaders in the previous level are candidates
         * for this one. Every rank advertises whether others may consider
         * it for inclusion via an Allgather. */

        bool is_candidate = (comm_count == 0
            || comms[comm_count - 1].manager_rank == ompi_rank);

        ret = ompi_comm->c_coll->coll_allgather(&is_candidate, 1,
            MPI_C_BOOL, comm_candidate, 1, MPI_C_BOOL,
            ompi_comm, ompi_comm->c_coll->coll_allgather_module);
        if(ret != OMPI_SUCCESS) {
            RETURN_WITH_ERROR(return_code, ret, comm_error);
        }

        for(int r = 0; r < ompi_size; r++) {

            /* If on a non-bottom comm, only managers of the previous
             * comm are "full" members. However, this procedure also has
             * to take place for the bottom-most comm; even if this is the
             * current rank's bottom-most comm, it may not actually be so,
             * for another rank (eg. with some non-symmetric hierarchies). */
            if(comm_candidate[r] == false) {
                continue;
            }

            // Non-local --> not part of the comm :/
            if(!PEER_IS_LOCAL(peer_info, r, xc->locality)) {
                continue;
            }

            /* The member ID means slightly different things whether on the
             * bottom-most comm or not. On the bottom-most comm, a rank can
             * either be a "full" member or not. However, on higher-up comms,
             * if a rank was not a manager on the previous comm, it will not
             * a "full" member. Instead, it will be a "potential" member, in
             * that it keeps information about this comm, and is ready to
             * take over duties and act as a normal member for a specific
             * collective (eg. dynamic leader feature, or root != manager). */
            if(r == ompi_rank || (comm_count > 0 && r == comms[comm_count - 1].manager_rank)) {
                xc->member_id = xc->size;
            }

            // First rank to join the comm becomes the manager
            if(xc->manager_rank == -1) {
                xc->manager_rank = r;
            }

            xc->size++;
        }

        /* If there are no local peers in regards to this locality, no
         * XHC comm is created for this process on this level. */
        if(xc->size <= 1) {
            opal_output_verbose(MCA_BASE_VERBOSE_WARN,
                ompi_coll_base_framework.framework_output,
                "coll:xhc: Warning: Locality 0x%04x does not result "
                "in any new groupings; skipping it", xc->locality);

            /* All ranks must participate in the "control struct sharing"
             * allgather, even if useless to this rank to some of them */

            ret = ompi_comm->c_coll->coll_allgather(&xc->ctrl_ds,
                sizeof(opal_shmem_ds_t), MPI_BYTE, comm_ctrl_ds,
                sizeof(opal_shmem_ds_t), MPI_BYTE, ompi_comm,
                ompi_comm->c_coll->coll_allgather_module);
            if(ret != OMPI_SUCCESS) {
                RETURN_WITH_ERROR(return_code, ret, comm_error);
            }

            xhc_comms_destroy(xc, 1);
            continue;
        }

        // ----

        /* Init comm stuff */

        xc->member_info = calloc(xc->size, sizeof(xhc_member_info_t));
        if(xc->member_info == NULL) {
            RETURN_WITH_ERROR(return_code, OMPI_ERR_OUT_OF_RESOURCE, comm_error);
        }

        xc->reduce_queue = OBJ_NEW(opal_list_t);
        if(!xc->reduce_queue) {
            RETURN_WITH_ERROR(return_code, OMPI_ERR_OUT_OF_RESOURCE, comm_error);
        }

        for(int m = 0; m < xc->size - 1; m++) {
            xhc_rq_item_t *item = OBJ_NEW(xhc_rq_item_t);
            if(!item) {
                RETURN_WITH_ERROR(return_code,
                    OMPI_ERR_OUT_OF_RESOURCE, comm_error);
            }

            opal_list_append(xc->reduce_queue, (opal_list_item_t *) item);
        }

        // ----

        // Create shared structs
        if(ompi_rank == xc->manager_rank) {
            size_t ctrl_len = sizeof(xhc_comm_ctrl_t) + smsc_reg_size
                + xc->size * sizeof(xhc_member_ctrl_t);

            char *ctrl_base = xhc_shmem_create(&xc->ctrl_ds, ctrl_len,
                ompi_comm, "ctrl", comm_count);
            if(ctrl_base == NULL) {
                RETURN_WITH_ERROR(return_code, OMPI_ERROR, comm_error);
            }

            /* Manually "touch" to assert allocation in local NUMA node
            * (assuming linux's default firt-touch-alloc NUMA policy) */
            memset(ctrl_base, 0, ctrl_len);

            xc->comm_ctrl = (void *) ctrl_base;
            xc->member_ctrl = (void *) (ctrl_base
                + sizeof(xhc_comm_ctrl_t) + smsc_reg_size);
        }

        /* The comm's managers share the details of the communication structs
         * with their children, so that they may attach to them. Because
         * there's not any MPI communicator formed that includes (only) the
         * members of the XHC comm, the sharing is achieved with a single
         * Allgather, instead of a Broadcast inside each XHC comm. */

        ret = ompi_comm->c_coll->coll_allgather(&xc->ctrl_ds,
            sizeof(opal_shmem_ds_t), MPI_BYTE, comm_ctrl_ds,
            sizeof(opal_shmem_ds_t), MPI_BYTE, ompi_comm,
            ompi_comm->c_coll->coll_allgather_module);
        if(ret != OMPI_SUCCESS) {
            RETURN_WITH_ERROR(return_code, ret, comm_error);
        }

        // Attach to manager's shared structs
        if(ompi_rank != xc->manager_rank) {
            xc->ctrl_ds = comm_ctrl_ds[xc->manager_rank];

            char *ctrl_base = xhc_shmem_attach(&xc->ctrl_ds);
            if(ctrl_base == NULL) {
                RETURN_WITH_ERROR(return_code, OMPI_ERROR, comm_error);
            }

            xc->comm_ctrl = (void *) ctrl_base;
            xc->member_ctrl = (void *) (ctrl_base
                + sizeof(xhc_comm_ctrl_t) + smsc_reg_size);
        }

        xc->my_member_ctrl = &xc->member_ctrl[xc->member_id];
        xc->my_member_info = &xc->member_info[xc->member_id];

        // ----

        comm_count++;

        continue;

        comm_error: {
            xhc_comms_destroy(comms, comm_count+1);
            comm_count = -1;

            goto end;
        }
    }

    REALLOC(comms, comm_count, xhc_comm_t);

    *comms_dst = comms;
    *comm_count_dst = comm_count;

    end:

    free(comm_ctrl_ds);
    free(comm_candidate);

    if(return_code != OMPI_SUCCESS) {
        free(comms);
    }

    return return_code;
}

static void xhc_comms_destroy(xhc_comm_t *comms, int comm_count) {
    bool is_manager = true;

    for(int i = 0; i < comm_count; i++) {
        xhc_comm_t *xc = &comms[i];

        if(xc->member_id != 0) {
            is_manager = false;
        }

        free(xc->member_info);

        if(xc->reduce_queue) {
            OPAL_LIST_RELEASE(xc->reduce_queue);
        }

        if(xc->comm_ctrl) {
            if(is_manager) {
                // OMPI issue #11123
                // opal_shmem_unlink(&xc->ctrl_ds);
                (void) is_manager;
            }

            opal_shmem_segment_detach(&xc->ctrl_ds);
        }

        *xc = (xhc_comm_t) {0};
    }
}

static int xhc_print_info(xhc_module_t *module,
        ompi_communicator_t *comm, xhc_data_t *data) {

    int rank = ompi_comm_rank(comm);
    int ret;

    if(rank == 0) {
        char *drval_str;
        char *lb_rla_str;
        char *un_min_str;

        switch(mca_coll_xhc_component.dynamic_reduce) {
            case OMPI_XHC_DYNAMIC_REDUCE_DISABLED:
                drval_str = "OFF"; break;
            case OMPI_XHC_DYNAMIC_REDUCE_NON_FLOAT:
                drval_str = "ON (non-float)"; break;
            case OMPI_XHC_DYNAMIC_REDUCE_ALL:
                drval_str = "ON (all)"; break;
            default:
                drval_str = "???";
        }

        switch(mca_coll_xhc_component.lb_reduce_leader_assist) {
            case OMPI_XHC_LB_RLA_TOP_LEVEL:
                lb_rla_str = "top level"; break;
            case OMPI_XHC_LB_RLA_FIRST_CHUNK:
                lb_rla_str = "first chunk"; break;
            case OMPI_XHC_LB_RLA_TOP_LEVEL | OMPI_XHC_LB_RLA_FIRST_CHUNK:
                lb_rla_str = "top level + first chunk"; break;
            case OMPI_XHC_LB_RLA_ALL:
                lb_rla_str = "all"; break;
            default:
                lb_rla_str = "???";
        }

        ret = opal_asprintf(&un_min_str, " (min '%zu' bytes)",
            mca_coll_xhc_component.uniform_chunks_min);
        if(ret < 0) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        printf("------------------------------------------------\n"
            "OMPI coll/xhc @ %s, priority %d\n"
            "  dynamic leader '%s', dynamic reduce '%s'\n"
            "  reduce load-balancing leader-assist '%s'\n"
            "  allreduce uniform chunks '%s'%s\n"
            "  CICO up until %zu bytes, barrier root %d\n\n"
            "------------------------------------------------\n",
            comm->c_name, mca_coll_xhc_component.priority,
            (mca_coll_xhc_component.dynamic_leader ? "ON" : "OFF"),
            drval_str, lb_rla_str,
            (mca_coll_xhc_component.uniform_chunks ? "ON" : "OFF"),
            (mca_coll_xhc_component.uniform_chunks ? un_min_str : ""),
            mca_coll_xhc_component.cico_max,
            mca_coll_xhc_component.barrier_root);

        free(un_min_str);
    }

    for(int i = 0; i < data->comm_count; i++) {
        char *mlist = NULL;
        char *tmp;

        ret = opal_asprintf(&mlist, "%d", data->comms[i].manager_rank);
        if(ret < 0) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        for(int m = 1; m < data->comms[i].size; m++) {
            if(m == data->comms[i].member_id) {
                if(i == 0 || data->comms[i-1].manager_rank == rank) {
                    ret = opal_asprintf(&tmp, "%s %d", mlist, rank);
                } else {
                    ret = opal_asprintf(&tmp, "%s _", mlist);
                }
            } else {
                ret = opal_asprintf(&tmp, "%s x", mlist);
            }

            free(mlist);
            mlist = tmp;

            if(ret < 0) {
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
        }

        printf("XHC comm loc=0x%08x chunk_size=%zu with %d members [%s]\n",
            data->comms[i].locality, data->comms[i].chunk_size,
            data->comms[i].size, mlist);

        free(mlist);
    }

    return OMPI_SUCCESS;
}

// ------------------------------------------------

static void *xhc_shmem_create(opal_shmem_ds_t *seg_ds, size_t size,
        ompi_communicator_t *ompi_comm, const char *name_chr_s, int name_chr_i) {

    char *shmem_file;
    int ret;

    // xhc_shmem_seg.<UID>@<HOST>.<JOBID>.<RANK@COMM_WORLD>:<CID>_<CHRS>:<CHRI>

    ret = opal_asprintf(&shmem_file, "%s" OPAL_PATH_SEP "xhc_shmem_seg.%u@%s.%x.%d:%d_%s:%d",
        mca_coll_xhc_component.shmem_backing, geteuid(), opal_process_info.nodename,
        OPAL_PROC_MY_NAME.jobid, ompi_comm_rank(MPI_COMM_WORLD), ompi_comm_get_local_cid(ompi_comm),
        name_chr_s, name_chr_i);

    if(ret < 0) {
        return NULL;
    }

    // Not 100% sure what this does!, copied from btl/sm
    opal_pmix_register_cleanup(shmem_file, false, false, false);

    ret = opal_shmem_segment_create(seg_ds, shmem_file, size);

    free(shmem_file);

    if(ret != OPAL_SUCCESS) {
        opal_output_verbose(MCA_BASE_VERBOSE_ERROR,
            ompi_coll_base_framework.framework_output,
            "coll:xhc: Error: Could not create shared memory segment");

        return NULL;
    }

    void *addr = xhc_shmem_attach(seg_ds);

    if(addr == NULL) {
        opal_shmem_unlink(seg_ds);
    }

    return addr;
}

static void *xhc_shmem_attach(opal_shmem_ds_t *seg_ds) {
    void *addr = opal_shmem_segment_attach(seg_ds);

    if(addr == NULL) {
        opal_output_verbose(MCA_BASE_VERBOSE_ERROR,
            ompi_coll_base_framework.framework_output,
            "coll:xhc: Error: Could not attach to shared memory segment");
    }

    return addr;
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

// ------------------------------------------------

void *mca_coll_xhc_get_cico(xhc_peer_info_t *peer_info, int rank) {
    if(OMPI_XHC_CICO_MAX == 0) {
        return NULL;
    }

    if(peer_info[rank].cico_buffer == NULL) {
        peer_info[rank].cico_buffer = xhc_shmem_attach(&peer_info[rank].cico_ds);
    }

    return peer_info[rank].cico_buffer;
}

int mca_coll_xhc_copy_expose_region(void *base, size_t len, xhc_copy_data_t **region_data) {
    if(mca_smsc_base_has_feature(MCA_SMSC_FEATURE_REQUIRE_REGISTRATION)) {
        void *data = MCA_SMSC_CALL(register_region, base, len);

        if(data == NULL) {
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

    if(smsc_ep == NULL) {
        return -1;
    }

    int status = MCA_SMSC_CALL(copy_from, smsc_ep,
        dst, src, size, access_token);

    return (status == OPAL_SUCCESS ? 0 : -1);
}

void mca_coll_xhc_copy_close_region(xhc_copy_data_t *region_data) {
    if(mca_smsc_base_has_feature(MCA_SMSC_FEATURE_REQUIRE_REGISTRATION))
        MCA_SMSC_CALL(deregister_region, region_data);
}

void *mca_coll_xhc_get_registration(xhc_peer_info_t *peer_info,
        void *peer_vaddr, size_t size, xhc_reg_t **reg) {

    mca_smsc_endpoint_t *smsc_ep = xhc_smsc_ep(peer_info);

    if(smsc_ep == NULL) {
        return NULL;
    }

    /* MCA_RCACHE_FLAGS_PERSIST will cause the registration to stick around.
     * Though actually, because smsc/xpmem initializes the ref count to 2,
     * as a means of keeping the registration around (instead of using the
     * flag), our flag here doesn't have much effect. If at some point we
     * would wish to actually detach memory in some or all cases, we should
     * either call the unmap method twice, or reach out to Open MPI devs and
     * inquire about the ref count. */

    void *local_ptr;

    *reg = MCA_SMSC_CALL(map_peer_region, smsc_ep,
        MCA_RCACHE_FLAGS_PERSIST, peer_vaddr, size, &local_ptr);

    if(*reg == NULL) {
        return NULL;
    }

    return local_ptr;
}

/* Won't actually unmap/detach, since we've set
 * the "persist" flag while creating the mapping */
void mca_coll_xhc_return_registration(xhc_reg_t *reg) {
    MCA_SMSC_CALL(unmap_peer_region, reg);
}
