/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 */

#include "ompi_config.h"
#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <errno.h>

#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/sbgp/ibnet/sbgp_ibnet.h"
#include "orte/mca/rml/rml.h"
#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/include/orte/types.h"
#include "ompi/mca/common/ofacm/base.h"
#include "ompi/mca/common/ofacm/connect.h"
#include "ompi/patterns/comm/coll_ops.h"
/*
 * Unused
static int ibnet_module_enable(mca_sbgp_base_module_t *module,
        struct ompi_communicator_t *comm);

*/

/*
 * Local functions
 */
static void
mca_sbgp_ibnet_module_construct(mca_sbgp_ibnet_module_t *module)
{
    module->cgroups = NULL;
    module->group_id = 0;
}

static void
mca_sbgp_ibnet_module_destruct(mca_sbgp_ibnet_module_t *module)
{

}

OBJ_CLASS_INSTANCE(mca_sbgp_ibnet_module_t,
                   mca_sbgp_base_module_t,
                   mca_sbgp_ibnet_module_construct,
                   mca_sbgp_ibnet_module_destruct);

static void
mca_sbgp_ibnet_proc_construct(mca_sbgp_ibnet_proc_t *proc)
{
    /* done */
    proc->ompi_proc = 0;
    proc->num_ports = 0;
    proc->use_port = NULL;
    proc->remote_ports_info = NULL;
    proc->duty = MCA_SBGP_IBNET_NONE;
}

static void
mca_sbgp_ibnet_proc_destruct(mca_sbgp_ibnet_proc_t *proc)
{
    /* done */
    if (NULL != proc->remote_ports_info) {
        free(proc->remote_ports_info);
        /* Pasha: need to check if we need
         * to release some data from inside of the proc*/
    }

    if (NULL != proc->use_port) {
        free(proc->use_port);
    }
}

OBJ_CLASS_INSTANCE(mca_sbgp_ibnet_proc_t,
                   opal_list_item_t,
                   mca_sbgp_ibnet_proc_construct,
                   mca_sbgp_ibnet_proc_destruct);


/* Pack all data to gather buffer */
static int pack_gather_sbuff(char* sbuffer)
{
    int port, cpc;
    coll_offload_support coll_offload_flag = OFFLOAD_CONNECTX_B0; /**< Pasha: add query for collectives offload support */

    char* pack_ptr = sbuffer;

    mca_sbgp_ibnet_device_t *device = NULL;
    uint32_t my_rank = orte_process_info.my_name.vpid;
    opal_list_t *devices = &mca_sbgp_ibnet_component.devices;

    /* Message format:
     *     - my rank                 (uint32_t)
     *     - number of active ports  (uint32_t)
     *     - for each active port:
     *          + lid                (uint16_t)
     *          + subnetid           (uint64_t)
     *          + mtu                (uint32_t)
     *          + colloffload        (uint8_t)
     *          + num of cpcs        (uint8_t)
     *          + for each cpc:      (uint8_t)
     *              * cpc index      (uint8_t)
     *              * cpc priority   (uint8_t)
     *              * cpc buffer len (uint8_t)
     *              * cpc buffer     (byte * buffer_len)
     *
     */

    /* Start to put data */

    /* Pack my rank , I need it because allgather doesn't work as expected */
    IBNET_VERBOSE(10, ("Send pack rank = %d\n", my_rank));
    IBNET_VERBOSE(10, ("packing %d of %d\n", 1, sizeof(uint32_t)));

    memcpy(pack_ptr, &my_rank, sizeof(uint32_t));
    pack_ptr += sizeof(uint32_t);

    /* Put number of ports that we send */
    IBNET_VERBOSE(10, ("Send pack num of ports = %d\n", mca_sbgp_ibnet_component.total_active_ports));
    IBNET_VERBOSE(10, ("packing %d of %d\n", 1, sizeof(uint32_t)));

    memcpy(pack_ptr, &mca_sbgp_ibnet_component.total_active_ports, sizeof(uint32_t));
    pack_ptr += sizeof(uint32_t);

    /* Go through list of device and build the message*/
    for (device = (mca_sbgp_ibnet_device_t *) opal_list_get_first(devices);
            device != (mca_sbgp_ibnet_device_t *) opal_list_get_end(devices);
            device  = (mca_sbgp_ibnet_device_t *) opal_list_get_next((opal_list_item_t *)device)) {
        for (port = 0; port < device->num_allowed_ports; ++port) {
            if (!device->ports[port].used) {
                continue;
            }

            /* put port num */
            IBNET_VERBOSE(10, ("Send pack port num = %d\n", device->ports[port].id));
            IBNET_VERBOSE(10, ("packing %d of %d\n", 1, sizeof(uint16_t)));

            memcpy(pack_ptr, &device->ports[port].id, sizeof(uint16_t));
            pack_ptr += sizeof(uint16_t);

            /* put lid */
            IBNET_VERBOSE(10, ("Send pack lid = %d\n", device->ports[port].lid));
            IBNET_VERBOSE(10, ("packing %d of %d\n", 1, sizeof(uint16_t)));

            memcpy(pack_ptr, &device->ports[port].lid, sizeof(uint16_t));
            pack_ptr += sizeof(uint16_t);

            /* put subnetid */
            IBNET_VERBOSE(10, ("Send pack subnet id = %lx\n", device->ports[port].subnet_id));
            IBNET_VERBOSE(10, ("packing %d of %d\n", 1, sizeof(uint64_t)));

            memcpy(pack_ptr, &device->ports[port].subnet_id, sizeof(uint64_t));
            pack_ptr += sizeof(uint64_t);

            /* put default mtu */
            IBNET_VERBOSE(10, ("Send pack MTU = %d\n", device->ports[port].mtu));
            IBNET_VERBOSE(10, ("packing %d of %d\n", 1, sizeof(uint32_t)));

            memcpy(pack_ptr, &device->ports[port].mtu, sizeof(uint32_t));
            pack_ptr += sizeof(uint32_t);

            /* collectives offload support */
            IBNET_VERBOSE(10, ("Send pack collectives offload = %d\n", OFFLOAD_CONNECTX_B0));
            IBNET_VERBOSE(10, ("packing %d of %d\n", 1, sizeof(uint8_t)));

            /* Pasha: add query for collectives offload support */
            memcpy(pack_ptr, &coll_offload_flag, sizeof(uint8_t));
            pack_ptr += sizeof(uint8_t);

            /* number of cpcs for this port */
            IBNET_VERBOSE(10, ("Send pack number of cpcs = %d\n", device->num_cpcs));
            IBNET_VERBOSE(10, ("packing %d of %d\n", 1, sizeof(uint8_t)));

            memcpy(pack_ptr, &device->num_cpcs, sizeof(uint8_t));
            pack_ptr += sizeof(uint8_t);

            for (cpc = 0; cpc < device->num_cpcs; cpc++) {
                uint8_t cpc_index;
                uint8_t cpc_buflen;

                /* cpc index */
                cpc_index = ompi_common_ofacm_base_get_cpc_index(device->cpcs[cpc]->data.cbm_component);

                IBNET_VERBOSE(10, ("Send pack cpc index  = %d\n", cpc_index));
                IBNET_VERBOSE(10, ("packing %d of %d\n", 1, sizeof(uint8_t)));

                memcpy(pack_ptr, &cpc_index, sizeof(uint8_t));
                pack_ptr += sizeof(uint8_t);

                /* cpc priority */
                IBNET_VERBOSE(10, ("Send pack cpc priority  = %d\n",
                                    device->cpcs[cpc]->data.cbm_priority));
                IBNET_VERBOSE(10, ("packing %d of %d\n", 1, sizeof(uint8_t)));

                memcpy(pack_ptr, &device->cpcs[cpc]->data.cbm_priority, sizeof(uint8_t));
                pack_ptr += sizeof(uint8_t);

                /* cpc buffer length in bytes */
                cpc_buflen = device->cpcs[cpc]->data.cbm_modex_message_len;

                IBNET_VERBOSE(10, ("Send pack cpc message len  = %d\n", cpc_buflen));
                IBNET_VERBOSE(10, ("packing %d of %d\n", 1, sizeof(uint8_t)));

                memcpy(pack_ptr, &cpc_buflen, sizeof(uint8_t));
                pack_ptr += sizeof(uint8_t);

                /* cpc buffer */
                if (0 != cpc_buflen) {
                    IBNET_VERBOSE(10, ("Send pack cpc buffer  len = %d\n", cpc_buflen));
                    IBNET_VERBOSE(10, ("packing %d of %d\n", 1, sizeof(uint8_t)));

                    memcpy(pack_ptr, device->cpcs[cpc]->data.cbm_modex_message, cpc_buflen);
                    pack_ptr += (size_t) cpc_buflen;
                }
            }
        }
    }

    return OMPI_SUCCESS;
}

/* Translation vpid to ompi_proc */
static int vpid_to_proc(orte_vpid_t vpid,
        struct ompi_proc_t ** procs, int n_procs_in, ompi_proc_t** out_proc)
{
    int i;
    for (i = 0; i < n_procs_in; i++) {
        if (vpid == procs[i]->proc_name.vpid) {
            *out_proc = procs[i];
            return i;
        }
    }

    return OMPI_ERROR;
}

static int unpack_and_load_gather_rbuff(char *rbuffer, int max_sent_bytes,
        struct ompi_proc_t ** procs, int n_procs_in, opal_list_t *peers_data)
{

    int i;
    char* unpack_ptr;

    /* Message format:
     *     - my rank                 (uint32_t)
     *     - number of active ports  (uint32_t)
     *     - for each active port:
     *          + lid                (uint16_t)
     *          + subnetid           (uint64_t)
     *          + mtu                (uint32_t)
     *          + colloffload        (uint8_t)
     *          + num of cpcs        (uint8_t)
     *          + for each cpc:      (uint8_t)
     *              * cpc index      (uint8_t)
     *              * cpc priority   (uint8_t)
     *              * cpc buffer len (uint8_t)
     *              * cpc buffer     (byte*buffer_len)
     *
     */

    /* Start to unpack data */
    for(i = 0; i < n_procs_in; i++) {
        uint32_t p;
        mca_sbgp_ibnet_proc_t *ibnet_proc;

        unpack_ptr = rbuffer + (size_t) (i * max_sent_bytes);

        /* create new proc */
        ibnet_proc = OBJ_NEW(mca_sbgp_ibnet_proc_t);

        IBNET_VERBOSE(10, ("element=%d unpacking %d of %d\n", i, 1, sizeof(uint32_t)));
        IBNET_VERBOSE(10, ("Recive remote rank %d\n", ibnet_proc->rank));

        memcpy(&ibnet_proc->rank, unpack_ptr, sizeof(uint32_t));
        unpack_ptr += sizeof(uint32_t);

        /* set back pointer to ompi_proc */
        ibnet_proc->ompi_proc_index =
            vpid_to_proc(ibnet_proc->rank, procs,
                    n_procs_in, &ibnet_proc->ompi_proc);
        if (OMPI_ERROR == ibnet_proc->ompi_proc_index) {
            return OMPI_ERROR;
        }

        IBNET_VERBOSE(10, ("element=%d unpacking %d of %d\n", i, 1, sizeof(uint32_t)));
        IBNET_VERBOSE(10, ("Recive number of ports %d\n", ibnet_proc->num_ports));

        memcpy(&ibnet_proc->num_ports, unpack_ptr, sizeof(uint32_t));
        unpack_ptr += sizeof(uint32_t);

        /* prepare place for port data*/
        ibnet_proc->remote_ports_info = calloc(ibnet_proc->num_ports, sizeof(mca_sbgp_ibnet_port_t));
        if (NULL == ibnet_proc->remote_ports_info) {
            return OMPI_ERROR;
        }

        /* load the data */
        for(p = 0; p < ibnet_proc->num_ports; p++) {
            mca_sbgp_ibnet_port_t *port = &ibnet_proc->remote_ports_info[p];
            uint32_t cpc;

            IBNET_VERBOSE(10, ("element=%d unpacking %d of %d\n", i, 1, sizeof(uint16_t)));
            IBNET_VERBOSE(10, ("Recive id %d\n", port->id));

            memcpy(&port->id, unpack_ptr, sizeof(uint16_t));
            unpack_ptr += sizeof(uint16_t);

            IBNET_VERBOSE(10, ("element=%d unpacking %d of %d\n", i, 1, sizeof(uint16_t)));
            IBNET_VERBOSE(10, ("Recive lid %d\n", port->lid));

            memcpy(&port->lid, unpack_ptr, sizeof(uint16_t));
            unpack_ptr += sizeof(uint16_t);

            IBNET_VERBOSE(10, ("element=%d unpacking %d of %d\n", i, 1, sizeof(uint64_t)));
            IBNET_VERBOSE(10, ("Recive subnet id %lx\n", port->subnet_id));

            memcpy(&port->subnet_id, unpack_ptr, sizeof(uint64_t));
            unpack_ptr += sizeof(uint64_t);

            IBNET_VERBOSE(10, ("element=%d unpacking %d of %d\n", i, 1, sizeof(uint32_t)));
            IBNET_VERBOSE(10, ("Recive mtu %d\n", port->mtu));

            memcpy(&port->mtu, unpack_ptr, sizeof(uint32_t));
            unpack_ptr += sizeof(uint32_t);

            IBNET_VERBOSE(10, ("element=%d unpacking %d of %d\n", i, 1, sizeof(uint8_t)));
            IBNET_VERBOSE(10, ("Recive offload %d\n", port->coll_offload));

            memcpy(&port->coll_offload, unpack_ptr, sizeof(uint8_t));
            unpack_ptr += sizeof(uint8_t);

            IBNET_VERBOSE(10, ("element=%d unpacking %d of %d\n", i, 1, sizeof(uint8_t)));
            IBNET_VERBOSE(10, ("Recive number of cpcs %d\n", port->num_cpcs));

            memcpy(&port->num_cpcs, unpack_ptr, sizeof(uint8_t));
            unpack_ptr += sizeof(uint8_t);

            port->pm_cpc_data = calloc(port->num_cpcs,
                    sizeof(ompi_common_ofacm_base_module_data_t));
            if (NULL == port->pm_cpc_data) {
                return OMPI_ERROR;
            }

            /* load cpc data */
            for (cpc = 0; cpc < port->num_cpcs; cpc++) {
                ompi_common_ofacm_base_module_data_t *cpc_data =
                    &port->pm_cpc_data[cpc];
                uint8_t cpc_index = -1;

                IBNET_VERBOSE(10, ("element=%d unpacking %d of %d\n", i, 1, sizeof(uint8_t)));
                IBNET_VERBOSE(10, ("Recive cpc index %d\n", cpc_index));

                memcpy(&cpc_index, unpack_ptr, sizeof(uint8_t));
                unpack_ptr += sizeof(uint8_t);

                cpc_data->cbm_component =
                    ompi_common_ofacm_base_get_cpc_byindex(cpc_index);
                if (NULL == cpc_data->cbm_component) {
                    IBNET_VERBOSE(10, ("Failed to resolve cpc index %d\n", cpc_index));
                    return OMPI_ERROR;
                }

                IBNET_VERBOSE(10, ("element=%d unpacking %d of %d\n", i, 1, sizeof(uint8_t)));
                IBNET_VERBOSE(10, ("Recive priority %d\n", cpc_data->cbm_priority));

                memcpy(&cpc_data->cbm_priority, unpack_ptr, sizeof(uint8_t));
                unpack_ptr += sizeof(uint8_t);

                IBNET_VERBOSE(10, ("element=%d unpacking %d of %d\n", i, 1, sizeof(uint8_t)));
                IBNET_VERBOSE(10, ("Recive cpc message len %d\n", cpc_data->cbm_modex_message_len));

                memcpy(&cpc_data->cbm_modex_message_len, unpack_ptr, sizeof(uint8_t));
                unpack_ptr += sizeof(uint8_t);

                if (0 != cpc_data->cbm_modex_message_len) {
                    int cpc_buflen = cpc_data->cbm_modex_message_len;

                    IBNET_VERBOSE(10, ("Recive cpc message data with len %d\n", cpc_buflen));
                    IBNET_VERBOSE(10, ("element=%d unpacking %d of %d\n", i, cpc_buflen, cpc_buflen));

                    memcpy(&cpc_data->cbm_modex_message, unpack_ptr, cpc_buflen);
                    unpack_ptr += (size_t) cpc_buflen;
                }
            }
        }

        /* Put the new proc to the list */
        opal_list_append(peers_data, (opal_list_item_t*) ibnet_proc);
    }

    assert((uint32_t) n_procs_in == opal_list_get_size(peers_data));
    return OMPI_SUCCESS;
}

static int cmp_cgroups(const void *p1, const void *p2)
{
    mca_sbgp_ibnet_connection_group_info_t *g1 =
        (mca_sbgp_ibnet_connection_group_info_t *)p1;
    mca_sbgp_ibnet_connection_group_info_t *g2 =
        (mca_sbgp_ibnet_connection_group_info_t *)p2;
    return (g2->num_procs - g1->num_procs);
}

static int set_ibnet_proc_on_cgroup(
                 mca_sbgp_ibnet_connection_group_info_t *cgroup,
                 mca_sbgp_ibnet_proc_t *ibnet_proc,
                 mca_sbgp_ibnet_device_t *device,
                 mca_sbgp_ibnet_module_t *module)
{
    uint32_t p;
    int k, rc, p_indx; /* port index in array of device */

    for (p_indx = 0; p_indx < device->num_allowed_ports; ++p_indx) {
        if (cgroup->port == device->ports[p_indx].id) {
            break;
        }
    }

    assert(device->num_act_ports > p_indx);

    if (NULL == ibnet_proc->use_port) {
        ibnet_proc->use_port = calloc(module->num_cgroups, sizeof(int));
        if (NULL == ibnet_proc->use_port) {
            IBNET_ERROR(("Failed to allocate use_port array."));
            return OMPI_ERROR;
        }
    }

    IBNET_VERBOSE(10, ("Local port is %d, idx - %d.\n",
                       device->ports[p_indx].id, p_indx));

    for(p = 0; p < ibnet_proc->num_ports; p++) {
        if (device->ports[p_indx].subnet_id  ==
                ibnet_proc->remote_ports_info[p].subnet_id) {
            ompi_common_ofacm_base_module_t *local_cpc = NULL;
            ompi_common_ofacm_base_module_data_t *remote_cpc_data = NULL;
            /* check if we have matching cpc on both sides */
            if (OMPI_SUCCESS !=
                    ompi_common_ofacm_base_find_match(device->cpcs,
                        device->num_cpcs,
                        ibnet_proc->remote_ports_info[p].pm_cpc_data,
                        ibnet_proc->remote_ports_info[p].num_cpcs,
                        &local_cpc,
                        &remote_cpc_data)) {
                /* Failed to match, can not use the port */
                IBNET_VERBOSE(10, ("Failed to match, can not use the port - %d.\n", p + 1));
                continue;
            }

            for (k = 0; k < module->num_cgroups && ((p + 1) != (uint32_t) ibnet_proc->use_port[k]); ++k)
                ;

            if (k < module->num_cgroups) {
                /* The port in use - another connection group use it */
                continue;
            }

            /* It means that connection group 'cgroup' communicates with
               this proc over its own remote port */
            ibnet_proc->use_port[cgroup->index] = p + 1;
            /* if it is no group array we need to create it*/
            if(OPAL_UNLIKELY(NULL == cgroup->ibnet_procs)) {
                cgroup->ibnet_procs = OBJ_NEW(opal_pointer_array_t);
                rc = opal_pointer_array_init(cgroup->ibnet_procs, 10, INT_MAX, 10);
                if (OPAL_SUCCESS != rc) {
                    IBNET_ERROR(("Failed to allocate opal_pointer_array"));
                    return OMPI_ERROR;
                }
            }

            IBNET_VERBOSE(10, ("Device idx %d, local port idx %d; "
                              "adding rank %d to the module %p, rem port %d",
                               device->device_index, p_indx, ibnet_proc->rank,
                               module, ibnet_proc->remote_ports_info[p].id));
            /* No need to remove: opal_list_remove_item(peers_data, (opal_list_item_t*)ibnet_proc); */
            rc = opal_pointer_array_set_item(cgroup->ibnet_procs,
                    /* num_selected, */ cgroup->num_procs,
                    (void *) ibnet_proc);
            if (OPAL_SUCCESS != rc) {
                IBNET_ERROR( ("Failed to set rank %d to index %d",
                            ibnet_proc->rank, 1 + cgroup->num_procs));
                return OMPI_ERROR;
            }

            /* put selected cpc data to this proc */
            ibnet_proc->remote_ports_info[p].local_cpc = local_cpc;
            ibnet_proc->remote_ports_info[p].remote_cpc_data = remote_cpc_data;

            ++cgroup->num_procs;
            /* we done for the proc, go to next one */
            break;
        }
    }

    return OMPI_SUCCESS;
}

static int setup_cgroup_all(
                mca_sbgp_ibnet_connection_group_info_t *cgroup,
                mca_sbgp_ibnet_device_t *device,
                mca_sbgp_ibnet_module_t *module,
                opal_list_t *peers_data)
{
    int rc;
    mca_sbgp_ibnet_proc_t *ibnet_proc = NULL;

    for (ibnet_proc = (mca_sbgp_ibnet_proc_t *) opal_list_get_first(peers_data);
            ibnet_proc != (mca_sbgp_ibnet_proc_t *) opal_list_get_end(peers_data);
            ibnet_proc  = (mca_sbgp_ibnet_proc_t *)
                opal_list_get_next((opal_list_item_t *)ibnet_proc)) {

        rc = set_ibnet_proc_on_cgroup(cgroup, ibnet_proc, device, module);
        if (OMPI_SUCCESS != rc) {
            return rc;
        }
    }

    return OMPI_SUCCESS;
}

static int setup_cgroup_node(mca_sbgp_ibnet_connection_group_info_t *cgroup, mca_sbgp_ibnet_device_t *device,
        mca_sbgp_ibnet_module_t *module, opal_list_t *peers_data)
{
    int rc, local = 0;
    mca_sbgp_ibnet_proc_t *ibnet_proc = NULL;

    for (ibnet_proc = (mca_sbgp_ibnet_proc_t *)opal_list_get_first(peers_data);
            ibnet_proc != (mca_sbgp_ibnet_proc_t *)opal_list_get_end(peers_data);
            ibnet_proc  = (mca_sbgp_ibnet_proc_t *)
            opal_list_get_next((opal_list_item_t *)ibnet_proc)) {

        local = OPAL_PROC_ON_LOCAL_NODE(ibnet_proc->ompi_proc->proc_flags);
        if (0 == local) {
            /* the remote process resides on different node */
            continue;
        }

        /* the process resides on the same machine */
        rc = set_ibnet_proc_on_cgroup(cgroup, ibnet_proc, device, module);
        if (OMPI_SUCCESS != rc) {
            return rc;
        }
    }

    return OMPI_SUCCESS;
}

/* The function should be the heart of the ibnet component.
 * Main purpose:
 *  The function should run over list of all peers and select only "reachable" peers.
 *  Peer that have subnet_id equal to subnet id that  I have on my ports is reachable.
 *  All peers that have the same number of active ports on the same subnet maybe grouped
 *  to subgroup?
 *  Need to think more about the select logic on this stage I just return list of all
 *  procs
 */
static int select_procs(mca_sbgp_ibnet_module_t *module, opal_list_t *peers_data)
{
    mca_sbgp_ibnet_device_t *device = NULL;
    mca_sbgp_ibnet_proc_t *ibnet_proc = NULL;
    mca_sbgp_ibnet_connection_group_info_t *cgroup = NULL;

    uint32_t p = 0;
    int i = 0, j, rc = OMPI_SUCCESS;
    int num_grouped = 0,
        groups_to_use = 1;

    mca_sbgp_ibnet_component_t *cs = &mca_sbgp_ibnet_component;

    IBNET_VERBOSE(10, ("Start to select procs.\n"));

    module->num_cgroups = 0;
    for (device = (mca_sbgp_ibnet_device_t *) opal_list_get_first(&cs->devices);
            device != (mca_sbgp_ibnet_device_t *) opal_list_get_end(&cs->devices);
            device  = (mca_sbgp_ibnet_device_t *)
            opal_list_get_next((opal_list_item_t *) device)) {
        module->num_cgroups += device->num_act_ports;
        IBNET_VERBOSE(10, ("Device num %d with index %d num of active ports %d\n",
                              ++i, device->device_index, device->num_act_ports));
    }

    module->cgroups = calloc(module->num_cgroups,
            sizeof(mca_sbgp_ibnet_connection_group_info_t));

    if (NULL == module->cgroups) {
        IBNET_ERROR(("Failed to allocate cgroups"));
        goto select_error;
    }

    IBNET_VERBOSE(10, ("Num of cgroups - %d.\n", module->num_cgroups));

    /* 1. Run over all active ports and build connection group
     * for each one */
    for (device = (mca_sbgp_ibnet_device_t *) opal_list_get_first(&cs->devices);
            device != (mca_sbgp_ibnet_device_t *) opal_list_get_end(&cs->devices);
            device  = (mca_sbgp_ibnet_device_t *)
            opal_list_get_next((opal_list_item_t *)device)) {
        /* run over active ports on the device */
        for(j = 0; j < device->num_act_ports; j++) {
            cgroup = &module->cgroups[num_grouped];

            /* Init cgroups structs */
            cgroup->device_index = device->device_index;
            cgroup->index = num_grouped;
            cgroup->port = device->ports[j].id;
            cgroup->num_procs = 0;

            /* Setup comunication group */
            switch(module->mode) {
                case MCA_SBGP_IBNET_ALL_NET:
                    rc = setup_cgroup_all(cgroup, device, module, peers_data);
                    break;
                case MCA_SBGP_IBNET_NODE_NET:
                    rc = setup_cgroup_node(cgroup, device, module, peers_data);
                    break;
                default:
                    rc = OMPI_ERROR;
                    IBNET_ERROR(("Module mode is unknow, fatal error"));
            }

            if (OMPI_SUCCESS != rc) {
                IBNET_ERROR(("Failed to setup cgroup."));
                goto select_error;
            }

            if (0 != cgroup->num_procs) {
                ++num_grouped;
            }
        }
    }

    if (0 == num_grouped) {
        /* No connection group was found */
        IBNET_ERROR(("No connection group was found."));
        goto select_error;
    }

    /* If we have more than one single cgroup,
     * we need to return groups that connects
     * to exactly the same peers
     */
    if (num_grouped > 1) {

        /* 2. Sort connection groups by size */
        qsort(module->cgroups, num_grouped,
                sizeof(mca_sbgp_ibnet_connection_group_info_t),
                cmp_cgroups);

        /* 3. What is the number of groups with maximal size */
        /* The first is Maximal */
        for (groups_to_use = 1; groups_to_use < num_grouped; groups_to_use++) {
            if (module->cgroups[0].num_procs != module->cgroups[groups_to_use].num_procs) {
                break;
            }
        }

        /* Ishai - It looks that noone is uses this groups_to_use value. In any case there is a bug in it. */
        /* 4. Check that all the maximal size groups are
         * connect to the same peers, if not we just use FIRST cgroup */
        if (groups_to_use > 1) {
            /* we need to check that all groups connects
             * the same set of peers. */
            for (j = groups_to_use - 1; j > 0; j--) {
                for (p = 0; p < module->cgroups[0].num_procs; p++) {
                    /* compare proc by proc....*/
                    if (opal_pointer_array_get_item(module->cgroups[0].ibnet_procs, p) !=
                            opal_pointer_array_get_item(module->cgroups[j].ibnet_procs, p)) {
                        /* peers are not equal, ignore this group and go to the next one */
                        groups_to_use--;
                        if (j != groups_to_use) {
                            /* it was not the last group, swap last and this one */
                            mca_sbgp_ibnet_connection_group_info_t tmp = module->cgroups[j];
                            module->cgroups[j] = module->cgroups[groups_to_use];
                            module->cgroups[groups_to_use] = tmp;
                        }

                        break; /* go to the next group */
                    }
                }
            }
        }
    }
    /* updating sgroup number */
    module->num_cgroups = groups_to_use;
    /* put array of ranks and size */

    module->super.group_size = module->cgroups[0].num_procs;
    module->super.group_list = (int *) calloc(module->super.group_size, sizeof(int));
    if (NULL == module->super.group_list) {
        IBNET_ERROR(("Failed to allocate memory for group list"));
        goto select_error;
    }

    for (i = 0; i < module->super.group_size; i++) {
        ibnet_proc = (mca_sbgp_ibnet_proc_t *)
            opal_pointer_array_get_item(module->cgroups[0].ibnet_procs, i);

        assert(NULL != ibnet_proc);
        IBNET_VERBOSE(10, ("Adding rank %d to group list", ibnet_proc->rank));

        module->super.group_list[i] = ibnet_proc->ompi_proc_index;
    }

    /* Let proc with lowest index be a leader of the subgroup */
    ibnet_proc = (mca_sbgp_ibnet_proc_t *)
        opal_pointer_array_get_item(module->cgroups[0].ibnet_procs, 0);

    assert(NULL != ibnet_proc);
    ibnet_proc->duty = MCA_SBGP_IBNET_NODE_LEADER;

#if OPAL_ENABLE_DEBUG
    IBNET_VERBOSE(10, ("Ibnet module: size - %d, num_cgroups - %d.\n",
                       module->super.group_size, module->num_cgroups));

    for (i = 0; i < module->num_cgroups; ++i) {
        IBNET_VERBOSE(10, ("cgroup %d uses port %d.\n",
                           i + 1, module->cgroups[i].port));
    }
#endif

    return OMPI_SUCCESS;

select_error:
    if (NULL != module->cgroups) {
        for (i = 0; i < num_grouped; i++) {
            if (NULL != module->cgroups[i].ibnet_procs) {
            /* Ishai: When do we destruct it if the fucntion was successful - only at the end of the process? */
                OBJ_DESTRUCT(module->cgroups[i].ibnet_procs);
            }
        }

        free(module->cgroups);
    }

    if (0 != module->super.group_size &&
            NULL != module->super.group_list) {
        free(module->super.group_list);
    }

    for (ibnet_proc = (mca_sbgp_ibnet_proc_t *) opal_list_get_first(peers_data);
          ibnet_proc != (mca_sbgp_ibnet_proc_t *) opal_list_get_end(peers_data);
                    ibnet_proc  = (mca_sbgp_ibnet_proc_t *)
                           opal_list_get_next((opal_list_item_t *) ibnet_proc)) {
        if (NULL != ibnet_proc->use_port) {
            free(ibnet_proc->use_port);
        }
    }

    return rc;
}

/* This routine is used to find the list of procs that run on the
** same host as the calling process.
*/

#define IBNET_ALL   "all"
#define IBNET_NODE  "node"

static int key2mode(char *key)
{
    if (NULL == key) {
        IBNET_VERBOSE(6, ("key is NULL, return  MCA_SBGP_IBNET_ALL"));
        return MCA_SBGP_IBNET_ALL_NET;
    }
    if (strlen(IBNET_ALL) == strlen(key) &&
            0 == strncmp(IBNET_ALL, key, strlen(IBNET_ALL))) {
        IBNET_VERBOSE(6, ("key is MCA_SBGP_IBNET_ALL"));
        return MCA_SBGP_IBNET_ALL_NET;
    }
    if (strlen(IBNET_NODE) == strlen(key) &&
            0 == strncmp(IBNET_NODE, key, strlen(IBNET_NODE))) {
        IBNET_VERBOSE(6, ("key is NODE"));
        return MCA_SBGP_IBNET_NODE_NET;
    }

    IBNET_VERBOSE(6, ("key was not detected, return MCA_SBGP_IBNET_NONE"));
    return MCA_SBGP_IBNET_NONE_NET;
}

static int mca_sbgp_ibnet_calc_sbuff_size(void)
{
    int bytes_tosend = 0, port, cpc;
    mca_sbgp_ibnet_device_t *device;

    opal_list_t *devices = &mca_sbgp_ibnet_component.devices;

    bytes_tosend += sizeof(uint32_t); /* OPAL_UINT32 rank */
    bytes_tosend += sizeof(uint32_t); /* OPAL_UINT32 num of active ports */

    /* Go through list of device and build the message*/
    for (device = (mca_sbgp_ibnet_device_t *) opal_list_get_first(devices);
            device != (mca_sbgp_ibnet_device_t *) opal_list_get_end(devices);
            device  = (mca_sbgp_ibnet_device_t *) opal_list_get_next((opal_list_item_t *) device)) {
        for (port = 0; port < device->num_allowed_ports; ++port) {
            if (!device->ports[port].used) {
                continue;
            }

            /* OPAL_UINT16 port num */
            bytes_tosend += sizeof(uint16_t);

            /* OPAL_UINT16 lid */
            bytes_tosend += sizeof(uint16_t);

            /* OPAL_UINT64 subnetid */
            bytes_tosend += sizeof(uint64_t);

            /* OPAL_UINT32 default mtu */
            bytes_tosend += sizeof(uint32_t);

            /* OPAL_UINT8 collectives offload support */
            bytes_tosend += sizeof(uint8_t);

            /* OPAL_UINT8 number of cpcs for this port */
            bytes_tosend += sizeof(uint8_t);

            for (cpc = 0; cpc < device->num_cpcs; ++cpc) {
                /* OPAL_UINT8 cpc index */
                bytes_tosend += sizeof(uint8_t);

                /* OPAL_UINT8 cpc priority */
                bytes_tosend += sizeof(uint8_t);

                /* cpc buffer length (OPAL_UINT8) in bytes */
                bytes_tosend += device->cpcs[cpc]->data.cbm_modex_message_len;
                bytes_tosend += sizeof(uint8_t);
            }
        }
    }

    return bytes_tosend;
}

mca_sbgp_base_module_t *mca_sbgp_ibnet_select_procs(struct ompi_proc_t **procs,
        int n_procs_in,
        struct ompi_communicator_t *comm,
        char *key,
        void *output_data
        )
{
    /* local variables */
    opal_list_t peers_data;
    mca_sbgp_ibnet_module_t *module;

    uint32_t rc;
    char *sbuff = NULL, *rbuff = NULL;

    int *sbgp_procs_ranks = NULL, *ranks_in_comm = NULL;
    int i, my_rank_in_group = -1, my_rank, num_bytes_tosend;

    struct mca_sbgp_ibnet_proc_t *ibnet_proc = NULL;
    mca_sbgp_ibnet_component_t *cs = &mca_sbgp_ibnet_component;

    /* Create the module */
    module = OBJ_NEW(mca_sbgp_ibnet_module_t);
    if (OPAL_UNLIKELY(NULL == module)) {
        return NULL;
    }

    module->num_cgroups = 0;
    module->cgroups = NULL;
    module->mode = key2mode(key);

    if (OPAL_UNLIKELY(MCA_SBGP_IBNET_NONE_NET == module->mode)) {
        goto Error_module;
    }

    module->super.group_size = 0;
    module->super.group_list = NULL;
    module->super.group_comm = comm;
    module->super.group_net = OMPI_SBGP_IBCX2;

    ranks_in_comm = (int *) malloc(n_procs_in * sizeof(int));
    if (OPAL_UNLIKELY(NULL == ranks_in_comm)) {
        IBNET_ERROR(("Cannot allocate memory.\n"));
        goto Error;
    }

    my_rank = ompi_comm_rank(&ompi_mpi_comm_world.comm);

    for (i = 0; i < n_procs_in; i++) {
        ranks_in_comm[i] = procs[i]->proc_name.vpid;
        if (my_rank == ranks_in_comm[i]) {
            my_rank_in_group = i;
        }
    }

    /* Prepare send data */
    num_bytes_tosend = mca_sbgp_ibnet_calc_sbuff_size();

    rc = comm_allreduce_pml(&num_bytes_tosend,
                            &num_bytes_tosend, 1,
                            MPI_INT, my_rank_in_group,
                            MPI_MAX, n_procs_in,
                            ranks_in_comm, &ompi_mpi_comm_world.comm);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        goto Error;
    }

    IBNET_VERBOSE(10, ("The size of the send buff is %d\n", num_bytes_tosend));

    assert(num_bytes_tosend > 0);

    /* Allocate send/recv buffers for allgather comunication */
    sbuff = (char *) malloc(num_bytes_tosend);
    rbuff = (char *) malloc(num_bytes_tosend * n_procs_in);
    if (OPAL_UNLIKELY(NULL == sbuff || NULL == rbuff)) {
        IBNET_ERROR(("Failed to allocate buffers for send/recv ibnet allgather\n"));
        goto Error;
    }

    rc = pack_gather_sbuff(sbuff);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        goto Error;
    }

    rc = comm_allgather_pml((void *) sbuff, (void *) rbuff,
                             num_bytes_tosend, MPI_BYTE,
                             my_rank_in_group, n_procs_in,
                             ranks_in_comm, &ompi_mpi_comm_world.comm);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        IBNET_ERROR(("Allgather call failed.\n"));
        goto Error;
    }

    /* Prepare list for arraving data */
    OBJ_CONSTRUCT(&peers_data, opal_list_t);

    /* Load the data to peers data */
    rc = unpack_and_load_gather_rbuff(rbuff, num_bytes_tosend,
                                procs, n_procs_in, &peers_data);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        goto Error;
    }

    /* Select logic */
    rc = select_procs(module, &peers_data);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        goto Error;
    }

    /* Put group id */
    sbgp_procs_ranks = (int *) malloc(module->super.group_size *
                                      sizeof(int));
    if (OPAL_UNLIKELY(NULL == sbgp_procs_ranks)) {
        IBNET_ERROR(("Cannot allocate memory.\n"));
        goto Error;
    }

    for (i = 0; i < module->super.group_size; ++i) {
        ibnet_proc = (struct mca_sbgp_ibnet_proc_t *)
                         opal_pointer_array_get_item(
                         module->cgroups[0].ibnet_procs, i);

        sbgp_procs_ranks[i] = ibnet_proc->ompi_proc->proc_name.vpid;
        if (my_rank == sbgp_procs_ranks[i]) {
            my_rank_in_group = i;
        }

    }

    assert(my_rank_in_group >= 0);

    rc = comm_allreduce_pml(&cs->curr_max_group_id,
                            &cs->curr_max_group_id, 1,
                            MPI_INT, my_rank_in_group,
                            MPI_MAX, module->super.group_size,
                            sbgp_procs_ranks, &ompi_mpi_comm_world.comm);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        goto Error;
    }

    module->group_id = cs->curr_max_group_id;
    cs->curr_max_group_id++;

    /* successful completion */
    /* clean up the temporary structures */
    OBJ_DESTRUCT(&peers_data);

    free(sbuff);
    free(rbuff);

    free(ranks_in_comm);
    free(sbgp_procs_ranks);

    IBNET_VERBOSE(10, ("Return ibnet module.\n"));
    return (mca_sbgp_base_module_t *) module;

    /* return with error */
Error:
    /* clean up */
    if(NULL != module->super.group_list) {
        free(module->super.group_list);
        module->super.group_list = NULL;
    }

    /* clean up the temporary structures */
    OBJ_DESTRUCT(&peers_data);

    if (NULL != sbgp_procs_ranks) {
        free(sbgp_procs_ranks);
    }

    if (NULL != ranks_in_comm) {
        free(ranks_in_comm);
    }

    if (NULL != sbuff) {
        free(sbuff);
    }

    if (NULL != rbuff) {
        free(rbuff);
    }

Error_module:
    OBJ_RELEASE(module);

    return NULL;
}
