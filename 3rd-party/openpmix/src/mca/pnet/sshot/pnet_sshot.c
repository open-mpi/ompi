/*
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 *
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"

#include <string.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif
#ifdef HAVE_SYS_TIME_H
#    include <sys/time.h>
#endif
#include <jansson.h>
#include <time.h>

#include "pmix_common.h"

#include "src/class/pmix_list.h"
#include "src/include/pmix_globals.h"
#include "src/mca/pcompress/pcompress.h"
#include "src/mca/preg/preg.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_name_fns.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_printf.h"
#include "src/util/pmix_show_help.h"

#include "pnet_sshot.h"
#include "src/mca/pnet/base/base.h"
#include "src/mca/pnet/pnet.h"

static pmix_status_t sshot_init(void);
static void sshot_finalize(void);
static pmix_status_t allocate(pmix_namespace_t *nptr,
                              pmix_info_t info[], size_t ninfo,
                              pmix_list_t *ilist);
static pmix_status_t setup_local_network(pmix_nspace_env_cache_t *nptr,
                                         pmix_info_t info[], size_t ninfo);
pmix_pnet_module_t pmix_sshot_module = {
    .name = "sshot",
    .init = sshot_init,
    .finalize = sshot_finalize,
    .allocate = allocate,
    .setup_local_network = setup_local_network,
    .register_fabric = pmix_pnet_sshot_register_fabric
};

/*    FORWARD-DECLARE LOCAL FUNCTIONS    */
static pmix_status_t compute_endpoint(pmix_endpoint_t *endpt, char *xname, uint16_t lrank);
static void compute_coord(pmix_coord_t *coord, char *xname, pmix_coord_view_t view);

/*    LOCAL VARIABLES */
static bool sessioninfo = false;

/*****     APIs     *****/

static pmix_status_t sshot_init(void)
{
    return PMIX_SUCCESS;
}

static void sshot_finalize(void)
{
}

/* PMIx_server_setup_application calls the "allocate" function
 * to allow us an opportunity to assign fabric resources to the
 * job when it is launched. The following data is included in
 * the function call:
 *
 * PMIX_NODE_MAP: a regular expression identifying the nodes
 *                being used by the job
 *
 * PMIX_PROC_MAP: a regular expressing identifying the ranks
 *                that will be executing on each node
 *
 * PMIX_ALLOC_FABRIC: a pmix_data_array_t of attributes identifying
 *                the fabric resources that are to be included in
 *                the allocation. May include some combination of:
 *
 *         - PMIX_ALLOC_FABRIC_ID: a string identifier for this
 *                allocation for tracking purposes
 *
 *         - PMIX_ALLOC_FABRIC_SEC_KEY: allocate a security credential
 *                for this job
 *
 *         - PMIX_SETUP_APP_ENVARS: harvest any sshot-related envars
 *                from the local environment and include them for
 *                forwarding to the backend
 *
 * NOTE: the component is always allowed to return any data it feels
 * is relevant to the execution of an application. The only requirement
 * on the host is that it must pass the PMIX_NODE_MAP and PMIX_PROC_MAP
 * attributes so the component can know the nodes involved and where procs
 * are expected to land. Other attributes may be provided to help components
 * direct optional behavior - e.g., if the user directed that only a particular
 * fabric component allocate resources for a given job (e.g., someone wanting
 * to exclusively use the "socket" fabric).
 *
 * Also note that the component is not REQUIRED to use the input node/proc
 * maps if it has an alternative method for accessing whatever information
 * it requires. For example, if the component can obtain the information
 * directly from the host system via a REST call, then it is welcome to do
 * so. Inclusion of the maps is done for portability to support cases where
 * the fabric is available in a variety of systems. This may not be the case
 * for this particular component, and so the option of directly obtaining the
 * maps is acknowledged.
 *
 * The endpoint in this case is a combination of the MAC address of the NIC
 * plus the local rank of the proc. The local rank of each proc in the job
 * is included when the host calls PMIx_server_register_nspace, so we will
 * be given that information on the backend. Thus, the backend server on the
 * compute node can compute the address of each proc if we simply provide it
 * with the MAC addresses of all NICs on each node in the job.
 *
 * We might also be able to take advantage of the "xname" naming scheme used
 * by the fabric to avoid having to send coordinates to each node by assigning
 * the NIC's xname as its UUID. The coordinate could then be computed on the
 * backend from that value.
 *
 * Another optimization applies to workflow-like environments where multiple
 * jobs may be launched within the same allocation. In this scenario, the host
 * may choose to pass the PMIX_SESSION_INFO attribute indicating that the node
 * list being provided contains all the nodes in the session and not just those
 * assigned to the particular job. This means that the component should track
 * that it has provided the information for ALL the nodes, and thus does not
 * need to provide any further information as other jobs are started. Alternatively,
 * the component could track which nodes have already been covered and avoid
 * resending information on those nodes.
 *
 * Bottom line: there is potentially significant optimization to reduce
 * the amount of info returned by the "allocate" function, which will in turn
 * reduce the size of the launch message and help scalability.
 *
 * The returned data shall be added to the list of values contained in the
 * "ilist" parameter using a unique attribute identifying it as belonging
 * to this component. The list of all returned data (obtained from all
 * active components in the pnet framework as well as other possible
 * sources within PMIx) shall be delivered to PMIx_server_setup_local_support
 * on compute nodes - this function will in turn call our "setup_local_support"
 * entry point so we can find our component attribute and act on the included
 * allocation information.
 *
 * The following implementation assumes the following optimizations:
 *
 * (a) the endpoints for each proc will be computed on the backend - thus,
 *     only the MAC addresses are "shipped" and the allocate function does
 *     not require access to the list of procs on each node
 *
 * (b) the NIC's xname will be used as its UUID, and that string can be
 *     parsed to obtain the device coordinates. Thus, the coordinates
 *     will also be computed on the backend
 *
 * (c) the final "blob" that is returned to the caller shall be compressed
 *     prior to adding it to the input "ilist"
 */
static pmix_status_t allocate(pmix_namespace_t *nptr,
                              pmix_info_t info[], size_t ninfo,
                              pmix_list_t *ilist)
{
    int n, m;

    /* Values retrieved from the fabric controller */
    char *vni = NULL;      // VNI assigned to the job by the FC
    int tclass = 0;        // Traffic class assigned by the FC
    char **macs = NULL;    // Argv-array of comma-delimited MAC addresses of NICs on each node
    char **xnames = NULL;  // Argv-array of comma-delimited xnames of NICs on each node
    char **osnames = NULL; // Argv-array of comma-delimited OS names of NICs on each node

    /* Values computed here as well as scratch storage */
    char **nodes = NULL; // Argv-array of hostnames
    char **targs = NULL;
    char *tmp;
    pmix_status_t rc;
    pmix_buffer_t mydata; // Buffer used to store information to be transmitted (scratch storage)
    pmix_kval_t *kv;
    pmix_byte_object_t bo;
    bool sinfo = false;
    struct timeval start = {0, 0}, end;

    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                        "pnet:sshot:allocate for nspace %s", nptr->nspace);

    /* Assume that we always provide the following information:
     *
     * Traffic class info, if supported
     * Each process gets an endpoint for each local NIC
     * VNI
     * Device coordinates (both physical and logical)
     *
     */
    if (0 == pmix_mca_pnet_sshot_component.numnodes) {
        /* check directives to get the node map */
        for (n = 0; n < (int) ninfo; n++) {
            pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                                "pnet:sshot:allocate processing key %s", info[n].key);
            if (PMIX_CHECK_KEY(&info[n], PMIX_NODE_MAP)) {
                /* passed to us as a regex - parse to retrieve the argv-array
                 * of nodes */
                rc = pmix_preg.parse_nodes(info[n].value.data.string, &nodes);
                if (PMIX_SUCCESS != rc) {
                    return PMIX_ERR_BAD_PARAM;
                }
            } else if (PMIX_CHECK_KEY(&info[n], PMIX_SESSION_INFO)) {
                sinfo = PMIX_INFO_TRUE(&info[n]);
            }
        }
    } else {
        for (n = 0; n < pmix_mca_pnet_sshot_component.numnodes; n++) {
            pmix_asprintf(&tmp, "nid%06d", n);
            PMIx_Argv_append_nosize(&nodes, tmp);
            free(tmp);

            for (m = 0; m < pmix_mca_pnet_sshot_component.numnodes; m++) {
                pmix_asprintf(&tmp, "%02d:%02d:%02d:%02d:%02d:%02d", n % 100, (n + 1) % 100,
                              (n + 2) % 100, (n + 3) % 100, (n + 4) % 100, m);
                PMIx_Argv_append_nosize(&targs, tmp);
                free(tmp);
            }
            tmp = PMIx_Argv_join(targs, ',');
            PMIx_Argv_append_nosize(&macs, tmp);
            free(tmp);
            PMIx_Argv_free(targs);
            targs = NULL;

            for (m = 0; m < pmix_mca_pnet_sshot_component.numnodes; m++) {
                pmix_asprintf(&tmp, "x30000c%1dr%02da%04d", n % 10, n % 100, m);
                PMIx_Argv_append_nosize(&targs, tmp);
                free(tmp);
            }
            tmp = PMIx_Argv_join(targs, ',');
            PMIx_Argv_append_nosize(&xnames, tmp);
            free(tmp);
            PMIx_Argv_free(targs);
            targs = NULL;

            for (m = 0; m < pmix_mca_pnet_sshot_component.numnodes; m++) {
                pmix_asprintf(&tmp, "eth%1d", m);
                PMIx_Argv_append_nosize(&targs, tmp);
                free(tmp);
            }
            tmp = PMIx_Argv_join(targs, ',');
            PMIx_Argv_append_nosize(&osnames, tmp);
            free(tmp);
            PMIx_Argv_free(targs);
            targs = NULL;
        }
        /* start timing here */
        gettimeofday(&start, NULL);
    }

    /* if we weren't told the nodes, then we cannot do anything - note that
     * this may not be true in the case where we have previously obtained
     * the session-level info on the nodes. In subsequent calls, it may
     * be that the only thing required is to get the VNI and traffic
     * class for the job, which may not require passing the list of
     * involved nodes to the fabric controller */
    if (NULL == nodes) {
        pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                            "pnet:sshot no nodes provided");
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }

    /* setup a buffer - we will pack the info into it for transmission to
     * the backend compute node daemons */
    PMIX_CONSTRUCT(&mydata, pmix_buffer_t);

    /* pass the full set of nodes for the job or session to the fabric
     * controller
     */
    if (NULL == macs || NULL == xnames || NULL == osnames) {
        /* cannot proceed */
        PMIx_Argv_free(nodes);
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }
    /* Get back the following:
     *
     * VNI for the job
     */
    vni = "VNI";
    /* pack the security credential - I'm not sure what form the VNI is
     * in, but will assume for now that it is a string */
    PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &mydata, &vni, 1, PMIX_STRING);

    /* store the VNI locally so our host can retrieve it if needed */
    PMIX_KVAL_NEW(kv, PMIX_CREDENTIAL);
    kv->value->type = PMIX_STRING;
    kv->value->data.string = vni;
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &pmix_globals.myid, PMIX_LOCAL, kv);
    PMIX_RELEASE(kv); // maintain refcount
    if (PMIX_SUCCESS != rc) {
        return rc;
    }

    /* Traffic class (if supported)
     */
    tclass = 1234;
    /* pack the traffic class, if we have it - I'm not sure what form that
     * will take, but will assume for now that it is an integer */
    PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &mydata, &tclass, 1, PMIX_INT);

    /* store the traffic class locally so our host can retrieve it if needed */
    PMIX_KVAL_NEW(kv, "HPE_TRAFFIC_CLASS");
    kv->value->type = PMIX_INT;
    kv->value->data.integer = tclass;
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &pmix_globals.myid, PMIX_LOCAL, kv);
    PMIX_RELEASE(kv); // maintain refcount
    if (PMIX_SUCCESS != rc) {
        return rc;
    }

    if (sessioninfo) {
        /* we have already provided all the fabric endpoints for nodes
         * within this allocation - no need to do it again */
        goto complete;
    }

    /* NICs on each node - assume you put these into an argv array by node, with
     *       each entry containing a comma-delimited string of the MAC addresses
     *       on that node. Take num_nics_max to be the highest number of NICs on
     *       any node
     */

    /* xnames of NICs on each node - assume you put these into an argv array by node, with
     *       each entry containing a comma-delimited string of the xnames of the
     *       NICs on that node
     */

    /* OS names of each NIC on each node - assume you put these into an argv array by node, with
     *       each entry containing a comma-delimited string of the OS names of the
     *       interfaces for the NICs on that node
     */

    /* for each node... */
    for (n = 0; NULL != nodes[n]; n++) {
        /* pack the node name - might be able to avoid this or at least reduce its
         * size. For example, we could assume that the node map on the compute
         * end will be provided in the same order as given here - thus, we could
         * correlate the address info with the node using the order. Or we could
         * use an integer node ID if one is available as that would likely be
         * shorter than a string name
         *
         * For this prototype, we will just include the node name
         */
        PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &mydata, &nodes[n], 1, PMIX_STRING);
        /* pack the MAC addresses on this node */
        PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &mydata, &macs[n], 1, PMIX_STRING);
        /* pack the xnames of the NICs on this node */
        PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &mydata, &xnames[n], 1, PMIX_STRING);
        /* pack the osnames of the NICs on this node */
        PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &mydata, &osnames[n], 1, PMIX_STRING);
    }

complete:
    /* load all our results into a buffer for xmission to the backend */
    PMIX_KVAL_NEW(kv, PMIX_PNET_SSHOT_BLOB);
    if (NULL == kv || NULL == kv->value) {
        PMIX_RELEASE(kv);
        PMIX_DESTRUCT(&mydata);
        return PMIX_ERR_NOMEM;
    }
    kv->value->type = PMIX_BYTE_OBJECT;
    PMIX_UNLOAD_BUFFER(&mydata, bo.bytes, bo.size);
    /* to help scalability, compress this blob */
    if (pmix_compress.compress((uint8_t *) bo.bytes, bo.size,
                               (uint8_t **) &kv->value->data.bo.bytes, &kv->value->data.bo.size)) {
        kv->value->type = PMIX_COMPRESSED_BYTE_OBJECT;
    } else {
        kv->value->data.bo.bytes = bo.bytes;
        kv->value->data.bo.size = bo.size;
    }
    PMIX_DESTRUCT(&mydata);
    pmix_list_append(ilist, &kv->super);

    /* if this is info covers the session, mark it */
    if (sinfo) {
        sessioninfo = true;
    }

    if (0 < pmix_mca_pnet_sshot_component.numnodes) {
        gettimeofday(&end, NULL);
        pmix_output(0, "TIME SPENT ALLOCATING DATA: %f seconds",
                    (float) (end.tv_sec - start.tv_sec)
                        + (float) (end.tv_usec - start.tv_usec) / 1000000.0);
    }
    /* be sure to release all the data from the fabric controller! */
    return PMIX_SUCCESS;
}

/* PMIx_server_setup_local_support calls the "setup_local_network" function.
 * The Standard requires that this come _after_ the host calls the
 * PMIx_server_register_nspace function to ensure that any required information
 * is available to the components. Thus, we have the PMIX_NODE_MAP and
 * PMIX_PROC_MAP available to us and can use them here.
 *
 * When the host calls "setup_local_support", it passes down an array
 * containing the information the "lead" server (e.g., "mpirun") collected
 * from PMIx_server_setup_application. In this case, we search for a blob
 * that our "allocate" function may have included in that info.
 */
static pmix_status_t setup_local_network(pmix_nspace_env_cache_t *nptr,
                                         pmix_info_t info[], size_t ninfo)
{
    size_t n, ndevs, m, d, ndims = 3;
    pmix_buffer_t bkt;
    int32_t cnt;
    char *vni;
    int tclass;
    char *hostname;
    char *macstring = NULL;
    char **macs = NULL;
    char *xnamestring = NULL;
    char **xnames = NULL;
    char *osnamestring = NULL;
    char **osnames = NULL;
    char *peers = NULL;
    char **prs = NULL;
    pmix_data_array_t *darray, *ndinfo;
    pmix_kval_t *kv;
    pmix_info_t *itmp, *iptr = NULL, lpeers[2];
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_proc_t proc;
    struct timeval start = {0, 0}, end;
    pmix_geometry_t *geometry;
    pmix_endpoint_t *endpts;
    uint8_t *data;
    size_t size;
    bool restore = false;
    pmix_rank_t rank = 0;
    pmix_cb_t cb;

    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                        "pnet:sshot:setup_local_network with %lu info", (unsigned long) ninfo);

    /* setup the namespace for the job */
    PMIX_LOAD_NSPACE(proc.nspace, nptr->ns->nspace);
    /* prep the unpack buffer */
    PMIX_CONSTRUCT(&bkt, pmix_buffer_t);

    for (n = 0; n < ninfo; n++) {
        /* look for my key */
        if (PMIX_CHECK_KEY(&info[n], PMIX_PNET_SSHOT_BLOB)) {
            pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                                "pnet:sshot:setup_local_network found my blob");

            if (0 < pmix_mca_pnet_sshot_component.numnodes) {
                gettimeofday(&start, NULL);
            }

            /* cache this so we can restore the payload after processing it.
             * This is necessary as the incoming info array belongs to our
             * host and so we cannot alter it */
            iptr = &info[n];

            /* if this is a compressed byte object, decompress it */
            if (PMIX_COMPRESSED_BYTE_OBJECT == info[n].value.type) {
                pmix_compress.decompress(&data, &size, (uint8_t *) info[n].value.data.bo.bytes,
                                         info[n].value.data.bo.size);
            } else {
                data = (uint8_t *) info[n].value.data.bo.bytes;
                size = info[n].value.data.bo.size;
                restore = true;
            }

            /* this macro NULLs and zero's the incoming bo */
            PMIX_LOAD_BUFFER(pmix_globals.mypeer, &bkt, data, size);

            /* unpack the VNI */
            cnt = 1;
            PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer, &bkt, &vni, &cnt, PMIX_STRING);
            if (PMIX_SUCCESS != rc) {
                goto cleanup;
            }

            /* load it into the CXI service */

            /* add it to the job-level info */
            proc.rank = PMIX_RANK_WILDCARD;
            PMIX_KVAL_NEW(kv, PMIX_CREDENTIAL);
            kv->value->type = PMIX_STRING;
            kv->value->data.string = vni;
            PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &proc, PMIX_INTERNAL, kv);
            PMIX_RELEASE(kv); // maintain refcount
            if (PMIX_SUCCESS != rc) {
                goto cleanup;
            }

            /* unpack the traffic class */
            cnt = 1;
            PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer, &bkt, &tclass, &cnt, PMIX_INT);
            if (PMIX_SUCCESS != rc) {
                goto cleanup;
            }

            /* load it into the CXI service */

            /* add it to the job info */
            PMIX_KVAL_NEW(kv, "HPE_TRAFFIC_CLASS");
            kv->value->type = PMIX_INT;
            kv->value->data.integer = tclass;
            PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &proc, PMIX_INTERNAL, kv);
            PMIX_RELEASE(kv); // maintain refcount
            if (PMIX_SUCCESS != rc) {
                goto cleanup;
            }

            /* while there are node names to unpack... */
            cnt = 1;
            PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer, &bkt, &hostname, &cnt, PMIX_STRING);
            while (PMIX_SUCCESS == rc) {
                /* prep the node info data array */
                PMIX_DATA_ARRAY_CREATE(ndinfo, 2, PMIX_INFO);
                itmp = (pmix_info_t *) ndinfo->array;

                /* insert the name into the nodeinfo array */
                PMIX_INFO_LOAD(&itmp[0], PMIX_HOSTNAME, hostname, PMIX_STRING);

                /* unpack the MAC addresses of the NICs on this node */
                cnt = 1;
                PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer, &bkt, &macstring, &cnt, PMIX_STRING);
                if (PMIX_SUCCESS != rc) {
                    free(hostname);
                    break;
                }
                macs = PMIx_Argv_split(macstring, ',');
                free(macstring);
                /* unpack the xnames of the NICs on this node */
                cnt = 1;
                PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer, &bkt, &xnamestring, &cnt, PMIX_STRING);
                if (PMIX_SUCCESS != rc) {
                    free(hostname);
                    break;
                }
                xnames = PMIx_Argv_split(xnamestring, ',');
                free(xnamestring);
                /* unpack the osnames of the NICs on this node */
                cnt = 1;
                PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer, &bkt, &osnamestring, &cnt, PMIX_STRING);
                if (PMIX_SUCCESS != rc) {
                    free(hostname);
                    break;
                }
                osnames = PMIx_Argv_split(osnamestring, ',');
                free(osnamestring);

                /* create the array of geometry objects for this node */
                ndevs = PMIx_Argv_count(macs);
                PMIX_GEOMETRY_CREATE(geometry, ndevs);

                /* load the objects */
                for (m = 0; m < ndevs; m++) {
                    geometry[m].uuid = strdup(xnames[m]);
                    geometry[m].osname = strdup(osnames[m]);
                    geometry[m].ncoords = 2;
                    PMIX_COORD_CREATE(geometry[m].coordinates, geometry[m].ncoords, ndims);
                    compute_coord(&geometry[m].coordinates[0], xnames[m], PMIX_COORD_LOGICAL_VIEW);
                    compute_coord(&geometry[m].coordinates[1], xnames[m], PMIX_COORD_PHYSICAL_VIEW);
                }
                /* add it to the node info array */
                darray = (pmix_data_array_t *) malloc(sizeof(pmix_data_array_t));
                darray->type = PMIX_GEOMETRY;
                darray->size = ndevs;
                darray->array = geometry;
                PMIX_INFO_LOAD(&itmp[1], PMIX_FABRIC_COORDINATES, darray, PMIX_DATA_ARRAY);

                /* store it */
                proc.rank = PMIX_RANK_WILDCARD;
                PMIX_KVAL_NEW(kv, PMIX_NODE_INFO_ARRAY);
                kv->value->type = PMIX_DATA_ARRAY;
                kv->value->data.darray = ndinfo;
                PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &proc, PMIX_LOCAL, kv);
                PMIX_RELEASE(kv); // maintain refcount
                if (PMIX_SUCCESS != rc) {
                    free(hostname);
                    goto cleanup;
                }

                /* get the list of local peers for this node */
                if (0 < pmix_mca_pnet_sshot_component.numnodes) {
                    if (0 < pmix_mca_pnet_sshot_component.ppn) {
                        /* simulating procs */
                        prs = NULL;
                        for (m = 0; m < (size_t) pmix_mca_pnet_sshot_component.ppn; m++) {
                            pmix_asprintf(&peers, "%d", (int) rank);
                            PMIx_Argv_append_nosize(&prs, peers);
                            free(peers);
                            ++rank;
                        }
                    }
                } else {
                    /* our host is required to have called register_nspace
                     * before calling us, so we should be able to retrieve
                     * this value from the GDS */
                    PMIX_CONSTRUCT(&cb, pmix_cb_t);
                    proc.rank = PMIX_RANK_WILDCARD;
                    cb.proc = &proc;
                    cb.key = PMIX_LOCAL_PEERS;
                    PMIX_INFO_LOAD(&lpeers[0], PMIX_NODE_INFO, NULL, PMIX_BOOL);
                    PMIX_INFO_LOAD(&lpeers[1], PMIX_HOSTNAME, hostname, PMIX_STRING);
                    cb.info = lpeers;
                    cb.ninfo = 2;
                    PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb);
                    cb.key = NULL;
                    if (PMIX_SUCCESS != rc) {
                        PMIX_ERROR_LOG(rc);
                        PMIX_DESTRUCT(&cb);
                        return rc;
                    }
                    /* the data is the first value on the cb.kvs list */
                    if (1 != pmix_list_get_size(&cb.kvs)) {
                        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
                        PMIX_DESTRUCT(&cb);
                        return PMIX_ERR_BAD_PARAM;
                    }
                    kv = (pmix_kval_t *) pmix_list_get_first(&cb.kvs);
                    if (NULL != kv->value->data.string) {
                        prs = PMIx_Argv_split(kv->value->data.string, ',');
                    } else {
                        prs = NULL;
                    }
                    PMIX_DESTRUCT(&cb);
                }
                /* if this node is hosting procs for this job, then
                 * construct endpts for each of them */
                if (NULL != prs) {
                    for (m = 0; NULL != prs[m]; m++) {
                        /* create an array of endpoints for this proc */
                        PMIX_ENDPOINT_CREATE(endpts, ndevs);
                        darray = (pmix_data_array_t *) malloc(sizeof(pmix_data_array_t));
                        darray->type = PMIX_ENDPOINT;
                        darray->size = ndevs;
                        darray->array = endpts;
                        /* for each fabric device, provide an endpt */
                        for (d = 0; d < ndevs; d++) {
                            endpts[m].uuid = strdup(xnames[d]);
                            endpts[m].osname = strdup(osnames[d]);
                            compute_endpoint(&endpts[m], macs[d], m);
                        }
                        /* store the result */
                        proc.rank = strtoul(prs[m], NULL, 10);
                        PMIX_KVAL_NEW(kv, PMIX_FABRIC_ENDPT);
                        kv->value->type = PMIX_DATA_ARRAY;
                        kv->value->data.darray = darray;
                        PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &proc, PMIX_LOCAL, kv);
                        PMIX_RELEASE(kv); // maintain refcount
                        if (PMIX_SUCCESS != rc) {
                            goto cleanup;
                        }
                    }
                    PMIx_Argv_free(prs);
                }

                /* cleanup */
                PMIx_Argv_free(macs);
                PMIx_Argv_free(xnames);
                PMIx_Argv_free(osnames);
                free(hostname);

                /* get the next hostname */
                cnt = 1;
                PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer, &bkt, &hostname, &cnt, PMIX_STRING);
            }

            /* we are done */
            break;
        }
    }

cleanup:
    if (restore) {
        /* restore the incoming data */
        iptr->value.data.bo.bytes = bkt.base_ptr;
        iptr->value.data.bo.size = bkt.bytes_used;
    }
    if (0 < pmix_mca_pnet_sshot_component.numnodes) {
        gettimeofday(&end, NULL);
        pmix_output(0, "TIME SPENT CONSTRUCTING BACKEND DATA: %f seconds",
                    (float) (end.tv_sec - start.tv_sec)
                        + (float) (end.tv_usec - start.tv_usec) / 1000000.0);
    }
    return PMIX_SUCCESS;
}

static void compute_coord(pmix_coord_t *coord, char *xname, pmix_coord_view_t view)
{
    PMIX_HIDE_UNUSED_PARAMS(xname);

    /* assume three dimensions */
    coord->view = view;
    coord->coord = (uint32_t *) malloc(3 * sizeof(uint32_t));
    /* do something to compute the coord */
    coord->coord[0] = 1;
    coord->coord[1] = 2;
    coord->coord[2] = 3;
}

static pmix_status_t compute_endpoint(pmix_endpoint_t *endpt, char *mac, uint16_t lrank)
{
    /* compute the endpt - can be string or any byte array */
    pmix_asprintf(&endpt->endpt.bytes, "%s:%d", mac, (int) lrank);
    endpt->endpt.size = strlen(endpt->endpt.bytes) + 1; // ensure the NULL is included

    return PMIX_SUCCESS;
}
