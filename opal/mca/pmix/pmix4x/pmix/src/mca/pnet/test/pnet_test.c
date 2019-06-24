/*
 * Copyright (c) 2015-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <src/include/pmix_config.h>

#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#include <time.h>

#include <pmix_common.h>

#include "src/mca/base/pmix_mca_base_var.h"
#include "src/include/pmix_socket_errno.h"
#include "src/include/pmix_globals.h"
#include "src/class/pmix_list.h"
#include "src/class/pmix_pointer_array.h"
#include "src/util/alfg.h"
#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/name_fns.h"
#include "src/util/output.h"
#include "src/util/pmix_environ.h"
#include "src/mca/preg/preg.h"

#include "src/mca/pnet/pnet.h"
#include "src/mca/pnet/base/base.h"
#include "pnet_test.h"

static pmix_status_t test_init(void);
static void test_finalize(void);
static pmix_status_t allocate(pmix_namespace_t *nptr,
                              pmix_info_t info[], size_t ninfo,
                              pmix_list_t *ilist);
static pmix_status_t setup_local_network(pmix_namespace_t *nptr,
                                         pmix_info_t info[],
                                         size_t ninfo);
static pmix_status_t setup_fork(pmix_namespace_t *nptr,
                                const pmix_proc_t *proc,
                                char ***env);
static void child_finalized(pmix_proc_t *peer);
static void local_app_finalized(pmix_namespace_t *nptr);
static void deregister_nspace(pmix_namespace_t *nptr);
static pmix_status_t collect_inventory(pmix_info_t directives[], size_t ndirs,
                                       pmix_inventory_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t deliver_inventory(pmix_info_t info[], size_t ninfo,
                                       pmix_info_t directives[], size_t ndirs,
                                       pmix_op_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t register_fabric(pmix_fabric_t *fabric,
                                     const pmix_info_t directives[],
                                     size_t ndirs);
static pmix_status_t deregister_fabric(pmix_fabric_t *fabric);
static pmix_status_t get_num_verts(pmix_fabric_t *fabric, uint32_t *nverts);
static pmix_status_t get_cost(pmix_fabric_t *fabric,
                              uint32_t src, uint32_t dest,
                              uint16_t *cost);
static pmix_status_t get_vertex(pmix_fabric_t *fabric,
                                uint32_t i,
                                pmix_value_t *identifier,
                                char **nodename);
static pmix_status_t get_index(pmix_fabric_t *fabric,
                               pmix_value_t *identifier,
                               uint32_t *i,
                               char **nodename);
pmix_pnet_module_t pmix_test_module = {
    .name = "test",
    .init = test_init,
    .finalize = test_finalize,
    .allocate = allocate,
    .setup_local_network = setup_local_network,
    .setup_fork = setup_fork,
    .child_finalized = child_finalized,
    .local_app_finalized = local_app_finalized,
    .deregister_nspace = deregister_nspace,
    .collect_inventory = collect_inventory,
    .deliver_inventory = deliver_inventory,
    .register_fabric = register_fabric,
    .deregister_fabric = deregister_fabric,
    .get_num_vertices = get_num_verts,
    .get_cost = get_cost,
    .get_vertex = get_vertex,
    .get_index = get_index
};

/* internal tracking structures */
typedef struct {
    pmix_list_item_t super;
    char *name;
    int index;
    void *node;     // pointer to node hosting this nic
    void *s;        // pointer to switch hosting this port, or
                    // pointer to switch this nic is attached to
    void *plane;    // pointer to plane this NIC is attached to
    void *link;     // nic this nic is connected to
} pnet_nic_t;
static void ncon(pnet_nic_t *p)
{
    p->name = NULL;
    p->index = -1;
    p->node = NULL;
    p->s = NULL;
    p->plane = NULL;
    p->link = NULL;
}
static void ndes(pnet_nic_t *p)
{
    if (NULL != p->name) {
        free(p->name);
    }
}
static PMIX_CLASS_INSTANCE(pnet_nic_t,
                           pmix_list_item_t,
                           ncon, ndes);

typedef struct {
    pmix_list_item_t super;
    char *name;
    int index;
    void *left;  // switch to the left of this one in the ring
    pnet_nic_t leftport;
    void *right; // switch to the right of this one in the ring
    pnet_nic_t rightport;
    pmix_list_t ports;  // NICs included in the switch
} pnet_switch_t;
static void scon(pnet_switch_t *p)
{
    p->name = NULL;
    p->index = -1;
    p->left = NULL;
    p->right = NULL;
    PMIX_CONSTRUCT(&p->leftport, pnet_nic_t);
    PMIX_CONSTRUCT(&p->rightport, pnet_nic_t);
    PMIX_CONSTRUCT(&p->ports, pmix_list_t);
}
static void sdes(pnet_switch_t *p)
{
    if (NULL != p->name) {
        free(p->name);
    }
    PMIX_DESTRUCT(&p->leftport);
    PMIX_DESTRUCT(&p->rightport);
    PMIX_LIST_DESTRUCT(&p->ports);
}
static PMIX_CLASS_INSTANCE(pnet_switch_t,
                           pmix_list_item_t,
                           scon, sdes);

typedef struct {
    pmix_list_item_t super;
    /* use an atomic lock for this object */
    pmix_atomic_lock_t atomlock;
    char *name;
    int index;
    bool dense;
    int nswitches;
    uint64_t nverts;
    uint16_t **costmatrix;
    pmix_list_t switches;
    uint64_t revision;
} pnet_plane_t;
static void pcon(pnet_plane_t *p)
{
    pmix_atomic_lock_init(&p->atomlock, 0);
    p->name = NULL;
    p->index = -1;
    p->dense = false;
    p->nswitches = 0;
    p->nverts = 0;
    p->costmatrix = NULL;
    PMIX_CONSTRUCT(&p->switches, pmix_list_t);
    p->revision = 0;
}
static void pdes(pnet_plane_t *p)
{
    uint64_t n;

    if (NULL != p->name) {
        free(p->name);
    }
    if (NULL != p->costmatrix) {
        for (n=0; n < p->nverts; n++) {
            free(p->costmatrix[n]);
        }
        free(p->costmatrix);
    }
    PMIX_LIST_DESTRUCT(&p->switches);
}
static PMIX_CLASS_INSTANCE(pnet_plane_t,
                           pmix_list_item_t,
                           pcon, pdes);

typedef struct {
    pmix_list_item_t super;
    char *name;
    pmix_list_t nics;
} pnet_node_t;
static void ndcon(pnet_node_t *p)
{
    p->name = NULL;
    PMIX_CONSTRUCT(&p->nics, pmix_list_t);
}
static void nddes(pnet_node_t *p)
{
    if (NULL != p->name) {
        free(p->name);
    }
    PMIX_LIST_DESTRUCT(&p->nics);
}
static PMIX_CLASS_INSTANCE(pnet_node_t,
                           pmix_list_item_t,
                           ndcon, nddes);

/* internal variables */
static pmix_list_t myplanes;
static pmix_list_t mynodes;
static pmix_pointer_array_t myfabrics;
static pmix_pointer_array_t mynics;
static char **myenvlist = NULL;
static char **myvalues = NULL;

static pmix_status_t test_init(void)
{
    int n, m, r, ns, nplane, nnodes, nports;
    uint64_t n64, m64;
    char **system=NULL, **ptr;
    pnet_plane_t *p;
    pnet_switch_t *s, *s2;
    pnet_nic_t *nic, *nic2;
    pnet_node_t *node;
    pmix_status_t rc;

    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                        "pnet: test init");

    PMIX_CONSTRUCT(&myplanes, pmix_list_t);
    PMIX_CONSTRUCT(&mynodes, pmix_list_t);
    PMIX_CONSTRUCT(&myfabrics, pmix_pointer_array_t);
    pmix_pointer_array_init(&myfabrics, 1, INT_MAX, 1);
    PMIX_CONSTRUCT(&mynics, pmix_pointer_array_t);
    pmix_pointer_array_init(&mynics, 8, INT_MAX, 8);

    /* if we have a config file, read it now */
    if (NULL != mca_pnet_test_component.cfg_file) {

    } else if (NULL != mca_pnet_test_component.nverts) {
        pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                            "pnet: test creating system configuration");
        /* the system description is configured as nodes and fabric planes
         * delineated by semi-colons */
        system = pmix_argv_split(mca_pnet_test_component.nverts, ';');
        /* there can be multiple planes defined, but only one set of
         * nodes. The nodes description contains the #nodes - we assume
         * that each node has a single NIC attached to each fabric plane.
         * Thus, the nodes entry has a single field associated with it that
         * contains the number of nodes in the system.
         *
         * Similarly, we assume that each switch in the plane contains
         * a port to connect to each node in the system. For simplicity,
         * we assume a ring connection topology between the switches and
         * reserve one port on each switch to connect to its "left" peer
         * and another to connect to its "right" peer.
         *
         * Thus, the #NICS in a node equals the number of planes in the
         * overall system. The #ports in a switch equals the #nodes in
         * the system plus two for cross-switch communications.
         */
        for (r=0; NULL != system[r]; r++) {
            if (0 == strncasecmp(system[r], "nodes", 5)) {
                /* the number of nodes must follow the colon after "nodes" */
                nnodes = strtoul(&system[r][6], NULL, 10);
                for (n=0; n < nnodes; n++) {
                    node = PMIX_NEW(pnet_node_t);
                    if (0 > asprintf(&node->name, "test%03d", n)) {
                        rc = PMIX_ERR_NOMEM;
                        PMIX_RELEASE(node);
                        goto cleanup;
                    }
                    pmix_list_append(&mynodes, &node->super);
                }
            } else if (0 == strncasecmp(system[r], "plane", 5)) {
                /* create a plane object */
                p = PMIX_NEW(pnet_plane_t);
                /* the plane contains a flag indicating how the nodes
                 * are to be distributed across the plane plus the
                 * number of switches in the plane */
                ptr = pmix_argv_split(&system[r][6], ':');
                if (1 == pmix_argv_count(ptr)) {
                    /* default to dense */
                    p->dense = true;
                    p->nswitches = strtoul(ptr[0], NULL, 10);
                } else {
                    if ('d' == ptr[0][0] || 'D' == ptr[0][0]) {
                        p->dense = true;
                    }
                    p->nswitches = strtoul(ptr[1], NULL, 10);
                }
                pmix_argv_free(ptr);
                pmix_list_append(&myplanes, &p->super);
            } else {
                PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
                rc = PMIX_ERR_BAD_PARAM;
                goto cleanup;
            }
        }
        /* setup the ports in each switch for each plane */
        nplane = 0;
        PMIX_LIST_FOREACH(p, &myplanes, pnet_plane_t) {
            /* assign a name to the plane */
            if (0 > asprintf(&p->name, "plane%03d", nplane)) {
                rc = PMIX_ERR_NOMEM;
                goto cleanup;
            }
            /* setup the ports on the switches */
            nports = nnodes / p->nswitches;
            /* if it didn't divide evenly, then we have to add
             * one to each switch to ensure we have enough ports */
            if (0 != nnodes % p->nswitches) {
                ++nports;
            }
            for (n=0; n < p->nswitches; n++) {
                s = PMIX_NEW(pnet_switch_t);
                if (0 > asprintf(&s->name, "%s:switch%03d", p->name, n)) {
                    rc = PMIX_ERR_NOMEM;
                    goto cleanup;
                }
                s->index = n;
                pmix_list_append(&p->switches, &s->super);
                if (0 > asprintf(&s->leftport.name, "%s:port000", s->name)) {
                    rc = PMIX_ERR_NOMEM;
                    goto cleanup;
                }
                if (0 > asprintf(&s->rightport.name, "%s:port%03d", s->name, nports+1)) {
                    rc = PMIX_ERR_NOMEM;
                    goto cleanup;
                }
                for (m=0; m < nports; m++) {
                    nic = PMIX_NEW(pnet_nic_t);
                    if (0 > asprintf(&nic->name, "%s:port%03d", s->name, m+1)) {
                        rc = PMIX_ERR_NOMEM;
                        goto cleanup;
                    }
                    nic->s = s;
                    nic->plane = p;
                    pmix_list_append(&s->ports, &nic->super);
                }
            }

            /* link the switch ring - first nic on each switch connects
             * to the left, last nic on each switch connects to
             * the right */
            s = (pnet_switch_t*)pmix_list_get_first(&p->switches);
            s->left = pmix_list_get_last(&p->switches);
            s->right = pmix_list_get_next(&s->super);
            /* setup his NICs to point to the right place */
            s2 = (pnet_switch_t*)s->left;
            s->leftport.link = &s2->rightport;
            s2->rightport.link = &s->leftport;

            s2 = (pnet_switch_t*)s->right;
            s->rightport.link = &s2->leftport;
            s2->leftport.link = &s->rightport;

            /* progress the search */
            s = (pnet_switch_t*)pmix_list_get_next(&s->super);
            while (s != (pnet_switch_t*)pmix_list_get_last(&p->switches)) {
                s->left = pmix_list_get_prev(&s->super);
                s->right = pmix_list_get_next(&s->super);
                /* setup his NICs to point to the right place */
                s2 = (pnet_switch_t*)s->left;
                s->leftport.link = &s2->rightport;
                s2->rightport.link = &s->leftport;

                s2 = (pnet_switch_t*)s->right;
                s->rightport.link = &s2->leftport;
                s2->leftport.link = &s->rightport;
                s2->left = s;

                /* progress the search */
                s = (pnet_switch_t*)pmix_list_get_next(&s->super);
            }
            /* s now points to the last item on the list */
            s->right = pmix_list_get_first(&p->switches);
            s2 = (pnet_switch_t*)s->left;
            s->leftport.link = &s2->rightport;
            s2->rightport.link = &s->leftport;
            s2 = (pnet_switch_t*)s->right;
            s->rightport.link = &s2->leftport;
            s2->leftport.link = &s->rightport;

            /* now cycle across the nodes and setup their connections
             * to the switches */
            if (p->dense) {
                /* connect each successive node to the same switch
                 * until that switch is full - then move to the next */
                s = (pnet_switch_t*)pmix_list_get_first(&p->switches);
                nic = (pnet_nic_t*)pmix_list_get_first(&s->ports);
                n = 0;
                ns = pmix_list_get_size(&s->ports);
                PMIX_LIST_FOREACH(node, &mynodes, pnet_node_t) {
                    nic2 = PMIX_NEW(pnet_nic_t);
                    if (0 > asprintf(&nic2->name, "%s:nic%03d", node->name, n)) {
                        rc = PMIX_ERR_NOMEM;
                        goto cleanup;
                    }
                    ++n;
                    --ns;
                    nic2->node = node;
                    nic2->s = s;
                    nic2->plane = p;
                    nic2->index = pmix_pointer_array_add(&mynics, nic2);
                    PMIX_RETAIN(nic2);
                    pmix_list_append(&node->nics, &nic2->super);
                    nic2->link = nic;
                    nic->link = nic2;
                    if (0 == ns) {
                        /* move to the next switch */
                        s = (pnet_switch_t*)pmix_list_get_next(&s->super);
                        nic = (pnet_nic_t*)pmix_list_get_first(&s->ports);
                        ns = pmix_list_get_size(&s->ports);
                    }
                }
            }

            /* setup the cost matrix - we assume switch-to-switch hops
             * have a cost of 1, as do all node-to-switch hops */
            p->nverts = nnodes;  // we ignore the switch ports for now
            p->costmatrix = (uint16_t**)malloc(p->nverts * sizeof(uint16_t*));
            for (n64=0; n64 < p->nverts; n64++) {
                p->costmatrix[n64] = malloc(p->nverts * sizeof(uint16_t));
            }
            /* fill the matrix with the #hops between each NIC, keeping it symmetric */
            for (n64=0; n64 < p->nverts; n64++) {
                p->costmatrix[n64][n64] = 0;
                nic = (pnet_nic_t*)pmix_pointer_array_get_item(&mynics, n64);
                if (NULL == nic) {
                    PMIX_ERROR_LOG(PMIX_ERR_NOT_FOUND);
                    continue;
                }
                for (m64=n64+1; m64 < p->nverts; m64++) {
                    nic2 = (pnet_nic_t*)pmix_pointer_array_get_item(&mynics, m64);
                    if (NULL == nic2) {
                        PMIX_ERROR_LOG(PMIX_ERR_NOT_FOUND);
                        continue;
                    }
                    /* if they are on the same switch, then cost is 2 */
                    if (nic->s == nic2->s) {
                        p->costmatrix[n64][m64] = 2;
                    } else {
                        /* the cost is increased by the distance
                         * between switches */
                        s = (pnet_switch_t*)nic->s;
                        s2 = (pnet_switch_t*)nic2->s;
                        if (s->index > s2->index) {
                            p->costmatrix[n64][m64] = 2 + s->index - s2->index;
                        } else {
                            p->costmatrix[n64][m64] = 2 + s2->index - s->index;
                        }
                    }
                    p->costmatrix[m64][n64] = p->costmatrix[n64][m64];
                }
            }
            ++nplane;
        }
        pmix_argv_free(system);
        system = NULL;
    }
    rc = PMIX_SUCCESS;

  cleanup:
    if (NULL != system) {
        pmix_argv_free(system);
    }

    return rc;
}

static void test_finalize(void)
{
    pmix_pnet_fabric_t *ft;
    pnet_nic_t *nic;
    int n;

    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                        "pnet: test finalize");

    for (n=0; n < myfabrics.size; n++) {
        if (NULL != (ft = (pmix_pnet_fabric_t*)pmix_pointer_array_get_item(&myfabrics, n))) {
            PMIX_RELEASE(ft);
        }
    }
    PMIX_DESTRUCT(&myfabrics);
    PMIX_LIST_DESTRUCT(&mynodes);
    for (n=0; n < mynics.size; n++) {
        if (NULL != (nic = (pnet_nic_t*)pmix_pointer_array_get_item(&mynics, n))) {
            PMIX_RELEASE(nic);
        }
    }
    PMIX_DESTRUCT(&mynics);
    PMIX_LIST_DESTRUCT(&myplanes);
}

/* NOTE: if there is any binary data to be transferred, then
 * this function MUST pack it for transport as the host will
 * not know how to do so */
static pmix_status_t allocate(pmix_namespace_t *nptr,
                              pmix_info_t info[], size_t ninfo,
                              pmix_list_t *ilist)
{
    pmix_kval_t *kv;
    bool seckey = false, envars = false;
    pmix_list_t mylist;
    size_t n, m, p, q, nreqs=0;
    pmix_info_t *requests = NULL, *iptr, *ip2;
    char *idkey = NULL, **locals = NULL;
    uint64_t unique_key = 12345;
    pmix_buffer_t buf;
    pmix_status_t rc;
    char **nodes = NULL, **procs = NULL;
    pmix_data_array_t *darray, *d2, *d3;
    pmix_rank_t rank;
    pnet_node_t *nd, *nd2;
    uint32_t *u32;

    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                        "pnet:test:allocate for nspace %s",
                        nptr->nspace);

    /* if I am not the scheduler, then ignore this call - should never
     * happen, but check to be safe */
    if (!PMIX_PROC_IS_SCHEDULER(pmix_globals.mypeer)) {
        return PMIX_SUCCESS;
    }

    if (NULL == info) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }
    /* check directives to see if a crypto key and/or
     * network resource allocations requested */
    for (n=0; n < ninfo; n++) {
        pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                            "pnet:test:allocate processing key %s",
                            info[n].key);
        if (PMIX_CHECK_KEY(&info[n], PMIX_SETUP_APP_ENVARS) ||
            PMIX_CHECK_KEY(&info[n], PMIX_SETUP_APP_ALL)) {
            envars = PMIX_INFO_TRUE(&info[n]);
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_ALLOC_NETWORK)) {
            /* this info key includes an array of pmix_info_t, each providing
             * a key (that is to be used as the key for the allocated ports) and
             * a number of ports to allocate for that key */
            if (PMIX_DATA_ARRAY != info[n].value.type ||
                NULL == info[n].value.data.darray ||
                PMIX_INFO != info[n].value.data.darray->type ||
                NULL == info[n].value.data.darray->array) {
                pmix_output(0, "NOTHING IN ARRAY");
                requests = NULL;
                nreqs = 0;
            } else {
                requests = (pmix_info_t*)info[n].value.data.darray->array;
                nreqs = info[n].value.data.darray->size;
            }
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_PROC_MAP)) {
            rc = pmix_preg.parse_procs(info[n].value.data.string, &procs);
            if (PMIX_SUCCESS != rc) {
                return PMIX_ERR_BAD_PARAM;
            }
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_NODE_MAP)) {
            rc = pmix_preg.parse_nodes(info[n].value.data.string, &nodes);
            if (PMIX_SUCCESS != rc) {
                return PMIX_ERR_BAD_PARAM;
            }
        }
    }

    PMIX_CONSTRUCT(&mylist, pmix_list_t);

    if (envars) {
        pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                            "pnet:test:allocate adding envar for nspace %s",
                            nptr->nspace);

        kv = PMIX_NEW(pmix_kval_t);
        if (NULL == kv) {
            rc = PMIX_ERR_NOMEM;
            goto cleanup;
        }
        kv->key = strdup(PMIX_SET_ENVAR);
        kv->value = (pmix_value_t*)malloc(sizeof(pmix_value_t));
        if (NULL == kv->value) {
            PMIX_RELEASE(kv);
            rc = PMIX_ERR_NOMEM;
            goto cleanup;
        }
        kv->value->type = PMIX_ENVAR;
        PMIX_ENVAR_LOAD(&kv->value->data.envar, "PMIX_TEST_ENVAR", "1", ':');
        pmix_list_append(&mylist, &kv->super);
    }

    if (NULL == requests) {
        pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                            "pnet:test:allocate no requests for nspace %s",
                            nptr->nspace);

        rc = PMIX_ERR_TAKE_NEXT_OPTION;
        goto complete;
    }

    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                        "pnet:test:allocate alloc_network for nspace %s",
                        nptr->nspace);

    /* cycle thru the provided array and get the ID key */
    for (n=0; n < nreqs; n++) {
        if (PMIX_CHECK_KEY(&requests[n], PMIX_ALLOC_NETWORK_ID)) {
            /* check for bozo error */
            if (PMIX_STRING != requests[n].value.type ||
                NULL == requests[n].value.data.string) {
                PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
                rc = PMIX_ERR_BAD_PARAM;
                goto cleanup;
            }
            idkey = requests[n].value.data.string;
        } else if (PMIX_CHECK_KEY(&requests[n], PMIX_ALLOC_NETWORK_SEC_KEY)) {
               seckey = PMIX_INFO_TRUE(&requests[n]);
           }
       }

    /* if they didn't give us a test key, just create one */
    if (NULL == idkey) {
        idkey = "TESTKEY";
    }

    /* must include the idkey */
    kv = PMIX_NEW(pmix_kval_t);
    if (NULL == kv) {
        rc = PMIX_ERR_NOMEM;
        goto cleanup;
    }
    kv->key = strdup(PMIX_ALLOC_NETWORK_ID);
    kv->value = (pmix_value_t*)malloc(sizeof(pmix_value_t));
    if (NULL == kv->value) {
        PMIX_RELEASE(kv);
        rc = PMIX_ERR_NOMEM;
        goto cleanup;
    }
    kv->value->type = PMIX_STRING;
    kv->value->data.string = strdup(idkey);
    pmix_list_append(&mylist, &kv->super);

    if (seckey) {
        pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                            "pnet:test:allocate assigning network security key for nspace %s",
                            nptr->nspace);

        kv = PMIX_NEW(pmix_kval_t);
        if (NULL == kv) {
            rc = PMIX_ERR_NOMEM;
            goto cleanup;
        }
        kv->key = strdup(PMIX_ALLOC_NETWORK_SEC_KEY);
        kv->value = (pmix_value_t*)malloc(sizeof(pmix_value_t));
        if (NULL == kv->value) {
            PMIX_RELEASE(kv);
            rc = PMIX_ERR_NOMEM;
            goto cleanup;
        }
        kv->value->type = PMIX_BYTE_OBJECT;
        kv->value->data.bo.bytes = (char*)malloc(sizeof(uint64_t));
        if (NULL == kv->value->data.bo.bytes) {
            PMIX_RELEASE(kv);
            rc = PMIX_ERR_NOMEM;
            goto cleanup;
        }
        memcpy(kv->value->data.bo.bytes, &unique_key, sizeof(uint64_t));
        kv->value->data.bo.size = sizeof(uint64_t);
        pmix_list_append(&mylist, &kv->super);
    }

    if (NULL == procs || NULL == nodes) {
        pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                            "pnet:test:allocate missing proc/node map for nspace %s",
                            nptr->nspace);
        /* not an error - continue to next active component */
        rc = PMIX_ERR_TAKE_NEXT_OPTION;
        goto complete;
    }

    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                        "pnet:test:allocate assigning endpoints for nspace %s",
                        nptr->nspace);

    /* cycle across the nodes and add the endpoints
     * for each proc on the node - we assume the same
     * list of static endpoints on each node */
    for (n=0; NULL != nodes[n]; n++) {
        /* split the procs for this node */
        locals = pmix_argv_split(procs[n], ',');
        if (NULL == locals) {
            /* aren't any on this node */
            continue;
        }
        /* find this node in our list */
        nd = NULL;
        PMIX_LIST_FOREACH(nd2, &mynodes, pnet_node_t) {
            if (0 == strcmp(nd2->name, nodes[n])) {
                nd = nd2;
                break;
            }
        }
        if (NULL == nd) {
            /* we don't have this node in our list */
            rc = PMIX_ERR_NOT_FOUND;
            goto cleanup;
        }
        kv = PMIX_NEW(pmix_kval_t);
        if (NULL == kv) {
            rc = PMIX_ERR_NOMEM;
            goto cleanup;
        }
        kv->key = strdup(PMIX_ALLOC_NETWORK_ENDPTS);
        kv->value = (pmix_value_t*)malloc(sizeof(pmix_value_t));
        if (NULL == kv->value) {
            PMIX_RELEASE(kv);
            rc = PMIX_ERR_NOMEM;
            goto cleanup;
        }
        kv->value->type = PMIX_DATA_ARRAY;
        /* for each proc, we will assign an endpt
         * for each NIC on the node */
        q = pmix_argv_count(locals);
        PMIX_DATA_ARRAY_CREATE(darray, q, PMIX_INFO);
        kv->value->data.darray = darray;
        iptr = (pmix_info_t*)darray->array;
        q = pmix_list_get_size(&nd->nics);
        for (m=0; NULL != locals[m]; m++) {
            /* each proc can have multiple endpoints depending
             * on the number of NICs available on the node. So
             * we package the endpoints for each proc as a data
             * array with the first element being the proc ID
             * and the remaining elements being the assigned
             * endpoints for that proc in priority order */
            PMIX_LOAD_KEY(iptr[m].key, PMIX_PROC_DATA);
            PMIX_DATA_ARRAY_CREATE(d2, 2, PMIX_INFO);
            iptr[m].value.type = PMIX_DATA_ARRAY;
            iptr[m].value.data.darray = d2;
            ip2 = (pmix_info_t*)d2->array;
            /* start with the rank */
            rank = m;
            PMIX_INFO_LOAD(&ip2[0], PMIX_RANK, &rank, PMIX_PROC_RANK);
            /* the second element in this array will itself
             * be a data array of endpts */
            PMIX_DATA_ARRAY_CREATE(d3, q, PMIX_UINT32);
            PMIX_LOAD_KEY(ip2[1].key, PMIX_NETWORK_ENDPT);
            ip2[1].value.type = PMIX_DATA_ARRAY;
            ip2[1].value.data.darray = d3;
            u32 = (uint32_t*)d3->array;
            for (p=0; p < q; p++) {
                u32[p] = 3180 + (m * 4) + p;
            }
        }
        pmix_argv_free(locals);
        locals = NULL;
        pmix_list_append(&mylist, &kv->super);
    }

  complete:
    /* pack all our results into a buffer for xmission to the backend */
    n = pmix_list_get_size(&mylist);
    if (0 < n) {
        PMIX_CONSTRUCT(&buf, pmix_buffer_t);
        /* cycle across the list and pack the kvals */
        while (NULL != (kv = (pmix_kval_t*)pmix_list_remove_first(&mylist))) {
            PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &buf, kv, 1, PMIX_KVAL);
            PMIX_RELEASE(kv);
            if (PMIX_SUCCESS != rc) {
                PMIX_DESTRUCT(&buf);
                goto cleanup;
            }
        }
        kv = PMIX_NEW(pmix_kval_t);
        kv->key = strdup("pmix-pnet-test-blob");
        kv->value = (pmix_value_t*)malloc(sizeof(pmix_value_t));
        if (NULL == kv->value) {
            PMIX_RELEASE(kv);
            PMIX_DESTRUCT(&buf);
            rc = PMIX_ERR_NOMEM;
            goto cleanup;
        }
        kv->value->type = PMIX_BYTE_OBJECT;
        PMIX_UNLOAD_BUFFER(&buf, kv->value->data.bo.bytes, kv->value->data.bo.size);
        PMIX_DESTRUCT(&buf);
        pmix_list_append(ilist, &kv->super);
    }

  cleanup:
    PMIX_LIST_DESTRUCT(&mylist);
    if (NULL != nodes) {
        pmix_argv_free(nodes);
    }
    if (NULL != procs) {
        pmix_argv_free(procs);
    }
    if (NULL != locals) {
        pmix_argv_free(locals);
    }
    return rc;
}

static pmix_status_t setup_local_network(pmix_namespace_t *nptr,
                                         pmix_info_t info[],
                                         size_t ninfo)
{
    size_t n, nvals;
    pmix_buffer_t bkt;
    int32_t cnt;
    pmix_kval_t *kv;
    pmix_status_t rc;
    char *idkey = NULL;
    uint64_t seckey = 0;
    pmix_info_t *iptr;

    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                        "pnet:test:setup_local_network");

    if (NULL != info) {
        for (n=0; n < ninfo; n++) {
            /* look for my key */
            if (PMIX_CHECK_KEY(&info[n], "pmix-pnet-test-blob")) {
                pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                                    "pnet:test:setup_local_network found my blob");
                /* this macro NULLs and zero's the incoming bo */
                PMIX_LOAD_BUFFER(pmix_globals.mypeer, &bkt,
                                 info[n].value.data.bo.bytes,
                                 info[n].value.data.bo.size);
                /* cycle thru the blob and extract the kvals */
                kv = PMIX_NEW(pmix_kval_t);
                cnt = 1;
                PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer,
                                   &bkt, kv, &cnt, PMIX_KVAL);
                while (PMIX_SUCCESS == rc) {
                    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                                        "recvd KEY %s %s", kv->key, PMIx_Data_type_string(kv->value->type));
                    /* check for the network ID */
                    if (PMIX_CHECK_KEY(kv, PMIX_ALLOC_NETWORK_ID)) {
                        if (NULL != idkey) {
                            PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
                            free(idkey);
                            return PMIX_ERR_BAD_PARAM;
                        }
                        idkey = strdup(kv->value->data.string);
                        pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                                            "pnet:test:setup_local_network idkey %s", idkey);
                    } else if (PMIX_CHECK_KEY(kv, PMIX_SET_ENVAR)) {
                        /* if this is an envar we are to set, save it on our
                         * list - we will supply it when setup_fork is called */
                        pmix_argv_append_nosize(&myenvlist, kv->value->data.envar.envar);
                        pmix_argv_append_nosize(&myvalues, kv->value->data.envar.value);
                    } else if (PMIX_CHECK_KEY(kv, PMIX_ALLOC_NETWORK_SEC_KEY)) {
                        /* our network security key was stored as a byte object but
                         * is really just a uint64_t */
                        memcpy(&seckey, kv->value->data.bo.bytes, sizeof(uint64_t));
                    } else if (PMIX_CHECK_KEY(kv, PMIX_ALLOC_NETWORK_ENDPTS)) {
                        iptr = (pmix_info_t*)kv->value->data.darray->array;
                        nvals = kv->value->data.darray->size;
                        /* each element in this array is itself an array containing
                         * the rank and the endpts assigned to that rank. This is
                         * precisely the data we need to cache for the job, so
                         * just do so) */
                        PMIX_GDS_CACHE_JOB_INFO(rc, pmix_globals.mypeer, nptr, iptr, nvals);
                        if (PMIX_SUCCESS != rc) {
                            PMIX_RELEASE(kv);
                            if (NULL != idkey) {
                                free(idkey);
                            }
                            return rc;
                        }
                    }
                    PMIX_RELEASE(kv);
                    kv = PMIX_NEW(pmix_kval_t);
                    cnt = 1;
                    PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer,
                                       &bkt, kv, &cnt, PMIX_KVAL);
                }
                PMIX_RELEASE(kv);
                /* restore the incoming data */
                info[n].value.data.bo.bytes = bkt.base_ptr;
                info[n].value.data.bo.size = bkt.bytes_used;
                bkt.base_ptr = NULL;
                bkt.bytes_used = 0;
            }
        }
    }

    if (NULL != idkey) {
        free(idkey);
    }
    return PMIX_SUCCESS;
}

static pmix_status_t setup_fork(pmix_namespace_t *nptr,
                                const pmix_proc_t *proc,
                                char ***env)
{
    int n;

    /* if we have any envars to contribute, do so here */
    if (NULL != myenvlist) {
        for (n=0; NULL != myenvlist[n]; n++) {
            pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                                "pnet:test:setup_fork setenv: %s=%s",
                                myenvlist[n], myvalues[n]);
            pmix_setenv(myenvlist[n], myvalues[n], true, env);
        }
    }
    return PMIX_SUCCESS;
}

static void child_finalized(pmix_proc_t *peer)
{
    pmix_output(0, "pnet:test CHILD %s:%d FINALIZED",
                peer->nspace, peer->rank);
}

static void local_app_finalized(pmix_namespace_t *nptr)
{
    pmix_output(0, "pnet:test NSPACE %s LOCALLY FINALIZED", nptr->nspace);
}

static void deregister_nspace(pmix_namespace_t *nptr)
{
    pmix_output(0, "pnet:test DEREGISTER NSPACE %s", nptr->nspace);
}

static pmix_status_t collect_inventory(pmix_info_t directives[], size_t ndirs,
                                       pmix_inventory_cbfunc_t cbfunc, void *cbdata)
{
    pmix_output(0, "pnet:test COLLECT INVENTORY");
    return PMIX_ERR_NOT_SUPPORTED;
}

static pmix_status_t deliver_inventory(pmix_info_t info[], size_t ninfo,
                                       pmix_info_t directives[], size_t ndirs,
                                       pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                        "pnet:test deliver inventory");

    return PMIX_ERR_NOT_SUPPORTED;
}

static pmix_status_t register_fabric(pmix_fabric_t *fabric,
                                     const pmix_info_t directives[],
                                     size_t ndirs)
{
    pmix_pnet_fabric_t *ft;
    pnet_plane_t *p, *p2;
    char *pln = NULL;
    size_t n;

    if (NULL == fabric) {
        return PMIX_ERR_BAD_PARAM;
    }
    /* see what plane they wanted */
    for (n=0; n < ndirs; n++) {
        if (PMIX_CHECK_KEY(&directives[n], PMIX_NETWORK_PLANE)) {
            pln = directives[n].value.data.string;
            break;
        }
    }
    if (NULL == pln) {
        /* just use the first on our list */
        p = (pnet_plane_t*)pmix_list_get_first(&myplanes);
    } else {
        /* find it */
        p = NULL;
        PMIX_LIST_FOREACH(p2, &myplanes, pnet_plane_t) {
            if (0 == strcmp(pln, p2->name)) {
                p = p2;
                break;
            }
        }
    }
    if (NULL == p) {
        return PMIX_ERR_NOT_FOUND;
    }

    ft = PMIX_NEW(pmix_pnet_fabric_t);
    ft->module = &pmix_test_module;
    ft->payload = p;

    /* pass to the user-level object */
    fabric->module = ft;
    fabric->revision = p->revision;

    return PMIX_SUCCESS;
}

static pmix_status_t deregister_fabric(pmix_fabric_t *fabric)
{
    fabric->module = NULL;
    return PMIX_SUCCESS;
}

static pmix_status_t get_num_verts(pmix_fabric_t *fabric, uint32_t *nverts)
{
    pmix_pnet_fabric_t *ft = (pmix_pnet_fabric_t*)fabric->module;
    pnet_plane_t *p = (pnet_plane_t*)ft->payload;
    int rc;

    rc = pmix_atomic_trylock(&p->atomlock);
    if (0 != rc) {
        return PMIX_ERR_RESOURCE_BUSY;
    }
    /* if fabric data has been updated since the last time
     * this was accessed, let them know */
    if (fabric->revision != p->revision) {
        /* update the revision */
        fabric->revision = p->revision;
        pmix_atomic_unlock(&p->atomlock);
        return PMIX_FABRIC_UPDATED;
    }

    /* this component only looks at node-resident NICs as
     * we assume switch-to-switch is done over dedicated
     * ports in a ring topology */
    *nverts = p->nverts;
    pmix_atomic_unlock(&p->atomlock);
    return PMIX_SUCCESS;
}

static pmix_status_t get_cost(pmix_fabric_t *fabric,
                              uint32_t src, uint32_t dest,
                              uint16_t *cost)
{
    pmix_pnet_fabric_t *ft = (pmix_pnet_fabric_t*)fabric->module;
    pnet_plane_t *p = (pnet_plane_t*)ft->payload;
    int rc;

    rc = pmix_atomic_trylock(&p->atomlock);
    if (0 != rc) {
        return PMIX_ERR_RESOURCE_BUSY;
    }
    /* if fabric data has been updated since the last time
     * this was accessed, let them know */
    if (fabric->revision != p->revision) {
        /* update the revision */
        fabric->revision = p->revision;
        pmix_atomic_unlock(&p->atomlock);
        return PMIX_FABRIC_UPDATED;
    }
    if (src >= p->nverts || dest >= p->nverts) {
        pmix_atomic_unlock(&p->atomlock);
        return PMIX_ERR_BAD_PARAM;
    }

    *cost = p->costmatrix[src][dest];
    pmix_atomic_unlock(&p->atomlock);
    return PMIX_SUCCESS;
}

static pmix_status_t get_vertex(pmix_fabric_t *fabric,
                                uint32_t i,
                                pmix_value_t *identifier,
                                char **nodename)
{
    pmix_pnet_fabric_t *ft = (pmix_pnet_fabric_t*)fabric->module;
    pnet_plane_t *p = (pnet_plane_t*)ft->payload;
    pnet_nic_t *nic;
    pnet_plane_t *pln;
    pnet_switch_t *sw;
    pnet_node_t *node;
    pmix_info_t *info;
    size_t n;
    int rc;

    if (NULL == p) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    rc = pmix_atomic_trylock(&p->atomlock);
    if (0 != rc) {
        return PMIX_ERR_RESOURCE_BUSY;
    }
    /* if fabric data has been updated since the last time
     * this was accessed, let them know */
    if (fabric->revision != p->revision) {
        /* update the revision */
        fabric->revision = p->revision;
        pmix_atomic_unlock(&p->atomlock);
        return PMIX_FABRIC_UPDATED;
    }
    if (i >= p->nverts) {
        pmix_atomic_unlock(&p->atomlock);
        return PMIX_ERR_BAD_PARAM;
    }

    /* find NIC that corresponds to this index */
    nic = (pnet_nic_t*)pmix_pointer_array_get_item(&mynics, i);
    if (NULL == nic) {
        pmix_atomic_unlock(&p->atomlock);
        return PMIX_ERR_NOT_FOUND;
    }
    node = (pnet_node_t*)nic->node;
    *nodename = strdup(node->name);
    /* the value we pass back will be a data array containing
     * info on the switch this NIC is connected to and the
     * plane it is on */
    identifier->type = PMIX_DATA_ARRAY;
    PMIX_DATA_ARRAY_CREATE(identifier->data.darray, 3, PMIX_INFO);
    info = (pmix_info_t*)identifier->data.darray->array;
    n = 0;
    pln = (pnet_plane_t*)nic->plane;
    PMIX_INFO_LOAD(&info[n], PMIX_NETWORK_PLANE, pln->name, PMIX_STRING);
    ++n;
    sw = (pnet_switch_t*)nic->s;
    PMIX_INFO_LOAD(&info[n], PMIX_NETWORK_SWITCH, sw->name, PMIX_STRING);
    ++n;
    PMIX_INFO_LOAD(&info[n], PMIX_NETWORK_NIC, nic->name, PMIX_STRING);

    pmix_atomic_unlock(&p->atomlock);
    return PMIX_SUCCESS;
}

static pmix_status_t get_index(pmix_fabric_t *fabric,
                               pmix_value_t *identifier,
                               uint32_t *i,
                               char **nodename)
{
    pmix_pnet_fabric_t *ft = (pmix_pnet_fabric_t*)fabric->module;
    pnet_plane_t *p = (pnet_plane_t*)ft->payload;
    pnet_node_t *node;
    pnet_nic_t *nic;
    int rc, m;
    pmix_status_t ret;
    pmix_info_t *info;
    char *nc=NULL;
    size_t n;

    if (NULL == p) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    rc = pmix_atomic_trylock(&p->atomlock);
    if (0 != rc) {
        return PMIX_ERR_RESOURCE_BUSY;
    }
    /* if fabric data has been updated since the last time
     * this was accessed, let them know */
    if (fabric->revision != p->revision) {
        /* update the revision */
        fabric->revision = p->revision;
        pmix_atomic_unlock(&p->atomlock);
        return PMIX_FABRIC_UPDATED;
    }

    /* see what they gave us */
    if (PMIX_DATA_ARRAY == identifier->type) {
        if (PMIX_INFO != identifier->data.darray->type) {
            ret = PMIX_ERR_BAD_PARAM;
            goto cleanup;
        }
        info = (pmix_info_t*)identifier->data.darray->array;
        for (n=0; n < identifier->data.darray->size; n++) {
            if (PMIX_CHECK_KEY(&info[n], PMIX_NETWORK_NIC)) {
                nc = info[n].value.data.string;
            }
        }
        if (NULL == nc) {
            ret = PMIX_ERR_BAD_PARAM;
            goto cleanup;
        }
        /* find the NIC */
        for (m=0; m < mynics.size; m++) {
            nic = (pnet_nic_t*)pmix_pointer_array_get_item(&mynics, m);
            if (NULL == nic) {
                continue;
            }
            if (0 == strcmp(nc, nic->name)) {
                *i = m;
                node = (pnet_node_t*)nic->node;
                *nodename = strdup(node->name);
                ret = PMIX_SUCCESS;
                goto cleanup;
            }
        }
        ret = PMIX_ERR_NOT_FOUND;
    } else if (PMIX_UINT32 == identifier->type) {
        /* they gave us the vertex number - in our case,
         * that is the NIC id */
        *i = identifier->data.uint32;
        nic = (pnet_nic_t*)pmix_pointer_array_get_item(&mynics, *i);
        if (NULL == nic) {
            ret = PMIX_ERR_NOT_FOUND;
            goto cleanup;
        }
        node = (pnet_node_t*)nic->node;
        *nodename = strdup(node->name);
        ret = PMIX_SUCCESS;
    } else {
        ret = PMIX_ERR_BAD_PARAM;
    }

  cleanup:
    pmix_atomic_unlock(&p->atomlock);
    return ret;
}
