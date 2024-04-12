/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2022      Triad National Security, LLC. All rights reserved.
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
#ifdef HAVE_SYS_STAT_H
#    include <sys/stat.h>
#endif
#ifdef HAVE_FCNTL_H
#    include <fcntl.h>
#endif
#ifdef HAVE_SYS_UTSNAME_H
#    include <sys/utsname.h>
#endif
#include <time.h>
#include <assert.h>
#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>

#include <hwloc.h>

#include "src/class/pmix_list.h"
#include "src/client/pmix_client_ops.h"
#include "src/include/pmix_globals.h"
#include "src/mca/bfrops/base/base.h"
#include "src/mca/pnet/pnet.h"
#include "src/server/pmix_server_ops.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_fd.h"
#include "src/util/pmix_name_fns.h"
#include "src/util/pmix_path.h"
#include "src/util/pmix_printf.h"
#include "src/util/pmix_show_help.h"
#include "src/util/pmix_vmem.h"

#include "pmix_common.h"

#include "pmix_hwloc.h"
#if HWLOC_API_VERSION >= 0x20000
#    include <hwloc/shmem.h>
#endif

static bool topo_in_shmem = false;
static bool passed_thru = false;
static char *vmhole = "biggest";
static pmix_vmem_hole_kind_t hole_kind = VMEM_HOLE_BIGGEST;
static char *topo_file = NULL;
static char *testcpuset = NULL;
static int pmix_hwloc_output = -1;
static int pmix_hwloc_verbose = 0;

#if HWLOC_API_VERSION >= 0x20000
static size_t shmemsize = 0;
static size_t shmemaddr;
static char *shmemfile = NULL;
static int shmemfd = -1;
static bool space_available = false;
static uint64_t amount_space_avail = 0;

static int enough_space(const char *filename, size_t space_req, uint64_t *space_avail,
                        bool *result);
#endif
static pmix_status_t load_xml(char *xml);
static char *popstr(pmix_cb_t *cb);
#if HWLOC_API_VERSION >= 0x20000
static size_t popsize(pmix_cb_t *cb);
static void print_maps(void);
#endif
static pmix_topology_t *popptr(pmix_cb_t *cb);
static int get_locality_string_by_depth(int d, hwloc_cpuset_t cpuset, hwloc_cpuset_t result);
static int set_flags(hwloc_topology_t topo, unsigned int flags);

pmix_status_t pmix_hwloc_register(void)
{
    (void) pmix_mca_base_var_register("pmix", "pmix", "hwloc", "verbose",
                                      "Verbosity for HWLOC operations",
                                      PMIX_MCA_BASE_VAR_TYPE_INT,
                                      &pmix_hwloc_verbose);
    if (0 < pmix_hwloc_verbose) {
        /* set default output */
        pmix_hwloc_output = pmix_output_open(NULL);
        pmix_output_set_verbosity(pmix_hwloc_output, pmix_hwloc_verbose);
    }

    vmhole = "biggest";
    (void) pmix_mca_base_var_register("pmix", "pmix", "hwloc", "hole_kind",
                                      "Kind of VM hole to identify - none, begin, biggest, libs, heap, stack (default=biggest)",
                                      PMIX_MCA_BASE_VAR_TYPE_STRING, &vmhole);
    if (0 == strcasecmp(vmhole, "none")) {
        hole_kind = VMEM_HOLE_NONE;
    } else if (0 == strcasecmp(vmhole, "begin")) {
        hole_kind = VMEM_HOLE_BEGIN;
    } else if (0 == strcasecmp(vmhole, "biggest")) {
        hole_kind = VMEM_HOLE_BIGGEST;
    } else if (0 == strcasecmp(vmhole, "libs")) {
        hole_kind = VMEM_HOLE_IN_LIBS;
    } else if (0 == strcasecmp(vmhole, "heap")) {
        hole_kind = VMEM_HOLE_AFTER_HEAP;
    } else if (0 == strcasecmp(vmhole, "stack")) {
        hole_kind = VMEM_HOLE_BEFORE_STACK;
    } else {
        pmix_output(0, "INVALID VM HOLE TYPE");
        return PMIX_ERROR;
    }

    (void) pmix_mca_base_var_register("pmix", "pmix", "hwloc", "topo_file",
                                      "Topology file to use instead of discovering it (mostly for testing purposes)",
                                      PMIX_MCA_BASE_VAR_TYPE_STRING, &topo_file);

    (void) pmix_mca_base_var_register("pmix", "pmix", "hwloc", "test_cpuset",
                                      "Cpuset for testing purposes",
                                      PMIX_MCA_BASE_VAR_TYPE_STRING,
                                      &testcpuset);

    return PMIX_SUCCESS;
}

void pmix_hwloc_finalize(void)
{
#if HWLOC_API_VERSION >= 0x20000
    if (NULL != shmemfile) {
        unlink(shmemfile);
        free(shmemfile);
    }
    if (0 <= shmemfd) {
        close(shmemfd);
    }
#endif
    if (NULL != pmix_globals.topology.topology && !pmix_globals.external_topology
        && !topo_in_shmem) {
        hwloc_topology_destroy(pmix_globals.topology.topology);
    }
    return;
}

pmix_status_t pmix_hwloc_setup_topology(pmix_info_t *info, size_t ninfo)
{
    pmix_cb_t cb;
    pmix_proc_t wildcard;
    char *xmlbuffer = NULL;
    int len;
    size_t n;
    pmix_kval_t kv, *kptr;
    pmix_value_t val;
    bool share = false;
    bool found_dep = false;
    bool found_new = false;
    pmix_topology_t *topo;
    char *file;
    pmix_status_t rc;

    /* only go thru here ONCE! */
    if (passed_thru) {
        return PMIX_SUCCESS;
    }
    passed_thru = true;

    pmix_output_verbose(2, pmix_hwloc_output,
                        "%s:%s", __FILE__, __func__);

    /* see if they want us to share the topology with our clients */
    for (n = 0; n < ninfo; n++) {
        if (PMIX_CHECK_KEY(&info[n], PMIX_SERVER_SHARE_TOPOLOGY)) {
            share = PMIX_INFO_TRUE(&info[n]);
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_TOPOLOGY2)) {
            if (found_dep) {
                /* must have come from PMIX_TOPOLOGY entry */
                free(pmix_globals.topology.source);
            }
            topo = info[n].value.data.topo;
            pmix_globals.topology.source = strdup(topo->source);
            pmix_globals.topology.topology = topo->topology;
            pmix_globals.external_topology = true;
            found_new = true;
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_TOPOLOGY)) {
            if (!found_new) { // prefer PMIX_TOPOLOGY2
                pmix_globals.topology.source = strdup("hwloc"); // we cannot know the version they used
                pmix_globals.topology.topology = (hwloc_topology_t) info[n].value.data.ptr;
                pmix_globals.external_topology = true;
                found_dep = true;
            }
        }
    }

    if (NULL != pmix_globals.topology.topology) {
        pmix_output_verbose(2, pmix_hwloc_output,
                            "%s:%s topology externally provided", __FILE__, __func__);
        /* record locally in case someone does a PMIx_Get to retrieve it */
        kv.key = PMIX_TOPOLOGY2;
        kv.value = &val;
        val.type = PMIX_TOPO;
        val.data.topo = &pmix_globals.topology;
        PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &pmix_globals.myid, PMIX_INTERNAL, &kv);
        pmix_output_verbose(2, pmix_hwloc_output, "%s:%s stored", __FILE__, __func__);
        if (PMIX_SUCCESS != rc) {
            return rc;
        }
        /* if we need to share it, go do that */
        if (share) {
            goto sharetopo;
        }
        /* otherwise, we are done */
        return PMIX_SUCCESS;
    }
    PMIX_LOAD_PROCID(&wildcard, pmix_globals.myid.nspace, PMIX_RANK_WILDCARD);

    /* try to get it ourselves */
#if HWLOC_API_VERSION >= 0x20000
    int fd;
    uint64_t addr, size;

    pmix_output_verbose(2, pmix_hwloc_output, "%s:%s checking shmem",
                        __FILE__, __func__);

    /* first try to get the shmem link, if available */
    PMIX_CONSTRUCT(&cb, pmix_cb_t);
    cb.key = PMIX_HWLOC_SHMEM_FILE;
    cb.proc = &wildcard;
    PMIX_GDS_FETCH_KV(rc, pmix_client_globals.myserver, &cb);
    if (PMIX_SUCCESS != rc) {
        cb.key = NULL;
        PMIX_DESTRUCT(&cb);
        goto tryxml;
    }
    file = popstr(&cb);

    cb.key = PMIX_HWLOC_SHMEM_ADDR;
    PMIX_GDS_FETCH_KV(rc, pmix_client_globals.myserver, &cb);
    if (PMIX_SUCCESS != rc) {
        cb.key = NULL;
        PMIX_DESTRUCT(&cb);
        free(file);
        goto tryxml;
    }
    addr = popsize(&cb);

    cb.key = PMIX_HWLOC_SHMEM_SIZE;
    PMIX_GDS_FETCH_KV(rc, pmix_client_globals.myserver, &cb);
    if (PMIX_SUCCESS != rc) {
        cb.key = NULL;
        PMIX_DESTRUCT(&cb);
        free(file);
        goto tryxml;
    }
    size = popsize(&cb);
    cb.key = NULL;
    PMIX_DESTRUCT(&cb);

    if (0 > (fd = open(file, O_RDONLY))) {
        free(file);
        /* it may be that a tool has connected to a remote
         * daemon, in which case the file won't be found.
         * Could also be some other error, but let's not
         * treat this as fatal */
        goto tryself;
    }
    free(file);
    rc = hwloc_shmem_topology_adopt((hwloc_topology_t *) &pmix_globals.topology.topology, fd, 0,
                                    (void *) addr, size, 0);
    if (0 == rc) {
        pmix_output_verbose(2, pmix_hwloc_output, "%s:%s shmem adopted",
                            __FILE__, __func__);
        /* got it - we are done */
#    ifdef HWLOC_VERSION
        pmix_asprintf(&pmix_globals.topology.source, "hwloc:%s", HWLOC_VERSION);
#    else
        pmix_globals.topology.source = strdup("hwloc");
#    endif
        /* record locally in case someone does a PMIx_Get to retrieve it */
        kv.key = PMIX_TOPOLOGY2;
        kv.value = &val;
        val.type = PMIX_TOPO;
        val.data.topo = &pmix_globals.topology;
        PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &pmix_globals.myid, PMIX_INTERNAL, &kv);
        pmix_output_verbose(2, pmix_hwloc_output, "%s:%s stored", __FILE__,
                            __func__);
        topo_in_shmem = true;
        return PMIX_SUCCESS;
    }

    /* failed to adopt from shmem, so provide some feedback and
     * then fallback to other ways to get the topology */
    if (4 < pmix_output_get_verbosity(pmix_hwloc_output)) {
        print_maps();
    }

tryxml:
    pmix_output_verbose(2, pmix_hwloc_output, "%s:%s checking v2 xml",
                        __FILE__, __func__);

    /* try to get the v2 XML string */
    PMIX_CONSTRUCT(&cb, pmix_cb_t);
    cb.key = PMIX_HWLOC_XML_V2;
    cb.proc = &wildcard;
    PMIX_GDS_FETCH_KV(rc, pmix_client_globals.myserver, &cb);
    if (PMIX_SUCCESS == rc) {
        file = popstr(&cb);
        if (NULL == file) {
            rc = PMIX_ERR_NOT_FOUND;
        } else {
            rc = load_xml(file);
            free(file);
        }
        cb.key = NULL;
        PMIX_DESTRUCT(&cb);
        if (PMIX_SUCCESS != rc) {
            goto tryv1;
        }

        pmix_output_verbose(2, pmix_hwloc_output,
                            "%s:%s v2 xml adopted", __FILE__, __func__);
        /* record locally in case someone does a PMIx_Get to retrieve it */
        kv.key = PMIX_TOPOLOGY2;
        kv.value = &val;
        val.type = PMIX_TOPO;
        val.data.topo = &pmix_globals.topology;
        PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &pmix_globals.myid, PMIX_INTERNAL, &kv);
        pmix_output_verbose(2, pmix_hwloc_output, "%s:%s stored",
                            __FILE__, __func__);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
        }
        if (share) {
            goto sharetopo;
        }
        return rc;
    }

tryv1:
#endif

    /* try to get the v1 XML string */
    pmix_output_verbose(2, pmix_hwloc_output,
                        "%s:%s checking v1 xml",
                        __FILE__, __func__);

    PMIX_CONSTRUCT(&cb, pmix_cb_t);
    cb.key = PMIX_HWLOC_XML_V1;
    cb.proc = &wildcard;
    PMIX_GDS_FETCH_KV(rc, pmix_client_globals.myserver, &cb);
    if (PMIX_SUCCESS == rc) {
        file = popstr(&cb);
        if (NULL == file) {
            rc = PMIX_ERR_NOT_FOUND;
        } else {
            rc = load_xml(file);
            free(file);
        }
        cb.key = NULL;
        PMIX_DESTRUCT(&cb);
        if (PMIX_SUCCESS != rc) {
            goto tryself;
        }
        pmix_output_verbose(2, pmix_hwloc_output,
                            "%s:%s v1 xml adopted", __FILE__, __func__);

        /* record locally in case someone does a PMIx_Get to retrieve it */
        kv.key = PMIX_TOPOLOGY2;
        kv.value = &val;
        val.type = PMIX_TOPO;
        val.data.topo = &pmix_globals.topology;
        PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &pmix_globals.myid, PMIX_INTERNAL, &kv);
        pmix_output_verbose(2, pmix_hwloc_output, "%s:%s stored",
                            __FILE__, __func__);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
        }
        if (share) {
            goto sharetopo;
        }
        return rc;
    }

tryself:
    /* did they give us one to use? */
    if (NULL != topo_file) {
        pmix_output_verbose(2, pmix_hwloc_output,
                            "%s:%s using MCA provided topo file", __FILE__, __func__);

        if (0 != hwloc_topology_init((hwloc_topology_t *) &pmix_globals.topology.topology)) {
            return PMIX_ERR_TAKE_NEXT_OPTION;
        }
        if (0 != hwloc_topology_set_xml((hwloc_topology_t) pmix_globals.topology.topology, topo_file)) {
            return PMIX_ERR_NOT_SUPPORTED;
        }
        /* since we are loading this from an external source, we have to
         * explicitly set a flag so hwloc sets things up correctly
         */
        if (0 != set_flags(pmix_globals.topology.topology, HWLOC_TOPOLOGY_FLAG_IS_THISSYSTEM)) {
            hwloc_topology_destroy(pmix_globals.topology.topology);
            return PMIX_ERROR;
        }
        /* now load the topology */
        if (0 != hwloc_topology_load(pmix_globals.topology.topology)) {
            hwloc_topology_destroy(pmix_globals.topology.topology);
            return PMIX_ERROR;
        }
        /* we don't know the version */
        pmix_globals.topology.source = strdup("hwloc");
    } else {
        pmix_output_verbose(2, pmix_hwloc_output, "%s:%s doing discovery",
                            __FILE__, __func__);
        /* we weren't given a topology, so get it for ourselves */
        if (0 != hwloc_topology_init((hwloc_topology_t *) &pmix_globals.topology.topology)) {
            return PMIX_ERR_TAKE_NEXT_OPTION;
        }

        if (0 != set_flags(pmix_globals.topology.topology, 0)) {
            hwloc_topology_destroy(pmix_globals.topology.topology);
            return PMIX_ERR_INIT;
        }

        if (0 != hwloc_topology_load(pmix_globals.topology.topology)) {
            PMIX_ERROR_LOG(PMIX_ERR_NOT_SUPPORTED);
            hwloc_topology_destroy(pmix_globals.topology.topology);
            return PMIX_ERR_NOT_SUPPORTED;
        }
#ifdef HWLOC_VERSION
        pmix_asprintf(&pmix_globals.topology.source, "hwloc:%s", HWLOC_VERSION);
#else
        pmix_globals.topology.source = strdup("hwloc");
#endif
        pmix_output_verbose(2, pmix_hwloc_output,
                            "%s:%s discovery complete - source %s", __FILE__, __func__,
                            pmix_globals.topology.source);
    }

    /* record locally in case someone does a PMIx_Get to retrieve it */
    kv.key = PMIX_TOPOLOGY2;
    kv.value = &val;
    val.type = PMIX_TOPO;
    val.data.topo = &pmix_globals.topology;
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &pmix_globals.myid, PMIX_INTERNAL, &kv);
    pmix_output_verbose(2, pmix_hwloc_output, "%s:%s stored", __FILE__,
                        __func__);

    /* if we don't need to share it, then we are done */
    if (!share) {
        return PMIX_SUCCESS;
    }

sharetopo:
    /* setup the XML representation(s) */
    pmix_output_verbose(2, pmix_hwloc_output,
                        "%s:%s sharing topology",
                        __FILE__, __func__);

#if HWLOC_API_VERSION < 0x20000
    /* pass the topology string as we don't
     * have HWLOC shared memory available - we do
     * this so the procs won't read the topology
     * themselves as this could overwhelm the local
     * system on large-scale SMPs */
    if (0 == hwloc_topology_export_xmlbuffer(pmix_globals.topology.topology, &xmlbuffer, &len)) {
        pmix_output_verbose(2, pmix_hwloc_output,
                            "%s:%s export v1 xml",
                            __FILE__, __func__);
        kptr = PMIX_NEW(pmix_kval_t);
        kptr->key = strdup(PMIX_HWLOC_XML_V1);
        kptr->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
        PMIX_VALUE_LOAD(kptr->value, xmlbuffer, PMIX_STRING);
        pmix_list_append(&pmix_server_globals.gdata, &kptr->super);
        /* save it with the deprecated key for older RMs */
        kptr = PMIX_NEW(pmix_kval_t);
        kptr->key = strdup(PMIX_LOCAL_TOPO);
        kptr->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
        PMIX_VALUE_LOAD(kptr->value, xmlbuffer, PMIX_STRING);
        pmix_list_append(&pmix_server_globals.gdata, &kptr->super);
        /* done with the buffer */
        hwloc_free_xmlbuffer(pmix_globals.topology.topology, xmlbuffer);
    }
    /* we don't have the ability to do shared memory, so we are done */
    return PMIX_SUCCESS;
#else
    /* pass the topology as a v2 xml string */
    if (0 == hwloc_topology_export_xmlbuffer(pmix_globals.topology.topology, &xmlbuffer, &len, 0)) {
        pmix_output_verbose(2, pmix_hwloc_output, "%s:%s export v2 xml",
                            __FILE__, __func__);
        kptr = PMIX_NEW(pmix_kval_t);
        kptr->key = strdup(PMIX_HWLOC_XML_V2);
        kptr->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
        PMIX_VALUE_LOAD(kptr->value, xmlbuffer, PMIX_STRING);
        pmix_list_append(&pmix_server_globals.gdata, &kptr->super);
        /* save it with the deprecated key for older RMs */
        kptr = PMIX_NEW(pmix_kval_t);
        kptr->key = strdup(PMIX_LOCAL_TOPO);
        kptr->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
        PMIX_VALUE_LOAD(kptr->value, xmlbuffer, PMIX_STRING);
        pmix_list_append(&pmix_server_globals.gdata, &kptr->super);
        hwloc_free_xmlbuffer(pmix_globals.topology.topology, xmlbuffer);
    }
    /* and as a v1 xml string, should an older client attach */
    if (0 == hwloc_topology_export_xmlbuffer(pmix_globals.topology.topology, &xmlbuffer, &len,
                                             HWLOC_TOPOLOGY_EXPORT_XML_FLAG_V1)) {
        pmix_output_verbose(2, pmix_hwloc_output, "%s:%s export v1 xml",
                            __FILE__, __func__);
        kptr = PMIX_NEW(pmix_kval_t);
        kptr->key = strdup(PMIX_HWLOC_XML_V1);
        kptr->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
        PMIX_VALUE_LOAD(kptr->value, xmlbuffer, PMIX_STRING);
        hwloc_free_xmlbuffer(pmix_globals.topology.topology, xmlbuffer);
        pmix_list_append(&pmix_server_globals.gdata, &kptr->super);
        /* cannot support the deprecated PMIX_LOCAL_TOPO key here as it would
         * overwrite the HWLOC v2 string */
    }

    /* if they specified no shared memory, then we are done */
    if (VMEM_HOLE_NONE == hole_kind) {
        pmix_output_verbose(2, pmix_hwloc_output,
                            "%s:%s no shmem requested", __FILE__, __func__);
        return PMIX_SUCCESS;
    }

    /* get the size of the topology shared memory segment */
    if (0 != hwloc_shmem_topology_get_length(pmix_globals.topology.topology, &shmemsize, 0)) {
        pmix_output_verbose(2, pmix_hwloc_output,
                            "%s hwloc topology shmem not available",
                            PMIX_NAME_PRINT(&pmix_globals.myid));
        return PMIX_SUCCESS;
    }

    /* try and find a hole */
    if (PMIX_SUCCESS != pmix_vmem_find_hole(hole_kind, &shmemaddr, shmemsize)) {
        /* we couldn't find a hole, so don't use the shmem support */
        if (4 < pmix_output_get_verbosity(pmix_hwloc_output)) {
            print_maps();
        }
        return PMIX_SUCCESS;
    }
    /* create the shmem file in our session dir so it
     * will automatically get cleaned up */
    pmix_asprintf(&shmemfile, "%s/hwloc.sm", pmix_server_globals.tmpdir);
    /* let's make sure we have enough space for the backing file */
    if (PMIX_SUCCESS != enough_space(shmemfile, shmemsize, &amount_space_avail, &space_available)) {
        pmix_output_verbose(2, pmix_hwloc_output,
                            "%s an error occurred while determining "
                            "whether or not %s could be created for topo shmem.",
                            PMIX_NAME_PRINT(&pmix_globals.myid), shmemfile);
        free(shmemfile);
        shmemfile = NULL;
        return PMIX_SUCCESS;
    }
    if (!space_available) {
        if (1 < pmix_output_get_verbosity(pmix_hwloc_output)) {
            pmix_show_help("help-pmix-ploc-hwloc.txt", "target full", true, shmemfile,
                           pmix_globals.hostname, (unsigned long) shmemsize,
                           (unsigned long long) amount_space_avail);
        }
        free(shmemfile);
        shmemfile = NULL;
        return PMIX_SUCCESS;
    }
    /* enough space is available, so create the segment */
    if (-1 == (shmemfd = open(shmemfile, O_CREAT | O_RDWR, 0600))) {
        int err = errno;
        if (1 < pmix_output_get_verbosity(pmix_hwloc_output)) {
            pmix_show_help("help-pmix-ploc-hwloc-hwloc.txt", "sys call fail", true,
                           pmix_globals.hostname, "open(2)", "", strerror(err), err);
        }
        free(shmemfile);
        shmemfile = NULL;
        return PMIX_SUCCESS;
    }
    /* ensure nobody inherits this fd */
    pmix_fd_set_cloexec(shmemfd);
    /* populate the shmem segment with the topology */
    rc = hwloc_shmem_topology_write(pmix_globals.topology.topology, shmemfd, 0, (void *) shmemaddr,
                                    shmemsize, 0);
    if (0 != rc) {
        pmix_output_verbose(2, pmix_hwloc_output,
                            "%s an error %d (%s) occurred while writing topology to %s",
                            PMIX_NAME_PRINT(&pmix_globals.myid), rc, strerror(errno), shmemfile);
        unlink(shmemfile);
        free(shmemfile);
        shmemfile = NULL;
        close(shmemfd);
        shmemfd = -1;
        return PMIX_SUCCESS;
    }
    pmix_output_verbose(2, pmix_hwloc_output, "%s:%s exported shmem",
                        __FILE__, __func__);

    /* add the requisite key-values to the global data to be
     * given to each client for older PMIx versions */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_HWLOC_SHMEM_FILE);
    kptr->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
    PMIX_VALUE_LOAD(kptr->value, shmemfile, PMIX_STRING);
    pmix_list_append(&pmix_server_globals.gdata, &kptr->super);

    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_HWLOC_SHMEM_ADDR);
    kptr->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
    PMIX_VALUE_LOAD(kptr->value, &shmemaddr, PMIX_SIZE);
    pmix_list_append(&pmix_server_globals.gdata, &kptr->super);

    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_HWLOC_SHMEM_SIZE);
    kptr->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
    PMIX_VALUE_LOAD(kptr->value, &shmemsize, PMIX_SIZE);
    pmix_list_append(&pmix_server_globals.gdata, &kptr->super);

#endif

    return PMIX_SUCCESS;
}

pmix_status_t pmix_hwloc_load_topology(pmix_topology_t *topo)
{
    pmix_cb_t cb;
    pmix_proc_t wildcard;
    pmix_status_t rc;
    pmix_topology_t *t;

    pmix_output_verbose(2, pmix_hwloc_output, "%s:%s", __FILE__, __func__);

    /* see if they stipulated the type of topology they want */
    if (NULL != topo->source) {
        if (0 != strncasecmp(topo->source, "hwloc", strlen("hwloc"))) {
            /* they want somebody else */
            pmix_output_verbose(2, pmix_hwloc_output,
                                "%s:%s no match - wanted %s", __FILE__, __func__, topo->source);
            return PMIX_ERR_TAKE_NEXT_OPTION;
        }
        /* if we already have a suitable version, just return it */
        if (NULL != pmix_globals.topology.topology) {
            if (0
                == strncasecmp(pmix_globals.topology.source, topo->source, strlen(topo->source))) {
                pmix_output_verbose(2, pmix_hwloc_output,
                                    "%s:%s matched sources", __FILE__, __func__);
                topo->topology = pmix_globals.topology.topology;
                return PMIX_SUCCESS;
            }
            /* nope - not a suitable version */
            pmix_output_verbose(2, pmix_hwloc_output,
                                "%s:%s present but not suitable", __FILE__, __func__);
            return PMIX_ERR_TAKE_NEXT_OPTION;
        }
    } else {
        /* they didn't stipulate a source, so if we already have something, just return it */
        if (NULL != pmix_globals.topology.topology) {
            pmix_output_verbose(2, pmix_hwloc_output,
                                "%s:%s no source stipulated - returning current version", __FILE__,
                                __func__);
            topo->source = strdup(pmix_globals.topology.source);
            topo->topology = pmix_globals.topology.topology;
            return PMIX_SUCCESS;
        }
    }

    /* see if we have it in storage */
    pmix_output_verbose(2, pmix_hwloc_output, "%s:%s checking storage",
                        __FILE__, __func__);
    PMIX_CONSTRUCT(&cb, pmix_cb_t);
    PMIX_LOAD_PROCID(&wildcard, pmix_globals.myid.nspace, PMIX_RANK_WILDCARD);
    cb.proc = &wildcard;
    cb.copy = true;
    cb.key = PMIX_TOPOLOGY2;
    PMIX_GDS_FETCH_KV(rc, pmix_client_globals.myserver, &cb);
    if (PMIX_SUCCESS == rc) {
        cb.key = NULL;
        t = popptr(&cb);
        PMIX_DESTRUCT(&cb);
        if (NULL != t) {
            pmix_output_verbose(2, pmix_hwloc_output,
                                "%s:%s found in storage", __FILE__, __func__);
            topo->source = strdup(t->source);
            topo->topology = t->topology;
            pmix_globals.topology.source = strdup(t->source);
            pmix_globals.topology.topology = t->topology;
            return PMIX_SUCCESS;
        }
    }

    /* we don't have it - better set it up */
    pmix_output_verbose(2, pmix_hwloc_output,
                        "%s:%s nothing found - calling setup", __FILE__, __func__);
    rc = pmix_hwloc_setup_topology(NULL, 0);
    if (PMIX_SUCCESS == rc) {
        topo->source = strdup(pmix_globals.topology.source);
        topo->topology = pmix_globals.topology.topology;
    }
    return rc;
}

pmix_status_t pmix_hwloc_generate_cpuset_string(const pmix_cpuset_t *cpuset,
                                                char **cpuset_string)
{
    char *tmp;

    if (NULL == cpuset || NULL == cpuset->bitmap) {
        *cpuset_string = NULL;
        return PMIX_ERR_BAD_PARAM;
    }

    /* if we aren't the source, then nothing we can do */
    if (0 != strncasecmp(cpuset->source, "hwloc", 5)) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }

    hwloc_bitmap_list_asprintf(&tmp, cpuset->bitmap);
    pmix_asprintf(cpuset_string, "hwloc:%s", tmp);
    free(tmp);

    return PMIX_SUCCESS;
}

pmix_status_t pmix_hwloc_parse_cpuset_string(const char *cpuset_string, pmix_cpuset_t *cpuset)
{
    char *src;

    /* if we aren't the source, then pass */
    src = strchr(cpuset_string, ':');
    if (NULL == src) {
        /* bad string */
        return PMIX_ERR_BAD_PARAM;
    }
    *src = '\0';
    if (0 != strncasecmp(cpuset_string, "hwloc", 5)) {
        *src = ':';
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }
    *src = ':';
    ++src;

    cpuset->source = strdup("hwloc");
    cpuset->bitmap = hwloc_bitmap_alloc();
    hwloc_bitmap_list_sscanf(cpuset->bitmap, src);

    return PMIX_SUCCESS;
}

pmix_status_t pmix_hwloc_generate_locality_string(const pmix_cpuset_t *cpuset, char **loc)
{
    char *locality = NULL, *tmp, *t2;
    unsigned depth, d;
    hwloc_cpuset_t result;
    hwloc_obj_type_t type;

    /* if we aren't the source, then pass */
    if (0 != strncasecmp(cpuset->source, "hwloc", 5)) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }

    /* if this proc is not bound, then there is no locality. We
     * know it isn't bound if the cpuset is NULL, or if it is
     * all 1's */
    if (NULL == cpuset->bitmap || hwloc_bitmap_isfull(cpuset->bitmap)) {
        *loc = NULL;
        return PMIX_SUCCESS;
    }

    /* we are going to use a bitmap to save the results so
     * that we can use a hwloc utility to print them */
    result = hwloc_bitmap_alloc();

    /* get the max depth of the topology */
    depth = hwloc_topology_get_depth(pmix_globals.topology.topology);

    /* start at the first depth below the top machine level */
    for (d = 1; d < depth; d++) {
        /* get the object type at this depth */
        type = hwloc_get_depth_type(pmix_globals.topology.topology, d);
        /* if it isn't one of interest, then ignore it */
        if (HWLOC_OBJ_NUMANODE != type && HWLOC_OBJ_PACKAGE != type &&
#if HWLOC_API_VERSION < 0x20000
            HWLOC_OBJ_CACHE != type &&
#else
            HWLOC_OBJ_L1CACHE != type && HWLOC_OBJ_L2CACHE != type && HWLOC_OBJ_L3CACHE != type &&
#endif
            HWLOC_OBJ_CORE != type && HWLOC_OBJ_PU != type) {
            continue;
        }

        if (get_locality_string_by_depth(d, cpuset->bitmap, result) < 0) {
            continue;
        }

        /* it should be impossible, but allow for the possibility
         * that we came up empty at this depth */
        if (!hwloc_bitmap_iszero(result)) {
            hwloc_bitmap_list_asprintf(&tmp, result);
            switch (type) {
                case HWLOC_OBJ_NUMANODE:
                    pmix_asprintf(&t2, "%sNM%s:", (NULL == locality) ? "" : locality, tmp);
                    if (NULL != locality) {
                        free(locality);
                    }
                    locality = t2;
                    break;
                case HWLOC_OBJ_PACKAGE:
                    pmix_asprintf(&t2, "%sSK%s:", (NULL == locality) ? "" : locality, tmp);
                    if (NULL != locality) {
                        free(locality);
                    }
                    locality = t2;
                    break;
#if HWLOC_API_VERSION < 0x20000
                case HWLOC_OBJ_CACHE: {
                    unsigned cachedepth = hwloc_get_obj_by_depth(pmix_globals.topology.topology, d, 0)
                    ->attr->cache.depth;
                    if (3 == cachedepth) {
                        pmix_asprintf(&t2, "%sL3%s:", (NULL == locality) ? "" : locality, tmp);
                        if (NULL != locality) {
                            free(locality);
                        }
                        locality = t2;
                        break;
                    } else if (2 == cachedepth) {
                        pmix_asprintf(&t2, "%sL2%s:", (NULL == locality) ? "" : locality, tmp);
                        if (NULL != locality) {
                            free(locality);
                        }
                        locality = t2;
                        break;
                    } else {
                        pmix_asprintf(&t2, "%sL1%s:", (NULL == locality) ? "" : locality, tmp);
                        if (NULL != locality) {
                            free(locality);
                        }
                        locality = t2;
                        break;
                    }
                } break;
#else
                case HWLOC_OBJ_L3CACHE:
                    pmix_asprintf(&t2, "%sL3%s:", (NULL == locality) ? "" : locality, tmp);
                    if (NULL != locality) {
                        free(locality);
                    }
                    locality = t2;
                    break;
                case HWLOC_OBJ_L2CACHE:
                    pmix_asprintf(&t2, "%sL2%s:", (NULL == locality) ? "" : locality, tmp);
                    if (NULL != locality) {
                        free(locality);
                    }
                    locality = t2;
                    break;
                case HWLOC_OBJ_L1CACHE:
                    pmix_asprintf(&t2, "%sL1%s:", (NULL == locality) ? "" : locality, tmp);
                    if (NULL != locality) {
                        free(locality);
                    }
                    locality = t2;
                    break;
#endif
                case HWLOC_OBJ_CORE:
                    pmix_asprintf(&t2, "%sCR%s:", (NULL == locality) ? "" : locality, tmp);
                    if (NULL != locality) {
                        free(locality);
                    }
                    locality = t2;
                    break;
                case HWLOC_OBJ_PU:
                    pmix_asprintf(&t2, "%sHT%s:", (NULL == locality) ? "" : locality, tmp);
                    if (NULL != locality) {
                        free(locality);
                    }
                    locality = t2;
                    break;
                default:
                    /* just ignore it */
                    break;
            }
            free(tmp);
        }
        hwloc_bitmap_zero(result);
    }

#if HWLOC_API_VERSION >= 0x20000
    if (get_locality_string_by_depth(HWLOC_TYPE_DEPTH_NUMANODE, cpuset->bitmap, result) == 0) {
        /* it should be impossible, but allow for the possibility
         * that we came up empty at this depth */
        if (!hwloc_bitmap_iszero(result)) {
            hwloc_bitmap_list_asprintf(&tmp, result);
            pmix_asprintf(&t2, "%sNM%s:", (NULL == locality) ? "" : locality, tmp);
            if (NULL != locality) {
                free(locality);
            }
            locality = t2;
            free(tmp);
        }
        hwloc_bitmap_zero(result);
    }
#endif

    hwloc_bitmap_free(result);

    /* remove the trailing colon */
    if (NULL != locality) {
        locality[strlen(locality) - 1] = '\0';
    }
    *loc = locality;
    return PMIX_SUCCESS;
}

pmix_status_t pmix_hwloc_get_relative_locality(const char *locality1,
                                               const char *locality2,
                                               pmix_locality_t *loc)
{
    pmix_locality_t locality;
    char *loc1, *loc2, **set1, **set2;
    hwloc_bitmap_t bit1, bit2;
    size_t n1, n2;
    pmix_status_t rc = PMIX_ERR_TAKE_NEXT_OPTION;

    /* check that locality was generated by us */
    if (0 != strncasecmp(locality1, "hwloc:", strlen("hwloc:"))
        || 0 != strncasecmp(locality2, "hwloc:", strlen("hwloc:"))) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }
    /* point to the first character past the ':' delimiter */
    loc1 = (char *) &locality1[strlen("hwloc:")];
    loc2 = (char *) &locality2[strlen("hwloc:")];

    /* start with what we know - they share a node */
    locality = PMIX_LOCALITY_SHARE_NODE;

    set1 = PMIx_Argv_split(loc1, ':');
    set2 = PMIx_Argv_split(loc2, ':');
    bit1 = hwloc_bitmap_alloc();
    bit2 = hwloc_bitmap_alloc();

    /* check each matching type */
    for (n1 = 0; NULL != set1[n1]; n1++) {
        /* convert the location into bitmap */
        hwloc_bitmap_list_sscanf(bit1, &set1[n1][2]);
        /* find the matching type in set2 */
        for (n2 = 0; NULL != set2[n2]; n2++) {
            if (0 == strncmp(set1[n1], set2[n2], 2)) {
                /* convert the location into bitmap */
                hwloc_bitmap_list_sscanf(bit2, &set2[n2][2]);
                /* see if they intersect */
                if (hwloc_bitmap_intersects(bit1, bit2)) {
                    /* set the corresponding locality bit */
                    if (0 == strncmp(set1[n1], "NM", 2)) {
                        locality |= PMIX_LOCALITY_SHARE_NUMA;
                    } else if (0 == strncmp(set1[n1], "SK", 2)) {
                        locality |= PMIX_LOCALITY_SHARE_PACKAGE;
                    } else if (0 == strncmp(set1[n1], "L3", 2)) {
                        locality |= PMIX_LOCALITY_SHARE_L3CACHE;
                    } else if (0 == strncmp(set1[n1], "L2", 2)) {
                        locality |= PMIX_LOCALITY_SHARE_L2CACHE;
                    } else if (0 == strncmp(set1[n1], "L1", 2)) {
                        locality |= PMIX_LOCALITY_SHARE_L1CACHE;
                    } else if (0 == strncmp(set1[n1], "CR", 2)) {
                        locality |= PMIX_LOCALITY_SHARE_CORE;
                    } else if (0 == strncmp(set1[n1], "HT", 2)) {
                        locality |= PMIX_LOCALITY_SHARE_HWTHREAD;
                    } else {
                        /* should never happen */
                        pmix_output(0, "UNRECOGNIZED LOCALITY %s", set1[n1]);
                        rc = PMIX_ERROR;
                    }
                }
                break;
            }
        }
    }
    PMIx_Argv_free(set1);
    PMIx_Argv_free(set2);
    hwloc_bitmap_free(bit1);
    hwloc_bitmap_free(bit2);
    *loc = locality;
    return rc;
}

pmix_status_t pmix_hwloc_get_cpuset(pmix_cpuset_t *cpuset, pmix_bind_envelope_t ref)
{
    int rc, flag;

    if (NULL != cpuset->source && 0 != strncasecmp(cpuset->source, "hwloc", 5)) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }

    if (PMIX_CPUBIND_PROCESS == ref) {
        flag = HWLOC_CPUBIND_PROCESS;
    } else if (PMIX_CPUBIND_THREAD == ref) {
        flag = HWLOC_CPUBIND_THREAD;
    } else {
        return PMIX_ERR_BAD_PARAM;
    }

    cpuset->bitmap = hwloc_bitmap_alloc();
    if (NULL != testcpuset) {
        rc = hwloc_bitmap_sscanf(cpuset->bitmap, testcpuset);
    } else {
        rc = hwloc_get_cpubind(pmix_globals.topology.topology, cpuset->bitmap, flag);
    }
    if (0 != rc) {
        hwloc_bitmap_free(cpuset->bitmap);
        cpuset->bitmap = NULL;
        return PMIX_ERR_NOT_FOUND;
    }
    if (NULL == cpuset->source) {
        cpuset->source = strdup("hwloc");
    }

    return PMIX_SUCCESS;
}

static hwloc_obj_t dsearch(hwloc_topology_t t, int depth, hwloc_cpuset_t cpuset)
{
    hwloc_obj_t obj;
    unsigned width, w;

    /* get the width of the topology at this depth */
    width = hwloc_get_nbobjs_by_depth(t, depth);
    if (0 == width) {
        return NULL;
    }
    /* scan all objects at this depth to see if
     * the location is under one of them
     */
    for (w = 0; w < width; w++) {
        /* get the object at this depth/index */
        obj = hwloc_get_obj_by_depth(t, depth, w);
        /* if this object doesn't have a cpuset, then ignore it */
        if (NULL == obj->cpuset) {
            continue;
        }
        /* see if the provided cpuset is completely included in this object */
        if (hwloc_bitmap_isincluded(cpuset, obj->cpuset)) {
            return obj;
        }
    }

    return NULL;
}

typedef struct {
    pmix_list_item_t super;
    pmix_device_distance_t dist;
} pmix_devdist_item_t;
static void dvcon(pmix_devdist_item_t *p)
{
    PMIX_DEVICE_DIST_CONSTRUCT(&p->dist);
}
static void dvdes(pmix_devdist_item_t *p)
{
    PMIX_DEVICE_DIST_DESTRUCT(&p->dist);
}
static PMIX_CLASS_INSTANCE(pmix_devdist_item_t, pmix_list_item_t, dvcon, dvdes);

typedef struct {
    hwloc_obj_osdev_type_t hwtype;
    pmix_device_type_t pxtype;
    char *name;
} pmix_type_conversion_t;

static pmix_type_conversion_t table[] = {
    {.hwtype = HWLOC_OBJ_OSDEV_BLOCK, .pxtype = PMIX_DEVTYPE_BLOCK, .name = "BLOCK"},
    {.hwtype = HWLOC_OBJ_OSDEV_GPU, .pxtype = PMIX_DEVTYPE_GPU, .name = "GPU"},
    {.hwtype = HWLOC_OBJ_OSDEV_NETWORK, .pxtype = PMIX_DEVTYPE_NETWORK, .name = "NETWORK"},
    {.hwtype = HWLOC_OBJ_OSDEV_OPENFABRICS, .pxtype = PMIX_DEVTYPE_OPENFABRICS, .name = "OPENFABRICS"},
    {.hwtype = HWLOC_OBJ_OSDEV_DMA, .pxtype = PMIX_DEVTYPE_DMA, .name = "DMA"},
#if HWLOC_API_VERSION >= 0x00010800
    {.hwtype = HWLOC_OBJ_OSDEV_COPROC, .pxtype = PMIX_DEVTYPE_COPROC, .name = "COPROCESSOR"},
#endif
};

static int countcolons(char *str)
{
    int cnt = 0;
    char *p;

    p = strchr(str, ':');
    while (NULL != p) {
        ++cnt;
        ++p;
        p = strchr(p, ':');
    }

    return cnt;
}

pmix_status_t pmix_hwloc_compute_distances(pmix_topology_t *topo, pmix_cpuset_t *cpuset,
                                           pmix_info_t info[], size_t ninfo,
                                           pmix_device_distance_t **dist, size_t *ndist)
{
    hwloc_obj_t obj = NULL;
    hwloc_obj_t tgt;
    hwloc_obj_t device;
    hwloc_obj_t ancestor;
    hwloc_obj_t pu;
    unsigned dp, depth;
    unsigned maxdist = 0;
    unsigned mindist = UINT_MAX;
    unsigned i;
    pmix_list_t dists;
    pmix_devdist_item_t *d;
    pmix_device_distance_t *array;
    size_t n, ntypes, dn;
    int cnt;
    unsigned w, width, pudepth;
    pmix_device_type_t type = 0;
    char **devids = NULL;
    bool found;

    if (NULL == topo->source || NULL == cpuset->source) {
        return PMIX_ERR_BAD_PARAM;
    }

    if (0 != strncasecmp(topo->source, "hwloc", 5)
        || 0 != strncasecmp(cpuset->source, "hwloc", 5)) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }

    /* set default returns */
    *dist = NULL;
    *ndist = 0;

    /* determine number of types we support */
    ntypes = sizeof(table) / sizeof(pmix_type_conversion_t);

    /* determine what they want us to look at */
    if (NULL == info) {
        /* find everything */
        for (n = 0; n < ntypes; n++) {
            type |= table[n].pxtype;
        }
    } else {
        for (n = 0; n < ninfo; n++) {
            if (PMIX_CHECK_KEY(&info[n], PMIX_DEVICE_TYPE)) {
                type |= info[n].value.data.devtype;
            } else if (PMIX_CHECK_KEY(&info[n], PMIX_DEVICE_ID)) {
                PMIx_Argv_append_nosize(&devids, info[n].value.data.string);
            }
        }
    }

    /* find the max depth of this topology */
    depth = hwloc_topology_get_depth(topo->topology);

    /* get the lowest object that completely covers the cpuset */
    for (dp = 1; dp < depth; dp++) {
        tgt = dsearch(topo->topology, dp, cpuset->bitmap);
        if (NULL == tgt) {
            /* nothing found at that depth, so we are done */
            break;
        }
        obj = tgt;
    }
    if (NULL == obj) {
        /* only the entire machine covers this cpuset - typically,
         * this means we are in some odd container where every
         * PU is in its own package. There is nothing useful
         * that can be done here */
        return PMIX_ERR_NOT_AVAILABLE;
    }

    /* get the PU depth */
    pudepth = (unsigned) hwloc_get_type_depth(topo->topology, HWLOC_OBJ_PU);
    width = hwloc_get_nbobjs_by_depth(topo->topology, pudepth);

    PMIX_CONSTRUCT(&dists, pmix_list_t);

    /* loop over the specified devices in the topology */
    for (n = 0; n < ntypes; n++) {
        if (!(type & table[n].pxtype)) {
            continue;
        }
        if (HWLOC_OBJ_OSDEV_BLOCK == table[n].hwtype || HWLOC_OBJ_OSDEV_DMA == table[n].hwtype
#if HWLOC_API_VERSION >= 0x00010800
            || HWLOC_OBJ_OSDEV_COPROC == table[n].hwtype
#endif
            ) {
            continue;
        }
        device = hwloc_get_obj_by_type(topo->topology, HWLOC_OBJ_OS_DEVICE, 0);
        while (NULL != device) {
            if (device->attr->osdev.type == table[n].hwtype) {

                d = PMIX_NEW(pmix_devdist_item_t);
                pmix_list_append(&dists, &d->super);

                d->dist.type = table[n].pxtype;

                /* Construct a UUID for this device */
                if (HWLOC_OBJ_OSDEV_NETWORK == table[n].hwtype) {
                    char *addr = NULL;
                    /* find the address */
                    for (i = 0; i < device->infos_count; i++) {
                        if (0 == strcasecmp(device->infos[i].name, "Address")) {
                            addr = device->infos[i].value;
                            break;
                        }
                    }
                    if (NULL == addr) {
                        /* couldn't find an address - report it as an error */
                        PMIX_LIST_DESTRUCT(&dists);
                        return PMIX_ERROR;
                    }
                    /* could be IPv4 or IPv6 */
                    cnt = countcolons(addr);
                    if (5 == cnt) {
                        pmix_asprintf(&d->dist.uuid, "ipv4://%s", addr);
                    } else if (19 == cnt) {
                        pmix_asprintf(&d->dist.uuid, "ipv6://%s", addr);
                    } else {
                        /* unknown address type */
                        PMIX_LIST_DESTRUCT(&dists);
                        return PMIX_ERROR;
                    }
                } else if (HWLOC_OBJ_OSDEV_OPENFABRICS == table[n].hwtype) {
                    char *ngid = NULL;
                    char *sgid = NULL;
                    /* find the UIDs */
                    for (i = 0; i < device->infos_count; i++) {
                        if (0 == strcasecmp(device->infos[i].name, "NodeGUID")) {
                            ngid = device->infos[i].value;
                        } else if (0 == strcasecmp(device->infos[i].name, "SysImageGUID")) {
                            sgid = device->infos[i].value;
                        }
                    }
                    if (NULL == ngid || NULL == sgid) {
                        PMIX_LIST_DESTRUCT(&dists);
                        return PMIX_ERROR;
                    }
                    pmix_asprintf(&d->dist.uuid, "fab://%s::%s", ngid, sgid);
                } else if (HWLOC_OBJ_OSDEV_GPU == table[n].hwtype) {
                    /* if the name starts with "card", then this is just the aux card of the GPU */
                    if (0 == strncasecmp(device->name, "card", 4)) {
                        pmix_list_remove_item(&dists, &d->super);
                        PMIX_RELEASE(d);
                        device = hwloc_get_next_osdev(topo->topology, device);
                        continue;
                    }
                    pmix_asprintf(&d->dist.uuid, "gpu://%s::%s", pmix_globals.hostname,
                                  device->name);
                } else {
                    /* unknown type */
                    pmix_list_remove_item(&dists, &d->super);
                    PMIX_RELEASE(d);
                    device = hwloc_get_next_osdev(topo->topology, device);
                    continue;
                }

                /* if device id was given, then check if this one matches either
                 * the UUID or osname */
                if (NULL != devids) {
                    found = false;
                    for (dn = 0; NULL != devids[dn]; dn++) {
                        if (0 == strcasecmp(devids[dn], device->name)
                            || 0 == strcasecmp(devids[dn], d->dist.uuid)) {
                            found = true;
                        }
                    }
                    if (!found) {
                        /* skip this one */
                        pmix_list_remove_item(&dists, &d->super);
                        PMIX_RELEASE(d);
                        device = hwloc_get_next_osdev(topo->topology, device);
                        continue;
                    }
                }
                /* save the osname */
                d->dist.osname = strdup(device->name);
                if (NULL == device->cpuset) {
                    /* climb the topology until we find a non-NULL cpuset */
                    tgt = device->parent;
                    while (NULL != tgt && NULL == tgt->cpuset) {
                        tgt = tgt->parent;
                    }
                    if (NULL == tgt) {
                        PMIX_LIST_DESTRUCT(&dists);
                        return PMIX_ERR_NOT_FOUND;
                    }
                } else {
                    tgt = device;
                }
                /* loop over the PUs on this node */
                maxdist = 0;
                mindist = UINT_MAX;
                for (w = 0; w < width; w++) {
                    /* get the pu at this index */
                    pu = hwloc_get_obj_by_depth(topo->topology, pudepth, w);
                    /* is this PU in our cpuset? */
                    if (!hwloc_bitmap_intersects(pu->cpuset, cpuset->bitmap)) {
                        continue;
                    }
                    /* find the common ancestor between the cpuset and NIC objects */
                    ancestor = hwloc_get_common_ancestor_obj(topo->topology, obj, tgt);
                    if (NULL != ancestor) {
                        if (0 == ancestor->depth) {
                            /* we only share the machine - need to do something more
                             * to compute the distance. This can, however, get a little
                             * hairy as there is no good measure of package-to-package
                             * distance - it is all typically given in terms of NUMA
                             * domains, which is no longer a valid way of looking at
                             * locations due to overlapping domains. For now, we will
                             * just take the depth of the device in its package and
                             * add that to the depth of the object in its package
                             * plus the depth of a package to ensure it is further away */
                            dp = obj->depth + depth;
                        } else {
                            /* the depth value can be used as an indicator of relative
                             * locality - the higher the value, the closer the device.
                             * We invert the pyramid to set the dist to be closer for
                             * smaller values */
                            dp = depth - ancestor->depth;
                        }
                    } else {
                        /* shouldn't happen - consider this an error condition */
                        PMIX_LIST_DESTRUCT(&dists);
                        return PMIX_ERROR;
                    }
                    if (mindist > dp) {
                        mindist = dp;
                    }
                    if (maxdist < dp) {
                        maxdist = dp;
                    }
                }
                d->dist.mindist = mindist;
                d->dist.maxdist = maxdist;
            }
            device = hwloc_get_next_osdev(topo->topology, device);
        }
    }

    /* create the return array */
    n = pmix_list_get_size(&dists);
    if (0 == n) {
        /* no devices found */
        return PMIX_ERR_NOT_FOUND;
    }
    PMIX_DEVICE_DIST_CREATE(array, n);
    *ndist = n;
    n = 0;
    PMIX_LIST_FOREACH (d, &dists, pmix_devdist_item_t) {
        array[n].uuid = strdup(d->dist.uuid);
        array[n].osname = strdup(d->dist.osname);
        array[n].type = d->dist.type;
        array[n].mindist = d->dist.mindist;
        array[n].maxdist = d->dist.maxdist;
        ++n;
    }
    PMIX_LIST_DESTRUCT(&dists);
    *dist = array;

    return PMIX_SUCCESS;
}

pmix_status_t pmix_hwloc_check_vendor(pmix_topology_t *topo,
                                      unsigned short vendorID,
                                      uint16_t class)
{
    hwloc_obj_t device;

    if (NULL == topo->source || 0 != strncasecmp(topo->source, "hwloc", 5)) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }

    device = hwloc_get_next_pcidev(topo->topology, NULL);
    while (NULL != device) {
        if (class == device->attr->pcidev.class_id &&
            device->attr->pcidev.vendor_id == vendorID) {
            return PMIX_SUCCESS;
        }
        device = hwloc_get_next_pcidev(topo->topology, device);
    }
    return PMIX_ERR_NOT_AVAILABLE;
}


static int set_flags(hwloc_topology_t topo, unsigned int flags)
{
#if HWLOC_API_VERSION < 0x20000
    flags = HWLOC_TOPOLOGY_FLAG_IO_DEVICES;
#else
    int ret = hwloc_topology_set_io_types_filter(topo, HWLOC_TYPE_FILTER_KEEP_IMPORTANT);
    if (0 != ret)
        return ret;
#endif
    if (0 != hwloc_topology_set_flags(topo, flags)) {
        return PMIX_ERR_INIT;
    }
#ifdef HWLOC_VERSION_MAJOR
    // Blacklist the "gl" component due to potential conflicts.
    // See "https://github.com/open-mpi/ompi/issues/10025" for
    // an explanation. Sadly, HWLOC doesn't define version numbers
    // until v2.0, so we cannot check versions here. Fortunately,
    // the blacklist ability was added in HWLOC v2.1, so we can't
    // do it for earlier versions anyway.
#if HWLOC_VERSION_MAJOR > 2
    hwloc_topology_set_components(topo, HWLOC_TOPOLOGY_COMPONENTS_FLAG_BLACKLIST, "gl");
#elif HWLOC_VERSION_MAJOR == 2 && HWLOC_VERSION_MINOR >= 1
    hwloc_topology_set_components(topo, HWLOC_TOPOLOGY_COMPONENTS_FLAG_BLACKLIST, "gl");
#endif
#endif

    return PMIX_SUCCESS;
}

static char *popstr(pmix_cb_t *cb)
{
    pmix_list_t *kvs = &cb->kvs;
    pmix_kval_t *kv;
    char *str;

    if (1 != pmix_list_get_size(kvs)) {
        return NULL;
    }
    kv = (pmix_kval_t *) pmix_list_get_first(kvs);
    if (PMIX_STRING != kv->value->type) {
        return NULL;
    }
    str = kv->value->data.string;
    kv->value->data.string = NULL;
    kv = (pmix_kval_t *) pmix_list_remove_first(kvs);
    while (NULL != kv) {
        PMIX_RELEASE(kv);
        kv = (pmix_kval_t *) pmix_list_remove_first(kvs);
    }
    return str;
}

#if HWLOC_API_VERSION >= 0x20000
static size_t popsize(pmix_cb_t *cb)
{
    pmix_list_t *kvs = &cb->kvs;
    pmix_kval_t *kv;
    size_t sz;

    if (1 != pmix_list_get_size(kvs)) {
        return UINT64_MAX;
    }
    kv = (pmix_kval_t *) pmix_list_get_first(kvs);
    if (PMIX_SIZE != kv->value->type) {
        return UINT64_MAX;
    }
    sz = kv->value->data.size;
    kv = (pmix_kval_t *) pmix_list_remove_first(kvs);
    while (NULL != kv) {
        PMIX_RELEASE(kv);
        kv = (pmix_kval_t *) pmix_list_remove_first(kvs);
    }
    return sz;
}
#endif

static pmix_topology_t *popptr(pmix_cb_t *cb)
{
    pmix_list_t *kvs = &cb->kvs;
    pmix_kval_t *kv;
    pmix_topology_t *t;

    if (1 != pmix_list_get_size(kvs)) {
        return NULL;
    }
    kv = (pmix_kval_t *) pmix_list_get_first(kvs);
    if (PMIX_TOPO != kv->value->type) {
        return NULL;
    }
    t = kv->value->data.topo;
    kv->value->data.topo = NULL;
    kv = (pmix_kval_t *) pmix_list_remove_first(kvs);
    while (NULL != kv) {
        PMIX_RELEASE(kv);
        kv = (pmix_kval_t *) pmix_list_remove_first(kvs);
    }
    return t;
}

static pmix_status_t load_xml(char *xml)
{
    /* load the topology */
    if (0 != hwloc_topology_init((hwloc_topology_t *) &pmix_globals.topology.topology)) {
        return PMIX_ERROR;
    }
    if (0 != hwloc_topology_set_xmlbuffer(pmix_globals.topology.topology, xml, strlen(xml) + 1)) {
        hwloc_topology_destroy(pmix_globals.topology.topology);
        return PMIX_ERROR;
    }
    /* since we are loading this from an external source, we have to
     * explicitly set a flag so hwloc sets things up correctly
     */
    if (0 != set_flags(pmix_globals.topology.topology, HWLOC_TOPOLOGY_FLAG_IS_THISSYSTEM)) {
        hwloc_topology_destroy(pmix_globals.topology.topology);
        return PMIX_ERROR;
    }
    /* now load the topology */
    if (0 != hwloc_topology_load(pmix_globals.topology.topology)) {
        hwloc_topology_destroy(pmix_globals.topology.topology);
        return PMIX_ERROR;
    }
    pmix_globals.topology.source = strdup("hwloc"); // don't know the version?
    return PMIX_SUCCESS;
}

#if HWLOC_API_VERSION >= 0x20000
static void print_maps(void)
{

    FILE *maps_file = fopen("/proc/self/maps", "r");
    if (maps_file) {
        char line[256];
        pmix_output(0, "%s Dumping /proc/self/maps", PMIX_NAME_PRINT(&pmix_globals.myid));
        while (fgets(line, sizeof(line), maps_file) != NULL) {
            char *end = strchr(line, '\n');
            if (end) {
                *end = '\0';
            }
            pmix_output(0, "%s", line);
        }
        fclose(maps_file);
    }
}
#endif

static int get_locality_string_by_depth(int d, hwloc_cpuset_t cpuset, hwloc_cpuset_t result)
{
    hwloc_obj_t obj;
    unsigned width, w;

    /* get the width of the topology at this depth */
    width = hwloc_get_nbobjs_by_depth(pmix_globals.topology.topology, d);
    if (0 == width) {
        return -1;
    }

    /* scan all objects at this depth to see if
     * the location overlaps with them
     */
    for (w = 0; w < width; w++) {
        /* get the object at this depth/index */
        obj = hwloc_get_obj_by_depth(pmix_globals.topology.topology, d, w);
        /* see if the location intersects with it */
        if (hwloc_bitmap_intersects(obj->cpuset, cpuset)) {
            hwloc_bitmap_set(result, w);
        }
    }

    return 0;
}

#if HWLOC_API_VERSION >= 0x20000
static int enough_space(const char *filename, size_t space_req, uint64_t *space_avail, bool *result)
{
    uint64_t avail = 0;
    size_t fluff = (size_t)(.05 * space_req);
    bool enough = false;
    char *last_sep = NULL;
    /* the target file name is passed here, but we need to check the parent
     * directory. store it so we can extract that info later. */
    char *target_dir = strdup(filename);
    int rc;

    if (NULL == target_dir) {
        rc = PMIX_ERR_OUT_OF_RESOURCE;
        goto out;
    }
    /* get the parent directory */
    last_sep = strrchr(target_dir, PMIX_PATH_SEP[0]);
    *last_sep = '\0';
    /* now check space availability */
    if (PMIX_SUCCESS != (rc = pmix_path_df(target_dir, &avail))) {
        goto out;
    }
    /* do we have enough space? */
    if (avail >= space_req + fluff) {
        enough = true;
    }

out:
    if (NULL != target_dir) {
        free(target_dir);
    }
    *result = enough;
    *space_avail = avail;
    return rc;
}
#endif
