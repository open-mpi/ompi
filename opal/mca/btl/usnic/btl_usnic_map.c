/*
 * Copyright (c) 2013-2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "opal/util/show_help.h"

#include "btl_usnic_compat.h"
#include "btl_usnic.h"
#include "btl_usnic_module.h"
#include "btl_usnic_util.h"
#include "btl_usnic_proc.h"

/*
 * qsort helper: compare modules by fabric name
 */
static int map_compare_modules(const void *aa, const void *bb)
{
    opal_btl_usnic_module_t *a = *((opal_btl_usnic_module_t**) aa);
    opal_btl_usnic_module_t *b = *((opal_btl_usnic_module_t**) bb);

    return strcmp(a->fabric_info->fabric_attr->name,
                  b->fabric_info->fabric_attr->name);
}

/*
 * Helper function to output "device:" lines
 */
static int map_output_modules(FILE *fp)
{
    int i;
    size_t size;
    opal_btl_usnic_module_t **modules;
    struct fi_usnic_info *uip;
    char ipv4[IPV4STRADDRLEN];
    struct sockaddr_in *sin;
    int prefix_len;

    fprintf(fp, "# Devices possibly used by this process:\n");

    /* First, we must sort the modules (by device name) so that
       they're always output in a repeatable order. */
    size = mca_btl_usnic_component.num_modules *
        sizeof(opal_btl_usnic_module_t*);
    modules = calloc(1, size);
    if (NULL == modules) {
        return OPAL_ERR_IN_ERRNO;
    }

    memcpy(modules, mca_btl_usnic_component.usnic_active_modules, size);
    qsort(modules, mca_btl_usnic_component.num_modules,
          sizeof(opal_btl_usnic_module_t*), map_compare_modules);


    /* Loop over and print the sorted module device information */
    for (i = 0; i < mca_btl_usnic_component.num_modules; ++i) {
        uip = &modules[i]->usnic_info;
        sin = modules[i]->fabric_info->src_addr;
        prefix_len = usnic_netmask_to_cidrlen(uip->ui.v1.ui_netmask_be);

        opal_btl_usnic_snprintf_ipv4_addr(ipv4, IPV4STRADDRLEN,
                                        sin->sin_addr.s_addr,
                                        prefix_len);

        fprintf(fp, "device=%s,ip=%s,mss=%" PRIsize_t "\n",
                modules[i]->fabric_info->fabric_attr->name,
                ipv4, modules[i]->fabric_info->ep_attr->max_msg_size);
    }

    /* Free the temp array */
    free(modules);

    return OPAL_SUCCESS;
}

/************************************************************************/

/*
 * qsort helper: compare endpoints by fabric name
 */
static int map_compare_endpoints(const void *aa, const void *bb)
{
    opal_btl_usnic_endpoint_t *a = *((opal_btl_usnic_endpoint_t**) aa);
    opal_btl_usnic_endpoint_t *b = *((opal_btl_usnic_endpoint_t**) bb);

    if (NULL == a && NULL == b) {
        return 0;
    } else if (NULL == a) {
        return 1;
    } else if (NULL == b) {
        return -1;
    }

    return strcmp(a->endpoint_module->fabric_info->fabric_attr->name,
                  b->endpoint_module->fabric_info->fabric_attr->name);
}

/*
 * Helper function to output devices for a single peer
 */
static int map_output_endpoints(FILE *fp, opal_btl_usnic_proc_t *proc)
{
    size_t i;
    size_t num_output;
    size_t size;
    opal_btl_usnic_endpoint_t **eps;
    char ipv4[IPV4STRADDRLEN];

    /* First, we must sort the endpoints on this proc by MCW rank so
       that they're always output in a repeatable order.  There may
       also be NULL endpoints (if we didn't match that peer's
       endpoint).  The sort will put NULLs at the end of the array,
       where they can be easily ignored. */
    size = proc->proc_endpoint_count * sizeof(opal_btl_usnic_endpoint_t *);
    eps = calloc(1, size);
    if (NULL == eps) {
        return OPAL_ERR_IN_ERRNO;
    }

    memcpy(eps, proc->proc_endpoints, size);
    qsort(eps, proc->proc_endpoint_count,
          sizeof(opal_btl_usnic_endpoint_t*),
          map_compare_endpoints);

    /* Loop over and print the sorted endpoint information, ignoring
       NULLs that might be at the end of the array. */
    for (num_output = i = 0; i < proc->proc_endpoint_count; ++i) {
        if (NULL == eps[i]) {
            break;
        }
        if (num_output > 0) {
            fprintf(fp, ",");
        }

        opal_btl_usnic_snprintf_ipv4_addr(ipv4, IPV4STRADDRLEN,
                                          eps[i]->endpoint_remote_modex.ipv4_addr,
                                          eps[i]->endpoint_remote_modex.netmask);

        fprintf(fp, "device=%s@peer_ip=%s",
                eps[i]->endpoint_module->fabric_info->fabric_attr->name,
                ipv4);
        ++num_output;
    }
    fprintf(fp, "\n");

    /* Free the temp array */
    free(eps);

    return OPAL_SUCCESS;
}

/************************************************************************/

/*
 * qsort helper: compare the procs by job ID and VPID
 */
static int map_compare_procs(const void *aa, const void *bb)
{
    opal_btl_usnic_proc_t *a = *((opal_btl_usnic_proc_t**) aa);
    opal_btl_usnic_proc_t *b = *((opal_btl_usnic_proc_t**) bb);
    opal_process_name_t *an = &(a->proc_opal->proc_name);
    opal_process_name_t *bn = &(b->proc_opal->proc_name);

    if (an > bn) {
        return 1;
    } else if (an < bn) {
        return -1;
    } else {
        return 0;
    }
}

/*
 * Helper function to output "peer:" lines
 */
static int map_output_procs(FILE *fp)
{
    size_t i;
    size_t num_procs;
    opal_btl_usnic_proc_t **procs;
    opal_btl_usnic_proc_t *pitem;

    fprintf(fp, "# Endpoints used to communicate to each peer MPI process:\n");

    /* First, we must sort the procs by MCW rank so that they're
       always output in a repeatable order. */
    num_procs = opal_list_get_size(&mca_btl_usnic_component.usnic_procs);
    procs = calloc(num_procs, sizeof(opal_btl_usnic_proc_t*));
    if (NULL == procs) {
        return OPAL_ERR_IN_ERRNO;
    }

    i = 0;
    OPAL_LIST_FOREACH(pitem, &mca_btl_usnic_component.usnic_procs,
                      opal_btl_usnic_proc_t) {
        procs[i] = pitem;
        ++i;
    }
    qsort(procs, num_procs, sizeof(opal_btl_usnic_proc_t*),
          map_compare_procs);

    /* Loop over and print the sorted module device information */
    int ret = OPAL_SUCCESS;
    for (i = 0; i < num_procs; ++i) {
        fprintf(fp, "peer=%d,", procs[i]->proc_opal->proc_name.vpid);
        fprintf(fp, "hostname=%s,", opal_get_proc_hostname(procs[i]->proc_opal));
        if (OPAL_SUCCESS != map_output_endpoints(fp, procs[i])) {
            break;
        }
    }

    /* Free the temp array */
    free(procs);

    return ret;
}

/************************************************************************/

/*
 * Output the connectivity map
 */
void opal_btl_usnic_connectivity_map(void)
{
    char *filename;
    FILE *fp;

    if (NULL == mca_btl_usnic_component.connectivity_map_prefix) {
        return;
    }

    /* Filename is of the form: <prefix>-<hostname>.<pid>.<job>.<MCW
       rank>.txt */
    asprintf(&filename, "%s-%s.pid%d.job%d.mcwrank%d.txt",
             mca_btl_usnic_component.connectivity_map_prefix,
             opal_get_proc_hostname(opal_proc_local_get()),
             getpid(),
             opal_proc_local_get()->proc_name.jobid,
             opal_proc_local_get()->proc_name.vpid);
    if (NULL == filename) {
        /* JMS abort? */
        return;
    }

    fp = fopen(filename, "w");
    if (NULL == fp) {
        char dirname[PATH_MAX];
        getcwd(dirname, sizeof(dirname));
        dirname[sizeof(dirname) - 1] = '\0';
        opal_show_help("help-mpi-btl-usnic.txt", "cannot write to map file",
                       true,
                       opal_process_info.nodename,
                       filename,
                       dirname,
                       strerror(errno), errno);
        return;
    }

    if (OPAL_SUCCESS == map_output_modules(fp)) {
        map_output_procs(fp);
    }

    fclose(fp);
}
