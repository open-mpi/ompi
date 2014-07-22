/*
 * Copyright (c) 2013-2014 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "opal/util/show_help.h"

#include "btl_usnic.h"
#include "btl_usnic_util.h"
#include "btl_usnic_proc.h"

/*
 * qsort helper: compare modules by IBV device name
 */
static int map_compare_modules(const void *aa, const void *bb)
{
    ompi_btl_usnic_module_t *a = *((ompi_btl_usnic_module_t**) aa);
    ompi_btl_usnic_module_t *b = *((ompi_btl_usnic_module_t**) bb);

    return strcmp(ibv_get_device_name(a->device),
                  ibv_get_device_name(b->device));
}

/*
 * Helper function to output "device:" lines
 */
static void map_output_modules(FILE *fp)
{
    size_t i;
    size_t size;
    ompi_btl_usnic_module_t **modules;
    char ipv4[IPV4STRADDRLEN];
    char mac[MACSTRLEN];

    fprintf(fp, "# Devices possibly used by this process:\n");

    /* First, we must sort the modules (by device name) so that
       they're always output in a repeatable order. */
    size = mca_btl_usnic_component.num_modules *
        sizeof(ompi_btl_usnic_module_t*);
    modules = calloc(1, size);
    if (NULL == modules) {
        fclose(fp);
        return;
    }

    memcpy(modules, mca_btl_usnic_component.usnic_active_modules, size);
    qsort(modules, mca_btl_usnic_component.num_modules,
          sizeof(ompi_btl_usnic_module_t*), map_compare_modules);

    /* Loop over and print the sorted module device information */
    for (i = 0; i < mca_btl_usnic_component.num_modules; ++i) {
        ompi_btl_usnic_snprintf_ipv4_addr(ipv4, IPV4STRADDRLEN,
                                          modules[i]->if_ipv4_addr,
                                          modules[i]->if_cidrmask);
        ompi_btl_usnic_sprintf_mac(mac, modules[i]->if_mac);

        fprintf(fp, "device=%s,interface=%s,ip=%s,mac=%s,mtu=%d\n",
                ibv_get_device_name(modules[i]->device),
                modules[i]->if_name, ipv4, mac, modules[i]->if_mtu);
    }

    /* Free the temp array */
    free(modules);
}

/************************************************************************/

/*
 * qsort helper: compare endpoints by IBV device name
 */
static int map_compare_endpoints(const void *aa, const void *bb)
{
    ompi_btl_usnic_endpoint_t *a = *((ompi_btl_usnic_endpoint_t**) aa);
    ompi_btl_usnic_endpoint_t *b = *((ompi_btl_usnic_endpoint_t**) bb);

    if (NULL == a && NULL == b) {
        return 0;
    } else if (NULL == a) {
        return 1;
    } else if (NULL == b) {
        return -1;
    }

    return strcmp(ibv_get_device_name(a->endpoint_module->device),
                  ibv_get_device_name(b->endpoint_module->device));
}

/*
 * Helper function to output devices for a single peer
 */
static void map_output_endpoints(FILE *fp, ompi_btl_usnic_proc_t *proc)
{
    size_t i;
    size_t num_output;
    size_t size;
    ompi_btl_usnic_endpoint_t **eps;
    char ipv4[IPV4STRADDRLEN];
    char mac[MACSTRLEN];

    /* First, we must sort the endpoints on this proc by MCW rank so
       that they're always output in a repeatable order.  There may
       also be NULL endpoints (if we didn't match that peer's
       endpoint).  The sort will put NULLs at the end of the array,
       where they can be easily ignored. */
    size = proc->proc_endpoint_count * sizeof(ompi_btl_usnic_endpoint_t *);
    eps = calloc(1, size);
    if (NULL == eps) {
        fclose(fp);
        return;
    }

    memcpy(eps, proc->proc_endpoints, size);
    qsort(eps, proc->proc_endpoint_count,
          sizeof(ompi_btl_usnic_endpoint_t*),
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

        ompi_btl_usnic_snprintf_ipv4_addr(ipv4, IPV4STRADDRLEN,
                                          eps[i]->endpoint_remote_addr.ipv4_addr,
                                          eps[i]->endpoint_remote_addr.cidrmask);
        ompi_btl_usnic_sprintf_mac(mac, eps[i]->endpoint_remote_addr.mac);

        fprintf(fp, "device=%s@peer_ip=%s@peer_mac=%s",
                ibv_get_device_name(eps[i]->endpoint_module->device),
                ipv4, mac);
        ++num_output;
    }
    fprintf(fp, "\n");

    /* Free the temp array */
    free(eps);
}

/************************************************************************/

/*
 * qsort helper: compare the procs by job ID and VPID
 */
static int map_compare_procs(const void *aa, const void *bb)
{
    ompi_btl_usnic_proc_t *a = *((ompi_btl_usnic_proc_t**) aa);
    ompi_btl_usnic_proc_t *b = *((ompi_btl_usnic_proc_t**) bb);
    ompi_process_name_t *an = &(a->proc_ompi->proc_name);
    ompi_process_name_t *bn = &(b->proc_ompi->proc_name);

    if (an->jobid > bn->jobid) {
        return 1;
    } else if (an->jobid < bn->jobid) {
        return -1;
    } else if (an->vpid > bn->vpid) {
        return 1;
    } else if (an->vpid < bn->vpid) {
        return -1;
    } else {
        return 0;
    }
}

/*
 * Helper function to output "peer:" lines
 */
static void map_output_procs(FILE *fp)
{
    size_t i;
    size_t num_procs;
    ompi_btl_usnic_proc_t **procs;
    ompi_btl_usnic_proc_t *pitem;

    fprintf(fp, "# Endpoints used to communicate to each peer MPI process:\n");

    /* First, we must sort the procs by MCW rank so that they're
       always output in a repeatable order. */
    num_procs = opal_list_get_size(&mca_btl_usnic_component.usnic_procs);
    procs = calloc(num_procs, sizeof(ompi_btl_usnic_proc_t*));
    if (NULL == procs) {
        fclose(fp);
        return;
    }

    i = 0;
    OPAL_LIST_FOREACH(pitem, &mca_btl_usnic_component.usnic_procs,
                      ompi_btl_usnic_proc_t) {
        procs[i] = pitem;
        ++i;
    }
    qsort(procs, num_procs, sizeof(ompi_btl_usnic_proc_t*),
          map_compare_procs);

    /* Loop over and print the sorted module device information */
    for (i = 0; i < num_procs; ++i) {
        fprintf(fp, "peer=%d,", procs[i]->proc_ompi->proc_name.vpid);
        if (procs[i]->proc_ompi->proc_hostname) {
            fprintf(fp, "hostname=%s,",
                    procs[i]->proc_ompi->proc_hostname);
        }

        map_output_endpoints(fp, procs[i]);
    }

    /* Free the temp array */
    free(procs);
}

/************************************************************************/

/*
 * Output the connectivity map
 */
void ompi_btl_usnic_connectivity_map(void)
{
    char *filename;
    FILE *fp;

    if (NULL == mca_btl_usnic_component.connectivity_map_prefix) {
        return;
    }

    /* Filename is of the form: <prefix>-<hostname>.<pid>.<jobid>.<MCW
       rank>.txt */
    asprintf(&filename, "%s-%s.pid%d.job%d.mcwrank%d.txt",
             mca_btl_usnic_component.connectivity_map_prefix,
             ompi_process_info.nodename,
             getpid(),
             ompi_proc_local()->proc_name.jobid,
             ompi_proc_local()->proc_name.vpid);
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
                       ompi_process_info.nodename,
                       filename,
                       dirname,
                       strerror(errno), errno);
        return;
    }

    map_output_modules(fp);
    map_output_procs(fp);

    fclose(fp);
}
