/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2023      Triad National Security, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"

#include <hwloc.h>
#if HWLOC_API_VERSION >= 0x20000
#include <hwloc/shmem.h>
#endif

#include "pmix_common.h"
#include "src/mca/bfrops/base/base.h"
#include "src/util/pmix_printf.h"
#include "pmix_hwloc.h"

pmix_status_t pmix_hwloc_pack_cpuset(pmix_buffer_t *buf, pmix_cpuset_t *src,
                                     pmix_pointer_array_t *regtypes)
{
    char *tmp;
    pmix_status_t rc;

    if (NULL == src) {
        /* pack a NULL string */
        tmp = NULL;
        PMIX_BFROPS_PACK_TYPE(rc, buf, &tmp, 1, PMIX_STRING, regtypes);
        return PMIX_SUCCESS;
    }

    if (NULL != src->source && 0 != strncasecmp(src->source, "hwloc", 5)) {
        return PMIX_ERR_NOT_SUPPORTED;
    }
    if (NULL == src->bitmap) {
        /* the process isn't bound */
        tmp = NULL;
    } else {
        /* express the cpuset as a string */
        if (0 != hwloc_bitmap_list_asprintf(&tmp, src->bitmap)) {
            return PMIX_ERROR;
        }
    }

    /* pack the string */
    PMIX_BFROPS_PACK_TYPE(rc, buf, &tmp, 1, PMIX_STRING, regtypes);
    free(tmp);

    return rc;
}

pmix_status_t pmix_hwloc_unpack_cpuset(pmix_buffer_t *buf, pmix_cpuset_t *dest,
                                       pmix_pointer_array_t *regtypes)
{
    pmix_status_t rc;
    int cnt;
    char *tmp;

    /* unpack the cpustring */
    cnt = 1;
    PMIX_BFROPS_UNPACK_TYPE(rc, buf, &tmp, &cnt, PMIX_STRING, regtypes);
    if (PMIX_SUCCESS != rc) {
        return rc;
    }
    if (NULL == tmp) {
        dest->bitmap = NULL;
    } else {
        /* convert to a bitmap */
        dest->bitmap = hwloc_bitmap_alloc();
        hwloc_bitmap_list_sscanf(dest->bitmap, tmp);
        free(tmp);
    }
    dest->source = strdup("hwloc");

    return PMIX_SUCCESS;
}

pmix_status_t pmix_hwloc_copy_cpuset(pmix_cpuset_t *dest, pmix_cpuset_t *src)
{
    if (NULL == src->source ||
        0 != strncasecmp(src->source, "hwloc", 5)) {
        return PMIX_ERR_NOT_SUPPORTED;
    }
    if (NULL == src->bitmap) {
        return PMIX_ERR_BAD_PARAM;
    }

    /* copy the src bitmap */
    dest->bitmap = hwloc_bitmap_dup(src->bitmap);
    dest->source = strdup("hwloc");

    return PMIX_SUCCESS;
}

char *pmix_hwloc_print_cpuset(pmix_cpuset_t *src)
{
    char *tmp;

    if (NULL == src->source || 0 != strncasecmp(src->source, "hwloc", 5)) {
        return NULL;
    }
    if (NULL == src->bitmap) {
        return NULL;
    }

    /* express the cpuset as a string */
    if (0 != hwloc_bitmap_list_asprintf(&tmp, src->bitmap)) {
        return NULL;
    }

    return tmp;
}

void pmix_hwloc_destruct_cpuset(pmix_cpuset_t *src)
{
    if (NULL == src || NULL == src->source ||
        0 != strncasecmp(src->source, "hwloc", 5)) {
        return;
    }

    if (NULL != src->bitmap) {
        hwloc_bitmap_free(src->bitmap);
        src->bitmap = NULL;
    }
    free(src->source);
    src->source = NULL;
}

// avoid ABI break
void pmix_ploc_base_destruct_cpuset(pmix_cpuset_t *cpuset)
{
    pmix_hwloc_destruct_cpuset(cpuset);
    return;
}

void pmix_hwloc_release_cpuset(pmix_cpuset_t *ptr, size_t sz)
{
    size_t n;

    if (NULL == ptr) {
        return;
    }

    for (n = 0; n < sz; n++) {
        pmix_hwloc_destruct_cpuset(&ptr[n]);
    }
    free(ptr);
}

// avoid ABI break
void pmix_ploc_base_release_cpuset(pmix_cpuset_t *cpuset, size_t n)
{
    pmix_hwloc_release_cpuset(cpuset, n);
    return;
}

pmix_status_t pmix_hwloc_get_cpuset_size(pmix_cpuset_t *ptr, size_t *sz)
{
    hwloc_bitmap_t test;
    PMIX_HIDE_UNUSED_PARAMS(ptr);

    test = hwloc_bitmap_alloc();
    hwloc_bitmap_fill(test);
    *sz = (size_t)hwloc_bitmap_weight(test);
    hwloc_bitmap_free(test);
    return PMIX_SUCCESS;
}

pmix_status_t pmix_hwloc_pack_topology(pmix_buffer_t *buf, pmix_topology_t *src,
                                       pmix_pointer_array_t *regtypes)
{
    /* NOTE: hwloc defines topology_t as a pointer to a struct! */
    pmix_status_t rc;
    char *xmlbuffer = NULL;
    int len;
    struct hwloc_topology_support *support;

    if (NULL == src) {
        /* pack a NULL string */
        PMIX_BFROPS_PACK_TYPE(rc, buf, &xmlbuffer, 1, PMIX_STRING, regtypes);
        return PMIX_SUCCESS;
    }

    if (NULL != src->source && 0 != strncasecmp(src->source, "hwloc", 5)) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* extract an xml-buffer representation of the tree */
#if HWLOC_API_VERSION < 0x20000
    if (0 != hwloc_topology_export_xmlbuffer(src->topology, &xmlbuffer, &len)) {
        return PMIX_ERROR;
    }
#else
    if (0 != hwloc_topology_export_xmlbuffer(src->topology, &xmlbuffer, &len, 0)) {
        return PMIX_ERROR;
    }
#endif

    /* add to buffer */
    PMIX_BFROPS_PACK_TYPE(rc, buf, &xmlbuffer, 1, PMIX_STRING, regtypes);
    free(xmlbuffer);
    if (PMIX_SUCCESS != rc) {
        return rc;
    }

    /* get the available support - hwloc unfortunately does
     * not include this info in its xml export!
     */
    support = (struct hwloc_topology_support *) hwloc_topology_get_support(src->topology);
    /* pack the discovery support */
    PMIX_BFROPS_PACK_TYPE(rc, buf, support->discovery,
                          sizeof(struct hwloc_topology_discovery_support), PMIX_BYTE, regtypes);
    if (PMIX_SUCCESS != rc) {
        return rc;
    }
    /* pack the cpubind support */
    PMIX_BFROPS_PACK_TYPE(rc, buf, support->cpubind, sizeof(struct hwloc_topology_cpubind_support),
                          PMIX_BYTE, regtypes);
    if (PMIX_SUCCESS != rc) {
        return rc;
    }

    /* pack the membind support */
    PMIX_BFROPS_PACK_TYPE(rc, buf, support->membind, sizeof(struct hwloc_topology_membind_support),
                          PMIX_BYTE, regtypes);
    if (PMIX_SUCCESS != rc) {
        return rc;
    }

    return PMIX_SUCCESS;
}

pmix_status_t pmix_hwloc_unpack_topology(pmix_buffer_t *buf, pmix_topology_t *dest,
                                         pmix_pointer_array_t *regtypes)
{
    /* NOTE: hwloc defines topology_t as a pointer to a struct! */
    pmix_status_t rc;
    char *xmlbuffer = NULL;
    int cnt;
    struct hwloc_topology_support *support;
    hwloc_topology_t t;
    unsigned long flags;

    /* unpack the xml string */
    cnt = 1;
    PMIX_BFROPS_UNPACK_TYPE(rc, buf, &xmlbuffer, &cnt, PMIX_STRING, regtypes);
    if (PMIX_SUCCESS != rc) {
        return rc;
    }
    /* if it is NULL, then return a NULL topology */
    if (NULL == xmlbuffer) {
        dest->source = strdup("hwloc");
        dest->topology = NULL;
        return PMIX_SUCCESS;
    }

    /* convert the xml */
    if (0 != hwloc_topology_init(&t)) {
        rc = PMIX_ERROR;
        free(xmlbuffer);
        return rc;
    }
    if (0 != hwloc_topology_set_xmlbuffer(t, xmlbuffer, strlen(xmlbuffer))) {
        rc = PMIX_ERROR;
        free(xmlbuffer);
        hwloc_topology_destroy(t);
        return rc;
    }
    free(xmlbuffer);

    /* since we are loading this from an external source, we have to
     * explicitly set a flag so hwloc sets things up correctly
     */
    flags = HWLOC_TOPOLOGY_FLAG_IS_THISSYSTEM;
#if HWLOC_API_VERSION < 0x00020000
    flags |= HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM;
    flags |= HWLOC_TOPOLOGY_FLAG_IO_DEVICES;
#else
    if (0 != hwloc_topology_set_io_types_filter(t, HWLOC_TYPE_FILTER_KEEP_IMPORTANT)) {
        hwloc_topology_destroy(t);
        return PMIX_ERROR;
    }
#    if HWLOC_API_VERSION < 0x00020100
    flags |= HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM;
#    else
    flags |= HWLOC_TOPOLOGY_FLAG_INCLUDE_DISALLOWED;
#    endif
#endif
    if (0 != hwloc_topology_set_flags(t, flags)) {
        hwloc_topology_destroy(t);
        return PMIX_ERROR;
    }
    /* now load the topology */
    if (0 != hwloc_topology_load(t)) {
        hwloc_topology_destroy(t);
        return PMIX_ERROR;
    }

    /* get the available support - hwloc unfortunately does
     * not include this info in its xml import!
     */
    support = (struct hwloc_topology_support *) hwloc_topology_get_support(t);
    cnt = sizeof(struct hwloc_topology_discovery_support);
    PMIX_BFROPS_UNPACK_TYPE(rc, buf, support->discovery, &cnt, PMIX_BYTE, regtypes);
    if (PMIX_SUCCESS != rc) {
        hwloc_topology_destroy(t);
        return PMIX_ERROR;
    }
    cnt = sizeof(struct hwloc_topology_cpubind_support);
    PMIX_BFROPS_UNPACK_TYPE(rc, buf, support->cpubind, &cnt, PMIX_BYTE, regtypes);
    if (PMIX_SUCCESS != rc) {
        hwloc_topology_destroy(t);
        return PMIX_ERROR;
    }
    cnt = sizeof(struct hwloc_topology_membind_support);
    PMIX_BFROPS_UNPACK_TYPE(rc, buf, support->membind, &cnt, PMIX_BYTE, regtypes);
    if (PMIX_SUCCESS != rc) {
        hwloc_topology_destroy(t);
        return PMIX_ERROR;
    }

    dest->source = strdup("hwloc");
    dest->topology = (void *) t;

    return PMIX_SUCCESS;
}

pmix_status_t pmix_hwloc_copy_topology(pmix_topology_t *dest, pmix_topology_t *src)
{
    if (NULL == src->source || 0 != strncasecmp(src->source, "hwloc", 5)) {
        return PMIX_ERR_NOT_SUPPORTED;
    }
    dest->source = strdup("hwloc");

    /* use the hwloc dup function */
    if (0 != hwloc_topology_dup((hwloc_topology_t *) &dest->topology, src->topology)) {
        return PMIX_ERROR;
    }

    return PMIX_SUCCESS;
}

#define PMIX_HWLOC_MAX_STRING 2048

static void print_hwloc_obj(char **output, char *prefix, hwloc_topology_t topo, hwloc_obj_t obj)
{
    hwloc_obj_t obj2;
    char string[1024], *tmp, *tmp2, *pfx;
    unsigned i;
    struct hwloc_topology_support *support;

    /* print the object type */
    hwloc_obj_type_snprintf(string, 1024, obj, 1);
    pmix_asprintf(&pfx, "\n%s\t", (NULL == prefix) ? "" : prefix);
    pmix_asprintf(&tmp, "%sType: %s Number of child objects: %u%sName=%s",
                  (NULL == prefix) ? "" : prefix, string, obj->arity, pfx,
                  (NULL == obj->name) ? "NULL" : obj->name);
    if (0 < hwloc_obj_attr_snprintf(string, 1024, obj, pfx, 1)) {
        /* print the attributes */
        pmix_asprintf(&tmp2, "%s%s%s", tmp, pfx, string);
        free(tmp);
        tmp = tmp2;
    }
    /* print the cpusets - apparently, some new HWLOC types don't
     * have cpusets, so protect ourselves here
     */
    if (NULL != obj->cpuset) {
        hwloc_bitmap_snprintf(string, PMIX_HWLOC_MAX_STRING, obj->cpuset);
        pmix_asprintf(&tmp2, "%s%sCpuset:  %s", tmp, pfx, string);
        free(tmp);
        tmp = tmp2;
    }
    if (HWLOC_OBJ_MACHINE == obj->type) {
        /* root level object - add support values */
        support = (struct hwloc_topology_support *) hwloc_topology_get_support(topo);
        pmix_asprintf(&tmp2, "%s%sBind CPU proc:   %s%sBind CPU thread: %s", tmp, pfx,
                      (support->cpubind->set_thisproc_cpubind) ? "TRUE" : "FALSE", pfx,
                      (support->cpubind->set_thisthread_cpubind) ? "TRUE" : "FALSE");
        free(tmp);
        tmp = tmp2;
        pmix_asprintf(&tmp2, "%s%sBind MEM proc:   %s%sBind MEM thread: %s", tmp, pfx,
                      (support->membind->set_thisproc_membind) ? "TRUE" : "FALSE", pfx,
                      (support->membind->set_thisthread_membind) ? "TRUE" : "FALSE");
        free(tmp);
        tmp = tmp2;
    }
    pmix_asprintf(&tmp2, "%s%s\n", (NULL == *output) ? "" : *output, tmp);
    free(tmp);
    free(pfx);
    pmix_asprintf(&pfx, "%s\t", (NULL == prefix) ? "" : prefix);
    for (i = 0; i < obj->arity; i++) {
        obj2 = obj->children[i];
        /* print the object */
        print_hwloc_obj(&tmp2, pfx, topo, obj2);
    }
    free(pfx);
    if (NULL != *output) {
        free(*output);
    }
    *output = tmp2;
}

char *pmix_hwloc_print_topology(pmix_topology_t *src)
{
    hwloc_obj_t obj;
    char *tmp = NULL;

    if (NULL == src->source || 0 != strncasecmp(src->source, "hwloc", 5)) {
        return NULL;
    }

    /* get root object */
    obj = hwloc_get_root_obj(src->topology);
    /* print it */
    print_hwloc_obj(&tmp, NULL, src->topology, obj);
    return tmp;
}

void pmix_hwloc_destruct_topology(pmix_topology_t *src)
{
    if (NULL == src || NULL == src->source ||
        0 != strncasecmp(src->source, "hwloc", 5)) {
        return;
    }

    if (NULL != src->topology) {
        hwloc_topology_destroy(src->topology);
        src->topology = NULL;
    }

    free(src->source);
    src->source = NULL;
}

// avoid ABI break
void pmix_ploc_base_destruct_topology(pmix_topology_t *topo)
{
    pmix_hwloc_destruct_topology(topo);
    return;
}

void pmix_hwloc_release_topology(pmix_topology_t *src, size_t sz)
{
    size_t n;

    if (NULL == src) {
        return;
    }

    for (n = 0; n < sz; n++) {
        pmix_hwloc_destruct_topology(&src[n]);
    }
}

// avoid ABI break
void pmix_ploc_base_release_topology(pmix_topology_t *topo, size_t n)
{
    pmix_hwloc_release_topology(topo, n);
    return;
}

pmix_status_t pmix_hwloc_get_topology_size(pmix_topology_t *ptr, size_t *sz)
{
#if HWLOC_API_VERSION < 0x20000
    PMIX_HIDE_UNUSED_PARAMS(ptr);
    *sz = 0;
    return PMIX_ERR_NOT_SUPPORTED;
#else
    int err;

    err = hwloc_shmem_topology_get_length(ptr->topology, sz, 0);
    if (0 != err) {
        *sz = 0;
        return PMIX_ERROR;
    }
    return PMIX_SUCCESS;
#endif
}
