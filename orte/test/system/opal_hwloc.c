/* -*- C -*-
 *
 * $HEADER$
 *
 */
#include <stdio.h>
#include <unistd.h>

#include "opal/mca/hwloc/base/base.h"
#include "opal/runtime/opal.h"


static hwloc_topology_t my_topology;

static void fill_cache_line_size(void)
{
    int i = 0;
    unsigned size;
    hwloc_obj_t obj;
    bool found = false;

    /* Look for the smallest L2 cache size */
    size = 4096;
    while (1) {
        obj = opal_hwloc_base_get_obj_by_type(opal_hwloc_topology,
                                              HWLOC_OBJ_CACHE, 2,
                                              i, OPAL_HWLOC_LOGICAL);
        if (NULL == obj) {
            break;
        } else { 
            found = true;
            if (NULL != obj->attr &&
                size > obj->attr->cache.linesize) {
                size = obj->attr->cache.linesize;
            }
        }
        ++i;
    }

    /* If we found an L2 cache size in the hwloc data, save it in
       opal_cache_line_size.  Otherwise, we'll leave whatever default
       was set in opal_init.c */
    if (found) {
        opal_cache_line_size = (int) size;
    }
}

int main(int argc, char* argv[])
{
    hwloc_obj_t obj;
    unsigned j, k;
    struct hwloc_topology_support *support;
    int rc;

    if (2 != argc) {
        fprintf(stderr, "Usage: opal_hwloc <topofile>\n");
        exit(1);
    }

    if (0 > (rc = opal_init(&argc, &argv))) {
        fprintf(stderr, "opal_hwloc: couldn't init opal - error code %d\n", rc);
        return rc;
    }
    
    if (0 != hwloc_topology_init(&my_topology)) {
        return OPAL_ERR_NOT_SUPPORTED;
    }
    if (0 != hwloc_topology_set_xml(my_topology, argv[1])) {
        hwloc_topology_destroy(my_topology);
        return OPAL_ERR_NOT_SUPPORTED;
    }
    /* since we are loading this from an external source, we have to
     * explicitly set a flag so hwloc sets things up correctly
     */
    if (0 != hwloc_topology_set_flags(my_topology,
                                      (HWLOC_TOPOLOGY_FLAG_IS_THISSYSTEM |
                                       HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM |
                                       HWLOC_TOPOLOGY_FLAG_IO_DEVICES))) {
        hwloc_topology_destroy(my_topology);
        return OPAL_ERR_NOT_SUPPORTED;
    }
    if (0 != hwloc_topology_load(my_topology)) {
        hwloc_topology_destroy(my_topology);
        return OPAL_ERR_NOT_SUPPORTED;
    }
    /* remove the hostname from the topology. Unfortunately, hwloc
     * decided to add the source hostname to the "topology", thus
     * rendering it unusable as a pure topological description. So
     * we remove that information here.
     */
    obj = hwloc_get_root_obj(my_topology);
    for (k=0; k < obj->infos_count; k++) {
        if (NULL == obj->infos[k].name ||
            NULL == obj->infos[k].value) {
            continue;
        }
        if (0 == strncmp(obj->infos[k].name, "HostName", strlen("HostName"))) {
            free(obj->infos[k].name);
            free(obj->infos[k].value);
            /* left justify the array */
            for (j=k; j < obj->infos_count-1; j++) {
                obj->infos[j] = obj->infos[j+1];
            }
            obj->infos[obj->infos_count-1].name = NULL;
            obj->infos[obj->infos_count-1].value = NULL;
            obj->infos_count--;
            break;
        }
    }
    /* unfortunately, hwloc does not include support info in its
     * xml output :-(( We default to assuming it is present as
     * systems that use this option are likely to provide
     * binding support
     */
    support = (struct hwloc_topology_support*)hwloc_topology_get_support(my_topology);
    support->cpubind->set_thisproc_cpubind = true;

    /* filter the cpus thru any default cpu set */
    opal_hwloc_base_filter_cpus(my_topology);

    /* fill opal_cache_line_size global with the smallest L1 cache
       line size */
    fill_cache_line_size();

    /* test it */
    if (NULL == hwloc_get_obj_by_type(my_topology, HWLOC_OBJ_CORE, 0)) {
        fprintf(stderr, "DIDN'T FIND A CORE\n");
    }

    hwloc_topology_destroy(my_topology);

    opal_finalize();

    return 0;
}
