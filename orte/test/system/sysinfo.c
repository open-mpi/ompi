/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of applications
 */

#include <stdio.h>
#include "orte/constants.h"

#include <sys/sysctl.h>

#include "opal/mca/sysinfo/sysinfo.h"
#include "opal/mca/sysinfo/base/base.h"

#include "orte/runtime/runtime.h"

int main(int argc, char* argv[])
{
    char *keys[] = {
        OPAL_SYSINFO_CPU_TYPE,
        OPAL_SYSINFO_CPU_MODEL,
        OPAL_SYSINFO_NUM_CPUS,
        OPAL_SYSINFO_MEM_SIZE,
        NULL
    };
    opal_list_t values;
    opal_sysinfo_value_t *info;
    opal_list_item_t *item;
    char *model;
    
    if (ORTE_SUCCESS != orte_init(ORTE_PROC_NON_MPI)) {
        fprintf(stderr, "Failed orte_init\n");
        exit(1);
    }
    
    OBJ_CONSTRUCT(&values, opal_list_t);
    
    opal_sysinfo_base_open();
    opal_sysinfo_base_select();
    
    opal_sysinfo.query(keys, &values);
    
    while (NULL != (item = opal_list_remove_first(&values))) {
        info = (opal_sysinfo_value_t*)item;
        fprintf(stderr, "Key: %s Value: ", info->key);
        if (OPAL_INT64 == info->type) {
            fprintf(stderr, "%ld\n", (long int)info->data.i64);
        } else if (OPAL_STRING == info->type) {
            fprintf(stderr, "%s\n", info->data.str);
        }
        OBJ_RELEASE(info);
    }
    
    OBJ_DESTRUCT(&values);
    
    opal_sysinfo_base_close();
    
    model = getenv("OMPI_MCA_cpu_model");
    fprintf(stderr, "Envar cpu_model: %s\n", (NULL == model) ? "NULL" : model);
    
    if (ORTE_SUCCESS != orte_finalize()) {
        fprintf(stderr, "Failed orte_finalize\n");
        exit(1);
    }
    return 0;
}
