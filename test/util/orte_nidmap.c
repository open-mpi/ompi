/*
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif  /* HAVE_SYS_PARAM_H */
#include <sys/stat.h>

#include "support.h"
#include "opal/runtime/opal.h"
#include "orte/include/orte/frameworks.h"
#include "orte/constants.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/regx/regx.h"
#include "orte/mca/regx/base/base.h"

static void check (const char *value, const char * expected) {
    size_t sz = strlen(expected);
    assert(strlen(value) >= sz);
    assert(0 == strncmp(value, expected, sz-1));
    assert('@' == value[sz] || '\0' == value[sz]);
}

int main(int argc, char* argv[])
{
    char * regex;
    char ** nodes;
    opal_pointer_array_t pool;
    orte_node_t * node;
    orte_proc_info(); /* initialize proc info structure */

    test_init("orte_nidmap");

    opal_init(&argc, &argv);
    orte_init(&argc, &argv, ORTE_PROC_TYPE_NONE);

    if (ORTE_SUCCESS != mca_base_framework_open(&orte_regx_base_framework, 0)) {
        return -1;
    }
    if (ORTE_SUCCESS != orte_regx_base_select()) {
        return -1;
    }

    OBJ_CONSTRUCT(&pool, opal_pointer_array_t);
    orte_node_pool = OBJ_NEW(opal_pointer_array_t);
    orte_job_data = OBJ_NEW(opal_hash_table_t);
    orte_job_t *jdata = OBJ_NEW(orte_job_t);
    jdata->jobid = 1;
    orte_process_info.my_name.jobid = 1;
    opal_hash_table_init(orte_job_data, 1);
    opal_hash_table_set_value_uint32(orte_job_data, jdata->jobid, jdata);


    node = OBJ_NEW(orte_node_t);
    node->daemon = OBJ_NEW(orte_proc_t);
    node->daemon->name.jobid = 1;
    node->daemon->name.vpid = 0;
    node->name = "n0";
    opal_pointer_array_add(&pool, node);

    orte_regx.nidmap_create(&pool, &regex);
    printf ("regex for n0 is %s\n", regex);

    nodes = NULL;
    orte_regx.extract_node_names(regex, &nodes);

    check(nodes[0], "n0");
    assert(NULL == nodes[1]);


    node = OBJ_NEW(orte_node_t);
    node->daemon = OBJ_NEW(orte_proc_t);
    node->daemon->name.vpid = 1;
    node->name = "n1";
    opal_pointer_array_add(&pool, node);

    regex = NULL;
    orte_regx.nidmap_create(&pool, &regex);
    printf ("regex for n0,n1 is %s\n", regex);

    nodes = NULL;
    orte_regx.extract_node_names(regex, &nodes);

    check(nodes[0], "n0");
    check(nodes[1], "n1");
    assert(NULL == nodes[2]);

    node = opal_pointer_array_get_item(&pool, 0);
    node->name = "n-0";
    node = opal_pointer_array_get_item(&pool, 1);
    node->name = "n-1";

    regex = NULL;
    orte_regx.nidmap_create(&pool, &regex);
    printf ("regex for n-0,n-1 is %s\n", regex);

    nodes = NULL;
    orte_regx.extract_node_names(regex, &nodes);

    check(nodes[0], "n-0");
    check(nodes[1], "n-1");
    assert(NULL == nodes[2]);


    node = opal_pointer_array_get_item(&pool, 0);
    node->name = "n-000";
    node = opal_pointer_array_get_item(&pool, 1);
    node->name = "n-001";

    regex = NULL;
    orte_regx.nidmap_create(&pool, &regex);
    printf ("regex for n-000,n-001 is %s\n", regex);

    nodes = NULL;
    orte_regx.extract_node_names(regex, &nodes);

    check(nodes[0], "n-000");
    check(nodes[1], "n-001");
    assert(NULL == nodes[2]);


    node = opal_pointer_array_get_item(&pool, 0);
    node->name = "n9";
    node = opal_pointer_array_get_item(&pool, 1);
    node->name = "n10";

    regex = NULL;
    orte_regx.nidmap_create(&pool, &regex);
    printf ("regex for n9,n10 is %s\n", regex);

    nodes = NULL;
    orte_regx.extract_node_names(regex, &nodes);

    check(nodes[0], "n9");
    check(nodes[1], "n10");
    assert(NULL == nodes[2]);


    node = opal_pointer_array_get_item(&pool, 0);
    node->name = "n99";
    node = opal_pointer_array_get_item(&pool, 1);
    node->name = "n100";

    regex = NULL;
    orte_regx.nidmap_create(&pool, &regex);
    printf ("regex for n99,n100 is %s\n", regex);

    nodes = NULL;
    orte_regx.extract_node_names(regex, &nodes);

    check(nodes[0], "n99");
    check(nodes[1], "n100");
    assert(NULL == nodes[2]);


    node = opal_pointer_array_get_item(&pool, 0);
    node->name = "c712f6n01";
    node = opal_pointer_array_get_item(&pool, 1);
    node->name = "c712f6n02";
    node = OBJ_NEW(orte_node_t);
    node->daemon = OBJ_NEW(orte_proc_t);
    node->daemon->name.vpid = 2;
    node->name = "c712f6n03";
    opal_pointer_array_add(&pool, node);

    regex = NULL;
    orte_regx.nidmap_create(&pool, &regex);
    printf ("regex for c712f6n01,c712f6n02,c712f6n03 is %s\n", regex);

    nodes = NULL;
    orte_regx.extract_node_names(regex, &nodes);

    check(nodes[0], "c712f6n01");
    check(nodes[1], "c712f6n02");
    check(nodes[2], "c712f6n03");
    assert(NULL == nodes[3]);


    node = opal_pointer_array_get_item(&pool, 0);
    node->name = "n01c712";
    node = opal_pointer_array_get_item(&pool, 1);
    node->name = "n02c712";
    node = opal_pointer_array_get_item(&pool, 2);
    node->name = "n03c712";

    regex = NULL;
    orte_regx.nidmap_create(&pool, &regex);
    printf ("regex for n01c712,n02c712,n03c712 is %s\n", regex);

    nodes = NULL;
    orte_regx.extract_node_names(regex, &nodes);

    check(nodes[0], "n01c712");
    check(nodes[1], "n02c712");
    check(nodes[2], "n03c712");
    assert(NULL == nodes[3]);


    node = opal_pointer_array_get_item(&pool, 0);
    node->name = "c8n";
    node = opal_pointer_array_get_item(&pool, 1);
    node->name = "c9n";
    node = opal_pointer_array_get_item(&pool, 2);
    node->name = "c10n";

    regex = NULL;
    orte_regx.nidmap_create(&pool, &regex);
    printf ("regex for c8n,c9n,c10n is %s\n", regex);

    nodes = NULL;
    orte_regx.extract_node_names(regex, &nodes);

    check(nodes[0], "c8n");
    check(nodes[1], "c9n");
    check(nodes[2], "c10n");
    assert(NULL == nodes[3]);



    /* clean up */
    orte_proc_info_finalize();

    test_finalize();
    return 0;
}
