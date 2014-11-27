/* -*- C -*-
 *
 * $HEADER$
 *
 */
#include <stdio.h>
#include <unistd.h>

#include "opal/mca/base/base.h"
#include "opal/util/cmd_line.h"
#include "opal/util/opal_environ.h"

#include "orte/constants.h"

#include "opal/mca/db/base/base.h"
#include "opal/runtime/opal.h"

static bool help;
#define NKV 1024

static opal_cmd_line_init_t cmd_line_init[] = {
    { NULL, 'h', NULL, "help", 0,
      &help, OPAL_CMD_LINE_TYPE_BOOL,
      "This help message" },

    /* End of list */
    { NULL, '\0', NULL, NULL, 0,
      NULL, OPAL_CMD_LINE_TYPE_NULL, NULL }
};

int main(int argc, char* argv[])
{
    int rc, i;
    opal_cmd_line_t cmd_line;
    opal_process_name_t uid;
    opal_value_t kvs;
    char *key;
    opal_process_name_t data, *dptr;
    float *fval;
    opal_list_t kvlist;

    opal_cmd_line_create(&cmd_line, cmd_line_init);
    mca_base_cmd_line_setup(&cmd_line);
    if (OPAL_SUCCESS != (rc = opal_cmd_line_parse(&cmd_line, true,
                                                  argc, argv)) ) {
        if (OPAL_ERR_SILENT != rc) {
            fprintf(stderr, "%s: command line error (%d)\n", argv[0], rc);
        }
        return rc;
    }

    /*
     * Since this process can now handle MCA/GMCA parameters, make sure to
     * process them.
     */
    mca_base_cmd_line_process_args(&cmd_line, &environ, &environ);

    /* init the OPAL framework, but only the test-required level */
    if (0 > (rc = opal_init_test())) {
        fprintf(stderr, "opal_db: couldn't init opal test - error code %d\n", rc);
        return rc;
    }
    
    /* open and select the database framework */
    if (ORTE_SUCCESS != (rc = mca_base_framework_open(&opal_db_base_framework, 0))) {
        fprintf(stderr, "%s: db_framework_open failed (%d)\n", argv[0], rc);
        return rc;
    }
    /* let the system pick any db plugin to test - will
     * be set by whatever param we were given on the cmd
     * line or environment
     */
    if (ORTE_SUCCESS != (rc = opal_db_base_select(false))) {
        fprintf(stderr, "%s: db_framework_select failed (%d)\n", argv[0], rc);
        return rc;
    }
    /* set our id */
    uid.jobid = 12345;
    uid.vpid = 67890;
    opal_db.set_id(&uid);

    /* create and store some arbitrary key-value pairs */
    for (i=0; i < NKV; i++) {
        asprintf(&key, "foo.%d", i);
        data.jobid = i;
        data.vpid = i;
        if (ORTE_SUCCESS != (rc = opal_db.store(&uid, OPAL_SCOPE_GLOBAL,
                                                key, &data, OPAL_NAME))) {
            fprintf(stderr, "%s: db_store failed (%d)\n", argv[0], rc);
            return rc;
        }
        free(key);
    }

    /* create another one */
    OBJ_CONSTRUCT(&kvs, opal_value_t);
    kvs.scope = OPAL_SCOPE_INTERNAL;
    kvs.key = strdup("pointer-value");
    kvs.type = OPAL_FLOAT;
    kvs.data.fval = 1.2334;

    /* store a pointer to it */
    if (ORTE_SUCCESS != (rc = opal_db.store_pointer((opal_identifier_t*)&uid, &kvs))) {
        fprintf(stderr, "%s: db_store_pointer failed (%d)\n", argv[0], rc);
        return rc;
    }

    /* commit the data - only has an effect if an external
     * database plugin was selected
     */
    opal_db.commit((opal_identifier_t*)&uid);

    /* fetch data and compare it to what was stored */
    dptr = &data;
    for (i=0; i < NKV; i++) {
        asprintf(&key, "foo.%d", i);
        if (ORTE_SUCCESS != (rc = opal_db.fetch(&uid, key, (void**)&dptr, OPAL_NAME))) {
            fprintf(stderr, "%s: db_fetch failed (%d)\n", argv[0], rc);
            return rc;
        }
        if (data.jobid != i || data.vpid != i) {
            fprintf(stderr, "%s: db_fetch return incorrect data: %s vs %d\n", argv[0],
                    OPAL_NAME_PRINT(data), i);
            return 1;
        }
        free(key);
    }

    /* fetch a pointer to data and ensure the
     * pointer and its value is correct
     */
    if (ORTE_SUCCESS != (rc = opal_db.fetch_pointer((opal_identifier_t*)&uid, "pointer-value", (void**)&fval, OPAL_FLOAT))) {
        fprintf(stderr, "%s: db_fetch_pointer failed (%d)\n", argv[0], rc);
        return rc;
    }
    if (kvs.data.fval != (*fval)) {
        fprintf(stderr, "%s: db_fetch_pointer return incorrect data: %f vs %f\n", argv[0], *fval, kvs.data.fval);
        return 1;
    }

    /* remove one key */
    if (ORTE_SUCCESS != (rc = opal_db.remove((opal_identifier_t*)&uid, "foo.0"))) {
        fprintf(stderr, "%s: db_remove failed (%d)\n", argv[0], rc);
        return rc;
    }

    /* verify that it no longer is in the store */
    if (ORTE_SUCCESS == (rc = opal_db.fetch(&uid, "foo.0", (void**)&dptr, OPAL_NAME))) {
        fprintf(stderr, "%s: db_fetch succeeded when it should have failed\n", argv[0]);
        return 1;
    }

    /* remove the pointer key */
    if (ORTE_SUCCESS != (rc = opal_db.remove((opal_identifier_t*)&uid, "pointer-value"))) {
        fprintf(stderr, "%s: db_remove failed (%d)\n", argv[0], rc);
        return rc;
    }

    /* verify that it no longer is in the store */
    if (ORTE_SUCCESS == (rc = opal_db.fetch_pointer((opal_identifier_t*)&uid, "pointer-value", (void**)&fval, OPAL_FLOAT))) {
        fprintf(stderr, "%s: db_fetch_pointer succeeded when it should have failed\n", argv[0]);
        return 1;
    }

    /* remove all keys */
    if (ORTE_SUCCESS != (rc = opal_db.remove((opal_identifier_t*)&uid, NULL))) {
        fprintf(stderr, "%s: db_remove failed (%d)\n", argv[0], rc);
        return rc;
    }

    /* verify that nothing is in the store */
    OBJ_CONSTRUCT(&kvlist, opal_list_t);
    if (ORTE_SUCCESS != (rc = opal_db.fetch_multiple((opal_identifier_t*)&uid, OPAL_SCOPE_ALL,
                                                     NULL, &kvlist))) {
        fprintf(stderr, "%s: db_fetch_multiple failed\n", argv[0]);
        return 1;
    }
    if (0 < opal_list_get_size(&kvlist)) {
        fprintf(stderr, "%s: db_fetch_multiple returned values when the db should have been empty\n", argv[0]);
        return 1;
    }

    mca_base_framework_close(&opal_db_base_framework);

    fprintf(stderr, "%s: SUCCESS\n", argv[0]);
    opal_finalize_test();
    return 0;
}
