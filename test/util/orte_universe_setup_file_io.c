/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif

#include "orte/util/univ_info.h"
#include "orte/util/universe_setup_file_io.h"
#include "support.h"

static bool test1(void);   /* verify it returns info */
static bool test2(void);   /* test second time through */

orte_universe_t orte_universe_info;

int main(int argc, char* argv[])
{
    test_init("universe_setup_file_io");

    /* initialize the univ_info structure */
    orte_universe_info.name = strdup("default-universe");
    orte_universe_info.host = strdup("test-host.domain.org");
    orte_universe_info.uid = strdup("this-is-me");
    orte_universe_info.persistence = true;
    orte_universe_info.scope = strdup("private");
    orte_universe_info.console = false;
    orte_universe_info.seed_uri = strdup("0.0.0;tcp://128.165.148.81:52424");
    orte_universe_info.console_connected = false;
    orte_universe_info.scriptfile = NULL;
    
    if (test1()) {
        test_success();
    }
    else {
      test_failure("universe_setup_file_io write failed");
    }

    if (test2()) {
        test_success();
    }
    else {
      test_failure("universe_setup_file_io read failed");
    }

    test_finalize();
    return 0;
}


static bool test1(void)
{
    int rc;
    
    /* Test write */

    if (ORTE_SUCCESS != (rc = orte_write_universe_setup_file("test-file", &orte_universe_info))) {
        fprintf(stderr, "universe_setup_file_io: failed write with code %d\n", rc);
        return false;
    }
    
    return true;
}


static bool test2(void)
{
    int rc;
    orte_universe_t univ;
    
    /* Test read */

    if (ORTE_SUCCESS != (rc = orte_read_universe_setup_file("test-file", &univ))) {
        fprintf(stderr, "universe_setup_file_io: failed read with code %d\n", rc);
        return false;
    }
    
    if (0 != strcmp(orte_universe_info.name, univ.name) ||
        0 != strcmp(orte_universe_info.host, univ.host) ||
        0 != strcmp(orte_universe_info.uid, univ.uid) ||
        orte_universe_info.persistence != univ.persistence ||
        0 != strcmp(orte_universe_info.scope, univ.scope) ||
        orte_universe_info.console != univ.console ||
        0 != strcmp(orte_universe_info.seed_uri, univ.seed_uri) ||
        orte_universe_info.console_connected != univ.console_connected ||
        orte_universe_info.scriptfile != univ.scriptfile) {
        fprintf(stderr, "universe_setup_file_io: read mismatch\n");
        return false;
    }
    
    return true;
}
