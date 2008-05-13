/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
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

#include "opal_config.h"

#include <stdio.h>
#include <string.h>

#include "opal/constants.h"

#include "opal/util/printf.h"

#include "filter_xml.h"


/*
 * Local functions
 */
static int xml_init(void);
static int xml_finalize(void);
static char* xml_process(char *str, int major_id, int minor_id, int num_tags, char **tags);

/*
 * module
 */
opal_filter_base_module_t opal_filter_xml_module = {
    /* Initialization function */
    xml_init,
    /* Finalize function */
    xml_finalize,
    /* Process function */
    xml_process
};

static int xml_init(void)
{
    return OPAL_SUCCESS;
}

static int xml_finalize(void)
{
    return OPAL_SUCCESS;
}

static char* xml_process(char *str, int major_id, int minor_id, int num_tags, char **tags)
{
    char *tmp, *tmp2;
    int i, len;
    uint16_t major_up, major_dn;
    
    /* strip trailing newlines from the data */
    len = strlen(str)-1;
    if (str[len] == '\n' || str[len] == '\r') {
        str[len] = '\0';
    }
    
    /* convert internal newlines */
    for (i=0; i < len; i++) {
        if (str[i] == '\n' || str[i] == '\r') {
            str[i] = ';';
        }
    }
    
    /* convert major-id */
    major_up = (0xffff0000 & major_id) >> 16;
    major_dn = 0x0000ffff & major_id;
    
    tmp = NULL;
    if (1 < num_tags) {
        asprintf(&tmp, " | %s", (NULL == tags[1]) ? "UNKNOWN" : tags[1]);
        for (i=2; i < num_tags; i++) {
            asprintf(&tmp2, "%s | %s", tmp, (NULL == tags[i]) ? "UNKNOWN" : tags[i]);
            free(tmp);
            tmp = tmp2;
        }
    }
    
    asprintf(&tmp2, "<xml| [[%d,%d],%d] | %s%s | %s |xml>",
             major_up, major_dn, minor_id,
             (NULL == tags[0]) ? "UNKNOWN" : tags[0],
             (NULL == tmp) ? "" : tmp, str);
        
    return tmp2;
}
