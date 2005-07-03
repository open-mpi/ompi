/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "ompi_config.h"

#include <stdio.h>
#include <string.h>

#include "include/constants.h"
#include "opal/class/opal_list.h"
#include "util/output.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/base/mca_base_param_internal.h"
#include "mca/base/mca_base_parse_paramfile_lex.h"


static const char *filename;


static int parse_line(void);
static void save_value(char *name, char *value);
static void parse_error(int num);


int mca_base_parse_paramfile(const char *paramfile)
{
    int val;

    /* Open the parameter file */

    mca_base_yyin = fopen(paramfile, "r");
    if (NULL == mca_base_yyin) {
        return OMPI_ERR_NOT_FOUND;
    }

    filename = paramfile;
    mca_base_parse_done = false;
    mca_base_yynewlines = 1;
    mca_base_param_init_buffer(mca_base_yyin);
    while (!mca_base_parse_done) {
        val = mca_base_yylex();
        switch (val) {
        case MCA_BASE_PARSE_DONE:
            /* This will also set mca_base_parse_done to true, so just
               break here */
            break;

        case MCA_BASE_PARSE_NEWLINE:
            /* blank line!  ignore it */
            break;

        case MCA_BASE_PARSE_SINGLE_WORD:
            parse_line();
            break;

        default:
            /* anything else is an error */
            parse_error(1);
            break;
        }
    }
    fclose(mca_base_yyin);

    return OMPI_SUCCESS;
}


static int parse_line(void)
{
    int val;
    char *name;

    /* Save the parameter name */

    name = strdup(mca_base_yytext);

    /* The first thing we have to see is an "=" */

    val = mca_base_yylex();
    if (mca_base_parse_done || MCA_BASE_PARSE_EQUAL != val) {
        parse_error(2);
        free(name);
        return OMPI_ERROR;
    }

    /* Next we get the value */

    val = mca_base_yylex();
    if (MCA_BASE_PARSE_SINGLE_WORD == val ||
        MCA_BASE_PARSE_VALUE == val) {
        save_value(name, mca_base_yytext);

        /* Now we need to see the newline */

        val = mca_base_yylex();
        if (MCA_BASE_PARSE_NEWLINE == val) {
            return OMPI_SUCCESS;
        }
    }

    /* Did we get an EOL or EOF? */

    else if (MCA_BASE_PARSE_DONE == val ||
             MCA_BASE_PARSE_NEWLINE == val) {
        save_value(name, NULL);
        return OMPI_SUCCESS;
    }

    /* Nope -- we got something unexpected.  Bonk! */

    parse_error(3);
    free(name);
    return OMPI_ERROR;
}


static void save_value(char *name, char *value)
{
    opal_list_item_t *item;
    mca_base_param_file_value_t *fv;

    /* First traverse through the list and ensure that we don't
       already have a param of this name.  If we do, just replace the
       value. */

    for (item = opal_list_get_first(&mca_base_param_file_values);
         opal_list_get_end(&mca_base_param_file_values) != item;
         item = opal_list_get_next(item)) {
        fv = (mca_base_param_file_value_t *) item;
        if (0 == strcmp(name, fv->mbpfv_param)) {
            free(name);
            free(fv->mbpfv_value);
            fv->mbpfv_value = strdup(value);
            return;
        }
    }

    /* We didn't already have the param, so append it to the list */

    fv = OBJ_NEW(mca_base_param_file_value_t);
    if (NULL != fv) {
        fv->mbpfv_param = name;
        if (NULL != value) {
            fv->mbpfv_value = strdup(value);
        } else {
            fv->mbpfv_value = NULL;
        }
        opal_list_append(&mca_base_param_file_values, (opal_list_item_t*) fv);
    }
}


static void parse_error(int num)
{
    /* JMS need better error/warning message here */
    ompi_output(0, "paramfile: error %d reading file %s at line %d:\n  %s\n",
                num, filename, mca_base_yynewlines, mca_base_yytext);
}
