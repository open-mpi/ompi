%{ /* -*- C -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"

#include <stdio.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "mca/rds/hostfile/rds_hostfile_lex.h"

/*
 * local functions
 */
static int orte_rds_hostfile_wrap(void);

/*
 * global variables
 */
int orte_rds_hostfile_line=1;
orte_rds_value_t orte_rds_hostfile_value;
bool orte_rds_hostfile_done = false;

%}

%%

\n                 { orte_rds_hostfile_line++; 
                     return ORTE_RDS_HOSTFILE_NEWLINE; }

\"[^\"]*\"         { orte_rds_hostfile_value.sval = yytext; 
                     return ORTE_RDS_HOSTFILE_QUOTED_STRING; }

[\f\t\v ]          ;			/* whitespace */

"="                { return ORTE_RDS_HOSTFILE_EQUAL; }

cpu                { orte_rds_hostfile_value.sval = yytext; 
                     return ORTE_RDS_HOSTFILE_CPU; }
count              { orte_rds_hostfile_value.sval = yytext; 
                     return ORTE_RDS_HOSTFILE_COUNT; }
slots              { orte_rds_hostfile_value.sval = yytext; 
                     return ORTE_RDS_HOSTFILE_SLOTS; }
slots-max          { orte_rds_hostfile_value.sval = yytext; 
                     return ORTE_RDS_HOSTFILE_SLOTS_MAX; }

[0-9]+             { orte_rds_hostfile_value.ival = atol(yytext); 
                     return ORTE_RDS_HOSTFILE_INT; }

[A-Za-z0-9_\-\.]*  { orte_rds_hostfile_value.sval = yytext; 
                     return ORTE_RDS_HOSTFILE_STRING; }

.	               { orte_rds_hostfile_value.sval = yytext; 
                     return ORTE_RDS_HOSTFILE_ERROR; }

%%


static int orte_rds_hostfile_wrap(void)
{
    orte_rds_hostfile_done = true;
    return 1;
}

