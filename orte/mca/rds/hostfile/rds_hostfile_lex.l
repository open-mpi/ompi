%{ /* -*- C -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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

#include <stdio.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "orte/mca/rds/hostfile/rds_hostfile_lex.h"

/*
 * local functions
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif  /* defined(c_plusplus) || defined(__cplusplus) */

int orte_rds_hostfile_wrap(void);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif  /* defined(c_plusplus) || defined(__cplusplus) */

int orte_rds_hostfile_wrap(void)
{
    orte_rds_hostfile_done = true;
    return 1;
}

/*
 * global variables
 */
int orte_rds_hostfile_line=1;
orte_rds_value_t orte_rds_hostfile_value;
bool orte_rds_hostfile_done = false;

%}

WHITE       [\f\t\v ]

%x comment

%%

{WHITE}*\n         { orte_rds_hostfile_line++; 
                     return ORTE_RDS_HOSTFILE_NEWLINE; }
#.*\n              { orte_rds_hostfile_line++; 
                     return ORTE_RDS_HOSTFILE_NEWLINE; }
"//".*\n           { orte_rds_hostfile_line++; 
                     return ORTE_RDS_HOSTFILE_NEWLINE; }

"/*"               { BEGIN(comment);
                     return ORTE_RDS_HOSTFILE_NEWLINE; }
<comment>[^*\n]*       ; /* Eat up non '*'s */
<comment>"*"+[^*/\n]*  ; /* Eat '*'s not followed by a '/' */
<comment>\n        { orte_rds_hostfile_line++;
                     return ORTE_RDS_HOSTFILE_NEWLINE; } 
<comment>"*"+"/"   { BEGIN(INITIAL); /* Done with Block Comment */
                     return ORTE_RDS_HOSTFILE_NEWLINE; }

\"[^\"]*\"         { orte_rds_hostfile_value.sval = yytext; 
                     return ORTE_RDS_HOSTFILE_QUOTED_STRING; }

{WHITE}+           ;			/* whitespace */

"="                { return ORTE_RDS_HOSTFILE_EQUAL; }

cpu                { orte_rds_hostfile_value.sval = yytext; 
                     return ORTE_RDS_HOSTFILE_CPU; }
count              { orte_rds_hostfile_value.sval = yytext; 
                     return ORTE_RDS_HOSTFILE_COUNT; }
slots              { orte_rds_hostfile_value.sval = yytext; 
                     return ORTE_RDS_HOSTFILE_SLOTS; }

"slots-max"        { orte_rds_hostfile_value.sval = yytext; 
                     return ORTE_RDS_HOSTFILE_SLOTS_MAX; }
slots_max          { orte_rds_hostfile_value.sval = yytext; 
                     return ORTE_RDS_HOSTFILE_SLOTS_MAX; }
"max-slots"        { orte_rds_hostfile_value.sval = yytext; 
                     return ORTE_RDS_HOSTFILE_SLOTS_MAX; }
max_slots          { orte_rds_hostfile_value.sval = yytext; 
                     return ORTE_RDS_HOSTFILE_SLOTS_MAX; }
"cpu-max"          { orte_rds_hostfile_value.sval = yytext; 
                     return ORTE_RDS_HOSTFILE_SLOTS_MAX; }
cpu_max            { orte_rds_hostfile_value.sval = yytext; 
                     return ORTE_RDS_HOSTFILE_SLOTS_MAX; }
"max-cpu"          { orte_rds_hostfile_value.sval = yytext; 
                     return ORTE_RDS_HOSTFILE_SLOTS_MAX; }
max_cpu            { orte_rds_hostfile_value.sval = yytext; 
                     return ORTE_RDS_HOSTFILE_SLOTS_MAX; }
"count-max"        { orte_rds_hostfile_value.sval = yytext; 
                     return ORTE_RDS_HOSTFILE_SLOTS_MAX; }
count_max          { orte_rds_hostfile_value.sval = yytext; 
                     return ORTE_RDS_HOSTFILE_SLOTS_MAX; }
"max-count"        { orte_rds_hostfile_value.sval = yytext; 
                     return ORTE_RDS_HOSTFILE_SLOTS_MAX; }
max_count          { orte_rds_hostfile_value.sval = yytext; 
                     return ORTE_RDS_HOSTFILE_SLOTS_MAX; }

username           { orte_rds_hostfile_value.sval = yytext;
                     return ORTE_RDS_HOSTFILE_USERNAME; }
"user-name"        { orte_rds_hostfile_value.sval = yytext;
                     return ORTE_RDS_HOSTFILE_USERNAME; }
"user_name"        { orte_rds_hostfile_value.sval = yytext;
                     return ORTE_RDS_HOSTFILE_USERNAME; }

[0-9]+             { orte_rds_hostfile_value.ival = atol(yytext); 
                     return ORTE_RDS_HOSTFILE_INT; }
%{ /* First detect hosts as standard Strings (but without ".")
    * then username@IPv4 or IPV4, then username@hostname or hostname
    */
%}

[A-za-z0-9_\-]*  { orte_rds_hostfile_value.sval = yytext;
                     return ORTE_RDS_HOSTFILE_STRING; }

([A-Za-z0-9][A-Za-z0-9_\-]*"@")?([0-9]{1,3}"."){3}[0-9]{1,3} {
                      orte_rds_hostfile_value.sval = yytext;
                      return ORTE_RDS_HOSTFILE_IPV4; }

([A-Za-z0-9][A-Za-z0-9_\-]*"@")?[A-Za-z][A-Za-z0-9_\-\.]*  {
                     orte_rds_hostfile_value.sval = yytext; 
                     return ORTE_RDS_HOSTFILE_HOSTNAME; }

.                  { orte_rds_hostfile_value.sval = yytext; 
                     return ORTE_RDS_HOSTFILE_ERROR; }

%%
