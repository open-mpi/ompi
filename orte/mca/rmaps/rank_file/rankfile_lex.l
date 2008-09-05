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
 * Copyright (c) 2008      Voltaire. All rights reserved
 *  
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "orte_config.h"

#include <stdio.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include "rankfile_lex.h"

/*
 * local functions
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif  /* defined(c_plusplus) || defined(__cplusplus) */

/*int rank_file_wrap(void);*/

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif  /* defined(c_plusplus) || defined(__cplusplus) */


int rank_file_wrap(void)
{
    rank_file_done = true;
    return 1;
}

/*
 * global variables
 */
int rank_file_line=1;
rank_file_value_t rank_file_value;
bool rank_file_done = false;

%}

WHITE       [\f\t\v ]

%x comment

%%

{WHITE}*\n         { rank_file_line++; 
                     return ORTE_RANKFILE_NEWLINE; }
#.*\n              { rank_file_line++; 
                     return ORTE_RANKFILE_NEWLINE; }
"//".*\n           { rank_file_line++; 
                     return ORTE_RANKFILE_NEWLINE; }

"/*"               { BEGIN(comment);
                     return ORTE_RANKFILE_NEWLINE; }
<comment>[^*\n]*       ; /* Eat up non '*'s */
<comment>"*"+[^*/\n]*  ; /* Eat '*'s not followed by a '/' */
<comment>\n        { rank_file_line++;
                     return ORTE_RANKFILE_NEWLINE; } 
<comment>"*"+"/"   { BEGIN(INITIAL); /* Done with Block Comment */
                     return ORTE_RANKFILE_NEWLINE; }

\"[^\"]*\"         { rank_file_value.sval = yytext; 
                     return ORTE_RANKFILE_QUOTED_STRING; }

{WHITE}+           ;			

"="                { return ORTE_RANKFILE_EQUAL; }

rank                { rank_file_value.sval = yytext; 
                     return ORTE_RANKFILE_RANK; }
slot               { rank_file_value.sval = yytext; 
                     return ORTE_RANKFILE_SLOT; }

username           { rank_file_value.sval = yytext;
                     return ORTE_RANKFILE_USERNAME; }
"user-name"        { rank_file_value.sval = yytext;
                     return ORTE_RANKFILE_USERNAME; }
"user_name"        { rank_file_value.sval = yytext;
                     return ORTE_RANKFILE_USERNAME; }

[0-9]+             { rank_file_value.ival = atol(yytext); 
                     return ORTE_RANKFILE_INT; }
%{ /* First detect hosts as standard Strings (but without ".")
    * then username@IPv4 or IPV4, then username@IPv6 or IPv6,
    * followed by username@hostname or hostname
    */
%}

[A-za-z0-9_\-,:*@]*  { rank_file_value.sval = yytext;
                     return ORTE_RANKFILE_STRING; }

([A-Za-z0-9][A-Za-z0-9_\-]*"@")?([0-9]{1,3}"."){3}[0-9]{1,3} {
                      rank_file_value.sval = yytext;
                      return ORTE_RANKFILE_IPV4; }

([A-Za-z0-9][A-Za-z0-9_\-]*"@")?([A-Fa-f0-9]{0,4}":")+[":"]*([A-Fa-f0-9]{0,4}":")+[A-Fa-f0-9]{1,4} {
                      rank_file_value.sval = yytext;
                      return ORTE_RANKFILE_IPV6; }

([A-Za-z0-9][A-Za-z0-9_\-]*"@")?[A-Za-z][A-Za-z0-9_\-\.]*  {
                     rank_file_value.sval = yytext; 
                     return ORTE_RANKFILE_HOSTNAME; }

.                  { rank_file_value.sval = yytext; 
                     return ORTE_RANKFILE_ERROR; }

%%

