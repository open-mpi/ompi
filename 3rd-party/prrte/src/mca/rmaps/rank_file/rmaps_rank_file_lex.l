%option nounput
%option noinput

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
 * Copyright (c) 2013      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 *
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "prte_config.h"

#include <stdio.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "src/mca/rmaps/rank_file/rmaps_rank_file_lex.h"
#include "src/util/pmix_output.h"

BEGIN_C_DECLS

int prte_rmaps_rank_file_yywrap(void);

END_C_DECLS

/*
 * global variables
 */
int prte_rmaps_rank_file_line=1;
prte_rmaps_rank_file_value_t prte_rmaps_rank_file_value = {0};
bool prte_rmaps_rank_file_done = false;

#ifndef YY_BREAK
#define YY_BREAK
#endif

%}

WHITE       [\f\t\v ]

%x comment

%%

{WHITE}*\n         { prte_rmaps_rank_file_line++;
                     return PRTE_RANKFILE_NEWLINE; }
#.*\n              { prte_rmaps_rank_file_line++;
                     return PRTE_RANKFILE_NEWLINE; }
"//".*\n           { prte_rmaps_rank_file_line++;
                     return PRTE_RANKFILE_NEWLINE; }

"/*"               { BEGIN(comment);
                     return PRTE_RANKFILE_NEWLINE; }
<comment>[^*\n]*       { break; } /* Eat up non '*'s */
<comment>"*"+[^*/\n]*  { break; } /* Eat '*'s not followed by a '/' */
<comment>\n        { prte_rmaps_rank_file_line++;
                     return PRTE_RANKFILE_NEWLINE; }
<comment>"*"+"/"   { BEGIN(INITIAL); /* Done with Block Comment */
                     return PRTE_RANKFILE_NEWLINE; }

\"[^\"]*\"         { prte_rmaps_rank_file_value.sval = yytext;
                     return PRTE_RANKFILE_QUOTED_STRING; }

{WHITE}+           { break; }

"="                { return PRTE_RANKFILE_EQUAL; }

rank                { prte_rmaps_rank_file_value.sval = yytext;
                     return PRTE_RANKFILE_RANK; }
slot               { prte_rmaps_rank_file_value.sval = yytext;
                     return PRTE_RANKFILE_SLOT; }
slots              { prte_rmaps_rank_file_value.sval = yytext;
                     return PRTE_RANKFILE_SLOT; }

username           { prte_rmaps_rank_file_value.sval = yytext;
                     return PRTE_RANKFILE_USERNAME; }
"user-name"        { prte_rmaps_rank_file_value.sval = yytext;
                     return PRTE_RANKFILE_USERNAME; }
"user_name"        { prte_rmaps_rank_file_value.sval = yytext;
                     return PRTE_RANKFILE_USERNAME; }

[0-9]+             { prte_rmaps_rank_file_value.ival = atol(yytext);
                     return PRTE_RANKFILE_INT; }
%{ /* First detect hosts as standard Strings (but without ".")
    * then username@IPv4 or IPV4, then username@IPv6 or IPv6,
    * followed by username@hostname or hostname
    */
%}

[A-Za-z0-9_\-,\;:*@]*  { prte_rmaps_rank_file_value.sval = yytext;
                         return PRTE_RANKFILE_STRING; }

([A-Za-z0-9][A-Za-z0-9_\-]*"@")?([0-9]{1,3}"."){3}[0-9]{1,3} {
                      prte_rmaps_rank_file_value.sval = yytext;
                      return PRTE_RANKFILE_IPV4; }

([A-Za-z0-9][A-Za-z0-9_\-]*"@")?([A-Fa-f0-9]{0,4}":")+[":"]*([A-Fa-f0-9]{0,4}":")+[A-Fa-f0-9]{1,4} {
                      prte_rmaps_rank_file_value.sval = yytext;
                      return PRTE_RANKFILE_IPV6; }

([A-Za-z0-9][A-Za-z0-9_\-]*"@")?[A-Za-z][A-Za-z0-9_\-\.]*  {
                     prte_rmaps_rank_file_value.sval = yytext;
                     return PRTE_RANKFILE_HOSTNAME; }

\+n[0-9]+           { prte_rmaps_rank_file_value.sval = yytext;
					 return PRTE_RANKFILE_RELATIVE; }

.                  { prte_rmaps_rank_file_value.sval = yytext;
                     return PRTE_RANKFILE_ERROR; }

%%

/* Old flex (2.5.4a? and older) does not define a destroy function */
#if !defined(YY_FLEX_SUBMINOR_VERSION)
#define YY_FLEX_SUBMINOR_VERSION 0
#endif

#if (YY_FLEX_MAJOR_VERSION < 2) || (YY_FLEX_MAJOR_VERSION == 2 && (YY_FLEX_MINOR_VERSION < 5 || (YY_FLEX_MINOR_VERSION == 5 && YY_FLEX_SUBMINOR_VERSION < 5)))
int prte_rmaps_rank_file_lex_destroy (void)
{
    if (NULL != YY_CURRENT_BUFFER) {
        yy_delete_buffer(YY_CURRENT_BUFFER);
#if defined(YY_CURRENT_BUFFER_LVALUE)
        YY_CURRENT_BUFFER_LVALUE = NULL;
#else
        YY_CURRENT_BUFFER = NULL;
#endif  /* YY_CURRENT_BUFFER_LVALUE */
    }
    return YY_NULL;
}
#endif

int prte_rmaps_rank_file_wrap(void)
{
    prte_rmaps_rank_file_done = true;
    return 1;
}
