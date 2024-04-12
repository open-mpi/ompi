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
 * Copyright (c) 2011-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2015-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * Copyright (c) 2024      Nanook Consulting  All rights reserved.
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

#include "src/util/hostfile/hostfile_lex.h"

/*
 * local functions
 */
BEGIN_C_DECLS

int prte_util_hostfile_wrap(void);

END_C_DECLS

int prte_util_hostfile_wrap(void)
{
    prte_util_hostfile_done = true;
    return 1;
}


/*
 * global variables
 */
int prte_util_hostfile_line=1;
prte_hostfile_value_t prte_util_hostfile_value = {0};
bool prte_util_hostfile_done = false;

#ifndef YY_BREAK
#define YY_BREAK
#endif

%}

WHITE       [\f\t\v ]

%x comment

%%

{WHITE}*\n         { prte_util_hostfile_line++;
                     return PRTE_HOSTFILE_NEWLINE; }
#.*\n              { prte_util_hostfile_line++;
                     return PRTE_HOSTFILE_NEWLINE; }
"//".*\n           { prte_util_hostfile_line++;
                     return PRTE_HOSTFILE_NEWLINE; }

"/*"               { BEGIN(comment);
                     return PRTE_HOSTFILE_NEWLINE; }
<comment>[^*\n]*       { break; } /* Eat up non '*'s */
<comment>"*"+[^*/\n]*  { break; } /* Eat '*'s not followed by a '/' */
<comment>\n        { prte_util_hostfile_line++;
                     return PRTE_HOSTFILE_NEWLINE; }
<comment>"*"+"/"   { BEGIN(INITIAL); /* Done with Block Comment */
                     return PRTE_HOSTFILE_NEWLINE; }

\"[^\"]*\"         { prte_util_hostfile_value.sval = yytext;
                     return PRTE_HOSTFILE_QUOTED_STRING; }

{WHITE}+           { break; }			/* whitespace */

"="                { return PRTE_HOSTFILE_EQUAL; }

cpu                { prte_util_hostfile_value.sval = yytext;
                     return PRTE_HOSTFILE_CPU; }
count              { prte_util_hostfile_value.sval = yytext;
                     return PRTE_HOSTFILE_COUNT; }
slots              { prte_util_hostfile_value.sval = yytext;
                     return PRTE_HOSTFILE_SLOTS; }
"slots-max"        { prte_util_hostfile_value.sval = yytext;
                     return PRTE_HOSTFILE_SLOTS_MAX; }
slots_max          { prte_util_hostfile_value.sval = yytext;
                     return PRTE_HOSTFILE_SLOTS_MAX; }
"max-slots"        { prte_util_hostfile_value.sval = yytext;
                     return PRTE_HOSTFILE_SLOTS_MAX; }
max_slots          { prte_util_hostfile_value.sval = yytext;
                     return PRTE_HOSTFILE_SLOTS_MAX; }
"cpu-max"          { prte_util_hostfile_value.sval = yytext;
                     return PRTE_HOSTFILE_SLOTS_MAX; }
cpu_max            { prte_util_hostfile_value.sval = yytext;
                     return PRTE_HOSTFILE_SLOTS_MAX; }
"max-cpu"          { prte_util_hostfile_value.sval = yytext;
                     return PRTE_HOSTFILE_SLOTS_MAX; }
max_cpu            { prte_util_hostfile_value.sval = yytext;
                     return PRTE_HOSTFILE_SLOTS_MAX; }
"count-max"        { prte_util_hostfile_value.sval = yytext;
                     return PRTE_HOSTFILE_SLOTS_MAX; }
count_max          { prte_util_hostfile_value.sval = yytext;
                     return PRTE_HOSTFILE_SLOTS_MAX; }
"max-count"        { prte_util_hostfile_value.sval = yytext;
                     return PRTE_HOSTFILE_SLOTS_MAX; }
max_count          { prte_util_hostfile_value.sval = yytext;
                     return PRTE_HOSTFILE_SLOTS_MAX; }

rank                { prte_util_hostfile_value.sval = yytext;
                     return PRTE_HOSTFILE_RANK; }
slot               { prte_util_hostfile_value.sval = yytext;
                     return PRTE_HOSTFILE_SLOT; }

username           { prte_util_hostfile_value.sval = yytext;
                     return PRTE_HOSTFILE_USERNAME; }
"user-name"        { prte_util_hostfile_value.sval = yytext;
                     return PRTE_HOSTFILE_USERNAME; }
"user_name"        { prte_util_hostfile_value.sval = yytext;
                     return PRTE_HOSTFILE_USERNAME; }

port               { prte_util_hostfile_value.sval = yytext;
                     return PRTE_HOSTFILE_PORT; }

boards             { prte_util_hostfile_value.sval = yytext;
                     return PRTE_HOSTFILE_BOARDS; }

sockets            { prte_util_hostfile_value.sval = yytext;
                     return PRTE_HOSTFILE_SOCKETS_PER_BOARD; }

sockets_per_board  { prte_util_hostfile_value.sval = yytext;
                     return PRTE_HOSTFILE_SOCKETS_PER_BOARD; }

"sockets-per-board" { prte_util_hostfile_value.sval = yytext;
                     return PRTE_HOSTFILE_SOCKETS_PER_BOARD; }

cores              { prte_util_hostfile_value.sval = yytext;
                     return PRTE_HOSTFILE_CORES_PER_SOCKET; }

cores_per_socket   { prte_util_hostfile_value.sval = yytext;
                     return PRTE_HOSTFILE_CORES_PER_SOCKET; }

"cores-per-socket" { prte_util_hostfile_value.sval = yytext;
                     return PRTE_HOSTFILE_CORES_PER_SOCKET; }

\+n[0-9]+	   { prte_util_hostfile_value.sval = yytext;
                     return PRTE_HOSTFILE_RELATIVE; }
\+[eE][\:][0-9]+   { prte_util_hostfile_value.sval = yytext;
                     return PRTE_HOSTFILE_RELATIVE; }
\+[eE]             { prte_util_hostfile_value.sval = yytext;
                     return PRTE_HOSTFILE_RELATIVE; }


[0-9]+             { prte_util_hostfile_value.ival = atol(yytext);
                     return PRTE_HOSTFILE_INT; }
%{ /* First detect hosts as standard Strings (but without ".")
    * then username@IPv4 or IPV4, then username@IPv6 or IPv6,
    * followed by username@hostname or hostname
    */
%}

[A-Za-z0-9_\-,:*@]*  { prte_util_hostfile_value.sval = yytext;
                     return PRTE_HOSTFILE_STRING; }

([A-Za-z0-9][A-Za-z0-9_\-]*"@")?([0-9]{1,3}"."){3}[0-9]{1,3} {
                      prte_util_hostfile_value.sval = yytext;
                      return PRTE_HOSTFILE_IPV4; }

([A-Za-z0-9][A-Za-z0-9_\-]*"@")?([A-Fa-f0-9]{0,4}":")+[":"]*([A-Fa-f0-9]{0,4}":")+[A-Fa-f0-9]{1,4} {
                      prte_util_hostfile_value.sval = yytext;
                      return PRTE_HOSTFILE_IPV6; }

(\^?[A-Za-z0-9][A-Za-z0-9_\-]*"@")?[A-Za-z0-9][A-Za-z0-9_\-\.]*  {
                     prte_util_hostfile_value.sval = yytext;
                     return PRTE_HOSTFILE_HOSTNAME; }

.                  { prte_util_hostfile_value.sval = yytext;
                     return PRTE_HOSTFILE_ERROR; }

%%
