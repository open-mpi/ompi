/*
 * Copyright (c) 2007 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PLPA_TASKSET_H
#define PLPA_TASKSET_H

#include <plpa.h>

/*
 * Function in flexer to set up the parser to read from a string
 * (vs. reading from a file)
 */
void parser_setup_string(char* str);

/*
 * Main bison parser.
 */
int token_parse(PLPA_NAME(cpu_set_t) *cpu_set);

/*
 * Main flex parser
 */
int yylex(void);


#endif
