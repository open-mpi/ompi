/*
 * Copyright (c) 2007      Los Alamos National Security, LLC.
 *                         All righs reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef BTL_PCIE_CFG_LEX_H_
#define BTL_PCIE_CFG_LEX_H_

#include "opal_config.h"

#ifdef malloc
#undef malloc
#endif
#ifdef realloc
#undef realloc
#endif
#ifdef free
#undef free
#endif

#include <stdio.h>

int btl_pcie_cfg_yylex(void);
int btl_pcie_cfg_init_buffer(FILE *file);

extern FILE *btl_pcie_cfg_yyin;
extern bool btl_pcie_cfg_parse_done;
extern char *btl_pcie_cfg_yytext;
extern int btl_pcie_cfg_yynewlines;

/*
 * Make lex-generated files not issue compiler warnings
 */
#define YY_STACK_USED 0
#define YY_ALWAYS_INTERACTIVE 0
#define YY_NEVER_INTERACTIVE 0
#define YY_MAIN 0
#define YY_NO_UNPUT 1
#define YY_SKIP_YYWRAP 1

enum {
    BTL_PCIE_CFG_PARSE_DONE = 1,
    BTL_PCIE_CFG_PARSE_ERROR,

    BTL_PCIE_CFG_PARSE_NEWLINE,
    BTL_PCIE_CFG_PARSE_HOSTNAME_CORE,
    BTL_PCIE_CFG_PARSE_HOSTNAME_DEVICE,
    BTL_PCIE_CFG_PARSE_DEVICE,

    BTL_PCIE_CFG_PARSE_MAX
};

#endif /* #ifndef BTL_PCIE_CFG_LEX_H_ */
