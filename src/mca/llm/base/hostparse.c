/* -*- C -*-
 *
 * $HEADER$
 *
 */

#include "mca/llm/base/base.h"
#include "mca/llm/base/base_internal.h"
#include "mca/llm/base/llm_base_parse_hostfile_lex.h"

#include <stdio.h>
#include <stdlib.h>

extern int yylex(void);
extern FILE *mca_llm_base_yyin;

int
main(int argc, char *argv[])
{
    int ret;

    if (argc != 2) {
        printf("usage: %s <hostfile>\n", argv[0]);
        exit(1);
    }

    mca_llm_base_yyin = fopen(argv[1], "r");
    while (!mca_llm_base_parse_done) {
        ret = mca_llm_base_yylex();
        printf("%d: %s\n", ret, mca_llm_base_string);
    }

    return 0;
}
