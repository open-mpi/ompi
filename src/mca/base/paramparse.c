/* -*- C -*-
 *
 * $HEADER$
 *
 */

#include "mca/base/base.h"
#include "mca/base/mca_base_parse_paramfile_lex.h"

#include <stdio.h>
#include <stdlib.h>

extern int mca_base_yylex(void);
extern FILE *mca_base_yyin;

int main(int argc, char *argv[])
{
    int ret;

    if (argc != 2) {
        printf("usage: %s <paramfile>\n", argv[0]);
        exit(1);
    }

    mca_base_yyin = fopen(argv[1], "r");
    while (!mca_base_parse_done) {
        ret = mca_base_yylex();
        printf("%d: [%s]\n", ret, mca_base_yytext);
    }

    return 0;
}
