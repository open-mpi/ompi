#ifndef COLL_ML_LEX_H_
#define COLL_ML_LEX_H_

#include "opal_config.h"
#include <stdio.h>

BEGIN_C_DECLS

int coll_ml_config_yylex(void);
int coll_ml_config_init_buffer(FILE *file);
int coll_ml_config_yylex_destroy(void);

extern FILE *coll_ml_config_yyin;
extern bool coll_ml_config_parse_done;
extern char *coll_ml_config_yytext;
extern int coll_ml_config_yynewlines;

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
    COLL_ML_CONFIG_PARSE_DONE,
    COLL_ML_CONFIG_PARSE_ERROR,
    COLL_ML_CONFIG_PARSE_NEWLINE,
    COLL_ML_CONFIG_PARSE_SECTION,
    COLL_ML_CONFIG_PARSE_COLLECTIVE,
    COLL_ML_CONFIG_PARSE_EQUAL,
    COLL_ML_CONFIG_PARSE_SINGLE_WORD,
    COLL_ML_CONFIG_PARSE_VALUE,
    COLL_ML_CONFIG_PARSE_MAX
};
END_C_DECLS
#endif
