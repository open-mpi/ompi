/* -*- C -*-
 *
 */

#ifndef MCA_LLM_BASE_PARSE_HOSTFILE_LEX_H_
#define MCA_LLM_BASE_PARSE_HOSTFILE_LEX_H_

#include <stdio.h>

extern int yylex(void);

extern FILE *yyin;
extern int mca_llm_base_parse_done;
extern char *mca_llm_base_string;
extern int yynewlines;

#define MCA_LLM_BASE_DONE           0
#define MCA_LLM_BASE_ERROR          1
#define MCA_LLM_BASE_QUOTED_STRING  2
#define MCA_LLM_BASE_EQUAL          3
#define MCA_LLM_BASE_STRING         4
#define MCA_LLM_BASE_COUNT          5
#define MCA_LLM_BASE_NEWLINE        6

#endif
