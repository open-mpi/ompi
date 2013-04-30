%option nounput

%{ /* -*- C -*- */
#include "opal_config.h"

#include <stdio.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "coll_ml_lex.h"

BEGIN_C_DECLS

/*
 * local functions
 */
static int finish_parsing(void) ;
static int coll_ml_config_yywrap(void);

END_C_DECLS

/*
 * global variables
 */
int coll_ml_config_yynewlines = 1;
bool coll_ml_config_parse_done = false;
char *coll_ml_config_string = NULL;

#define yyterminate() \
  return finish_parsing()

%}

WHITE       [\f\t\v ]
CHAR        [A-Za-z0-9_\-\.]
NAME_CHAR   [A-Za-z0-9_\-\.\\\/]

%x comment
%x section_name
%x collective_name
%x section_end
%x collective_end
%x value

%%

{WHITE}*\n          { ++coll_ml_config_yynewlines;
                      return COLL_ML_CONFIG_PARSE_NEWLINE; }
#.*\n               { ++coll_ml_config_yynewlines;
                      return COLL_ML_CONFIG_PARSE_NEWLINE; }
"//".*\n            { ++coll_ml_config_yynewlines;
                      return COLL_ML_CONFIG_PARSE_NEWLINE; }

"/*"                { BEGIN(comment);
                      return COLL_ML_CONFIG_PARSE_NEWLINE; }
<comment>[^*\n]*       ; /* Eat up non '*'s */
<comment>"*"+[^*/\n]*  ; /* Eat '*'s not followed by a '/' */
<comment>\n         { ++coll_ml_config_yynewlines;
                      return COLL_ML_CONFIG_PARSE_NEWLINE; }
<comment>"*"+"/"    { BEGIN(INITIAL); /* Done with block comment */
                      return COLL_ML_CONFIG_PARSE_NEWLINE; }

{WHITE}*\[{WHITE}*  { BEGIN(collective_name); }
<collective_name>({NAME_CHAR}|{WHITE})*{NAME_CHAR}/{WHITE}*\] {
                      BEGIN(collective_end);
                      return COLL_ML_CONFIG_PARSE_COLLECTIVE; }
<collective_name>\n    { ++coll_ml_config_yynewlines;
                      return COLL_ML_CONFIG_PARSE_ERROR; }
<collective_name>.     { return COLL_ML_CONFIG_PARSE_ERROR; }
<collective_end>{WHITE}*\]{WHITE}*\n {
                      BEGIN(INITIAL);
                      ++coll_ml_config_yynewlines;
                      return COLL_ML_CONFIG_PARSE_NEWLINE; }

{WHITE}*\<{WHITE}*  { BEGIN(section_name); }
<section_name>({NAME_CHAR}|{WHITE})*{NAME_CHAR}/{WHITE}*\> {
                      BEGIN(section_end);
                      return COLL_ML_CONFIG_PARSE_SECTION; }
<section_name>\n    { ++coll_ml_config_yynewlines;
                      return COLL_ML_CONFIG_PARSE_ERROR; }
<section_name>.     { return COLL_ML_CONFIG_PARSE_ERROR; }
<section_end>{WHITE}*\>{WHITE}*\n {
                      BEGIN(INITIAL);
                      ++coll_ml_config_yynewlines;
                      return COLL_ML_CONFIG_PARSE_NEWLINE; }

{WHITE}*"="{WHITE}* { BEGIN(value);
                      return COLL_ML_CONFIG_PARSE_EQUAL; }
{WHITE}+            ; /* whitespace */
{CHAR}+             { return COLL_ML_CONFIG_PARSE_SINGLE_WORD; }

<value>{WHITE}*\n   { BEGIN(INITIAL);
                      ++coll_ml_config_yynewlines;
                      return COLL_ML_CONFIG_PARSE_NEWLINE; }
<value>[^\n]*[^\t \n]/[\t ]* {
                      return COLL_ML_CONFIG_PARSE_VALUE; }

.                   { return COLL_ML_CONFIG_PARSE_ERROR; }
%%


/*
 * This cleans up at the end of the parse (since, in this case, we
 * always parse the entire file) and prevents a memory leak.
 */
static int finish_parsing(void)
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


static int coll_ml_config_yywrap(void)
{
    coll_ml_config_parse_done = true;
    return 1;
}


/*
 * Ensure that we have a valid yybuffer to use.  Specifically, if this
 * scanner is invoked a second time, finish_parsing() (above) will
 * have been executed, and the current buffer will have been freed.
 * Flex doesn't recognize this fact because as far as it's concerned,
 * its internal state was already initialized, so it thinks it should
 * have a valid buffer.  Hence, here we ensure to give it a valid
 * buffer.
 */
int coll_ml_config_init_buffer(FILE *file)
{
    YY_BUFFER_STATE buf = yy_create_buffer(file, YY_BUF_SIZE);
    yy_switch_to_buffer(buf);

    return 0;
}
