/*
 * $HEADER$
 *
 * $Id: argv.h,v 1.1 2004/01/07 08:35:06 jsquyres Exp $
 */

#ifndef LAM_ARGV_H
#define LAM_ARGV_H

int lam_argv_add(int *argc, char ***argv, char *arg);
void lam_argv_free(char **argv);
char **lam_argv_split(char *src_string, int delimiter);
int lam_argv_count(char **argv);
char *lam_argv_join(char **argv, int delimiter);
size_t lam_argv_len(char **argv);
char **lam_argv_copy(char **argv);

#endif /* LAM_ARGV_H */
