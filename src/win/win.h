/* 
 * $HEADER$
 */

#ifndef LAM_WIN_H
#define LAM_WIN_H


#include "lfc/lam_object.h"
#include "lfc/lam_hash_table.h"


extern lam_class_t lam_win_t_class;
struct lam_win_t {
  lam_object_t w_base;

  /* Attributes */
  lam_hash_table_t *keyhash;

  /* Other stuffs */
};

typedef struct lam_win_t lam_win_t;

#endif
