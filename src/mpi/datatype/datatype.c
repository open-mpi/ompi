/*
 * $HEADER$
 */

/*
 * lam_datatype_t implementation
 */

#include "lam_config.h"
#include "datatype.h"

static void lam_datatype_t_construct(lam_datatype_t *datatype);
static void lam_datatype_t_destruct(lam_datatype_t *datatype);

OBJ_CLASS_INSTANCE(lam_datatype_t,
                   lam_object_t,
                   lam_datatype_t_construct,
                   lam_datatype_t_destruct);


static void lam_datatype_t_construct(lam_datatype_t *datatype) {}


static void lam_datatype_t_destruct(lam_datatype_t *datatype) {}
