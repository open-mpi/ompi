/*
 * $HEADER$
 */

/*
 * lam_datatype_t implementation
 */

#include "lam_config.h"
#include "mpi/datatype/datatype.h"

/*
 * Global variables
 * Sizes and alignments from configure
 */

int lam_sizeof_f77_integer = LAM_SIZEOF_FORTRAN_INT;
int lam_sizeof_f77_real = LAM_SIZEOF_FORTRAN_REAL;
int lam_sizeof_f77_dblprec = LAM_SIZEOF_FORTRAN_DBLPREC;
int lam_sizeof_f77_complex = LAM_SIZEOF_FORTRAN_COMPLEX;
int lam_sizeof_f77_dblcomplex = LAM_SIZEOF_FORTRAN_DBLCOMPLEX;

int lam_alignment_f77_integer = LAM_ALIGNMENT_FORTRAN_INT;
int lam_alignment_f77_real = LAM_ALIGNMENT_FORTRAN_REAL;
int lam_alignment_f77_dblprec = LAM_ALIGNMENT_FORTRAN_DBLPREC;
int lam_alignment_f77_complex = LAM_ALIGNMENT_FORTRAN_COMPLEX;
int lam_alignment_f77_dblcomplex = LAM_ALIGNMENT_FORTRAN_DBLCOMPLEX;


static void lam_datatype_t_construct(lam_datatype_t *datatype);
static void lam_datatype_t_destruct(lam_datatype_t *datatype);

OBJ_CLASS_INSTANCE(lam_datatype_t,
                   lam_object_t,
                   lam_datatype_t_construct,
                   lam_datatype_t_destruct);


static void lam_datatype_t_construct(lam_datatype_t *datatype) {}


static void lam_datatype_t_destruct(lam_datatype_t *datatype) {}

