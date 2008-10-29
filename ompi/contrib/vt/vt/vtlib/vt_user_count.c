/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2008, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#include <string.h>
#include "otf.h"
#if (defined (VT_OMPI) || defined (VT_OMP))
  #include "opari_omp.h"
#endif
#include "vt_fbindings.h"
#include "vt_inttypes.h"
#include "vt_memhook.h"
#include "vt_pform.h"
#include "vt_trc.h"
#include "vt_error.h"
#define VTRACE
#undef VTRACE_NO_COUNT
#include "vt_user_count.h"

static int vt_init = 1;        /* is initialization needed? */
static uint32_t def_gid = 0;   /* default counter group id */

#define VT_INIT \
  if ( vt_init ) { \
    VT_MEMHOOKS_OFF(); \
    vt_init = 0; \
    vt_open(); \
    VT_MEMHOOKS_ON(); \
  }

unsigned int VT_User_count_group_def__(char* gname)
{
  uint32_t gid;

  VT_INIT;

  VT_MEMHOOKS_OFF();

# if defined (VT_OMPI) || defined (VT_OMP)
    if (omp_in_parallel()) {
#     pragma omp critical (vt_api_count_1)
      {
	gid = (uint32_t)vt_def_counter_group(gname);
      }
    }
    else
    {
      gid = (uint32_t)vt_def_counter_group(gname);
    }
# else
    gid = (uint32_t)vt_def_counter_group(gname);
# endif

  VT_MEMHOOKS_ON();

  return gid;
}

unsigned int VT_User_count_def__(char* cname, char* cunit, int ctype,
				 unsigned int gid)
{
  uint32_t cid;
  uint32_t cprop = OTF_COUNTER_TYPE_ABS|OTF_COUNTER_SCOPE_NEXT;

  VT_INIT;

  VT_MEMHOOKS_OFF();

  if (gid == (uint32_t)VT_COUNT_DEFGROUP)
  {
    if (def_gid == 0)
      def_gid = VT_User_count_group_def__("User");

    gid = def_gid;
  }

  switch(ctype)
  {
    case VT_COUNT_TYPE_SIGNED:
    case VT_COUNT_TYPE_INTEGER:
    case VT_COUNT_TYPE_INTEGER8:
    {
      cprop |= OTF_COUNTER_VARTYPE_SIGNED8;
      break;
    }
    case VT_COUNT_TYPE_UNSIGNED:
    {
      cprop |= OTF_COUNTER_VARTYPE_UNSIGNED8;
      break;
    }
    case VT_COUNT_TYPE_FLOAT:
    case VT_COUNT_TYPE_REAL:
    {
      cprop |= OTF_COUNTER_VARTYPE_FLOAT;
      break;
    }
    case VT_COUNT_TYPE_DOUBLE:
    {
      cprop |= OTF_COUNTER_VARTYPE_DOUBLE;
      break;
    }
    default:
    {
      vt_error_msg("Unknown counter type %i", ctype);
      break;
    }
  }

# if defined (VT_OMPI) || defined (VT_OMP)
    if (omp_in_parallel()) {
#     pragma omp critical (vt_api_count_2)
      {
	cid = (uint32_t)vt_def_counter(cname, cprop, gid, cunit);
      }
    }
    else
    {
      cid = (uint32_t)vt_def_counter(cname, cprop, gid, cunit);
    }
# else
    cid = (uint32_t)vt_def_counter(cname, cprop, gid, cunit);
# endif

    VT_MEMHOOKS_ON();

    return cid;
}

void VT_User_count_signed_val__(unsigned int cid, long long val)
{
  uint64_t time;
  uint64_t cval;

  VT_INIT;

  VT_MEMHOOKS_OFF();

  time = vt_pform_wtime();
  cval = OTF_Signed2Counter((int64_t)val);
  vt_count(&time, cid, cval);

  VT_MEMHOOKS_ON();
}

void VT_User_count_unsigned_val__(unsigned int cid, unsigned long long val)
{
  uint64_t time;
  uint64_t cval;

  VT_INIT;

  VT_MEMHOOKS_OFF();

  time = vt_pform_wtime();
  cval = OTF_Unsigned2Counter((uint64_t)val);
  vt_count(&time, cid, cval);

  VT_MEMHOOKS_ON();
}

void VT_User_count_float_val__(unsigned int cid, float val)
{
  uint64_t time;
  uint64_t cval;

  VT_INIT;

  VT_MEMHOOKS_OFF();

  time = vt_pform_wtime();
  cval = OTF_Float2Counter(val);
  vt_count(&time, cid, cval);

  VT_MEMHOOKS_ON();
}

void VT_User_count_double_val__(unsigned int cid, double val)
{
  uint64_t time;
  uint64_t cval;

  VT_INIT;

  VT_MEMHOOKS_OFF();

  time = vt_pform_wtime();
  cval = OTF_Double2Counter(val);
  vt_count(&time, cid, cval);

  VT_MEMHOOKS_ON();
}

/*
 * Fortran version
 */

void VT_User_count_group_def___f(char* gname, unsigned int* gid, int nl);
void VT_User_count_def___f(char* cname, char* cunit, int* ctype,
			   unsigned int* gid, unsigned int* cid,
			   int nl, int ul);
void VT_User_count_integer_val___f(unsigned int* cid, int* val);
void VT_User_count_integer8_val___f(unsigned int* cid, long long* val);
void VT_User_count_real_val___f(unsigned int* cid, float* val);
void VT_User_count_double_val___f(unsigned int* cid, double* val);

void VT_User_count_group_def___f(char* gname, unsigned int* gid, int nl)
{
  int namlen;
  char fnambuf[128];

  /* -- convert Fortran to C strings -- */
  namlen = ( nl < 128 ) ? nl : 127;
  strncpy(fnambuf, gname, namlen);
  fnambuf[namlen] = '\0';

  *gid = VT_User_count_group_def__(fnambuf);
} VT_GENERATE_F77_BINDINGS(vt_user_count_group_def__, VT_USER_COUNT_GROUP_DEF__,
			   VT_User_count_group_def___f,
			   (char* gname, unsigned int* gid, int nl),
			   (gname, gid, nl))

void VT_User_count_def___f(char* cname, char* cunit, int* ctype,
			   unsigned int* gid, unsigned int* cid,
			   int nl, int ul)
{
  int namlen;
  int unilen;
  char fnambuf[128];
  char funibuf[128];

 /* -- convert Fortran to C strings -- */
  namlen = ( nl < 128 ) ? nl : 127;
  unilen = ( ul < 128 ) ? ul : 127;
  strncpy(fnambuf, cname, namlen);
  fnambuf[namlen] = '\0';
  strncpy(funibuf, cunit, unilen);
  funibuf[unilen] = '\0';

  *cid = VT_User_count_def__(fnambuf, funibuf, *ctype, *gid);
} VT_GENERATE_F77_BINDINGS(vt_user_count_def__, VT_USER_COUNT_DEF__,
			   VT_User_count_def___f,
			   (char* cname, char* cunit, int* ctype, unsigned int* gid, unsigned int* cid, int nl, int ul),
			   (cname, cunit, ctype, gid, cid, nl, ul))


void VT_User_count_integer_val___f(unsigned int* cid, int* val)
{
  uint64_t time;
  uint64_t cval;

  VT_INIT;

  VT_MEMHOOKS_OFF();

  time = vt_pform_wtime();
  cval = OTF_Signed2Counter((int64_t)(*val));
  vt_count(&time, *cid, cval);

  VT_MEMHOOKS_ON();
} VT_GENERATE_F77_BINDINGS(vt_user_count_integer_val__,
			   VT_USER_COUNT_INTEGER_VAL__,
			   VT_User_count_integer_val___f,
			   (unsigned int* cid, int* val),
			   (cid, val))

void VT_User_count_integer8_val___f(unsigned int* cid, long long* val)
{
  uint64_t time;
  uint64_t cval;

  VT_INIT;

  VT_MEMHOOKS_OFF();

  time = vt_pform_wtime();
  cval = OTF_Signed2Counter((int64_t)(*val));
  vt_count(&time, *cid, cval);

  VT_MEMHOOKS_ON();
} VT_GENERATE_F77_BINDINGS(vt_user_count_integer8_val__,
			   VT_USER_COUNT_INTEGER8_VAL__,
			   VT_User_count_integer8_val___f,
			   (unsigned int* cid, long long* val),
			   (cid, val))

void VT_User_count_real_val___f(unsigned int* cid, float* val)
{
  uint64_t time;
  uint64_t cval;

  VT_INIT;

  VT_MEMHOOKS_OFF();

  time = vt_pform_wtime();
  cval = OTF_Float2Counter(*val);
  vt_count(&time, *cid, cval);

  VT_MEMHOOKS_ON();
} VT_GENERATE_F77_BINDINGS(vt_user_count_real_val__,
			   VT_USER_COUNT_real_VAL__,
			   VT_User_count_real_val___f,
			   (unsigned int* cid, float* val),
			   (cid, val))

void VT_User_count_double_val___f(unsigned int* cid, double* val)
{
  uint64_t time;
  uint64_t cval;

  VT_INIT;

  VT_MEMHOOKS_OFF();

  time = vt_pform_wtime();
  cval = OTF_Double2Counter(*val);
  vt_count(&time, *cid, cval);

  VT_MEMHOOKS_ON();
} VT_GENERATE_F77_BINDINGS(vt_user_count_double_val__,
			   VT_USER_COUNT_double_VAL__,
			   VT_User_count_double_val___f,
			   (unsigned int* cid, double* val),
			   (cid, val))
