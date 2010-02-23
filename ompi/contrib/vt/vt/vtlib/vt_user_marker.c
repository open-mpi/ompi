/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2010, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#include <string.h>

#include "otf.h"

#include "vt_fbindings.h"
#include "vt_inttypes.h"
#include "vt_memhook.h"
#include "vt_pform.h"
#include "vt_thrd.h"
#include "vt_trc.h"
#include "vt_error.h"
#define VTRACE
#undef VTRACE_NO_MARKER
#include "vt_user.h"

static int vt_init = 1;        /* is initialization needed? */

#define VT_INIT \
  if ( vt_init ) { \
    VT_MEMHOOKS_OFF(); \
    vt_init = 0; \
    vt_open(); \
    VT_MEMHOOKS_ON(); \
  }

unsigned int VT_User_marker_def__(const char* mname, int mtype)
{
  uint32_t mid;
  uint32_t mtype_otf = OTF_MARKER_TYPE_UNKNOWN;

  VT_INIT;

  VT_MEMHOOKS_OFF();

  switch(mtype)
  {
    case VT_MARKER_TYPE_ERROR:
    {
      mtype_otf = OTF_MARKER_TYPE_ERROR;
      break;
    }
    case VT_MARKER_TYPE_WARNING:
    {
      mtype_otf = OTF_MARKER_TYPE_WARNING;
      break;
    }
    case VT_MARKER_TYPE_HINT:
    {
      mtype_otf = OTF_MARKER_TYPE_HINT;
      break;
    }
    default:
    {
      vt_error_msg("Unknown marker type %i", mtype);
      break;
    }
  }

#if (defined(VT_MT) || defined(VT_HYB))
  VTTHRD_LOCK_IDS();
#endif
  mid = (uint32_t)vt_def_marker(mname, mtype_otf);
#if (defined(VT_MT) || defined(VT_HYB))
  VTTHRD_UNLOCK_IDS();
#endif

    VT_MEMHOOKS_ON();

    return mid;
}

void VT_User_marker__(unsigned int mid, const char* mtext)
{
  uint64_t time;

  VT_INIT;

  VT_MEMHOOKS_OFF();

  time = vt_pform_wtime();
  vt_marker(&time, mid, mtext);

  VT_MEMHOOKS_ON();
}

/*
 * Fortran version
 */

void VT_User_marker_def___f(const char* mname, int* mtype, unsigned int* mid, int nl);
void VT_User_marker___f(unsigned int* mid, const char* mtext, int tl);

void VT_User_marker_def___f(const char* mname, int* mtype, unsigned int* mid, int nl)
{
  int namlen;
  char fnambuf[128];

  /* -- convert Fortran to C strings -- */
  namlen = ( nl < 128 ) ? nl : 127;
  strncpy(fnambuf, mname, namlen);
  fnambuf[namlen] = '\0';

  *mid = VT_User_marker_def__(fnambuf, *mtype);
} VT_GENERATE_F77_BINDINGS(vt_user_marker_def__, VT_USER_MARKER_DEF__,
			   VT_User_marker_def___f,
			   (const char* mname, int* mtype, unsigned int* mid, int nl),
			   (mname, mtype, mid, nl))

void VT_User_marker___f(unsigned int* mid, const char* mtext, int tl)
{
  int texlen;
  char ftexbuf[1024];

  /* -- convert Fortran to C strings -- */
  texlen = ( tl < 1024 ) ? tl : 1023;
  strncpy(ftexbuf, mtext, texlen);
  ftexbuf[texlen] = '\0';

  VT_User_marker__(*mid, ftexbuf);
} VT_GENERATE_F77_BINDINGS(vt_user_marker__, VT_USER_MARKER__,
			   VT_User_marker___f,
			   (unsigned int* mid, const char* mtext, int tl),
			   (mid, mtext, tl))
