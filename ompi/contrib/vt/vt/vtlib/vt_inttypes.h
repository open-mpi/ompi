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

#ifndef _VT_INTTYPES_H
#define _VT_INTTYPES_H

#include "config.h"

#if defined(HAVE_STDINT_H) && HAVE_STDINT_H && !defined(__sgi)
# include <stdint.h>
#elif defined(HAVE_INTTYPES_H) && HAVE_INTTYPES_H
# include <inttypes.h>
#else /* HAVE_INTTYPES_H || HAVE_STDINT_H */

  /* Signed. */
  typedef signed char             int8_t;
  typedef signed short int        int16_t;
  typedef signed int  	        int32_t;

# if SIZEOF_LONG == 8
    typedef signed long int         int64_t;
#else /* SIZEOF_LONG */
    typedef signed long long int    int64_t;
#endif /* SIZEOF_LONG */

/* Unsigned. */
  typedef unsigned char           uint8_t;
  typedef unsigned short int      uint16_t;
  typedef unsigned int            uint32_t;

#if SIZEOF_LONG == 8
    typedef unsigned long int       uint64_t;
#else /* SIZEOF_LONG */
    typedef unsigned long long int  uint64_t;
#endif /* SIZEOF_LONG */

#endif /* HAVE_INTTYPES_H || HAVE_STDINT_H */

#if (defined(VT_MPI) || defined(VT_HYB))
# if (defined(_SX) && defined(_W8))
    typedef long long VT_MPI_INT;
# else /* _SX && _W8 */
    typedef int       VT_MPI_INT;
# endif /* _SX && _W8 */
#endif /* VT_MPI || VT_HYB */

#endif /* _VT_INTTYPES_H */
