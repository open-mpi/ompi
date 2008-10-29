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

#ifndef _VT_FBINDINGS_H
#define _VT_FBINDINGS_H

#define VT_GENERATE_F77_BINDINGS(lower_case, \
                                 upper_case, \
                                 wrapper_function, \
                                 signature, \
                                 params) \
  void lower_case signature; \
  void lower_case signature { wrapper_function params; } \
  void lower_case##_ signature; \
  void lower_case##_ signature { wrapper_function params; } \
  void lower_case##__ signature; \
  void lower_case##__ signature { wrapper_function params; } \
  void upper_case signature; \
  void upper_case signature { wrapper_function params; }

#endif
