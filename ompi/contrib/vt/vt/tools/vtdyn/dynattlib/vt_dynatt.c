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

#ifdef __GNUC__
# define INITROU dynatt_init
  void __attribute__ ((constructor)) INITROU(void);
#else
# define INITROU _init
#endif

extern void VT_Dyn_attach(void);
void INITROU(void);

void INITROU()
{
  VT_Dyn_attach();
}
