/*
 * dict.c
 *
 * Implementation of generic dictionary routines.
 * Copyright (C) 2001-2004 Farooq Mela.
 * Copyright (c) 2022      IBM Corporation.  All rights reserved.
 *
 * $Id: dict.c,v 1.7 2001/11/25 06:00:49 farooq Exp farooq $
 */

#include <stdlib.h>

#include "dict.h"
#include "dict_private.h"

dict_malloc_func ompi_coll_libnbc_dict_malloc = malloc;
dict_free_func ompi_coll_libnbc_dict_free = free;

int
ompi_coll_libnbc_dict_uint_cmp(const void *k1, const void *k2)
{
	const unsigned int *a = (unsigned int*)k1, *b = (unsigned int*)k2;

	return (*a < *b) ? -1 : (*a > *b) ? +1 : 0;
}

int
ompi_coll_libnbc_dict_long_cmp(const void *k1, const void *k2)
{
	const long *a = (long*)k1, *b = (long*)k2;

	return (*a < *b) ? -1 : (*a > *b) ? +1 : 0;
}

int
ompi_coll_libnbc_dict_ulong_cmp(const void *k1, const void *k2)
{
	const unsigned long *a = (unsigned long*)k1, *b = (unsigned long*)k2;

	return (*a < *b) ? -1 : (*a > *b) ? +1 : 0;
}

int
ompi_coll_libnbc_dict_ptr_cmp(const void *k1, const void *k2)
{
	return (k1 > k2) - (k1 < k2);
}

int
ompi_coll_libnbc_dict_str_cmp(const void *k1, const void *k2)
{
	const char *a = (char*)k1, *b = (char*)k2;
	char p, q;

	for (;;) {
		p = *a++; q = *b++;
		if (p == 0 || p != q)
			break;
	}
	return (p > q) - (p < q);
}

void
ompi_coll_libnbc_dict_destroy(dict *dct, int del)
{
	ASSERT(dct != NULL);

	dct->_destroy(dct->_object, del);
	FREE(dct);
}

void
ompi_coll_libnbc_dict_itor_destroy(dict_itor *itor)
{
	ASSERT(itor != NULL);

	itor->_destroy(itor->_itor);
	FREE(itor);
}
