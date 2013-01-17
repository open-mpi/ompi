/*
 * Copyright (c) 2011      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef _OPAL_DATATYPE_CUDA_H
#define _OPAL_DATATYPE_CUDA_H

void mca_cuda_convertor_init(opal_convertor_t* convertor, const void *pUserBuf);
bool opal_cuda_check_bufs(char *dest, char *src);
void* opal_cuda_memcpy(void * dest, const void * src, size_t size, opal_convertor_t* convertor);
void* opal_cuda_memcpy_sync(void * dest, void * src, size_t size);
void* opal_cuda_memmove(void * dest, void * src, size_t size);
void opal_cuda_add_initialization_function(int (*fptr)(void));
void opal_cuda_set_copy_function_async(opal_convertor_t* convertor, void *stream);

#endif
