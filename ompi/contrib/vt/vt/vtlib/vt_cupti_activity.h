/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2012, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#ifndef VT_CUPTI_ACTIVITY_H
#define	VT_CUPTI_ACTIVITY_H

#ifdef __cplusplus
# define EXTERN extern "C"
#else
# define EXTERN extern
#endif

/* 
 * Initialize the VampirTrace CUPTI Activity implementation.
 */
EXTERN void vt_cupti_activity_init(void);

/* 
 * Finalize the VampirTrace CUPTI Activity implementation.
 */
EXTERN void vt_cupti_activity_finalize(void);

/*
 * Create and add a new context to list of contexts.
 * 
 * @param cuCtx the CUDA context, specifying the queue
 */
EXTERN void vt_cuptiact_addContext(CUcontext cuCtx, CUdevice cuDev);

/*
 * Handle activities buffered by CUPTI. 
 * 
 * NVIDIA:
 * "Global Queue: The global queue collects all activity records that
 * are not associated with a valid context. All API activity records
 * are collected in the global queue. A buffer is enqueued in the
 * global queue by specifying \p context == NULL.
 *
 * Context Queue: Each context queue collects activity records
 * associated with that context that are not associated with a
 * specific stream or that are associated with the default stream
 * (stream ID 0). A buffer is enqueued in a context queue by
 * specifying the context and a stream ID of 0.
 *
 * Stream Queue: Each stream queue collects memcpy, memset, and kernel
 * activity records associated with the stream. A buffer is enqueued
 * in a stream queue by specifying a context and a non-zero stream ID."
 * 
 * @param cuCtx CUDA context, NULL to handle globally buffered activities
 */
EXTERN void vt_cuptiact_flushCtxActivities(CUcontext cuCtx);


EXTERN void vt_cuptiact_writeMalloc(uint32_t ctxID, CUcontext cuCtx, 
                                    void *devPtr, size_t size);

EXTERN void vt_cuptiact_writeFree(uint32_t ctxID, CUcontext cuCtx, 
                                  void *devPtr);

/*
 * To provide correlation data between API call and activity.
 * 
 * @param ctxID the CUDA context identifier
 * @param correlationID correlation between memory copy and API call
 */
EXTERN void vt_cuptiact_addCorrelation(uint32_t ctxID, uint32_t correlationID, 
                                       uint32_t ptid);


#endif	/* VT_CUPTI_ACTIVITY_H */

