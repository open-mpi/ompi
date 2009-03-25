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

#include "config.h"

#include "vt_otf_gen.h"
#include "vt_otf_sum.h"
#include "vt_defs.h"
#include "vt_env.h"
#include "vt_error.h"
#include "vt_iowrap.h"
#include "vt_inttypes.h"
#include "vt_metric.h"
#include "vt_pform.h"
#include "vt_trc.h"

#include <unistd.h>
#include <string.h>
#include <stdlib.h>

#include "otf.h"

#if (defined (VT_MPI) || defined (VT_OMPI))
#include "mpi.h"
#endif

/*
 *-----------------------------------------------------------------------------
 * Macro functions
 *-----------------------------------------------------------------------------
 */

#define VTGEN_CHECK(gen)                                            \
  if (gen == NULL) vt_error_msg("Abort: Uninitialized trace buffer")

#define VTGEN_ALLOC_DEF(gen, bytes)                                 \
  if ((uint64_t)(gen->buf->pos - gen->buf->mem) >                   \
      (uint64_t)(gen->buf->size - (bytes)))                         \
    VTGen_flush(gen, 1, vt_pform_wtime(), NULL);

#define VTGEN_ALLOC_EVENT(gen, bytes)                               \
  if ((uint64_t)(gen->buf->pos - gen->buf->mem) >                   \
      (uint64_t)(gen->buf->size - (bytes)))                         \
    VTGen_flush(gen, 1, *time, time);

#define VTGEN_ALIGN_LENGTH(bytes)                                   \
  ( bytes % SIZEOF_VOIDP ) ?                                        \
    ( bytes / SIZEOF_VOIDP + 1 ) * SIZEOF_VOIDP : bytes

#define VTGEN_JUMP(gen, bytes)                                      \
  gen->buf->pos += length

#define VTGEN_CHECK_FLUSHCNTR(gen)                                  \
  if(gen->flushcntr == 0) {                                         \
    gen->flushcntr = -1;                                            \
    vt_trace_off(1);                                                \
    vt_cntl_msg("Maximum number of buffer flushes reached (%d)",    \
                vt_env_max_flushes());                              \
  }

#define VTGEN_IS_TRACE_ON(gen) (gen->mode & 1) != 0
#define VTGEN_IS_SUM_ON(gen) (gen->mode & 2) != 0

/*
 *-----------------------------------------------------------------------------
 * VTGen
 *-----------------------------------------------------------------------------
 */

/* Data types */

typedef enum { BUF_ENTRY_TYPE__DefinitionComment,
	       BUF_ENTRY_TYPE__DefSclFile,
               BUF_ENTRY_TYPE__DefScl,
	       BUF_ENTRY_TYPE__DefFileGroup,
	       BUF_ENTRY_TYPE__DefFile,
	       BUF_ENTRY_TYPE__DefFunctionGroup,
	       BUF_ENTRY_TYPE__DefFunction,
	       BUF_ENTRY_TYPE__DefCollectiveOperation,
	       BUF_ENTRY_TYPE__DefCounterGroup,
	       BUF_ENTRY_TYPE__DefCounter,
	       BUF_ENTRY_TYPE__DefProcessGroup,
	       BUF_ENTRY_TYPE__Enter,
	       BUF_ENTRY_TYPE__Leave,
	       BUF_ENTRY_TYPE__FileOperation,
	       BUF_ENTRY_TYPE__Counter,
	       BUF_ENTRY_TYPE__Comment,
	       BUF_ENTRY_TYPE__SendMsg,
	       BUF_ENTRY_TYPE__RecvMsg,
	       BUF_ENTRY_TYPE__CollectiveOperation,
	       BUF_ENTRY_TYPE__FunctionSummary,
	       BUF_ENTRY_TYPE__MessageSummary,
	       BUF_ENTRY_TYPE__FileOperationSummary
} VTBuf_EntryTypes;

typedef struct
{
  buffer_t  mem;
  buffer_t  pos;
  size_t    size;
} VTBuf;

struct VTGen_struct
{
  OTF_FileManager*    filemanager;
  OTF_WStream*        file;
  OTF_FileCompression filecomp;
  char                name[VT_PATH_MAX];
  uint32_t            trcid;
  uint32_t            tid;
  int32_t             flushcntr;
  uint8_t             isfirstflush;
  uint8_t             mode;
  VTSum*              sum;
  VTBuf*              buf;
};

typedef struct
{
  VTBuf_EntryTypes type;
  uint8_t length;
} VTBuf_Entry_Base;

/* BUF_ENTRY_TYPE__DefinitionComment */

typedef struct
{
  VTBuf_EntryTypes type;
  uint8_t length;

  char* comment;
} VTBuf_Entry_DefinitionComment;

/* BUF_ENTRY_TYPE__DefSclFile */

typedef struct
{
  VTBuf_EntryTypes type;
  uint8_t length;

  uint32_t fid;
  char*   fname;
} VTBuf_Entry_DefSclFile;

/* BUF_ENTRY_TYPE__DefScl */

typedef struct
{
  VTBuf_EntryTypes type;
  uint8_t length;

  uint32_t sid;
  uint32_t fid;
  uint32_t ln;
} VTBuf_Entry_DefScl;

/* BUF_ENTRY_TYPE__DefFileGroup */

typedef struct
{
  VTBuf_EntryTypes type;
  uint8_t length;

  uint32_t gid;
  char*    gname;
} VTBuf_Entry_DefFileGroup;

/* BUF_ENTRY_TYPE__DefFile */

typedef struct
{
  VTBuf_EntryTypes type;
  uint8_t length;

  uint32_t fid;
  char*    fname;
  uint32_t gid;
} VTBuf_Entry_DefFile;

/* BUF_ENTRY_TYPE__DefFunctionGroup */

typedef struct
{
  VTBuf_EntryTypes type;
  uint8_t length;

  uint32_t rdid;
  char*   rdesc;    
} VTBuf_Entry_DefFunctionGroup;

/* BUF_ENTRY_TYPE__DefFunction */

typedef struct
{
  VTBuf_EntryTypes type;
  uint8_t length;

  uint32_t rid;
  char*   rname;
  uint32_t rdid;
  uint32_t sid;
} VTBuf_Entry_DefFunction;

/* BUF_ENTRY_TYPE__DefCollectiveOperation */

typedef struct
{
  VTBuf_EntryTypes type;
  uint8_t length;

  uint32_t cid;
  char*   cname;
  uint32_t ctype;
} VTBuf_Entry_DefCollectiveOperation;

/* BUF_ENTRY_TYPE__DefCounterGroup */

typedef struct
{
  VTBuf_EntryTypes type;
  uint8_t length;

  uint32_t gid;
  char*   gname;
} VTBuf_Entry_DefCounterGroup;

/* BUF_ENTRY_TYPE__DefCounter */

typedef struct
{
  VTBuf_EntryTypes type;
  uint8_t length;

  uint32_t cid;
  char*   cname;
  uint32_t cprop;
  uint32_t gid;
  char*   cunit;
} VTBuf_Entry_DefCounter;

/* BUF_ENTRY_TYPE__DefProcessGroup */

typedef struct
{
  VTBuf_EntryTypes type;
  uint8_t length;

  uint32_t  cid;
  char*    grpn;
  uint32_t  grpc;
  uint32_t* grpv;
} VTBuf_Entry_DefProcessGroup;

/* BUF_ENTRY_TYPE__Enter / BUF_ENTRY_TYPE__Leave */

typedef struct
{
  VTBuf_EntryTypes type;
  uint8_t length;

  uint64_t time;
  uint32_t rid;
  uint32_t sid;
  uint8_t metc;
  uint64_t metv[1];
} VTBuf_Entry_EnterLeave;

/* BUF_ENTRY_TYPE__FileOperation */

typedef struct
{
  VTBuf_EntryTypes type;
  uint8_t length;

  uint64_t time;
  uint64_t etime;
  uint32_t fid;
  uint64_t hid;
  uint32_t op;
  uint32_t bytes;
  uint32_t sid;
} VTBuf_Entry_FileOperation;

/* BUF_ENTRY_TYPE__Counter */

typedef struct
{
  VTBuf_EntryTypes type;
  uint8_t length;

  uint64_t time;
  uint32_t cid;
  uint64_t cval;
} VTBuf_Entry_Counter;

/* BUF_ENTRY_TYPE__Comment */

typedef struct
{
  VTBuf_EntryTypes type;
  uint8_t length;

  uint64_t time;
  char* comment;
} VTBuf_Entry_Comment;

/* BUF_ENTRY_TYPE__SendMsg / BUF_ENTRY_TYPE__RecvMsg */

typedef struct
{
  VTBuf_EntryTypes type;
  uint8_t length;

  uint64_t time;
  uint32_t spid;
  uint32_t dpid;
  uint32_t cid;
  uint32_t tag;
  uint32_t len;
  uint32_t sid;
} VTBuf_Entry_SendRecvMsg;

/* BUF_ENTRY_TYPE__CollectiveOperation */

typedef struct
{
  VTBuf_EntryTypes type;
  uint8_t length;

  uint64_t time;
  uint64_t etime;
  uint32_t rid;
  uint32_t cid;
  uint32_t rpid;
  uint32_t sent;
  uint32_t recvd;
  uint32_t sid;
} VTBuf_Entry_CollectiveOperation;

/* BUF_ENTRY_TYPE__FunctionSummary */

typedef struct
{
  VTBuf_EntryTypes type;
  uint8_t length;

  uint64_t time;
  uint32_t rid;
  uint64_t cnt;
  uint64_t excl;
  uint64_t incl;
} VTBuf_Entry_FunctionSummary;

/* BUF_ENTRY_TYPE__MessageSummary */

typedef struct
{
  VTBuf_EntryTypes type;
  uint8_t length;

  uint64_t time;
  uint32_t peer;
  uint32_t cid;
  uint32_t tag;
  uint64_t scnt;
  uint64_t rcnt;
  uint64_t sent;
  uint64_t recvd;
} VTBuf_Entry_MessageSummary;

/* BUF_ENTRY_TYPE__FileOperationSummary */

typedef struct
{
  VTBuf_EntryTypes type;
  uint8_t length;

  uint64_t time;
  uint32_t fid;
  uint64_t nopen;
  uint64_t nclose;
  uint64_t nread;
  uint64_t nwrite;
  uint64_t nseek;
  uint64_t read;
  uint64_t wrote;
} VTBuf_Entry_FileOperationSummary;


VTGen* VTGen_open(const char* namestub, uint32_t tid,
		  size_t buffer_size, uint8_t mode)
{
  VTGen* gen;

  /* allocate VTGen record */

  gen = (VTGen*)malloc(sizeof(VTGen));
  if (gen == NULL) 
    vt_error();

  /* check write permissions of local temp. directory */
  if (access( vt_env_ldir(), W_OK ) != 0 )
    vt_error_msg("Cannot access %s: Permission denied", vt_env_ldir());

  /* open file manager for writer stream */
  gen->filemanager = OTF_FileManager_open(3);

  /* open writer stream */

  gen->file = OTF_WStream_open(namestub, tid+1, gen->filemanager);

  if( gen->file == NULL )
    vt_error_msg("Cannot open OTF writer stream [namestub %s id %x]",
		  namestub, tid+1); 
  else
    vt_cntl_msg("Opened OTF writer stream [namestub %s id %x] for generation [buffer %d bytes]", 
		 namestub, tid+1, buffer_size);

  /* set file compression */

  if( vt_env_compression() &&
      OTF_WStream_setCompression(gen->file, OTF_FILECOMPRESSION_COMPRESSED) )
    gen->filecomp = OTF_FILECOMPRESSION_COMPRESSED;
  else
    gen->filecomp = OTF_FILECOMPRESSION_UNCOMPRESSED;

  /* store file prefix */
  strcpy(gen->name, namestub);

  /* initialize trace id */
  gen->trcid = 0;

  /* store thread id */
  gen->tid = tid;

  /* initialize flush counter */
  gen->flushcntr = vt_env_max_flushes();
  if( gen->flushcntr == 0 ) gen->flushcntr = -1;

  /* initialize first flush flag */
  gen->isfirstflush = 1;

  /* initialize trace mode flags */
  gen->mode = mode;

  /* allocate summary record */

  gen->sum = NULL;
  if (VTGEN_IS_SUM_ON(gen))
  {
    gen->sum = VTSum_open(gen, (uint32_t)vt_env_stat_intv());
    if (gen->sum == NULL)
      vt_error();
  }

  /* allocate buffer record */

  gen->buf = (VTBuf*)malloc(sizeof(VTBuf));  
  if (gen->buf == NULL) 
    vt_error();

  /* allocate buffer */

  gen->buf->mem = malloc(buffer_size);  
  if (gen->buf->mem == NULL) 
    vt_error();

  /* initialize buffer */
  
  gen->buf->pos  = gen->buf->mem;
  /* subtraction leaves space for size of FLUSH record */
  gen->buf->size =
     buffer_size - (2 * (VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_EnterLeave) +
			 VT_METRIC_MAXNUM * sizeof(uint64_t))));

  /* return */
  return gen;
}

void VTGen_flush(VTGen* gen, uint8_t markFlush,
		 uint64_t flushBTime, uint64_t* flushETime )
{
  uint8_t  end_flush_marked = 0;
  buffer_t p;
  int i;

  if(gen->buf->pos == gen->buf->mem)
    return;

  /* Disable I/O tracing */
  VT_SUSPEND_IO_TRACING();

  /* mark begin of flush */
  if(markFlush)
    vt_enter_flush(&flushBTime);

  if(gen->isfirstflush)
  {
    if( gen->tid == 0 )
    {
      char creator[100];
      uint64_t res = vt_pform_clockres();

      /* write creator record */

      snprintf( creator, sizeof(creator),
		"%s", PACKAGE_STRING );

      OTF_WStream_writeOtfVersion( gen->file );
      OTF_WStream_writeDefCreator( gen->file, creator );
      
      /* write timer resolution record */
      
      OTF_WStream_writeDefTimerResolution( gen->file, res );
    }

    /* write process definition record */
    {
      uint32_t ppid;
      char pname[100];

      if(gen->tid > 0)
      {
	snprintf(pname, sizeof(pname), " Thread %u/%u", gen->tid, gen->trcid);
	ppid = gen->trcid+1;
      }
      else
      {
	snprintf(pname, sizeof(pname), "Process %u", gen->trcid);
	ppid = 0;
      }

      OTF_WStream_writeDefProcess(gen->file,
				  65536 * gen->tid + gen->trcid + 1,
				  pname, ppid);
    }

    /* write process group definition record (node name) */
    {
       uint32_t pid = 65536 * gen->tid + gen->trcid + 1;
       char pgname[100];

       snprintf(pgname, sizeof(pgname), "__NODE__ %s", vt_pform_node_name());

       OTF_WStream_writeDefProcessGroup(gen->file,
                                        0, /* id will be given by vtunify */
					pgname, 1, &pid);
    }

    gen->isfirstflush = 0;
  }

  p = gen->buf->mem;

  while(p < gen->buf->pos)
  {
     switch(((VTBuf_Entry_Base*)p)->type)
     {
       case BUF_ENTRY_TYPE__DefinitionComment:
       {
	 VTBuf_Entry_DefinitionComment* entry =
	   (VTBuf_Entry_DefinitionComment*)p;

	 OTF_WStream_writeDefinitionComment(gen->file,
	   entry->comment);

	 free(entry->comment);
	 break;
       }
       case BUF_ENTRY_TYPE__DefSclFile:
       {
	 VTBuf_Entry_DefSclFile* entry =
	   (VTBuf_Entry_DefSclFile*)p; 

	 OTF_WStream_writeDefSclFile(gen->file,
	   entry->fid,
	   entry->fname);
	 
	 free(entry->fname);
	 break;
       }
       case BUF_ENTRY_TYPE__DefScl:
       {
	 VTBuf_Entry_DefScl* entry =
	   (VTBuf_Entry_DefScl*)p; 

	 OTF_WStream_writeDefScl(gen->file,
	   entry->sid,
	   entry->fid,
	   entry->ln);

	 break;
       }
       case BUF_ENTRY_TYPE__DefFileGroup:
       {
	 VTBuf_Entry_DefFileGroup* entry =
	   (VTBuf_Entry_DefFileGroup*)p; 

	 OTF_WStream_writeDefFileGroup(gen->file,
	   entry->gid,
	   entry->gname);

	 free(entry->gname);
	 break;
       }
       case BUF_ENTRY_TYPE__DefFile:
       {     
	 VTBuf_Entry_DefFile* entry =
	   (VTBuf_Entry_DefFile*)p;  

	 OTF_WStream_writeDefFile(gen->file,
	   entry->fid,
	   entry->fname,
	   entry->gid);

	 free(entry->fname);
	 break;
       }
       case BUF_ENTRY_TYPE__DefFunctionGroup:
       {
	 VTBuf_Entry_DefFunctionGroup* entry =
	   (VTBuf_Entry_DefFunctionGroup*)p; 

	 OTF_WStream_writeDefFunctionGroup(gen->file,
	   entry->rdid,
	   entry->rdesc);
	 
	 free(entry->rdesc);
	 break;
       }
       case BUF_ENTRY_TYPE__DefFunction:
       {
	 VTBuf_Entry_DefFunction* entry =
	   (VTBuf_Entry_DefFunction*)p; 

	 OTF_WStream_writeDefFunction(gen->file,
	   entry->rid,
	   entry->rname,
	   entry->rdid,
	   entry->sid);
				      
	 free(entry->rname);
	 break;
       }
       case BUF_ENTRY_TYPE__DefCollectiveOperation:
       {
	 VTBuf_Entry_DefCollectiveOperation* entry =
	   (VTBuf_Entry_DefCollectiveOperation*)p;
	  
	 OTF_WStream_writeDefCollectiveOperation(gen->file,
	   entry->cid,
	   entry->cname,
	   entry->ctype);

	 free(entry->cname);
	 break;
       }
       case BUF_ENTRY_TYPE__DefCounterGroup:
       {
	 VTBuf_Entry_DefCounterGroup* entry =
	   (VTBuf_Entry_DefCounterGroup*)p; 

	 OTF_WStream_writeDefCounterGroup(gen->file,
	   entry->gid,
	   entry->gname);

	 free(entry->gname);
	 break;
       }
       case BUF_ENTRY_TYPE__DefCounter:
       {     
	 VTBuf_Entry_DefCounter* entry =
	   (VTBuf_Entry_DefCounter*)p;  
	  
	 OTF_WStream_writeDefCounter(gen->file,
	   entry->cid,
	   entry->cname,
	   entry->cprop,
	   entry->gid,
	   entry->cunit);

	 free(entry->cname);
	 free(entry->cunit);

	 break;
       }
       case BUF_ENTRY_TYPE__DefProcessGroup:
       {
	 VTBuf_Entry_DefProcessGroup* entry =
	   (VTBuf_Entry_DefProcessGroup*)p;  

	 OTF_WStream_writeDefProcessGroup(gen->file,
	   entry->cid,
	   entry->grpn,
	   entry->grpc,
	   entry->grpv);
	 
	 free(entry->grpn);
	 free(entry->grpv);

	 break;
       }
       case BUF_ENTRY_TYPE__Enter:
       {
	 VTBuf_Entry_EnterLeave* entry =
	   (VTBuf_Entry_EnterLeave*)p;  

	 OTF_WStream_writeEnter(gen->file,
	   entry->time,
	   entry->rid,
	   65536 * gen->tid + gen->trcid + 1,
	   entry->sid);

	 for(i = 0; i < entry->metc; i++)
	 {
	   OTF_WStream_writeCounter(gen->file,
	     entry->time,
	     65536 * gen->tid + gen->trcid + 1,
	     i+1,
	     (uint64_t)entry->metv[i]);
	 }

	 break;
       }
       case BUF_ENTRY_TYPE__Leave:
       {
	 VTBuf_Entry_EnterLeave* entry =
	   (VTBuf_Entry_EnterLeave*)p;  

	 for(i = 0; i < entry->metc; i++)
	 {
	   OTF_WStream_writeCounter(gen->file,
	     entry->time,
	     65536 * gen->tid + gen->trcid + 1,
	     i+1,
	     (uint64_t)entry->metv[i]);
	 }

	 OTF_WStream_writeLeave(gen->file,
	   entry->time,
	   entry->rid,
	   65536 * gen->tid + gen->trcid + 1,
	   entry->sid);

	 break;
       }
       case BUF_ENTRY_TYPE__FileOperation:
       {
	 VTBuf_Entry_FileOperation* entry =
	   (VTBuf_Entry_FileOperation*)p;

	 OTF_WStream_writeFileOperation(gen->file,
	   entry->time,
	   entry->fid,
	   65536 * gen->tid + gen->trcid + 1,
	   entry->hid,
	   entry->op,
	   entry->bytes,
	   entry->etime - entry->time,
	   entry->sid);

	 break;
       }
       case BUF_ENTRY_TYPE__Counter:
       {
	 VTBuf_Entry_Counter* entry =
	   (VTBuf_Entry_Counter*)p;  

	 OTF_WStream_writeCounter(gen->file,
	   entry->time,
	   65536 * gen->tid + gen->trcid + 1,
	   entry->cid, entry->cval);

	 break;
       }
       case BUF_ENTRY_TYPE__Comment:
       {
	 VTBuf_Entry_Comment* entry =
	   (VTBuf_Entry_Comment*)p;  

	 OTF_WStream_writeEventComment(gen->file,
	   entry->time,
	   65536 * gen->tid + gen->trcid + 1,
	   entry->comment);

	 free(entry->comment);
	 break;
       }
       case BUF_ENTRY_TYPE__SendMsg:
       {
	 VTBuf_Entry_SendRecvMsg* entry =
	   (VTBuf_Entry_SendRecvMsg*)p;

	 OTF_WStream_writeSendMsg(gen->file,
	   entry->time,
	   gen->trcid+1,
	   entry->dpid,
	   entry->cid,
	   entry->tag,
	   entry->len,
	   entry->sid);

	 break;
       }
       case BUF_ENTRY_TYPE__RecvMsg:
       {
	 VTBuf_Entry_SendRecvMsg* entry =
	   (VTBuf_Entry_SendRecvMsg*)p;

	 OTF_WStream_writeRecvMsg(gen->file,
	   entry->time,
	   gen->trcid+1,
	   entry->spid,
	   entry->cid,
	   entry->tag,
	   entry->len,
	   entry->sid);

	 break;
       }
       case BUF_ENTRY_TYPE__CollectiveOperation:
       {
	 VTBuf_Entry_CollectiveOperation* entry =
	   (VTBuf_Entry_CollectiveOperation*)p;

	 OTF_WStream_writeCollectiveOperation(gen->file,
	   entry->time,
	   65536 * gen->tid + gen->trcid + 1,
	   entry->rid,
	   entry->cid,
	   entry->rpid,
	   entry->sent,
	   entry->recvd,
	   entry->etime - entry->time,
	   entry->sid);

	 break;
       }
       case BUF_ENTRY_TYPE__FunctionSummary:
       {
	 VTBuf_Entry_FunctionSummary* entry =
	   (VTBuf_Entry_FunctionSummary*)p;

	 OTF_WStream_writeFunctionSummary(gen->file,
	   entry->time,
	   entry->rid,
	   65536 * gen->tid + gen->trcid + 1,
	   entry->cnt,
	   entry->excl,
	   entry->incl);

	 break;
       }
       case BUF_ENTRY_TYPE__MessageSummary:
       {
	 VTBuf_Entry_MessageSummary* entry =
	   (VTBuf_Entry_MessageSummary*)p;

	 OTF_WStream_writeMessageSummary(gen->file,
	   entry->time,
	   65536 * gen->tid + gen->trcid + 1,
	   entry->peer,
	   entry->cid,
	   entry->tag,
	   entry->scnt,
	   entry->rcnt,
	   entry->sent,
	   entry->recvd);

	 break;
       }
       case BUF_ENTRY_TYPE__FileOperationSummary:
       {
	 VTBuf_Entry_FileOperationSummary* entry =
	   (VTBuf_Entry_FileOperationSummary*)p;

	 OTF_WStream_writeFileOperationSummary(gen->file,
	   entry->time,
	   entry->fid,
	   65536 * gen->tid + gen->trcid + 1,
	   entry->nopen,
	   entry->nclose,
	   entry->nread,
	   entry->nwrite,
	   entry->nseek,
	   entry->read,
	   entry->wrote);

	 break;
       }
       default:
       {
	 vt_error();
       }
     }

     /* last buffer entry and end flush not marked ? */
     if(!end_flush_marked &&
	p + ((VTBuf_Entry_Base*)p)->length >= gen->buf->pos)
     {
       /* mark end of flush */
       if(markFlush)
       {
	 uint64_t flush_etime = vt_pform_wtime();
	 vt_exit_flush(&flush_etime);
	 if( flushETime != NULL ) *flushETime = flush_etime;
       }

       end_flush_marked = 1;
     }

     p += ((VTBuf_Entry_Base*)p)->length;
  }

  /* reset buffer */
  gen->buf->pos = gen->buf->mem;

  /* decrement flush counter */
  if( gen->flushcntr > 0 ) gen->flushcntr--;

  vt_cntl_msg("Flushed OTF writer stream [namestub %s id %x]",
	       gen->name, gen->tid+1);

  /* Enable I/O tracing again */
  VT_RESUME_IO_TRACING();
}

void VTGen_close(VTGen* gen)
{
  /* close summary */
  if (VTGEN_IS_SUM_ON(gen))
    VTSum_close(gen->sum);

  /* flush buffer if necessary */
  if(gen->buf->pos > gen->buf->mem)
    VTGen_flush(gen, 0, 0, NULL);

  /* close writer stream */
  OTF_WStream_close(gen->file);

  vt_cntl_msg("Closed OTF writer stream [namestub %s id %x]",
	       gen->name, gen->tid+1);

  /* free summary record */
  free(gen->sum);

  /* free buffer memory */
  free(gen->buf->mem); 

  /* free buffer record */
  free(gen->buf); 
}

void VTGen_delete(VTGen* gen)
{
  char* tmp_namev[4];
  uint8_t i;

  /* determine (local) files for removal */
  tmp_namev[0] = VTGen_get_defname(gen);
  tmp_namev[1] = VTGen_get_eventname(gen);
  tmp_namev[2] = VTGen_get_statname(gen);
  tmp_namev[3] = NULL;

  i = 0;
  while(tmp_namev[i] != NULL)
  {
    int result;

    if (vt_env_do_clean())
    {
      /* delete the temporary trace file */
      result = remove(tmp_namev[i]);
      if (result == 0)
	vt_cntl_msg("Removed trace file %s", tmp_namev[i]);
    }
    else
    {
      vt_cntl_msg("*Left* trace file %s", tmp_namev[i]);
    }

    i++;
  }

  free(tmp_namev[0]);
  free(tmp_namev[1]);
  free(tmp_namev[2]);
  
  /* close file manager of writer stream */
  OTF_FileManager_close(gen->filemanager);

  /* free gen record */
  free(gen); 
}

void VTGen_init_trc_id(VTGen* gen, uint32_t trcid)
{
  gen->trcid = trcid;
}

char* VTGen_get_name(VTGen* gen)
{
  return gen->name;
}

char* VTGen_get_defname(VTGen* gen)
{
  return OTF_getFilename(gen->name, gen->tid+1,
			 OTF_FILETYPE_DEF | gen->filecomp,
			 0, NULL);
}

char* VTGen_get_eventname(VTGen* gen)
{
  return OTF_getFilename(gen->name, gen->tid+1,
			 OTF_FILETYPE_EVENT | gen->filecomp,
			 0, NULL);
}

char* VTGen_get_statname(VTGen* gen)
{
  return OTF_getFilename(gen->name, gen->tid+1,
			 OTF_FILETYPE_STATS | gen->filecomp,
			 0, NULL);
}

/* -- Writing trace records -- */


/* - Definition records - */

void VTGen_write_DEFINITION_COMMENT(VTGen* gen,
				     const char* comment)
{
  VTBuf_Entry_DefinitionComment* new_entry;

  uint8_t length =
    VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_DefinitionComment));

  VTGEN_CHECK(gen);

  VTGEN_ALLOC_DEF(gen, length);

  new_entry = ((VTBuf_Entry_DefinitionComment*)gen->buf->pos);

  new_entry->type    = BUF_ENTRY_TYPE__DefinitionComment;
  new_entry->length  = length;
  new_entry->comment = strdup(comment);

  VTGEN_JUMP(gen, length);
  VTGEN_CHECK_FLUSHCNTR(gen);
}

void VTGen_write_DEF_SCL_FILE(VTGen* gen,
			       uint32_t fid,
			       const char* fname)
{
  VTBuf_Entry_DefSclFile* new_entry;

  uint8_t length =
    VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_DefSclFile));

  VTGEN_CHECK(gen);

  VTGEN_ALLOC_DEF(gen, length);

  new_entry = ((VTBuf_Entry_DefSclFile*)gen->buf->pos);
  
  new_entry->type   = BUF_ENTRY_TYPE__DefSclFile;
  new_entry->length = length;
  new_entry->fid    = fid;
  new_entry->fname  = strdup(fname);

  VTGEN_JUMP(gen, length);
  VTGEN_CHECK_FLUSHCNTR(gen);
}

void VTGen_write_DEF_SCL(VTGen* gen,
			  uint32_t sid,
			  uint32_t fid,
			  uint32_t ln)
{
  VTBuf_Entry_DefScl* new_entry;

  uint8_t length =
    VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_DefScl));

  VTGEN_CHECK(gen);

  VTGEN_ALLOC_DEF(gen, length);

  new_entry = ((VTBuf_Entry_DefScl*)gen->buf->pos);
  new_entry->type   = BUF_ENTRY_TYPE__DefScl;
  new_entry->length = length;
  new_entry->sid    = sid;
  new_entry->fid    = fid;
  new_entry->ln     = ln;

  VTGEN_JUMP(gen, length);
  VTGEN_CHECK_FLUSHCNTR(gen);
}

void VTGen_write_DEF_FILE_GROUP(VTGen* gen,
				 uint32_t gid,
				 const char* gname)
{
  VTBuf_Entry_DefFileGroup* new_entry;

  uint8_t length =
    VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_DefFileGroup));

  VTGEN_CHECK(gen);

  VTGEN_ALLOC_DEF(gen, length);

  new_entry = ((VTBuf_Entry_DefFileGroup*)gen->buf->pos);
    
  new_entry->type   = BUF_ENTRY_TYPE__DefFileGroup;
  new_entry->length = length;
  new_entry->gid    = gid;
  new_entry->gname  = strdup(gname);

  VTGEN_JUMP(gen, length);
  VTGEN_CHECK_FLUSHCNTR(gen);
}

void VTGen_write_DEF_FILE(VTGen* gen,
			   uint32_t fid,
			   const char* fname,
			   uint32_t gid)
{
  VTBuf_Entry_DefFile* new_entry;

  uint8_t length =
    VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_DefFile));

  VTGEN_CHECK(gen);

  VTGEN_ALLOC_DEF(gen, length);

  new_entry = ((VTBuf_Entry_DefFile*)gen->buf->pos);

  new_entry->type   = BUF_ENTRY_TYPE__DefFile;
  new_entry->length = length;
  new_entry->fid    = fid;
  new_entry->fname  = strdup(fname);
  new_entry->gid    = gid;
    
  VTGEN_JUMP(gen, length);
  VTGEN_CHECK_FLUSHCNTR(gen);
}

void VTGen_write_DEF_FUNCTION_GROUP(VTGen* gen,
				     uint32_t rdid,
				     const char* rdesc)
{
  VTBuf_Entry_DefFunctionGroup* new_entry;

  uint8_t length =
    VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_DefFunctionGroup));

  VTGEN_CHECK(gen);

  VTGEN_ALLOC_DEF(gen, length);

  new_entry = ((VTBuf_Entry_DefFunctionGroup*)gen->buf->pos);

  new_entry->type   = BUF_ENTRY_TYPE__DefFunctionGroup;
  new_entry->length = length;
  new_entry->rdid   = rdid;
  new_entry->rdesc  = strdup(rdesc);

  VTGEN_JUMP(gen, length);
  VTGEN_CHECK_FLUSHCNTR(gen);
}

void VTGen_write_DEF_FUNCTION(VTGen* gen,
			       uint32_t rid,
			       const char* rname,
			       uint32_t rdid,
			       uint32_t sid)
{
  VTBuf_Entry_DefFunction* new_entry;

  uint8_t length =
    VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_DefFunction));

  VTGEN_CHECK(gen);

  VTGEN_ALLOC_DEF(gen, length);

  new_entry = ((VTBuf_Entry_DefFunction*)gen->buf->pos);
    
  new_entry->type   = BUF_ENTRY_TYPE__DefFunction;
  new_entry->length = length;
  new_entry->rid    = rid;
  new_entry->rname  = strdup(rname);
  new_entry->rdid   = rdid;
  new_entry->sid    = sid;

  VTGEN_JUMP(gen, length);
  VTGEN_CHECK_FLUSHCNTR(gen);
}

void VTGen_write_DEF_COLLECTIVE_OPERATION(VTGen* gen,
					   uint32_t cid,
					   const char* cname,
					   uint32_t ctype)
{
  VTBuf_Entry_DefCollectiveOperation* new_entry;
  
  uint8_t length = 
    VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_DefCollectiveOperation));

  VTGEN_CHECK(gen);

  VTGEN_ALLOC_DEF(gen, length);
  
  new_entry = ((VTBuf_Entry_DefCollectiveOperation*)gen->buf->pos);
  
  new_entry->type   = BUF_ENTRY_TYPE__DefCollectiveOperation;
  new_entry->length = length;
  new_entry->cid    = cid;
  new_entry->cname  = strdup(cname);
  new_entry->ctype  = ctype;
  
  VTGEN_JUMP(gen, length);
  VTGEN_CHECK_FLUSHCNTR(gen);
}

void VTGen_write_DEF_COUNTER_GROUP(VTGen* gen,
				    uint32_t gid,
				    const char* gname)
{
  VTBuf_Entry_DefCounterGroup* new_entry;

  uint8_t length =
    VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_DefCounterGroup));

  VTGEN_CHECK(gen);

  VTGEN_ALLOC_DEF(gen, length);

  new_entry = ((VTBuf_Entry_DefCounterGroup*)gen->buf->pos);
    
  new_entry->type   = BUF_ENTRY_TYPE__DefCounterGroup;
  new_entry->length = length;
  new_entry->gid    = gid;
  new_entry->gname  = strdup(gname);

  VTGEN_JUMP(gen, length);
  VTGEN_CHECK_FLUSHCNTR(gen);
}

void VTGen_write_DEF_COUNTER(VTGen* gen,
			      uint32_t cid,
			      const char* cname,
			      uint32_t cprop,
			      uint32_t gid,
			      const char* cunit)
{
  VTBuf_Entry_DefCounter* new_entry;

  uint8_t length =
    VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_DefCounter));

  VTGEN_CHECK(gen);

  VTGEN_ALLOC_DEF(gen, length);

  new_entry = ((VTBuf_Entry_DefCounter*)gen->buf->pos);

  new_entry->type   = BUF_ENTRY_TYPE__DefCounter;
  new_entry->length = length;
  new_entry->cid    = cid;
  new_entry->cname  = strdup(cname);
  new_entry->cprop  = cprop;
  new_entry->gid    = gid;
  new_entry->cunit  = strdup(cunit);
    
  VTGEN_JUMP(gen, length);
  VTGEN_CHECK_FLUSHCNTR(gen);
}

void VTGen_write_DEF_PROCESS_GROUP(VTGen* gen,
				    uint32_t cid,
				    const char* grpn,
				    uint32_t grpc,
				    uint32_t grpv[])
{
  VTBuf_Entry_DefProcessGroup* new_entry;

  uint8_t length =
    VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_DefProcessGroup));

  VTGEN_CHECK(gen);

  VTGEN_ALLOC_DEF(gen, length);
    
  new_entry = ((VTBuf_Entry_DefProcessGroup*)gen->buf->pos);
    
  new_entry->type   = BUF_ENTRY_TYPE__DefProcessGroup;
  new_entry->length = length;
  new_entry->cid    = cid;
  new_entry->grpn   = strdup(grpn);
  new_entry->grpc   = grpc;
  new_entry->grpv   = (uint32_t*)calloc(grpc, sizeof(uint32_t));
  if(new_entry->grpv == NULL)
    vt_error();

  memcpy(new_entry->grpv, grpv, grpc * sizeof(uint32_t));
  
  VTGEN_JUMP(gen, length);
  VTGEN_CHECK_FLUSHCNTR(gen);
}


/* - Event records - */

/* -- Region -- */

void VTGen_write_ENTER(VTGen* gen, uint64_t* time, uint32_t rid, uint32_t sid,
       uint8_t metc, uint64_t metv[])
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_EnterLeave* new_entry;

    uint8_t length;

    length = VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_EnterLeave) +
	       (metc > 0 ? (metc - 1) * sizeof(uint64_t) : 0 )));

    VTGEN_ALLOC_EVENT(gen, length);

    new_entry = ((VTBuf_Entry_EnterLeave*)gen->buf->pos);
    
    new_entry->type   = BUF_ENTRY_TYPE__Enter;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->rid    = rid;
    new_entry->sid    = sid;
    new_entry->metc   = metc;
    if( metc > 0 )
      memcpy(new_entry->metv, metv, metc * sizeof(uint64_t));
    
    VTGEN_JUMP(gen, length);
  }

  if (VTGEN_IS_SUM_ON(gen))
    VTSum_enter(gen->sum, time, rid);

  VTGEN_CHECK_FLUSHCNTR(gen);
}

void VTGen_write_LEAVE(VTGen* gen, uint64_t* time, uint32_t rid, uint32_t sid,
       uint8_t metc, uint64_t metv[])
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_EnterLeave* new_entry;

    uint8_t length;

    length = VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_EnterLeave) + 
	       (metc > 0 ? (metc - 1) * sizeof(uint64_t) : 0)));

    VTGEN_ALLOC_EVENT(gen, length);

    new_entry = ((VTBuf_Entry_EnterLeave*)gen->buf->pos);

    new_entry->type   = BUF_ENTRY_TYPE__Leave;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->rid    = rid;
    new_entry->sid    = sid;
    new_entry->metc   = metc;
    if( metc > 0 )
      memcpy(new_entry->metv, metv, metc * sizeof(uint64_t));

    VTGEN_JUMP(gen, length);
  }

  if (VTGEN_IS_SUM_ON(gen))
    VTSum_exit(gen->sum, time, rid);

  VTGEN_CHECK_FLUSHCNTR(gen);
}


/* -- File I/O -- */

void VTGen_write_FILE_OPERATION(VTGen* gen, uint64_t* time,
       uint64_t* etime, uint32_t fid, uint64_t hid,
       uint32_t op, uint64_t bytes, uint32_t sid)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_FileOperation* new_entry;

    uint8_t length;
    
    length = VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_FileOperation));
    
    *etime -= *time;
    VTGEN_ALLOC_EVENT(gen, length);
    *etime += *time;
    
    new_entry = ((VTBuf_Entry_FileOperation*)gen->buf->pos);
      
    new_entry->type   = BUF_ENTRY_TYPE__FileOperation;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->etime  = *etime;
    new_entry->fid    = fid;
    new_entry->hid    = hid;
    new_entry->op     = op;
    new_entry->bytes  = bytes;
    new_entry->sid    = sid;

    VTGEN_JUMP(gen, length);
  }

  if (VTGEN_IS_SUM_ON(gen))
  {
    switch( op )
    {
      case OTF_FILEOP_OPEN:
      {
	VTSum_fop_open(gen->sum, time, fid);
	break;
      }
      case OTF_FILEOP_CLOSE:
      {
	VTSum_fop_close(gen->sum, time, fid);
	break;
      }
      case OTF_FILEOP_READ:
      {
	VTSum_fop_read(gen->sum, time, fid, bytes);
	break;
      }
      case OTF_FILEOP_WRITE:
      {
	VTSum_fop_write(gen->sum, time, fid, bytes);
	break;
      }
      case OTF_FILEOP_SEEK:
      {
        VTSum_fop_seek(gen->sum, time, fid);
	break;
      }
    }
  }

  VTGEN_CHECK_FLUSHCNTR(gen);
}


/* -- Counter -- */

void VTGen_write_COUNTER(VTGen* gen, uint64_t* time, uint32_t cid,
       uint64_t cval)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_Counter* new_entry;

    uint8_t length;

    length = VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_Counter));

    VTGEN_ALLOC_EVENT(gen, length);

    new_entry = ((VTBuf_Entry_Counter*)gen->buf->pos);

    new_entry->type   = BUF_ENTRY_TYPE__Counter;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->cid    = cid;
    new_entry->cval   = cval;

    VTGEN_JUMP(gen, length);
    VTGEN_CHECK_FLUSHCNTR(gen);
  }
}


/* -- Comment -- */

void VTGen_write_COMMENT(VTGen* gen, uint64_t* time,
       const char* comment)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_Comment* new_entry;

    uint8_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_Comment));

    VTGEN_ALLOC_EVENT(gen, length);

    new_entry = ((VTBuf_Entry_Comment*)gen->buf->pos);

    new_entry->type    = BUF_ENTRY_TYPE__Comment;
    new_entry->length  = length;
    new_entry->time    = *time;
    new_entry->comment = strdup(comment);
    
    VTGEN_JUMP(gen, length);
    VTGEN_CHECK_FLUSHCNTR(gen);
  }
}


/* -- MPI-1 -- */

void VTGen_write_SEND_MSG(VTGen* gen, uint64_t* time, uint32_t dpid,
       uint32_t cid, uint32_t tag, uint32_t sent, uint32_t sid)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_SendRecvMsg* new_entry;

    uint8_t length;

    length = VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_SendRecvMsg));

    VTGEN_ALLOC_EVENT(gen, length);
    
    new_entry = ((VTBuf_Entry_SendRecvMsg*)gen->buf->pos);
    
    new_entry->type   = BUF_ENTRY_TYPE__SendMsg;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->dpid   = dpid;
    new_entry->cid    = cid;
    new_entry->tag    = tag;
    new_entry->len    = sent;
    new_entry->sid    = sid;
      
    VTGEN_JUMP(gen, length);
  }

  if (VTGEN_IS_SUM_ON(gen))
    VTSum_mpi_send(gen->sum, time, dpid, cid, tag, (uint64_t)sent);

  VTGEN_CHECK_FLUSHCNTR(gen);
}

void VTGen_write_RECV_MSG(VTGen* gen, uint64_t* time, uint32_t spid,
       uint32_t cid, uint32_t tag, uint32_t recvd, uint32_t sid)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_SendRecvMsg* new_entry;
  
    uint8_t length;
    
    length = VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_SendRecvMsg));

    VTGEN_ALLOC_EVENT(gen, length);

    new_entry = ((VTBuf_Entry_SendRecvMsg*)gen->buf->pos);

    new_entry->type   = BUF_ENTRY_TYPE__RecvMsg;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->spid   = spid;
    new_entry->cid    = cid;
    new_entry->tag    = tag;
    new_entry->len    = recvd;
    new_entry->sid    = sid;
    
    VTGEN_JUMP(gen, length);
  }

  if (VTGEN_IS_SUM_ON(gen))
    VTSum_mpi_recv(gen->sum, time, spid, cid, tag, (uint64_t)recvd);

  VTGEN_CHECK_FLUSHCNTR(gen);
}

void VTGen_write_COLLECTIVE_OPERATION(VTGen* gen, uint64_t* time,
       uint64_t* etime, uint32_t rid, uint32_t cid, uint32_t rpid,
       uint32_t sent, uint32_t recvd, uint32_t sid)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_CollectiveOperation* new_entry;

    uint8_t length;
    
    length = VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_CollectiveOperation));
    
    *etime -= *time;
    VTGEN_ALLOC_EVENT(gen, length);
    *etime += *time;
    
    new_entry = ((VTBuf_Entry_CollectiveOperation*)gen->buf->pos);
    
    new_entry->type   = BUF_ENTRY_TYPE__CollectiveOperation;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->etime  = *etime;
    new_entry->rid    = rid;
    new_entry->cid    = cid;
    new_entry->rpid   = rpid;
    new_entry->sent   = sent;
    new_entry->recvd  = recvd;
    new_entry->sid    = sid;

    VTGEN_JUMP(gen, length);
    VTGEN_CHECK_FLUSHCNTR(gen);
  }
}


/* -- OpenMP -- */

void VTGen_write_OMP_FORK(VTGen* gen, uint64_t* time)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_EnterLeave* new_entry;

    uint8_t length;
    
    length = VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_EnterLeave)));
    
    VTGEN_ALLOC_EVENT(gen, length);
    
    new_entry = ((VTBuf_Entry_EnterLeave*)gen->buf->pos);
    
    new_entry->type   = BUF_ENTRY_TYPE__Enter;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->rid    = vt_trc_regid[VT__PREG];
    new_entry->sid    = 0;
    new_entry->metc   = 0;
    
    VTGEN_JUMP(gen, length);
    VTGEN_CHECK_FLUSHCNTR(gen);
  }
}

void VTGen_write_OMP_JOIN(VTGen* gen, uint64_t* time)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_EnterLeave* new_entry;

    uint8_t length;

    length = VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_EnterLeave)));

    VTGEN_ALLOC_EVENT(gen, length);

    new_entry = ((VTBuf_Entry_EnterLeave*)gen->buf->pos);
    
    new_entry->type   = BUF_ENTRY_TYPE__Leave;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->rid    = 0;
    new_entry->sid    = 0;
    new_entry->metc   = 0;

    VTGEN_JUMP(gen, length);
    VTGEN_CHECK_FLUSHCNTR(gen);
  }
}


/* -- Summary -- */

void VTGen_write_FUNCTION_SUMMARY(VTGen* gen, uint64_t* time,
     uint32_t rid, uint64_t cnt, uint64_t excl, uint64_t incl)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_SUM_ON(gen))
  {
    VTBuf_Entry_FunctionSummary* new_entry;

    uint8_t length;

    length = VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_FunctionSummary));

    VTGEN_ALLOC_EVENT(gen, length);

    new_entry = ((VTBuf_Entry_FunctionSummary*)gen->buf->pos);
    
    new_entry->type   = BUF_ENTRY_TYPE__FunctionSummary;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->rid    = rid;
    new_entry->cnt    = cnt;
    new_entry->excl   = excl;
    new_entry->incl   = incl;
    
    VTGEN_JUMP(gen, length);
    VTGEN_CHECK_FLUSHCNTR(gen);
  }
}

void VTGen_write_MESSAGE_SUMMARY(VTGen* gen, uint64_t* time,
       uint32_t peer, uint32_t cid, uint32_t tag,
       uint64_t scnt, uint64_t rcnt, uint64_t sent, uint64_t recvd)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_SUM_ON(gen))
  {
    VTBuf_Entry_MessageSummary* new_entry;

    uint8_t length;

    length = VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_MessageSummary));

    VTGEN_ALLOC_EVENT(gen, length);

    new_entry = ((VTBuf_Entry_MessageSummary*)gen->buf->pos);
    
    new_entry->type   = BUF_ENTRY_TYPE__MessageSummary;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->peer   = peer;
    new_entry->cid    = cid;
    new_entry->tag    = tag;
    new_entry->scnt   = scnt;
    new_entry->rcnt   = rcnt;
    new_entry->sent   = sent;
    new_entry->recvd  = recvd;
    
    VTGEN_JUMP(gen, length);
    VTGEN_CHECK_FLUSHCNTR(gen);
  }
}

void VTGen_write_FILE_OPERATION_SUMMARY(VTGen* gen, uint64_t* time,
       uint32_t fid, uint64_t nopen, uint64_t nclose, uint64_t nread,
       uint64_t nwrite, uint64_t nseek, uint64_t read, uint64_t wrote)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_SUM_ON(gen))
  {
    VTBuf_Entry_FileOperationSummary* new_entry;

    uint8_t length;

    length = VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_FileOperationSummary));

    VTGEN_ALLOC_EVENT(gen, length);

    new_entry = ((VTBuf_Entry_FileOperationSummary*)gen->buf->pos);
    
    new_entry->type   = BUF_ENTRY_TYPE__FileOperationSummary;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->fid    = fid;
    new_entry->nopen  = nopen;
    new_entry->nclose = nclose;
    new_entry->nread  = nread;
    new_entry->nwrite = nwrite;
    new_entry->nseek  = nseek;
    new_entry->read   = read;
    new_entry->wrote  = wrote;
    
    VTGEN_JUMP(gen, length);
    VTGEN_CHECK_FLUSHCNTR(gen);
  }
}


/* -- VampirTrace Internal -- */

void VTGen_write_ENTER_STAT(VTGen* gen, uint64_t* time, 
       uint8_t metc, uint64_t metv[])
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_EnterLeave* new_entry;

    uint8_t length;

    length = VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_EnterLeave) + 
				(metc > 0 ? (metc - 1) * sizeof(uint64_t) : 0)));

    VTGEN_ALLOC_EVENT(gen, length);

    new_entry = ((VTBuf_Entry_EnterLeave*)gen->buf->pos);
    
    new_entry->type   = BUF_ENTRY_TYPE__Enter;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->rid    = vt_trc_regid[VT__STAT];
    new_entry->sid    = 0;
    new_entry->metc   = metc;
    if( metc > 0 )
      memcpy(new_entry->metv, metv, metc * sizeof(uint64_t));

    VTGEN_JUMP(gen, length);
    VTGEN_CHECK_FLUSHCNTR(gen);
  }
}

void VTGen_write_EXIT_STAT(VTGen* gen, uint64_t* time,
       uint8_t metc, uint64_t metv[])
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_EnterLeave* new_entry;

    uint8_t length;

    length = VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_EnterLeave) + 
				(metc > 0 ? (metc - 1) * sizeof(uint64_t) : 0)));

    VTGEN_ALLOC_EVENT(gen, length);

    new_entry = ((VTBuf_Entry_EnterLeave*)gen->buf->pos);
    
    new_entry->type   = BUF_ENTRY_TYPE__Leave;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->rid    = 0;
    new_entry->sid    = 0;
    new_entry->metc   = metc;
    if( metc > 0 )
      memcpy(new_entry->metv, metv, metc * sizeof(uint64_t));

    VTGEN_JUMP(gen, length);
    VTGEN_CHECK_FLUSHCNTR(gen);
  }
}

void VTGen_write_ENTER_FLUSH(VTGen* gen, uint64_t* time, 
       uint8_t metc, uint64_t metv[])
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_EnterLeave* new_entry;

    uint8_t length;

    length = VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_EnterLeave) + 
				(metc > 0 ? (metc - 1) * sizeof(uint64_t) : 0)));

    /* NB: No VTGEN_ALLOC_EVENT since space reserved at buffer creation */

    new_entry = ((VTBuf_Entry_EnterLeave*)gen->buf->pos);
    
    new_entry->type   = BUF_ENTRY_TYPE__Enter;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->rid    = vt_trc_regid[VT__FLUSH];
    new_entry->sid    = 0;
    new_entry->metc   = metc;
    if( metc > 0 )
      memcpy(new_entry->metv, metv, metc * sizeof(uint64_t));

    VTGEN_JUMP(gen, length);
  }
}

void VTGen_write_EXIT_FLUSH(VTGen* gen, uint64_t* time,
       uint8_t metc, uint64_t metv[])
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_EnterLeave* new_entry;

    uint8_t length;

    length = VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_EnterLeave) + 
				(metc > 0 ? (metc - 1) * sizeof(uint64_t) : 0)));

    /* NB: No VTGEN_ALLOC_EVENT since space reserved at buffer creation */

    new_entry = ((VTBuf_Entry_EnterLeave*)gen->buf->pos);
    
    new_entry->type   = BUF_ENTRY_TYPE__Leave;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->rid    = 0;
    new_entry->sid    = 0;
    new_entry->metc   = metc;
    if( metc > 0 )
      memcpy(new_entry->metv, metv, metc * sizeof(uint64_t));

    VTGEN_JUMP(gen, length);
  }
}

