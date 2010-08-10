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

#include "config.h"

#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

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

#include "otf.h"

/*
 *-----------------------------------------------------------------------------
 * Macro functions
 *-----------------------------------------------------------------------------
 */

#define VTGEN_CHECK(gen)                                            \
  if (gen == NULL) vt_error_msg("Abort: Uninitialized trace buffer")

#define VTGEN_ALLOC_DEF(gen, bytes)                                 \
  if ((uint64_t)((gen)->buf->pos - (gen)->buf->mem) >               \
      (uint64_t)((gen)->buf->size - (bytes)))                       \
    VTGen_flush((gen), 0, vt_pform_wtime(), NULL);

#define VTGEN_ALLOC_EVENT(gen, bytes)                               \
  if ((uint64_t)((gen)->buf->pos - (gen)->buf->mem) >               \
      (uint64_t)((gen)->buf->size - (bytes))) {                     \
    VTGen_flush((gen), 0, *time, time);                             \
    if((gen)->flushcntr == 0) return;                               \
  }

#define VTGEN_ALIGN_LENGTH(bytes)                                   \
  ( (bytes) % SIZEOF_VOIDP ) ?                                      \
    ( (bytes) / SIZEOF_VOIDP + 1 ) * SIZEOF_VOIDP : (bytes)

#define VTGEN_JUMP(gen, bytes)                                      \
  gen->buf->pos += (bytes)

#define VTGEN_IS_TRACE_ON(gen) ((gen)->mode & VT_MODE_TRACE) != 0
#define VTGEN_IS_SUM_ON(gen) ((gen)->mode & VT_MODE_STAT) != 0
#define VTGEN_IS_SUM_PROP_ON(gen, prop) \
  (VTGEN_IS_SUM_ON((gen)) && ((gen)->sum_props & (prop)) != 0)

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
	       BUF_ENTRY_TYPE__DefMarker,
	       BUF_ENTRY_TYPE__Enter,
	       BUF_ENTRY_TYPE__Leave,
	       BUF_ENTRY_TYPE__FileOperation,
	       BUF_ENTRY_TYPE__BeginFileOperation,
	       BUF_ENTRY_TYPE__EndFileOperation,
	       BUF_ENTRY_TYPE__Counter,
	       BUF_ENTRY_TYPE__Comment,
	       BUF_ENTRY_TYPE__Marker,
	       BUF_ENTRY_TYPE__SendMsg,
	       BUF_ENTRY_TYPE__RecvMsg,
               BUF_ENTRY_TYPE__RMAPut,
               BUF_ENTRY_TYPE__RMAPutRE,
               BUF_ENTRY_TYPE__RMAGet,
               BUF_ENTRY_TYPE__RMAEnd,
	       BUF_ENTRY_TYPE__CollectiveOperation,
	       BUF_ENTRY_TYPE__FunctionSummary,
	       BUF_ENTRY_TYPE__MessageSummary,
	       BUF_ENTRY_TYPE__CollectiveOperationSummary,
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
  OTF_WStream*        filestream;
  OTF_FileCompression filecomp;
  char*               fileprefix;
  const char*         tnameprefix;
  const char*         tnamesuffix;
  const char*         tnameextern;
  uint32_t            ptid;
  uint32_t            tid;
  int32_t             flushcntr;
  uint8_t             isfirstflush;
  uint8_t             mode;
  uint8_t             sum_props;
  VTSum*              sum;
  VTBuf*              buf;
};

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;
} VTBuf_Entry_Base;

/* BUF_ENTRY_TYPE__DefinitionComment */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  char comment[1];
} VTBuf_Entry_DefinitionComment;

/* BUF_ENTRY_TYPE__DefSclFile */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint32_t fid;
  char     fname[1];
} VTBuf_Entry_DefSclFile;

/* BUF_ENTRY_TYPE__DefScl */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint32_t sid;
  uint32_t fid;
  uint32_t ln;
} VTBuf_Entry_DefScl;

/* BUF_ENTRY_TYPE__DefFileGroup */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint32_t gid;
  char     gname[1];
} VTBuf_Entry_DefFileGroup;

/* BUF_ENTRY_TYPE__DefFile */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint32_t fid;
  uint32_t gid;
  char     fname[1];
} VTBuf_Entry_DefFile;

/* BUF_ENTRY_TYPE__DefFunctionGroup */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint32_t rdid;
  char     rdesc[1];
} VTBuf_Entry_DefFunctionGroup;

/* BUF_ENTRY_TYPE__DefFunction */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint32_t rid;
  uint32_t rdid;
  uint32_t sid;
  char     rname[1];
} VTBuf_Entry_DefFunction;

/* BUF_ENTRY_TYPE__DefCollectiveOperation */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint32_t cid;
  uint32_t ctype;
  char     cname[1];
} VTBuf_Entry_DefCollectiveOperation;

/* BUF_ENTRY_TYPE__DefCounterGroup */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint32_t gid;
  char     gname[1];
} VTBuf_Entry_DefCounterGroup;

/* BUF_ENTRY_TYPE__DefCounter */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint32_t cid;
  uint32_t cprop;
  uint32_t gid;
  char     cunit[100];
  char     cname[1];
} VTBuf_Entry_DefCounter;

/* BUF_ENTRY_TYPE__DefProcessGroup */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint32_t  cid;
  char      grpn[100];
  uint32_t  grpc;
  uint32_t  grpv[1];
} VTBuf_Entry_DefProcessGroup;

/* BUF_ENTRY_TYPE__DefMarker */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint32_t mid;
  uint32_t mtype;
  char     mname[1];
} VTBuf_Entry_DefMarker;

/* BUF_ENTRY_TYPE__Enter / BUF_ENTRY_TYPE__Leave */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint64_t time;
  uint32_t rid;
  uint32_t sid;
  uint8_t  metc;
  uint64_t metv[1];
} VTBuf_Entry_EnterLeave;

/* BUF_ENTRY_TYPE__FileOperation */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint64_t time;
  uint64_t etime;
  uint32_t fid;
  uint64_t hid;
  uint32_t op;
  uint32_t bytes;
  uint32_t sid;
} VTBuf_Entry_FileOperation;

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint64_t time;
  uint64_t hid;
  uint32_t sid;
} VTBuf_Entry_BeginFileOperation;

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint64_t time;
  uint32_t fid;
  uint64_t hid;
  uint32_t op;
  uint32_t bytes;
  uint32_t sid;
} VTBuf_Entry_EndFileOperation;

/* BUF_ENTRY_TYPE__Counter */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint64_t time;
  uint32_t cid;
  uint64_t cval;
} VTBuf_Entry_Counter;

/* BUF_ENTRY_TYPE__Comment */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint64_t time;
  char     comment[1];
} VTBuf_Entry_Comment;

/* BUF_ENTRY_TYPE__Marker */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint64_t time;
  uint32_t mid;
  char     mtext[1];
} VTBuf_Entry_Marker;

/* BUF_ENTRY_TYPE__SendMsg / BUF_ENTRY_TYPE__RecvMsg */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint64_t time;
  uint32_t pid;
  uint32_t cid;
  uint32_t tag;
  uint32_t len;
  uint32_t sid;
} VTBuf_Entry_SendRecvMsg;

/* BUF_ENTRY_TYPE__CollectiveOperation */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint64_t time;
  uint64_t etime;
  uint32_t rid;
  uint32_t cid;
  uint32_t rpid;
  uint32_t sent;
  uint32_t recvd;
  uint32_t sid;
} VTBuf_Entry_CollectiveOperation;

/* BUF_ENTRY_TYPE__RMAPut / BUF_ENTRY_TYPE__RMAPutRE / BUF_ENTRY_TYPE__RMAGet */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint64_t time; 
  uint32_t opid; 
  uint32_t tpid; 
  uint32_t cid;
  uint32_t tag; 
  uint64_t len; 
  uint32_t sid;
}VTBuf_Entry_RMAPutGet;

/* BUF_ENTRY_TYPE__RMAEnd */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint64_t time;
  uint32_t rpid;
  uint32_t cid;
  uint32_t tag;
  uint32_t sid;
}VTBuf_Entry_RMAEnd;

/* BUF_ENTRY_TYPE__FunctionSummary */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

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
  uint32_t length;

  uint64_t time;
  uint32_t peer;
  uint32_t cid;
  uint32_t tag;
  uint64_t scnt;
  uint64_t rcnt;
  uint64_t sent;
  uint64_t recvd;
} VTBuf_Entry_MessageSummary;

/* BUF_ENTRY_TYPE__CollectiveOperationSummary */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint64_t time;
  uint32_t cid;
  uint32_t rid;
  uint64_t scnt;
  uint64_t rcnt;
  uint64_t sent;
  uint64_t recvd;
} VTBuf_Entry_CollectiveOperationSummary;

/* BUF_ENTRY_TYPE__FileOperationSummary */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

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

VTGen* VTGen_open(const char* tnameprefix, const char* tnamesuffix,
                  const char* tnameextern, uint32_t ptid, uint32_t tid,
                  size_t buffer_size)
{
  VTGen* gen;
  char* ldir = vt_env_ldir();
  char* gdir = vt_env_gdir();

  /* check write permissions */

  if (vt_env_ldir_check())
  {
    if (access(ldir, W_OK) != 0)
      vt_error_msg("Cannot access %s: Permission denied", ldir);
  }
  if (vt_env_gdir_check())
  {
    if (access(gdir, W_OK) != 0)
      vt_error_msg("Cannot access %s: Permission denied", gdir);
  }

  /* allocate VTGen record */
  gen = (VTGen*)calloc(1, sizeof(VTGen));
  if (gen == NULL)
    vt_error();

  /* store thread's name prefix*/
  gen->tnameprefix = tnameprefix;

  /* store thread's name suffix */
  gen->tnamesuffix = tnamesuffix;

  /* store thread's external name */
  gen->tnameextern = tnameextern;

  /* store parent thread id */
  gen->ptid = ptid;

  /* store thread id */
  gen->tid = tid;

  /* initialize flush counter */
  gen->flushcntr = vt_env_max_flushes();
  if( gen->flushcntr == 0 ) gen->flushcntr = -1;

  /* initialize first flush flag */
  gen->isfirstflush = 1;

  /* initialize trace mode flags */
  gen->mode = (uint8_t)vt_env_mode();

  /* initialize statistics properties */
  gen->sum_props = (uint8_t)vt_env_stat_props();

  /* allocate VTSum record */
  gen->sum = NULL;
  if (VTGEN_IS_SUM_ON(gen))
    gen->sum = VTSum_open(gen);

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

void VTGen_flush(VTGen* gen, uint8_t lastFlush,
		 uint64_t flushBTime, uint64_t* flushETime )
{
  uint8_t end_flush_marked = 0;
  buffer_t p;
  int i;

  /* intermediate flush and max. buffer flushes reached? */
  if(!lastFlush && gen->flushcntr == 0) return;

  /* Disable I/O tracing */
  VT_SUSPEND_IO_TRACING();

  /* mark begin of flush */
  if(!lastFlush)
    vt_enter_flush(&flushBTime);

  if(gen->isfirstflush)
  {
    /* set base name of the temporary files (basename includes local path
       but neither thread identifier nor suffix) */

    gen->fileprefix = (char*)calloc(VT_PATH_MAX + 1, sizeof(char));
    if(gen->fileprefix == NULL)
      vt_error();

    snprintf(gen->fileprefix, VT_PATH_MAX, "%s/%s.%lx.%u",
             vt_env_ldir(), vt_env_fprefix(),
             vt_pform_node_id(), getpid());

    /* open file manager for writer stream */
    gen->filemanager = OTF_FileManager_open(4);

    /* open writer stream */
    gen->filestream = OTF_WStream_open(gen->fileprefix, gen->tid+1,
                                       gen->filemanager);

    if( gen->filestream == NULL )
      vt_error_msg("Cannot open OTF writer stream [namestub %s id %x]",
                   gen->fileprefix, gen->tid+1); 
    else
      vt_cntl_msg(2, "Opened OTF writer stream [namestub %s id %x] for "
                     "generation [buffer %d bytes]", 
                     gen->fileprefix, gen->tid+1, gen->buf->size);

    /* set file compression */

    if( vt_env_compression() &&
        OTF_WStream_setCompression(gen->filestream,
                                   OTF_FILECOMPRESSION_COMPRESSED) )
    {
      gen->filecomp = OTF_FILECOMPRESSION_COMPRESSED;
    }
    else
    {
      gen->filecomp = OTF_FILECOMPRESSION_UNCOMPRESSED;
    }

    if( gen->tid == 0 )
    {
      char creator[100];
      uint64_t res = vt_pform_clockres();

      /* write creator record */

      snprintf(creator, sizeof(creator) - 1,
               "%s", PACKAGE_STRING);

      OTF_WStream_writeOtfVersion( gen->filestream );
      OTF_WStream_writeDefCreator( gen->filestream, creator );

      /* write timer resolution record */

      OTF_WStream_writeDefTimerResolution( gen->filestream, res );
    }

    /* write process definition record */
    {
      uint32_t ptid = 0;
      char pname[1024];

      /* use external name, if available */
      if(strlen(gen->tnameextern) > 0)
      {
        strncpy(pname, gen->tnameextern, sizeof(pname)-1);
      }
      else
      {
        if(gen->tid != 0)
          ptid = 65536 * gen->ptid + vt_my_trace + 1;

        snprintf(pname, sizeof(pname) - 1, "%s %d%s",
                 gen->tnameprefix, vt_my_trace, gen->tnamesuffix);
      }

      OTF_WStream_writeDefProcess(gen->filestream,
                                  65536 * gen->tid + vt_my_trace + 1,
                                  pname, ptid);
    }

    /* write process group definition record (node name) */
    {
       uint32_t pid = 65536 * gen->tid + vt_my_trace + 1;
       char pgname[100];

       snprintf(pgname, sizeof(pgname) - 1, "__NODE__ %s",
		vt_pform_node_name());

       OTF_WStream_writeDefProcessGroup(gen->filestream,
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

	 OTF_WStream_writeDefinitionComment(gen->filestream,
	   entry->comment);

	 break;
       }
       case BUF_ENTRY_TYPE__DefSclFile:
       {
	 VTBuf_Entry_DefSclFile* entry =
	   (VTBuf_Entry_DefSclFile*)p;

	 OTF_WStream_writeDefSclFile(gen->filestream,
	   entry->fid,
	   entry->fname);

	 break;
       }
       case BUF_ENTRY_TYPE__DefScl:
       {
	 VTBuf_Entry_DefScl* entry =
	   (VTBuf_Entry_DefScl*)p; 

	 OTF_WStream_writeDefScl(gen->filestream,
	   entry->sid,
	   entry->fid,
	   entry->ln);

	 break;
       }
       case BUF_ENTRY_TYPE__DefFileGroup:
       {
	 VTBuf_Entry_DefFileGroup* entry =
	   (VTBuf_Entry_DefFileGroup*)p;

	 OTF_WStream_writeDefFileGroup(gen->filestream,
	   entry->gid,
	   entry->gname);

	 break;
       }
       case BUF_ENTRY_TYPE__DefFile:
       {
	 VTBuf_Entry_DefFile* entry =
	   (VTBuf_Entry_DefFile*)p;

	 OTF_WStream_writeDefFile(gen->filestream,
	   entry->fid,
	   entry->fname,
	   entry->gid);

	 break;
       }
       case BUF_ENTRY_TYPE__DefFunctionGroup:
       {
	 VTBuf_Entry_DefFunctionGroup* entry =
	   (VTBuf_Entry_DefFunctionGroup*)p;

	 OTF_WStream_writeDefFunctionGroup(gen->filestream,
	   entry->rdid,
	   entry->rdesc);

	 break;
       }
       case BUF_ENTRY_TYPE__DefFunction:
       {
	 VTBuf_Entry_DefFunction* entry =
	   (VTBuf_Entry_DefFunction*)p;

	 OTF_WStream_writeDefFunction(gen->filestream,
	   entry->rid,
	   entry->rname,
	   entry->rdid,
	   entry->sid);

	 break;
       }
       case BUF_ENTRY_TYPE__DefCollectiveOperation:
       {
	 VTBuf_Entry_DefCollectiveOperation* entry =
	   (VTBuf_Entry_DefCollectiveOperation*)p;

	 OTF_WStream_writeDefCollectiveOperation(gen->filestream,
	   entry->cid,
	   entry->cname,
	   entry->ctype);

	 break;
       }
       case BUF_ENTRY_TYPE__DefCounterGroup:
       {
	 VTBuf_Entry_DefCounterGroup* entry =
	   (VTBuf_Entry_DefCounterGroup*)p;

	 OTF_WStream_writeDefCounterGroup(gen->filestream,
	   entry->gid,
	   entry->gname);

	 break;
       }
       case BUF_ENTRY_TYPE__DefCounter:
       {
	 VTBuf_Entry_DefCounter* entry =
	   (VTBuf_Entry_DefCounter*)p;  

	 OTF_WStream_writeDefCounter(gen->filestream,
	   entry->cid,
	   entry->cname,
	   entry->cprop,
	   entry->gid,
	   entry->cunit);

	 break;
       }
       case BUF_ENTRY_TYPE__DefProcessGroup:
       {
	 VTBuf_Entry_DefProcessGroup* entry =
	   (VTBuf_Entry_DefProcessGroup*)p;

	 OTF_WStream_writeDefProcessGroup(gen->filestream,
	   entry->cid,
	   entry->grpn,
	   entry->grpc,
	   entry->grpv);

	 break;
       }
       case BUF_ENTRY_TYPE__DefMarker:
       {
	 VTBuf_Entry_DefMarker* entry =
	   (VTBuf_Entry_DefMarker*)p;

	 OTF_WStream_writeDefMarker(gen->filestream,
	   entry->mid,
	   entry->mname,
	   entry->mtype);

	 break;
       }
       case BUF_ENTRY_TYPE__Enter:
       {
	 VTBuf_Entry_EnterLeave* entry =
	   (VTBuf_Entry_EnterLeave*)p;  

	 OTF_WStream_writeEnter(gen->filestream,
	   entry->time,
	   entry->rid,
	   65536 * gen->tid + vt_my_trace + 1,
	   entry->sid);

	 for(i = 0; i < entry->metc; i++)
	 {
	   OTF_WStream_writeCounter(gen->filestream,
	     entry->time,
	     65536 * gen->tid + vt_my_trace + 1,
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
	   OTF_WStream_writeCounter(gen->filestream,
	     entry->time,
	     65536 * gen->tid + vt_my_trace + 1,
	     i+1,
	     (uint64_t)entry->metv[i]);
	 }

	 OTF_WStream_writeLeave(gen->filestream,
	   entry->time,
	   entry->rid,
	   65536 * gen->tid + vt_my_trace + 1,
	   entry->sid);

	 break;
       }
       case BUF_ENTRY_TYPE__FileOperation:
       {
	 VTBuf_Entry_FileOperation* entry =
	   (VTBuf_Entry_FileOperation*)p;

	 OTF_WStream_writeFileOperation(gen->filestream,
	   entry->time,
	   entry->fid,
	   65536 * gen->tid + vt_my_trace + 1,
	   entry->hid,
	   entry->op,
	   entry->bytes,
	   entry->etime - entry->time,
	   entry->sid);

	 break;
       }
       case BUF_ENTRY_TYPE__BeginFileOperation:
       {
	 VTBuf_Entry_BeginFileOperation* entry =
	   (VTBuf_Entry_BeginFileOperation*)p;

	 OTF_WStream_writeBeginFileOperation(gen->filestream,
	   entry->time,
	   65536 * gen->tid + vt_my_trace + 1,
	   entry->hid,
	   entry->sid);

	 break;
       }
       case BUF_ENTRY_TYPE__EndFileOperation:
       {
	 VTBuf_Entry_EndFileOperation* entry =
	   (VTBuf_Entry_EndFileOperation*)p;

	 OTF_WStream_writeEndFileOperation(gen->filestream,
	   entry->time,
	   65536 * gen->tid + vt_my_trace + 1,
	   entry->fid,
	   entry->hid,
	   entry->op,
	   entry->bytes,
	   entry->sid);

	 break;
       }
       case BUF_ENTRY_TYPE__Counter:
       {
	 VTBuf_Entry_Counter* entry =
	   (VTBuf_Entry_Counter*)p;  

	 OTF_WStream_writeCounter(gen->filestream,
	   entry->time,
	   65536 * gen->tid + vt_my_trace + 1,
	   entry->cid, entry->cval);

	 break;
       }
       case BUF_ENTRY_TYPE__Comment:
       {
	 VTBuf_Entry_Comment* entry =
	   (VTBuf_Entry_Comment*)p;

	 OTF_WStream_writeEventComment(gen->filestream,
	   entry->time,
	   65536 * gen->tid + vt_my_trace + 1,
	   entry->comment);

	 break;
       }
       case BUF_ENTRY_TYPE__Marker:
       {
	 VTBuf_Entry_Marker* entry =
	   (VTBuf_Entry_Marker*)p;

	 OTF_WStream_writeMarker(gen->filestream,
	   entry->time,
	   65536 * gen->tid + vt_my_trace + 1,
	   entry->mid, entry->mtext);

	 break;
       }
       case BUF_ENTRY_TYPE__SendMsg:
       {
	 VTBuf_Entry_SendRecvMsg* entry =
	   (VTBuf_Entry_SendRecvMsg*)p;

	 OTF_WStream_writeSendMsg(gen->filestream,
	   entry->time,
	   65536 * gen->tid + vt_my_trace + 1,
	   entry->pid,
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

	 OTF_WStream_writeRecvMsg(gen->filestream,
	   entry->time,
	   65536 * gen->tid + vt_my_trace + 1,
	   entry->pid,
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

	 OTF_WStream_writeCollectiveOperation(gen->filestream,
	   entry->time,
	   65536 * gen->tid + vt_my_trace + 1,
	   entry->rid,
	   entry->cid,
	   entry->rpid,
	   entry->sent,
	   entry->recvd,
	   entry->etime - entry->time,
	   entry->sid);

	 break;
       }
       case BUF_ENTRY_TYPE__RMAPut:
        { 
          VTBuf_Entry_RMAPutGet* entry = (VTBuf_Entry_RMAPutGet*)p;

          OTF_WStream_writeRMAPut(gen->filestream,
                                  entry->time,
                                  65536 * gen->tid + vt_my_trace + 1,
                                  entry->opid,
                                  entry->tpid,
                                  entry->cid,
                                  entry->tag,
                                  entry->len,
                                  entry->sid);
          break;
        }
       case BUF_ENTRY_TYPE__RMAPutRE:
        {
          VTBuf_Entry_RMAPutGet* entry = (VTBuf_Entry_RMAPutGet*)p;

          OTF_WStream_writeRMAPutRemoteEnd(gen->filestream,
                                            entry->time,
                                            65536 * gen->tid + vt_my_trace + 1,
                                            entry->opid,
                                            entry->tpid,
                                            entry->cid,
                                            entry->tag,
                                            entry->len,
                                            entry->sid);
          break;
        }
       case BUF_ENTRY_TYPE__RMAGet:
        {
          VTBuf_Entry_RMAPutGet* entry = (VTBuf_Entry_RMAPutGet*)p;

          OTF_WStream_writeRMAGet(gen->filestream,
                                  entry->time,
                                  65536 * gen->tid + vt_my_trace + 1,
                                  entry->opid,
                                  entry->tpid,
                                  entry->cid,
                                  entry->tag,
                                  entry->len,
                                  entry->sid);
          break;
        }
       case BUF_ENTRY_TYPE__RMAEnd:
        {
          VTBuf_Entry_RMAEnd* entry = (VTBuf_Entry_RMAEnd*)p;

          OTF_WStream_writeRMAEnd(gen->filestream,
                                    entry->time,
                                    65536 * gen->tid + vt_my_trace + 1,
                                    entry->rpid,
                                    entry->cid,
                                    entry->tag,
                                    entry->sid);
          break;
        }
       case BUF_ENTRY_TYPE__FunctionSummary:
       {
	 VTBuf_Entry_FunctionSummary* entry =
	   (VTBuf_Entry_FunctionSummary*)p;

	 OTF_WStream_writeFunctionSummary(gen->filestream,
	   entry->time,
	   entry->rid,
	   65536 * gen->tid + vt_my_trace + 1,
	   entry->cnt,
	   entry->excl,
	   entry->incl);

	 break;
       }
       case BUF_ENTRY_TYPE__MessageSummary:
       {
	 VTBuf_Entry_MessageSummary* entry =
	   (VTBuf_Entry_MessageSummary*)p;

	 OTF_WStream_writeMessageSummary(gen->filestream,
	   entry->time,
	   65536 * gen->tid + vt_my_trace + 1,
	   entry->peer,
	   entry->cid,
	   entry->tag,
	   entry->scnt,
	   entry->rcnt,
	   entry->sent,
	   entry->recvd);

	 break;
       }
       case BUF_ENTRY_TYPE__CollectiveOperationSummary:
       {
	 VTBuf_Entry_CollectiveOperationSummary* entry =
	   (VTBuf_Entry_CollectiveOperationSummary*)p;

	 OTF_WStream_writeCollopSummary(gen->filestream,
					entry->time,
					65536 * gen->tid + vt_my_trace + 1,
					entry->cid,
					entry->rid,
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

	 OTF_WStream_writeFileOperationSummary(gen->filestream,
	   entry->time,
	   entry->fid,
	   65536 * gen->tid + vt_my_trace + 1,
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
	 vt_assert(0);
       }
     }

     /* last buffer entry and end flush not marked ? */
     if(!end_flush_marked &&
	p + ((VTBuf_Entry_Base*)p)->length >= gen->buf->pos)
     {
       /* mark end of flush, if it's not the last (invisible) flush and 
          max flushes not reached */
       if(!lastFlush && gen->flushcntr > 1)
       {
	 uint64_t flush_etime = vt_pform_wtime();
	 vt_exit_flush(&flush_etime);
	 if( flushETime != NULL ) *flushETime = flush_etime;
       }

       end_flush_marked = 1;
     }

     p += ((VTBuf_Entry_Base*)p)->length;
  }

  /* if it's the last flush write event/summary comment record, in order that
     all event/summary files will exist */
  if(lastFlush)
  {
    if(VTGEN_IS_TRACE_ON(gen))
    {
      OTF_WStream_writeEventComment(gen->filestream,
        vt_pform_wtime(),
        65536 * gen->tid + vt_my_trace + 1,
        "");
    }
    else /* VTGEN_IS_SUM_ON(gen) */
    {
      OTF_WStream_writeSummaryComment(gen->filestream,
        vt_pform_wtime(),
        65536 * gen->tid + vt_my_trace + 1,
        "");
    }
  }

  /* reset buffer */
  gen->buf->pos = gen->buf->mem;

  vt_cntl_msg(2, "Flushed OTF writer stream [namestub %s id %x]",
	      gen->fileprefix, gen->tid+1);

  /* decrement flush counter */
  if(gen->flushcntr > 0) gen->flushcntr--;

  /* switch tracing off, if number of max flushes reached */
  if(!lastFlush && gen->flushcntr == 0)
  {
    int max_flushes = vt_env_max_flushes();
    vt_cntl_msg(1, "Maximum number of buffer flushes reached "
		"(VT_MAX_FLUSHES=%d)", max_flushes);
    vt_trace_off(1, 1);
    vt_def_comment("__VT_COMMENT__ WARNING: This trace is "
		   "incomplete, because the maximum number of "
		   "buffer flushes was reached. "
		   "(VT_MAX_FLUSHES=%d)", max_flushes);
  }

  /* Enable I/O tracing again */
  VT_RESUME_IO_TRACING();
}

void VTGen_close(VTGen* gen)
{
  /* close summary */
  if (VTGEN_IS_SUM_ON(gen))
    VTSum_close(gen->sum);

  /* flush buffer if necessary */
  VTGen_flush(gen, 1, 0, NULL);

  if(gen->fileprefix)
  {
    /* close writer stream */
    OTF_WStream_close(gen->filestream);

    /* close file manager of writer stream */
    OTF_FileManager_close(gen->filemanager);

    vt_cntl_msg(2, "Closed OTF writer stream [namestub %s id %x]",
                gen->fileprefix, gen->tid+1);
  }

  /* free buffer memory */
  free(gen->buf->mem); 

  /* free buffer record */
  free(gen->buf); 
}

void VTGen_delete(VTGen* gen)
{
  if (gen->fileprefix)
  {
    char* tmp_namev[5];
    char* global_name;
    uint32_t global_name_len;
    char* suffix;

    char* gdir = vt_env_gdir();
    char* fprefix = vt_env_fprefix();
    int do_clean = vt_env_do_clean();
    int do_rename = do_clean;

    uint8_t i;

    /* determine (local) files for removal */
    tmp_namev[0] =
      OTF_getFilename(gen->fileprefix, gen->tid+1,
                      OTF_FILETYPE_DEF | gen->filecomp,
                      0, NULL);
    tmp_namev[1] =
      OTF_getFilename(gen->fileprefix, gen->tid+1,
                      OTF_FILETYPE_EVENT | gen->filecomp,
                      0, NULL);
    tmp_namev[2] =
      OTF_getFilename(gen->fileprefix, gen->tid+1,
                      OTF_FILETYPE_STATS | gen->filecomp,
                      0, NULL);
    tmp_namev[3] =
      OTF_getFilename(gen->fileprefix, gen->tid+1,
                      OTF_FILETYPE_MARKER | gen->filecomp,
                      0, NULL);
    tmp_namev[4] = NULL;

    i = 0;
    while(tmp_namev[i] != NULL)
    {
      /* local temp. trace file exists? */
      if (access(tmp_namev[i], R_OK) != 0)
      {
        free(tmp_namev[i++]);
        continue;
      }

      /* determine file suffix */
      suffix = strchr(tmp_namev[i]+strlen(gen->fileprefix)+1, '.');

      /* build global file name */
      global_name_len = strlen(gdir) + strlen(fprefix) + 32;
      global_name = (char*)calloc(global_name_len+1, sizeof(char));

      if (vt_my_funique > 0)
      {
        snprintf(global_name, global_name_len, "%s/%s_%u.%x%s",
                 gdir, fprefix, vt_my_funique, 65536*gen->tid+(vt_my_trace+1),
                 suffix);
      }
      else
      {
        snprintf(global_name, global_name_len, "%s/%s.%x%s",
                 gdir, fprefix, 65536*gen->tid+(vt_my_trace+1), suffix);
      }

      /* rename file, if possible */
      if (do_rename)
      {
        if (rename(tmp_namev[i], global_name) == 0)
        {
          vt_cntl_msg(2, "Moved trace file %s to %s", tmp_namev[i], global_name);
        }
        else
        {
          do_rename = 0;
          free(global_name);
          continue;
        }
      }
      /* otherwise, copy file */
      else
      {
        size_t bytes_read;
        void *buffer;
        size_t buflen;
        FILE* infile;
        FILE* outfile;

        /* allocate buffer */
        buflen = vt_env_copy_bsize();
        buffer = malloc( buflen );
        if( !buffer )
          vt_error_msg( "Cannot allocate %u bytes for copy buffer", buflen );

        /* open files */
        if ((infile = fopen(tmp_namev[i], "rb")) == NULL )
          vt_error_msg("Cannot open trace file %s for reading", tmp_namev[i]);
        if ((outfile = fopen(global_name, "wb")) == NULL)
          vt_error_msg("Cannot open trace file %s for writing", global_name); 

        /* copy file */
        while((bytes_read = fread(buffer, 1, buflen, infile)))
        {
          if( bytes_read > fwrite(buffer, 1, bytes_read, outfile) )
          {
            fclose( infile );
            fclose( outfile );
            free( buffer );
            vt_error_msg("Failed to write to file %s", global_name);
          }
        }

        /* close files */
        fclose(infile);
        fclose(outfile);

        /* free buffer */
        free( buffer );

        vt_cntl_msg(2, "Copied trace file %s to %s", tmp_namev[i], global_name);

        /* remove local temp. trace file, if desired */
        if (do_clean)
        {
          if (remove(tmp_namev[i]) == 0 )
            vt_cntl_msg(2, "Removed trace file %s", tmp_namev[i]);
          else
            vt_error_msg("Cannot remove trace file %s", tmp_namev[i]);
        }
        else
        {
          vt_cntl_msg(2, "*Left* trace file %s", tmp_namev[i]);
        }
      }

      free(global_name);
      free(tmp_namev[i]);

      i++;
    }

    free(gen->fileprefix);
  }

  /* delete sum record */
  if (VTGEN_IS_SUM_ON(gen)) VTSum_delete(gen->sum);

  /* free gen record */
  free(gen);
}

void VTGen_destroy(VTGen* gen)
{
  if(gen->fileprefix)
  {
    /* close writer stream */
    OTF_WStream_close(gen->filestream);

    /* close file manager of writer stream */
    OTF_FileManager_close(gen->filemanager);
  }

  /* destroy sum record */
  if (VTGEN_IS_SUM_ON(gen)) VTSum_destroy(gen->sum);

  /* free buffer memory */
  free(gen->buf->mem); 

  /* free buffer record */
  free(gen->buf); 

  /* free gen record */
  free(gen); 
}


uint8_t VTGen_get_buflevel(VTGen* gen)
{
  VTGEN_CHECK(gen);

  return (uint8_t)(((gen->buf->pos - gen->buf->mem) * 100) / gen->buf->size);
}


/* -- Writing trace records -- */


/* - Definition records - */

void VTGen_write_DEFINITION_COMMENT(VTGen* gen, const char* comment)
{
  VTBuf_Entry_DefinitionComment* new_entry;

  uint32_t length =
    VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_DefinitionComment) +
                        (strlen(comment) * sizeof(char))));

  VTGEN_CHECK(gen);

  VTGEN_ALLOC_DEF(gen, length);

  new_entry = ((VTBuf_Entry_DefinitionComment*)gen->buf->pos);

  new_entry->type    = BUF_ENTRY_TYPE__DefinitionComment;
  new_entry->length  = length;
  strcpy(new_entry->comment, comment);

  VTGEN_JUMP(gen, length);
}

void VTGen_write_DEF_SCL_FILE(VTGen* gen,
			       uint32_t fid,
			       const char* fname)
{
  VTBuf_Entry_DefSclFile* new_entry;

  uint32_t length =
    VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_DefSclFile) +
                        (strlen(fname) * sizeof(char))));

  VTGEN_CHECK(gen);

  VTGEN_ALLOC_DEF(gen, length);

  new_entry = ((VTBuf_Entry_DefSclFile*)gen->buf->pos);

  new_entry->type   = BUF_ENTRY_TYPE__DefSclFile;
  new_entry->length = length;
  new_entry->fid    = fid;
  strcpy(new_entry->fname, fname);

  VTGEN_JUMP(gen, length);
}

void VTGen_write_DEF_SCL(VTGen* gen,
			  uint32_t sid,
			  uint32_t fid,
			  uint32_t ln)
{
  VTBuf_Entry_DefScl* new_entry;

  uint32_t length =
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
}

void VTGen_write_DEF_FILE_GROUP(VTGen* gen,
				 uint32_t gid,
				 const char* gname)
{
  VTBuf_Entry_DefFileGroup* new_entry;

  uint32_t length =
    VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_DefFileGroup) +
                        (strlen(gname) * sizeof(char))));

  VTGEN_CHECK(gen);

  VTGEN_ALLOC_DEF(gen, length);

  new_entry = ((VTBuf_Entry_DefFileGroup*)gen->buf->pos);

  new_entry->type   = BUF_ENTRY_TYPE__DefFileGroup;
  new_entry->length = length;
  new_entry->gid    = gid;
  strcpy(new_entry->gname, gname);

  VTGEN_JUMP(gen, length);
}

void VTGen_write_DEF_FILE(VTGen* gen,
			   uint32_t fid,
			   const char* fname,
			   uint32_t gid)
{
  VTBuf_Entry_DefFile* new_entry;

  uint32_t length =
    VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_DefFile) +
                        (strlen(fname) * sizeof(char))));

  VTGEN_CHECK(gen);

  VTGEN_ALLOC_DEF(gen, length);

  new_entry = ((VTBuf_Entry_DefFile*)gen->buf->pos);

  new_entry->type   = BUF_ENTRY_TYPE__DefFile;
  new_entry->length = length;
  new_entry->fid    = fid;
  new_entry->gid    = gid;
  strcpy(new_entry->fname, fname);

  VTGEN_JUMP(gen, length);
}

void VTGen_write_DEF_FUNCTION_GROUP(VTGen* gen,
				     uint32_t rdid,
				     const char* rdesc)
{
  VTBuf_Entry_DefFunctionGroup* new_entry;

  uint32_t length =
    VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_DefFunctionGroup) +
                        (strlen(rdesc) * sizeof(char))));

  VTGEN_CHECK(gen);

  VTGEN_ALLOC_DEF(gen, length);

  new_entry = ((VTBuf_Entry_DefFunctionGroup*)gen->buf->pos);

  new_entry->type   = BUF_ENTRY_TYPE__DefFunctionGroup;
  new_entry->length = length;
  new_entry->rdid   = rdid;
  strcpy(new_entry->rdesc, rdesc);

  VTGEN_JUMP(gen, length);
}

void VTGen_write_DEF_FUNCTION(VTGen* gen,
			       uint32_t rid,
			       const char* rname,
			       uint32_t rdid,
			       uint32_t sid)
{
  VTBuf_Entry_DefFunction* new_entry;

  uint32_t length =
    VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_DefFunction) +
                        (strlen(rname) * sizeof(char))));

  VTGEN_CHECK(gen);

  VTGEN_ALLOC_DEF(gen, length);

  new_entry = ((VTBuf_Entry_DefFunction*)gen->buf->pos);

  new_entry->type   = BUF_ENTRY_TYPE__DefFunction;
  new_entry->length = length;
  new_entry->rid    = rid;
  new_entry->rdid   = rdid;
  new_entry->sid    = sid;
  strcpy(new_entry->rname, rname);

  VTGEN_JUMP(gen, length);
}

void VTGen_write_DEF_COLLECTIVE_OPERATION(VTGen* gen,
					   uint32_t cid,
					   const char* cname,
					   uint32_t ctype)
{
  VTBuf_Entry_DefCollectiveOperation* new_entry;

  uint32_t length =
    VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_DefCollectiveOperation) +
                        (strlen(cname) * sizeof(char))));

  VTGEN_CHECK(gen);

  VTGEN_ALLOC_DEF(gen, length);

  new_entry = ((VTBuf_Entry_DefCollectiveOperation*)gen->buf->pos);

  new_entry->type   = BUF_ENTRY_TYPE__DefCollectiveOperation;
  new_entry->length = length;
  new_entry->cid    = cid;
  new_entry->ctype  = ctype;
  strcpy(new_entry->cname, cname);

  VTGEN_JUMP(gen, length);
}

void VTGen_write_DEF_COUNTER_GROUP(VTGen* gen,
				    uint32_t gid,
				    const char* gname)
{
  VTBuf_Entry_DefCounterGroup* new_entry;

  uint32_t length =
    VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_DefCounterGroup) +
                        (strlen(gname) * sizeof(char))));

  VTGEN_CHECK(gen);

  VTGEN_ALLOC_DEF(gen, length);

  new_entry = ((VTBuf_Entry_DefCounterGroup*)gen->buf->pos);

  new_entry->type   = BUF_ENTRY_TYPE__DefCounterGroup;
  new_entry->length = length;
  new_entry->gid    = gid;
  strcpy(new_entry->gname, gname);

  VTGEN_JUMP(gen, length);
}

void VTGen_write_DEF_COUNTER(VTGen* gen,
			      uint32_t cid,
			      const char* cname,
			      uint32_t cprop,
			      uint32_t gid,
			      const char* cunit)
{
  VTBuf_Entry_DefCounter* new_entry;

  uint32_t length =
    VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_DefCounter) +
                        (strlen(cname) * sizeof(char))));

  VTGEN_CHECK(gen);

  VTGEN_ALLOC_DEF(gen, length);

  new_entry = ((VTBuf_Entry_DefCounter*)gen->buf->pos);

  new_entry->type   = BUF_ENTRY_TYPE__DefCounter;
  new_entry->length = length;
  new_entry->cid    = cid;
  new_entry->cprop  = cprop;
  new_entry->gid    = gid;
  strncpy(new_entry->cunit, cunit, sizeof(new_entry->cunit)-1);
  new_entry->cunit[sizeof(new_entry->cunit)-1] = '\0';
  strcpy(new_entry->cname, cname);

  VTGEN_JUMP(gen, length);
}

void VTGen_write_DEF_PROCESS_GROUP(VTGen* gen,
				    uint32_t cid,
				    const char* grpn,
				    uint32_t grpc,
				    uint32_t grpv[])
{
  VTBuf_Entry_DefProcessGroup* new_entry;

  uint32_t length =
    VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_DefProcessGroup) +
                        (grpc > 0 ? (grpc - 1) * sizeof(uint32_t) : 0 )));

  VTGEN_CHECK(gen);

  VTGEN_ALLOC_DEF(gen, length);

  new_entry = ((VTBuf_Entry_DefProcessGroup*)gen->buf->pos);

  new_entry->type   = BUF_ENTRY_TYPE__DefProcessGroup;
  new_entry->length = length;
  new_entry->cid    = cid;
  strncpy(new_entry->grpn, grpn, sizeof(new_entry->grpn)-1);
  new_entry->grpn[sizeof(new_entry->grpn)-1] = '\0';
  new_entry->grpc   = grpc;
  if( grpc > 0 )
    memcpy(new_entry->grpv, grpv, grpc * sizeof(uint32_t));

  VTGEN_JUMP(gen, length);
}

/* -- Marker -- */

void VTGen_write_DEF_MARKER(VTGen* gen,
			     uint32_t mid,
			     const char* mname,
			     uint32_t mtype )
{
  VTBuf_Entry_DefMarker* new_entry;

  uint32_t length =
    VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_DefMarker) +
                        (strlen(mname) * sizeof(char))));

  VTGEN_CHECK(gen);

  VTGEN_ALLOC_DEF(gen, length);

  new_entry = ((VTBuf_Entry_DefMarker*)gen->buf->pos);

  new_entry->type   = BUF_ENTRY_TYPE__DefMarker;
  new_entry->length = length;
  new_entry->mid    = mid;
  new_entry->mtype  = mtype;
  strcpy(new_entry->mname, mname);

  VTGEN_JUMP(gen, length);
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

    uint32_t length =
      VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_EnterLeave) +
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

  if (VTGEN_IS_SUM_PROP_ON(gen, VT_SUM_PROP_FUNC))
    VTSum_enter(gen->sum, time, rid);
}

void VTGen_write_LEAVE(VTGen* gen, uint64_t* time, uint32_t rid, uint32_t sid,
       uint8_t metc, uint64_t metv[])
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_EnterLeave* new_entry;

    uint32_t length =
      VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_EnterLeave) +
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

  if (VTGEN_IS_SUM_PROP_ON(gen, VT_SUM_PROP_FUNC))
    VTSum_exit(gen->sum, time, rid);
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

    uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_FileOperation));

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
    new_entry->op     = (op == VT_IOOP_DUP) ? OTF_FILEOP_OPEN : op;
    new_entry->bytes  = bytes;
    new_entry->sid    = sid;

    VTGEN_JUMP(gen, length);
  }

  if (VTGEN_IS_SUM_PROP_ON(gen, VT_SUM_PROP_FILEOP))
  {
    switch( op )
    {
      case OTF_FILEOP_OPEN:
      {
	VTSum_fileop_open(gen->sum, time, fid);
	break;
      }
      case OTF_FILEOP_CLOSE:
      {
	VTSum_fileop_close(gen->sum, time, fid);
	break;
      }
      case OTF_FILEOP_READ:
      {
	VTSum_fileop_read(gen->sum, time, fid, bytes);
	break;
      }
      case OTF_FILEOP_WRITE:
      {
	VTSum_fileop_write(gen->sum, time, fid, bytes);
	break;
      }
      case OTF_FILEOP_SEEK:
      {
        VTSum_fileop_seek(gen->sum, time, fid);
	break;
      }
    }
  }
}

void VTGen_write_BEGIN_FILE_OPERATION(VTGen* gen, uint64_t* time,
       uint64_t hid, uint32_t sid)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_BeginFileOperation* new_entry;

    uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_BeginFileOperation));

    VTGEN_ALLOC_EVENT(gen, length);

    new_entry = ((VTBuf_Entry_BeginFileOperation*)gen->buf->pos);

    new_entry->type   = BUF_ENTRY_TYPE__BeginFileOperation;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->hid    = hid;
    new_entry->sid    = sid;

    VTGEN_JUMP(gen, length);
  }
}

void VTGen_write_END_FILE_OPERATION(VTGen* gen, uint64_t* time,
       uint32_t fid, uint64_t hid, uint32_t op, uint64_t bytes,
       uint32_t sid)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_EndFileOperation* new_entry;

    uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_EndFileOperation));

    VTGEN_ALLOC_EVENT(gen, length);

    new_entry = ((VTBuf_Entry_EndFileOperation*)gen->buf->pos);

    new_entry->type   = BUF_ENTRY_TYPE__EndFileOperation;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->fid    = fid;
    new_entry->hid    = hid;
    new_entry->op     = op;
    new_entry->bytes  = bytes;
    new_entry->sid    = sid;

    VTGEN_JUMP(gen, length);
  }

/* TODO: Summary records for begin/end I/O records */
#if 0
  if (VTGEN_IS_SUM_PROP_ON(gen, VT_SUM_PROP_FILEOP))
  {
    switch( op )
    {
      case OTF_FILEOP_OPEN:
      {
	VTSum_fileop_open(gen->sum, time, fid);
	break;
      }
      case OTF_FILEOP_CLOSE:
      {
	VTSum_fileop_close(gen->sum, time, fid);
	break;
      }
      case OTF_FILEOP_READ:
      {
	VTSum_fileop_read(gen->sum, time, fid, bytes);
	break;
      }
      case OTF_FILEOP_WRITE:
      {
	VTSum_fileop_write(gen->sum, time, fid, bytes);
	break;
      }
      case OTF_FILEOP_SEEK:
      {
        VTSum_fileop_seek(gen->sum, time, fid);
	break;
      }
    }
  }
#endif
}

/* -- Counter -- */

void VTGen_write_COUNTER(VTGen* gen, uint64_t* time, uint32_t cid,
       uint64_t cval)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_Counter* new_entry;

    uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_Counter));

    VTGEN_ALLOC_EVENT(gen, length);

    new_entry = ((VTBuf_Entry_Counter*)gen->buf->pos);

    new_entry->type   = BUF_ENTRY_TYPE__Counter;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->cid    = cid;
    new_entry->cval   = cval;

    VTGEN_JUMP(gen, length);
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

    uint32_t length =
      VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_Comment) +
                          (strlen(comment) * sizeof(char))));

    VTGEN_ALLOC_EVENT(gen, length);

    new_entry = ((VTBuf_Entry_Comment*)gen->buf->pos);

    new_entry->type    = BUF_ENTRY_TYPE__Comment;
    new_entry->length  = length;
    new_entry->time    = *time;
    strcpy(new_entry->comment, comment);

    VTGEN_JUMP(gen, length);
  }
}

/* -- Marker -- */

void VTGen_write_MARKER(VTGen* gen, uint64_t* time, uint32_t mid,
			const char* mtext)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_Marker* new_entry;

    uint32_t length =
      VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_Marker) +
                          (strlen(mtext) * sizeof(char))));

    VTGEN_ALLOC_EVENT(gen, length);

    new_entry = ((VTBuf_Entry_Marker*)gen->buf->pos);

    new_entry->type    = BUF_ENTRY_TYPE__Marker;
    new_entry->length  = length;
    new_entry->time    = *time;
    new_entry->mid     = mid;
    strcpy(new_entry->mtext, mtext);

    VTGEN_JUMP(gen, length);
  }
}

/* -- MPI-1 -- */

void VTGen_write_SEND_MSG(VTGen* gen, uint64_t* time, uint32_t pid,
       uint32_t cid, uint32_t tag, uint32_t sent, uint32_t sid)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_SendRecvMsg* new_entry;

    uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_SendRecvMsg));

    VTGEN_ALLOC_EVENT(gen, length);

    new_entry = ((VTBuf_Entry_SendRecvMsg*)gen->buf->pos);

    new_entry->type   = BUF_ENTRY_TYPE__SendMsg;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->pid    = pid;
    new_entry->cid    = cid;
    new_entry->tag    = tag;
    new_entry->len    = sent;
    new_entry->sid    = sid;
      
    VTGEN_JUMP(gen, length);
  }

  if (VTGEN_IS_SUM_PROP_ON(gen, VT_SUM_PROP_MSG))
    VTSum_msg_send(gen->sum, time, pid, cid, tag, (uint64_t)sent);
}

void VTGen_write_RECV_MSG(VTGen* gen, uint64_t* time, uint32_t pid,
       uint32_t cid, uint32_t tag, uint32_t recvd, uint32_t sid)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_SendRecvMsg* new_entry;

    uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_SendRecvMsg));

    VTGEN_ALLOC_EVENT(gen, length);

    new_entry = ((VTBuf_Entry_SendRecvMsg*)gen->buf->pos);

    new_entry->type   = BUF_ENTRY_TYPE__RecvMsg;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->pid    = pid;
    new_entry->cid    = cid;
    new_entry->tag    = tag;
    new_entry->len    = recvd;
    new_entry->sid    = sid;
    
    VTGEN_JUMP(gen, length);
  }

  if (VTGEN_IS_SUM_PROP_ON(gen, VT_SUM_PROP_MSG))
    VTSum_msg_recv(gen->sum, time, pid, cid, tag, (uint64_t)recvd);
}

void VTGen_write_COLLECTIVE_OPERATION(VTGen* gen, uint64_t* time,
       uint64_t* etime, uint32_t rid, uint32_t cid, uint32_t rpid,
       uint32_t sent, uint32_t recvd, uint32_t sid)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_CollectiveOperation* new_entry;

    uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_CollectiveOperation));

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
  }

  if (VTGEN_IS_SUM_PROP_ON(gen, VT_SUM_PROP_COLLOP) && (sent > 0 || recvd > 0))
    VTSum_collop(gen->sum, time, rid, cid, (uint64_t)sent, (uint64_t)recvd);
}

/* -- RMA - 1sided --*/

void VTGen_write_RMA_PUT(VTGen* gen, uint64_t* time, uint32_t opid, 
        uint32_t tpid, uint32_t cid, uint32_t tag, uint32_t len, uint32_t sid)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_RMAPutGet* new_entry;

    uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_RMAPutGet));

    VTGEN_ALLOC_EVENT(gen, length);

    new_entry = ((VTBuf_Entry_RMAPutGet*)gen->buf->pos);

    new_entry->type   = BUF_ENTRY_TYPE__RMAPut;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->opid   = opid;
    new_entry->tpid   = tpid;
    new_entry->cid    = cid;
    new_entry->tag    = tag;
    new_entry->len    = len;
    new_entry->sid    = sid;
      
    VTGEN_JUMP(gen, length);
  }

/*
  if (VTGEN_IS_SUM_PROP_ON(gen, VT_SUM_PROP_MSG))
    VTSum_msg_send(gen->sum, time, dpid, cid, tag, (uint64_t)sent);
*/
}

void VTGen_write_RMA_PUTRE(VTGen* gen, uint64_t* time, uint32_t opid, 
        uint32_t tpid, uint32_t cid, uint32_t tag, uint64_t len, uint32_t sid)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_RMAPutGet* new_entry;

    uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_RMAPutGet));

    VTGEN_ALLOC_EVENT(gen, length);

    new_entry = ((VTBuf_Entry_RMAPutGet*)gen->buf->pos);

    new_entry->type   = BUF_ENTRY_TYPE__RMAPutRE;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->opid   = opid;
    new_entry->tpid   = tpid;
    new_entry->cid    = cid;
    new_entry->tag    = tag;
    new_entry->len    = len;
    new_entry->sid    = sid;

    VTGEN_JUMP(gen, length);
  } 
}

void VTGen_write_RMA_GET(VTGen* gen, uint64_t* time, uint32_t opid, 
        uint32_t tpid, uint32_t cid, uint32_t tag, uint64_t len, uint32_t sid)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_RMAPutGet* new_entry;

    uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_RMAPutGet));

    VTGEN_ALLOC_EVENT(gen, length);

    new_entry = ((VTBuf_Entry_RMAPutGet*)gen->buf->pos);

    new_entry->type   = BUF_ENTRY_TYPE__RMAGet;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->opid   = opid;
    new_entry->tpid   = tpid;
    new_entry->cid    = cid;
    new_entry->tag    = tag;
    new_entry->len    = len;
    new_entry->sid    = sid;

    VTGEN_JUMP(gen, length);
  } 
}

void VTGen_write_RMA_END(VTGen* gen, uint64_t* time, uint32_t rpid,
        uint32_t cid, uint32_t tag, uint32_t sid)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_RMAEnd* new_entry;

    uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_RMAEnd));

    VTGEN_ALLOC_EVENT(gen, length);

    new_entry = ((VTBuf_Entry_RMAEnd*)gen->buf->pos);

    new_entry->type   = BUF_ENTRY_TYPE__RMAEnd;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->rpid   = rpid;
    new_entry->cid    = cid;
    new_entry->tag    = tag;
    new_entry->sid    = sid;

    VTGEN_JUMP(gen, length);
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

    uint32_t length =
      VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_EnterLeave) + 
                          (metc > 0 ? (metc - 1) * sizeof(uint64_t) : 0)));

    VTGEN_ALLOC_EVENT(gen, length);

    new_entry = ((VTBuf_Entry_EnterLeave*)gen->buf->pos);
    
    new_entry->type   = BUF_ENTRY_TYPE__Enter;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->rid    = vt_trc_regid[VT__TRC_STAT];
    new_entry->sid    = 0;
    new_entry->metc   = metc;
    if( metc > 0 )
      memcpy(new_entry->metv, metv, metc * sizeof(uint64_t));

    VTGEN_JUMP(gen, length);
  }
}

void VTGen_write_EXIT_STAT(VTGen* gen, uint64_t* time,
       uint8_t metc, uint64_t metv[])
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_EnterLeave* new_entry;

    uint32_t length =
      VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_EnterLeave) + 
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
  }
}

void VTGen_write_ENTER_FLUSH(VTGen* gen, uint64_t* time, 
       uint8_t metc, uint64_t metv[])
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_EnterLeave* new_entry;

    uint32_t length =
      VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_EnterLeave) + 
                          (metc > 0 ? (metc - 1) * sizeof(uint64_t) : 0)));

    /* NB: No VTGEN_ALLOC_EVENT since space reserved at buffer creation */

    new_entry = ((VTBuf_Entry_EnterLeave*)gen->buf->pos);
    
    new_entry->type   = BUF_ENTRY_TYPE__Enter;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->rid    = vt_trc_regid[VT__TRC_FLUSH];
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

    uint32_t length =
      VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_EnterLeave) + 
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


/* - Summary records - */


void VTGen_write_FUNCTION_SUMMARY(VTGen* gen, uint64_t* time,
     uint32_t rid, uint64_t cnt, uint64_t excl, uint64_t incl)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_SUM_PROP_ON(gen, VT_SUM_PROP_FUNC))
  {
    VTBuf_Entry_FunctionSummary* new_entry;

    uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_FunctionSummary));

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
  }
}

void VTGen_write_MESSAGE_SUMMARY(VTGen* gen, uint64_t* time,
       uint32_t peer, uint32_t cid, uint32_t tag,
       uint64_t scnt, uint64_t rcnt, uint64_t sent, uint64_t recvd)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_SUM_PROP_ON(gen, VT_SUM_PROP_MSG))
  {
    VTBuf_Entry_MessageSummary* new_entry;

    uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_MessageSummary));

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
  }
}

void VTGen_write_COLLECTIVE_OPERATION_SUMMARY(VTGen* gen, uint64_t* time,
       uint32_t cid, uint32_t rid, uint64_t scnt, uint64_t rcnt,
       uint64_t sent, uint64_t recvd)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_SUM_PROP_ON(gen, VT_SUM_PROP_COLLOP))
  {
    VTBuf_Entry_CollectiveOperationSummary* new_entry;

    uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_CollectiveOperationSummary));

    VTGEN_ALLOC_EVENT(gen, length);

    new_entry = ((VTBuf_Entry_CollectiveOperationSummary*)gen->buf->pos);

    new_entry->type   = BUF_ENTRY_TYPE__CollectiveOperationSummary;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->cid    = cid;
    new_entry->rid    = rid;
    new_entry->scnt   = scnt;
    new_entry->rcnt   = rcnt;
    new_entry->sent   = sent;
    new_entry->recvd  = recvd;

    VTGEN_JUMP(gen, length);
  }
}

void VTGen_write_FILE_OPERATION_SUMMARY(VTGen* gen, uint64_t* time,
       uint32_t fid, uint64_t nopen, uint64_t nclose, uint64_t nread,
       uint64_t nwrite, uint64_t nseek, uint64_t read, uint64_t wrote)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_SUM_PROP_ON(gen, VT_SUM_PROP_FILEOP))
  {
    VTBuf_Entry_FileOperationSummary* new_entry;

    uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_FileOperationSummary));

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
  }
}
