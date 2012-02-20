/*
    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
*/
/*
 * File         : mpi_MPI.c
 * Headerfile   : mpi_MPI.h 
 * Author       : SungHoon Ko, Xinying Li (contributions from MAEDA Atusi)
 * Created      : Thu Apr  9 12:22:15 1998
 * Revision     : $Revision: 1.17 $
 * Updated      : $Date: 2003/01/17 01:50:37 $
 * Copyright: Northeast Parallel Architectures Center
 *            at Syracuse University 1998
 */
#include "ompi_config.h"

#include <stdio.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_TARGETCONDITIONALS_H
#include <TargetConditionals.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#if OPAL_WANT_LIBLTDL
  #ifndef __WINDOWS__
    #if OPAL_LIBLTDL_INTERNAL
      #include "opal/libltdl/ltdl.h"
    #else
      #include "ltdl.h"
    #endif
  #else
    #include "ltdl.h"
  #endif
#endif

#include "opal/util/output.h"

#include "mpi.h"
#include "mpi_MPI.h"
#include "mpiJava.h"

ompi_java_globals_t ompi_java;

static int len = 0;
static char** sargs = 0;


/*
 * Class:    mpi_MPI
 * Method:   loadGlobalLibraries
 *
 * Java implementations typically default to loading dynamic
 * libraries strictly to a local namespace. This breaks the
 * Open MPI model where components reference back up to the
 * base libraries (e.g., libmpi) as it requires that the
 * symbols in those base libraries be globally available.
 *
 * One option, of course, is to build with --disable-dlopen.
 * However, this would preclude the ability to pickup 3rd-party
 * binary plug-ins at time of execution. This is a valuable
 * capability that would be a negative factor towards use of
 * the Java bindings.
 *
 * The other option is to explicitly dlopen libmpi ourselves
 * and instruct dlopen to add all those symbols to the global
 * namespace. This must be done prior to calling any MPI
 * function (e.g., MPI_Init) or else Java will have already
 * loaded the library to the local namespace. So create a
 * special JNI entry point that just loads the required libmpi
 * to the global namespace and call it first (see MPI.java),
 * thus making all symbols available to subsequent dlopen calls
 * when opening OMPI components.
 */
JNIEXPORT jboolean JNICALL Java_mpi_MPI_loadGlobalLibraries(JNIEnv *env, jclass obj)
{
#if OPAL_WANT_LIBLTDL
    lt_dladvise advise;

    if (lt_dlinit() != 0) {
        return JNI_FALSE;
    }

#if OPAL_HAVE_LTDL_ADVISE
    /* open the library into the global namespace */
    if (lt_dladvise_init(&advise)) {
        return JNI_FALSE;
    }

    if (lt_dladvise_ext(&advise)) {
        lt_dladvise_destroy(&advise);
        return JNI_FALSE;
    }

    if (lt_dladvise_global(&advise)) {
        lt_dladvise_destroy(&advise);
        return JNI_FALSE;
    }

    /* we don't care about the return value
     * on dlopen - it might return an error
     * because the lib is already loaded,
     * depending on the way we were built
     */
    lt_dlopenadvise("libmpi", advise);
    lt_dladvise_destroy(&advise);

    return JNI_TRUE;
#endif
    /* need to balance the ltdl inits */
    lt_dlexit();
    /* if we don't have advise, then we are hosed */
    return JNI_FALSE;
#endif
    /* if dlopen was disabled, then all symbols
     * should have been pulled up into the libraries,
     * so we don't need to do anything as the symbols
     * are already available
     */
    return JNI_TRUE;
}

/*
 * Class:     mpi_MPI
 * Method:    InitNative
 * Signature: ([Ljava/lang/String;)[Ljava/lang/String;
 */
JNIEXPORT jobjectArray JNICALL Java_mpi_MPI_InitNative(JNIEnv *env, jclass obj, jobjectArray argv)
{
    jsize i;
    jstring jc;
    jclass string;
    jobject value;

    len = (*env)->GetArrayLength(env,argv);
    sargs = (char**)calloc(len+1, sizeof(char*));
    for (i=0; i<len; i++) {
        jc=(jstring)(*env)->GetObjectArrayElement(env,argv,i);
        sargs[i] = (char*)calloc(strlen((*env)->GetStringUTFChars(env,jc,0)) + 1,
                                 sizeof(char));
        strcpy(sargs[i],(*env)->GetStringUTFChars(env,jc,0));
    }

    MPI_Init(&len, &sargs);

    string = (*env)->FindClass(env, "java/lang/String");
    value = (*env)->NewObjectArray(env, len, string, NULL);
    for (i = 0; i < len; i++) {
        jc = (*env)->NewStringUTF(env, sargs[i]);
        (*env)->SetObjectArrayElement(env, value, i, jc);
    }

    ompi_java_init_native_Datatype() ;

    return value;
}                                                   

/*
 * Class:     mpi_MPI
 * Method:    Finalize
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_mpi_MPI_Finalize(JNIEnv *env, jclass obj)
{
    ompi_java_clearFreeList(env) ;

#if OPAL_WANT_LIBLTDL
    /* need to balance the ltdl inits */
    lt_dlexit();
#endif

    MPI_Finalize();
}
                                             
/*
 * Class:     mpi_MPI
 * Method:    Get_processor_name
 * Signature: ([B)I
 */
JNIEXPORT jint JNICALL Java_mpi_MPI_Get_1processor_1name(JNIEnv *env, jclass obj, jbyteArray buf)
{
    int len;
    jboolean isCopy; 
    jbyte* bufc = (jbyte*)((*env)->GetByteArrayElements(env,buf,&isCopy)) ;

    ompi_java_clearFreeList(env) ;

    MPI_Get_processor_name((char*)bufc, &len); 
    (*env)->ReleaseByteArrayElements(env,buf,bufc,0) ;
    return len;
}

/*
 * Class:     mpi_MPI
 * Method:    Wtime
 * Signature: ()D
 */
JNIEXPORT jdouble JNICALL Java_mpi_MPI_Wtime(JNIEnv *env, jclass jthis)
{
    ompi_java_clearFreeList(env) ;

    return MPI_Wtime();
}

/*
 * Class:     mpi_MPI
 * Method:    Wtick
 * Signature: ()D
 */
JNIEXPORT jdouble JNICALL Java_mpi_MPI_Wtick(JNIEnv *env, jclass jthis)
{
    ompi_java_clearFreeList(env) ;

    return MPI_Wtick();
}

/*
 * Class:     mpi_MPI
 * Method:    Initialized
 * Signature: ()Z
 */
JNIEXPORT jboolean JNICALL Java_mpi_MPI_Initialized(JNIEnv *env, jclass jthis)
{
    int flag;

    ompi_java_clearFreeList(env) ;

    MPI_Initialized(&flag);
    if (flag==0) {
        return JNI_FALSE;
    } else {
        return JNI_TRUE;
    }
}

/*
 * Class:     mpi_MPI
 * Method:    Buffer_attach_native
 * Signature: ([B)V
 */
JNIEXPORT void JNICALL Java_mpi_MPI_Buffer_1attach_1native(JNIEnv *env, jclass jthis, jbyteArray buf)
{
    jboolean isCopy;

    int size=(*env)->GetArrayLength(env,buf);
    jbyte* bufptr = (*env)->GetByteArrayElements(env,buf,&isCopy) ;

    ompi_java_clearFreeList(env) ;

    MPI_Buffer_attach(bufptr,size); 
}

/*
 * Class:     mpi_MPI
 * Method:    Buffer_detach_native
 * Signature: ([B)V
 */
JNIEXPORT void JNICALL Java_mpi_MPI_Buffer_1detach_1native(JNIEnv *env, jclass jthis, jbyteArray buf)
{
    /*jboolean isCopy;*/

    int size;
    /*char* bufptr ;*/
    jbyte* bufptr ;

    ompi_java_clearFreeList(env) ;

    MPI_Buffer_detach(&bufptr, &size);

    if (buf != NULL) {
        (*env)->ReleaseByteArrayElements(env,buf,bufptr,0);
    }
}

/*
 * Class:     mpi_MPI
 * Method:    SetConstant
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_mpi_MPI_SetConstant(JNIEnv *env, jclass jthis)
{
    jfieldID anysourceID=(*env)->GetStaticFieldID(env,jthis,"ANY_SOURCE","I");
    jfieldID anytagID=(*env)->GetStaticFieldID(env,jthis,"ANY_TAG","I");
    jfieldID procnullID=(*env)->GetStaticFieldID(env,jthis,"PROC_NULL","I");  
    jfieldID graphID=(*env)->GetStaticFieldID(env,jthis,"GRAPH","I");
    jfieldID cartID=(*env)->GetStaticFieldID(env,jthis,"CART","I");
    jfieldID bsendoverID=(*env)->GetStaticFieldID(env,jthis,"BSEND_OVERHEAD","I");
    jfieldID undefinedID=(*env)->GetStaticFieldID(env,jthis,"UNDEFINED","I");
   
    jfieldID identID=(*env)->GetStaticFieldID(env,jthis,"IDENT","I");
    jfieldID congruentID=(*env)->GetStaticFieldID(env,jthis,"CONGRUENT","I");
    jfieldID similarID=(*env)->GetStaticFieldID(env,jthis,"SIMILAR","I");
    jfieldID unequalID=(*env)->GetStaticFieldID(env,jthis,"UNEQUAL","I");
    jfieldID tagubID=(*env)->GetStaticFieldID(env,jthis,"TAG_UB","I");
    jfieldID hostID=(*env)->GetStaticFieldID(env,jthis,"HOST","I");
    jfieldID ioID=(*env)->GetStaticFieldID(env,jthis,"IO","I");

    (*env)->SetStaticIntField(env,jthis,anysourceID,MPI_ANY_SOURCE);
    (*env)->SetStaticIntField(env,jthis,anytagID,MPI_ANY_TAG);
    (*env)->SetStaticIntField(env,jthis,procnullID,MPI_PROC_NULL);
    (*env)->SetStaticIntField(env,jthis,graphID,MPI_GRAPH);
    (*env)->SetStaticIntField(env,jthis,cartID,MPI_CART);
#ifdef GC_DOES_PINNING
    (*env)->SetStaticIntField(env,jthis,bsendoverID,MPI_BSEND_OVERHEAD);
#else
    (*env)->SetStaticIntField(env,jthis,bsendoverID,
                              MPI_BSEND_OVERHEAD + sizeof(int));
#endif  /* GC_DOES_PINNING */

    (*env)->SetStaticIntField(env,jthis,undefinedID,MPI_UNDEFINED);
    
    (*env)->SetStaticIntField(env,jthis,identID,MPI_IDENT);
    (*env)->SetStaticIntField(env,jthis,congruentID,MPI_CONGRUENT);
    (*env)->SetStaticIntField(env,jthis,similarID,MPI_SIMILAR);
    (*env)->SetStaticIntField(env,jthis,unequalID,MPI_UNEQUAL);
    (*env)->SetStaticIntField(env,jthis,tagubID,MPI_TAG_UB);
    (*env)->SetStaticIntField(env,jthis,hostID,MPI_HOST);
    (*env)->SetStaticIntField(env,jthis,ioID,MPI_IO);
}

void ompi_java_clearFreeList(JNIEnv *env)
{
    jclass mpi ;
    jmethodID clearID ;

    mpi = (*env)->FindClass(env, "mpi/MPI");
    clearID = (*env)->GetStaticMethodID(env, mpi, "clearFreeList", "()V");
    (*env)->CallStaticVoidMethod(env, mpi, clearID) ;
}

