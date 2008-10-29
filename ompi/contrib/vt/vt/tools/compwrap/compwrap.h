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

#ifndef _COMPWRAP_H_
#define _COMPWRAP_H_

#define VTSEQLIB "-lvt"
#define VTMPILIB "-lvt.mpi"
#define VTOMPLIB "-lvt.omp"
#define VTHYBLIB "-lvt.ompi"

typedef enum
{
   INST_TYPE_GNU       = 0x1,     // auto. instrumentation by GNU comp.
   INST_TYPE_INTEL     = 0x2,     // ^ Intel version >=10
   INST_TYPE_PATHSCALE = 0x4,     // ^ Pathscale version >=3.1
   INST_TYPE_PGI       = 0x8,     // ^ PGI
   INST_TYPE_SUN       = 0x10,    // ^ SUN
   INST_TYPE_XL        = 0x20,    // ^ IBM (xlc, xlC,...)
   INST_TYPE_FTRACE    = 0x40,    // ^ NEC SX
   INST_TYPE_MANUAL    = 0x80,    // manual instr. by VT API
   INST_TYPE_POMP      = 0x100,   // semi auto. instr. by POMP directives
   INST_TYPE_DYNINST   = 0x200    // binary instrumentation by Dyninst
} InstTypeT;

struct ProperiesS
{
   ProperiesS() :
      version(""), language(""), includedir(""), libdir(""),
      comp_cmd_env(""), comp_flags_env(""), comp_cmd(""), comp_args(""),
      comp_flags(""), comp_ldflags(""), comp_iflags(""), comp_ulibs(""),
      opari_cmd(""), opari_args(""), opari_tab_comp_cmd(""),
      opari_tab_comp_flags(""),
      pmpilib(""), fmpilib(""), dynattlib(""),
      iflags_gnu(""), iflags_intel(""), iflags_pathscale(""), iflags_pgi(""),
      iflags_sun(""), iflags_xl(""), iflags_ftrace(""),
      inst_type(INST_TYPE_MANUAL), inst_avail(0),
      beverbose(false), componly(false), uses_mpi(false), uses_omp(false),
      showme(false), showme_compile(false), showme_link(false) {}

   std::string version;         // version
   std::string language;        // language

   std::string includedir;      // VT's include directory
   std::string libdir;          // VT's library directory

   std::string comp_cmd_env;    // compiler command env. name
   std::string comp_flags_env;  // compiler flags env. name
   std::string comp_cmd;        // compiler command
   std::string comp_args;       // compiler arguments
   std::string comp_flags;      // compiler flags
   std::string comp_ldflags;    // linker flags
   std::string comp_iflags;     // compiler instrumentation flag
   std::string comp_ulibs;      // necessary libs. for VT
   
   std::string opari_cmd;       // OPARI command
   std::string opari_args;      // OPARI arguments
   std::pair<std::string, std::string>
      opari_tab_file;           // OPARI's table source file 
   std::string opari_tab_comp_cmd;   // compiler command for OPARI's table file
   std::string opari_tab_comp_flags; // compiler flags for OPARI's table file
   std::vector<std::string>
   vec_opari_files;             // OPARI's input source files
   std::vector<std::string>
   vec_opari_mfiles_src;        // OPARI's output source files (*.mod.*)
   std::vector<std::string>
   vec_opari_mfiles_obj;        // ^ corresponding obj. files

   std::string pmpilib;
   std::string fmpilib;
   std::string dynattlib;

   std::string iflags_gnu;
   std::string iflags_intel;
   std::string iflags_pathscale;
   std::string iflags_pgi;
   std::string iflags_sun;
   std::string iflags_xl;
   std::string iflags_ftrace;

   InstTypeT inst_type;         // instrumentation type
                                // (e.g. gnu,intel,manual,...)
   int       inst_avail;        // bitmask for available instr.-types
   bool      beverbose;         // FLAG: be verbose ?
   bool      componly;          // FLAG: compile only ?
   bool      uses_mpi;          // FLAG: uses MPI ?
   bool      uses_omp;          // FLAG: uses OpenMP ?
   bool      showme;            // FLAG: show compiler/linker flags ?
   bool      showme_compile;    // FLAG: show compiler flags ?
   bool      showme_link;       // FLAG: show linker flags ?
} Properties;

class Wrapper
{
public:
   
   Wrapper();    // contructor
   ~Wrapper();   // destructor

   void showVersion();
   void showUsageText();
   void showInfo();
   void show();
   int  run();

   bool setInstType( const InstTypeT type );
   InstTypeT getInstType() { return Properties.inst_type; }
   bool isInstAvail( InstTypeT type ) {
      return (Properties.inst_avail & type); }

   void setUsesMPI( const bool set, const bool lock = false,
		    const bool ovwrt = false );
   bool usesMPI() { return Properties.uses_mpi; }
   void setUsesOMP( const bool set, const bool lock = false,
		    const bool ovwrt = false );
   bool usesOMP() { return Properties.uses_omp; }
   void setBeVerbose( const bool set ) { Properties.beverbose = set; }
   bool beverbose() { return Properties.beverbose; }
   void setComponly( const bool set ) { Properties.componly = set; }
   bool componly() { return Properties.componly; }
   void setShowme( const bool set ) {
      Properties.showme = Properties.showme_compile =
	 Properties.showme_link = set; }
   bool showme() { return Properties.showme; }
   void setShowmeCompile( const bool set ) { Properties.showme_compile = set; }
   bool showmeCompile() { return Properties.showme_compile; }
   void setShowmeLink( const bool set ) { Properties.showme_link = set; }
   bool showmeLink() { return Properties.showme_link; }

   void comp_setCmd( const std::string cmd );
   void comp_addArg( const std::string arg );
   void comp_addULib( const std::string ulib );

   void opari_setTabFile( const std::string tabfile );
   void opari_addArg( const std::string arg );
   void opari_addSrcFile( const std::string srcfile );
   std::vector<std::string> opari_getIncFilesFromTabFile
      ( const std::string tabfile );

};

extern Wrapper * theWrapper;

#endif // _COMPWRAP_H_
