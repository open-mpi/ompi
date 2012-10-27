
SET src=%1
SET cwd=D:\temp\OpenMPI

copy open-mpi-logo.ico %src%\contrib\platform\win32\
copy uninstall.ico %src%\contrib\platform\win32\

REM build 32 debug version
cd %cwd%\COMPILE
md %src%-shared-debug-32
cd %src%-shared-debug-32

cmake -G "Visual Studio 10" -D CMAKE_INSTALL_PREFIX:PATH=%cwd%\%src%\installed-32 -D BUILD_SHARED_LIBS:BOOL=TRUE -D CMAKE_BUILD_TYPE:STRING=debug -D ORTE_WANT_CCP:BOOL=FALSE -D OMPI_WANT_CXX_BINDINGS:BOOL=TRUE -D OMPI_RELEASE_BUILD:BOOL=TRUE -D CMAKE_Fortran_COMPILER:FILEPATH="C:/Program Files (x86)/Intel/ComposerXE-2011/bin/ia32/ifort.exe" -D OMPI_WANT_F77_BINDINGS:BOOL=TRUE ..\..\%src% > configure-log.txt

devenv.com OpenMPI.sln /build debug > build-log.txt

devenv.com OpenMPI.sln /project INSTALL.vcxproj /build debug

cd ..\..

REM build 32 release version
cd %cwd%\COMPILE
md %src%-shared-release-32
cd %src%-shared-release-32

cmake -G "Visual Studio 10" -D CMAKE_INSTALL_PREFIX:PATH=%cwd%\%src%\installed-32 -D BUILD_SHARED_LIBS:BOOL=TRUE -D CMAKE_BUILD_TYPE:STRING=release -D ORTE_WANT_CCP:BOOL=FALSE -D OMPI_WANT_CXX_BINDINGS:BOOL=TRUE -D CMAKE_Fortran_COMPILER:FILEPATH="C:/Program Files (x86)/Intel/ComposerXE-2011/bin/ia32/ifort.exe" -D OMPI_WANT_F77_BINDINGS:BOOL=TRUE ..\..\%src% > configure-log.txt

devenv.com OpenMPI.sln /build release > build-log.txt

devenv.com OpenMPI.sln /project INSTALL.vcxproj /build release

devenv.com OpenMPI.sln /project PACKAGE.vcxproj /build release

cd ..\..


