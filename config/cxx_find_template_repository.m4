dnl -*- shell-script -*-
dnl
dnl $HEADER$
dnl

define([OMPI_CXX_FIND_TEMPLATE_REPOSITORY],[
#
# Arguments: None
#
# Dependencies: None
#
# See if the compiler makes template repository directories
# Warning: this is a really screwy example! -JMS
#
# Sets OMPI_CXX_TEMPLATE_REPOSITORY to the template repository, or blank.
# Must call AC_SUBST manually
#

# Find the repository
AC_MSG_CHECKING([for C++ template repository directory])
mkdir conf_tmp_$$
cd conf_tmp_$$
cat > conftest.h <<EOF
template <class T>
class foo {
public:
  foo(T yow) : data(yow) { yow.member(3); };
  void member(int i);
private:
  T data;
};

class bar {
public:
  bar(int i) { data = i; };
  void member(int j) { data = data * j; };

private:
  int data;
};
EOF

cat > conftest2.C <<EOF
#include "conftest.h"

void
some_other_function(void)
{
  foo<bar> var1(6);
  foo< foo<bar> > var2(var1);
}
EOF

cat > conftest1.C <<EOF
#include "conftest.h"

void some_other_function(void);

template <class T>
void
foo<T>::member(int i)
{
  i += 2;
}

int
main(int argc, char *argv[])
{
  foo<bar> var1(6);
  foo< foo<bar> > var2(var1);

  some_other_function();
  return 0;
}
EOF

ompi_template_failed=
echo configure:__oline__: $CXX $CXXFLAGS -c conftest1.C >&5
$CXX $CXXFLAGS -c conftest1.C >&5 2>&5
if test ! -f conftest1.o ; then
    AC_MSG_RESULT([templates not supported?])
    echo configure:__oline__: here is the program that failed: >&5
    cat conftest1.C >&5
    echo configure:__oline__: here is conftest.h: >&5
    cat conftest.h >&5
    ompi_template_failed=1
else
    echo configure:__oline__: $CXX $CXXFLAGS -c conftest2.C >&5
    $CXX $CXXFLAGS -c conftest2.C >&5 2>&5
    if test ! -f conftest2.o ; then
	AC_MSG_RESULT([unknown error])
	echo configure:__oline__: here is the program that failed: >&5
	cat conftest2.C >&5
	echo configure:__oline__: here is conftest.h: >&5
	cat conftest.h >&5
    else
	rm -rf conftest*

	for ompi_file in `ls`
	do
	    if test "$ompi_file" != "." -a "$ompi_file" != ".."; then
		# Is it a directory?
		if test -d "$ompi_file"; then
		    ompi_template_dir="$ompi_file $ompi_template_dir"
		    
		# Or is it a file?
		else
		    name="`echo $ompi_file | cut -d. -f1`"
		    
		    temp_mask=
		    if test "$name" = "main" -o "$name" = "other"; then
			temp_mask="`echo $ompi_file | cut -d. -f2`"
			if test "$ompi_template_filemask" = ""; then
			ompi_template_filemask="$temp_mask";
			elif test "`echo $ompi_template_filemask | grep $temp_mask`" = ""; then
			ompi_template_filemask="$ompi_template_filemask $temp_mask"
			fi
		    fi
		fi
	    fi
	done
	if test "$ompi_template_filemask" != ""; then
	    temp_mask=
	    for mask in $ompi_template_filemask
	    do
		temp_mask="*.$mask $temp_mask"
	    done
	    ompi_template_filemask=$temp_mask
	fi
    fi
fi
OMPI_CXX_TEMPLATE_REPOSITORY="$ompi_template_dir $ompi_template_filemask"

if test "`echo $OMPI_CXX_TEMPLATE_REPOSITORY`" != ""; then
    AC_MSG_RESULT([$OMPI_CXX_TEMPLATE_REPOSITORY])
else
    AC_MSG_RESULT([not used])
fi
cd ..
rm -rf conf_tmp_$$

# Clean up
unset ompi_file ompi_template_failed ompi_template_dir])dnl
