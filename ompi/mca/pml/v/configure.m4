#
# Copyright (c) 2004-2007 The Trustees of the University of Tennessee.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AC_DEFUN([MCA_pml_v_CONFIG],[
    # We are going to make recursive call in shell, nothing is impossible
    # Still, we need to be extra careful
    (
        # Change srcdir (for DIRECT builds)
        srcdir=`cd $srcdir && pwd`
        srcdir="$srcdir/$project/mca/$framework/$component"

        # Create directory structure (for VPATH builds)
        AS_MKDIR_P("$project/mca/$framework/$component/vprotocol")
        
        cd "$project/mca/$framework/$component"
        rm -f vprotocol/mca && ln -s . vprotocol/mca
        rm -f vprotocol/vprotocol && ln -s . vprotocol/vprotocol                
        rm -f $srcdir/vprotocol/mca && ln -s . $srcdir/vprotocol/mca
        rm -f $srcdir/vprotocol/vprotocol && ln -s . $srcdir/vprotocol/vprotocol                
        
        MCA_CONFIGURE_FRAMEWORK(vprotocol, vprotocol, 1)
        rm -f vprotocol/mca
        rm -f vprotocol/vprotocol
        rm -f $srcdir/vprotocol/mca
        rm -f $srcdir/vprotocol/vprotocol

        cat >mca_vprotocol_config_output <<EOF
            #
            # /!\ This is Automatically generated file. Do not edit. 
            #

            # Apply the modifications to current shell context
            MCA_vprotocol_ALL_COMPONENTS="$MCA_vprotocol_ALL_COMPONENTS"
            MCA_vprotocol_STATIC_COMPONENTS="$MCA_vprotocol_STATIC_COMPONENTS"
            MCA_vprotocol_DSO_COMPONENTS="$MCA_vprotocol_DSO_COMPONENTS"
            MCA_vprotocol_STATIC_LTLIBS="$MCA_vprotocol_STATIC_LTLIBS"
            
            # Remove leading mca/vprotocol from subdirs
            MCA_vprotocol_ALL_SUBDIRS="`echo $MCA_vprotocol_ALL_SUBDIRS | sed 's/mca\/vprotocol\///'`"
            MCA_vprotocol_STATIC_SUBDIRS="`echo $MCA_vprotocol_STATIC_SUBDIRS | sed 's/mca\/vprotocol\///'`"
            MCA_vprotocol_DSO_SUBDIRS="`echo $MCA_vprotocol_DSO_SUBDIRS | sed 's/mca\/vprotocol\///'`"
EOF
    )
    
    # Reload the output from vprotocol framework's config
    . $project/mca/$framework/$component/mca_vprotocol_config_output

    AC_SUBST(MCA_vprotocol_ALL_COMPONENTS)
    AC_SUBST(MCA_vprotocol_STATIC_COMPONENTS)
    AC_SUBST(MCA_vprotocol_DSO_COMPONENTS)
    AC_SUBST(MCA_vprotocol_STATIC_LTLIBS)
    
    AC_SUBST(MCA_vprotocol_ALL_SUBDIRS)
    AC_SUBST(MCA_vprotocol_STATIC_SUBDIRS)
    AC_SUBST(MCA_vprotocol_DSO_SUBDIRS)

    m4_foreach(mca_component, [mca_vprotocol_no_config_component_list],
        [m4_ifval(mca_component, [
            [BUILD_vprotocol_]mca_component[_DSO]="$[BUILD_vprotocol_]mca_component[_DSO]"
            AM_CONDITIONAL([OMPI_BUILD_vprotocol_]mca_component[_DSO], test "$[BUILD_vprotocol_]mca_component[_DSO]" = "1")])])

    m4_foreach(mca_component, [mca_vprotocol_m4_config_component_list],
        [m4_ifval(mca_component, [
            [BUILD_vprotocol_]mca_component[_DSO]= $[BUILD_vprotocol_]mca_component[_DSO]
            AM_CONDITIONAL([OMPI_BUILD_vprotocol_]mca_component[_DSO], test "$[BUILD_vprotocol_]mca_component[_DSO]" = "1")])])



])
