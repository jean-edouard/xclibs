#
# Copyright (c) 2011 Citrix Systems, Inc.
# 
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
# 
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
#

#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.61])
AC_INIT([xclogging], [], [])

# Checks for programs.
AC_PROG_CC
AC_SYS_LARGEFILE

LIBXCLOGGING_MAJOR_VERSION=`cat $srcdir/version-major`
LIBXCLOGGING_MINOR_VERSION=`cat $srcdir/version-minor`
LIBXCLOGGING_MICRO_VERSION=`cat $srcdir/version-micro`

LIBXCLOGGING_INTERFACE_VERSION_MIN=$LIBXCLOGGING_MINOR_VERSION
LIBXCLOGGING_INTERFACE_VERSION_MAX=$LIBXCLOGGING_MINOR_VERSION
LIBXCLOGGING_INTERFACE_REVISION=1

LIBXCLOGGING_VERSION=$LIBXCLOGGING_MAJOR_VERSION.$LIBXCLOGGING_MINOR_VERSION.$LIBXCLOGGING_MICRO_VERSION

AC_SUBST(LIBXCLOGGING_MAJOR_VERSION)
AC_SUBST(LIBXCLOGGING_MINOR_VERSION)
AC_SUBST(LIBXCLOGGING_MICRO_VERSION)
AC_SUBST(LIBXCLOGGING_VERSION)

AC_SUBST(LIBXCLOGGING_INTERFACE_VERSION_MIN)
AC_SUBST(LIBXCLOGGING_INTERFACE_VERSION_MAX)
AC_SUBST(LIBXCLOGGING_INTERFACE_REVISION)


LT_RELEASE=$LIBXCLOGGING_MAJOR_VERSION.$LIBXCLOGGING_MINOR_VERSION
LT_CURRENT=$LIBXCLOGGING_INTERFACE_VERSION_MAX
LT_REVISION=$LIBXCLOGGING_MICRO_VERSION
LT_AGE=`expr $LIBXCLOGGING_INTERFACE_VERSION_MAX - $LIBXCLOGGING_INTERFACE_VERSION_MIN`

AC_SUBST(LT_RELEASE)
AC_SUBST(LT_CURRENT)
AC_SUBST(LT_REVISION)
AC_SUBST(LT_AGE)

VERSION=$LIBXCLOGGING_VERSION
PACKAGE=libxclogging

AC_CONFIG_SRCDIR([config.h.in])
AM_CONFIG_HEADER(config.h)
AC_CONFIG_MACRO_DIR([m4])

AM_INIT_AUTOMAKE($PACKAGE,$VERSION)
AM_PROG_LIBTOOL
AM_PROG_CC_C_O

AC_CHECK_PROG(MD5SUM,md5sum,md5sum)
AC_CHECK_PROG(GREP,grep,grep)
AC_CHECK_PROG(SED,sed,sed)
AC_PROG_AWK

# Checks for libraries
AC_CHECK_LIB([dl], [dladdr])

# Checks for header files.
AC_CHECK_HEADERS([stdlib.h string.h syslog.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.
AC_CHECK_FUNCS([strerror strtol strdup])

AC_CONFIG_FILES([Makefile
                 src/Makefile
                 example/Makefile
                 libxclogging.pc.src
                 libxclogging-config.src], [chmod +x libxclogging-config.src])

AC_OUTPUT
