#
# Copyright (c) 2012 Citrix Systems, Inc.
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
AC_INIT([xcxenstore], [1.0], [])

LT_INIT

LIBXCXENSTORE_MAJOR_VERSION=`cat $srcdir/version-major`
LIBXCXENSTORE_MINOR_VERSION=`cat $srcdir/version-minor`
LIBXCXENSTORE_MICRO_VERSION=`cat $srcdir/version-micro`

LIBXCXENSTORE_INTERFACE_VERSION_MIN=$LIBXCXENSTORE_MINOR_VERSION
LIBXCXENSTORE_INTERFACE_VERSION_MAX=$LIBXCXENSTORE_MINOR_VERSION
LIBXCXENSTORE_INTERFACE_REVISION=1

LIBXCXENSTORE_VERSION=$LIBXCXENSTORE_MAJOR_VERSION.$LIBXCXENSTORE_MINOR_VERSION.$LIBXCXENSTORE_MICRO_VERSION

AC_SUBST(LIBXCXENSTORE_MAJOR_VERSION)
AC_SUBST(LIBXCXENSTORE_MINOR_VERSION)
AC_SUBST(LIBXCXENSTORE_MICRO_VERSION)
AC_SUBST(LIBXCXENSTORE_VERSION)

AC_SUBST(LIBXCXENSTORE_INTERFACE_VERSION_MIN)
AC_SUBST(LIBXCXENSTORE_INTERFACE_VERSION_MAX)
AC_SUBST(LIBXCXENSTORE_INTERFACE_REVISION)

LT_RELEASE=$LIBXCXENSTORE_MAJOR_VERSION.$LIBXCXENSTORE_MINOR_VERSION
LT_CURRENT=$LIBXCXENSTORE_INTERFACE_VERSION_MAX
LT_REVISION=$LIBXCXENSTORE_MICRO_VERSION
LT_AGE=`expr $LIBXCXENSTORE_INTERFACE_VERSION_MAX - $LIBXCXENSTORE_INTERFACE_VERSION_MIN`

AC_SUBST(LT_RELEASE)
AC_SUBST(LT_CURRENT)
AC_SUBST(LT_REVISION)
AC_SUBST(LT_AGE)

VERSION=$LIBXCXENSTORE_VERSION
PACKAGE=libxcxenstore

AM_INIT_AUTOMAKE($PACKAGE, $VERSION)
# Checks for programs.
AC_PROG_CC
AM_PROG_CC_C_O
PKG_PROG_PKG_CONFIG([0.22])
AC_PROG_AWK
AC_CHECK_PROG(MD5SUM, md5sum, md5sum)
AC_CHECK_PROG(GREP, grep, grep)
AC_CHECK_PROG(SED, sed, sed)

# Checks for header files.
AC_CHECK_HEADERS([stdlib.h string.h syslog.h unistd.h])

# Checks for library functions.
AC_CHECK_FUNCS([strerror strtol])

#check for libevent
AC_ARG_WITH([libevent],
	AC_HELP_STRING([--with-libevent=PATH], [Path to prefix where libevent (or libev) is installed]),
	[LIBEVENT_PREFIX=$with_libevent], [])

case "x$LIBEVENT_PREFIX" in
	x|xno|xyes)
		LIBEVENT_INC=""
		LIBEVENT_LIB="-levent"
		;;
	*)
		LIBEVENT_INC="-I${LIBEVENT_PREFIX}/include"
		LIBEVENT_LIB="-L${LIBEVENT_PREFIX}/lib -levent"
		;;
esac

AC_SUBST(LIBEVENT_INC)
AC_SUBST(LIBEVENT_LIB)

have_libevent=true

ORIG_LDFLAGS="${LDFLAGS}"
ORIG_CPPFLAGS="${CPPFLAGS}"
	LDFLAGS="${LDFLAGS} ${LIBEVENT_LIB}"
	CPPFLAGS="${CPPFLAGS} ${LIBEVENT_INC}"
	AC_CHECK_HEADERS([event.h], [], [have_libevent=false])
	AC_CHECK_FUNC([event_init], [], [have_libevent=false])
LDFLAGS="${ORIG_LDFLAGS}"
CPPFLAGS="${ORIG_CPPFLAGS}"

if test "x$have_libevent" = "xfalse"; then
	AC_MSG_ERROR([
*** At least libev or libevent is required.
])
fi

#output files
AC_CONFIG_MACRO_DIR([m4])
AM_CONFIG_HEADER(src/config.h)

AC_CONFIG_FILES([Makefile
		src/Makefile
		sample/Makefile
		libxcxenstore.pc.src
		libxcxenstore-config.src],[chmod +x libxcxenstore-config.src])

AC_OUTPUT
