#!/bin/sh
# Configure script bootstrap for SXEmacs
#
# Copyright (C) 2005 - 2012 Steve Youngs.
# Copyright (C) 2006, 2007, 2008 Sebastian Freundt.
# Copyright (C) 2007, 2010, 2011 Nelson Ferreira

# This file is part of SXEmacs.

# SXEmacs is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# SXEmacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Parts of SXEmacs are also distributed under a BSD-like licence.
# Check file headers for more information.

# BSD's m4 probably isn't gonna cut it, use gm4 if it is available
if test -z "$M4"
then
   type gm4 >/dev/null 2>&1 && M4=gm4 || M4=m4
fi

M4_VERSION=$($M4 --version | head -n1 | sed -e 's/^\(m4 \)\?(\?GNU M4)\? *//g' )
GOOD_M4=$( echo $M4_VERSION | awk -F. '{if( ($1>1) || ( ($1==1) && ($2>4) ) || ( ($1==1) && ($2==4) && ($3>=6) )) print 1 }')

if [ "$GOOD_M4" != "1" ]; then
    echo You have m4 version $M4_VERSION.  SXEmacs requires m4 version 1.4.6 or later.
    exit 1
fi

# To cater for Solaris
if test -d "/usr/xpg4/bin"; then
    # don't add xpg4 dir to PATH if on OpenIndiana
    grep -q OpenIndiana /etc/release 2>/dev/null||PATH=/usr/xpg4/bin:$PATH
    export PATH
fi

if test -z "$GIT"
then
    type git >/dev/null 2>&1 && GIT=git
fi

olddir=$(pwd)
srcdir=$(dirname $0)
cd "$srcdir"

EXPECTED_TREE_VERSION="22.1.17"

emacs_is_beta=t
if test -n "$GIT" -a -n "$($GIT symbolic-ref HEAD 2>/dev/null)"; then
	TREE_VERSION="$($GIT tag|tail -n1|tr -d v)"
	GIT_VERSION="$($GIT describe | head -n1)"
	GIT_BRANCH="$(git branch --no-color | awk '/^\*/ { print $2 }')"
	IN_GIT="1"
fi
if test -z "$TREE_VERSION"; then
	TREE_VERSION="$EXPECTED_TREE_VERSION"
	if test -n "$IN_GIT"; then
	    echo "If you cloned this branch into your own you could issue:"
	    echo "\tgit tag -s v${TREE_VERSION}.<your branch_name>"
	    echo ""
	    echo "Be careful about pushing the tags as they probably will be "
	    echo "more of a nuisance..."
	    echo ""
	    TREE_VERSION="$EXPECTED_TREE_VERSION.$GIT_BRANCH"
	    echo "For now I am assuming the tre version will be $TREE_VERSION"
	    echo ""
	fi
fi
if test -z "$GIT_VERSION"; then
	GIT_VERSION="${TREE_VERSION}-no_git_version"
fi

emacs_major_version="$(echo $TREE_VERSION|cut -d. -f1)"
emacs_minor_version="$(echo $TREE_VERSION|cut -d. -f2)"
emacs_beta_version="$(echo $TREE_VERSION|cut -d. -f3)"
emacs_full_version="$emacs_major_version.$emacs_minor_version.$emacs_beta_version"
sxemacs_codename="Hudson Custom Eight"
sxemacs_git_version="$GIT_VERSION"

if test "$emacs_full_version" != "$EXPECTED_TREE_VERSION"; then
    # Note, there is no need check for git repos, because
    # it can only happen in such a case anyway...
    echo "*******************************************"
    echo " WARNING: Your git tags may be out of date "
    echo ""
    echo " Expected tree version $EXPECTED_TREE_VERSION "
    echo " got $emacs_full_version (from $TREE_VERSION) "
    set -x
    git tag
    git describe
    git describe --long
    git config -l
    set +x
    echo "*******************************************"
fi

test -z "$AUTOCONF"   && type autoconf   >/dev/null 2>&1 && AUTOCONF=autoconf
export AUTOCONF
test -z "$AUTORECONF" && type autoreconf >/dev/null 2>&1 && AUTORECONF=autoreconf
export AUTORECONF
test -z "$AUTOHEADER" && type autoheader >/dev/null 2>&1 && AUTOHEADER=autoheader
export AUTOHEADER
test -z "$AUTOMAKE"   && type automake   >/dev/null 2>&1 && AUTOMAKE=automake
export AUTOMAKE
test -z "$ACLOCAL"    && type aclocal    >/dev/null 2>&1 && ACLOCAL=aclocal
export ACLOCAL
test -z "$LIBTOOL"    && type libtool    >/dev/null 2>&1 && LIBTOOL=libtool
export LIBTOOL
if test -z "$LIBTOOLIZE"; then
  if type glibtoolize >/dev/null 2>&1; then
    LIBTOOLIZE=glibtoolize
  elif type libtoolize >/dev/null 2>&1; then
    LIBTOOLIZE=libtoolize
  fi
fi
export LIBTOOLIZE

autoconf_ver=$($AUTOCONF --version 2>/dev/null | head -n1)
if test -z "$autoconf_ver"; then
    echo Could not determine autoconf
    exit 1
fi
autoreconf_ver=$($AUTORECONF --version 2>/dev/null | head -n1)
if test -z "$autoreconf_ver"; then
    echo Could not determine autoreconf
    exit 1
fi
autoheader_ver=$($AUTOHEADER --version 2>/dev/null | head -n1)
if test -z "$autoheader_ver"; then
    echo Could not determine autoheader
    exit 1
fi
automake_ver=$($AUTOMAKE --version 2>/dev/null | head -n1)
if test -z "$automake_ver"; then
    echo Could not determine automake
    exit 1
fi
aclocal_ver=$($ACLOCAL --version 2>/dev/null | head -n1)
if test -z "$aclocal_ver"; then
    echo Could not determine aclocal
    exit 1
fi
libtool_ver=$($LIBTOOL --version 2>/dev/null | head -n1)
if test -z "$libtool_ver" -a -n "$LIBTOOLIZE"; then
    libtool_ver=$($LIBTOOLIZE --version 2>/dev/null | head -n1)
fi
if test -z "$libtool_ver"; then
    echo WARNING: Could not determine libtool
fi

# When things go wrong... get a bigger hammer!
if test -n "$PHAMMER"; then
    HAMMER=$PHAMMER
fi

if test -n "$HAMMER"; then
	if test -n "$GIT" -a -n "$($GIT symbolic-ref HEAD 2>/dev/null)"; then
		$GIT clean -fxd
	else
		echo "ERROR: Not a git workspace, or you don't have git" >&2
		exit 1
	fi
	unset HAMMER
fi


cat>sxemacs_version.m4<<EOF
dnl autogenerated version number
m4_define([SXEM4CS_VERSION], [$emacs_full_version])
m4_define([SXEM4CS_MAJOR_VERSION], [$emacs_major_version])
m4_define([SXEM4CS_MINOR_VERSION], [$emacs_minor_version])
m4_define([SXEM4CS_BETA_VERSION], [$emacs_beta_version])
m4_define([SXEM4CS_BETA_P], [$emacs_is_beta])
m4_define([SXEM4CS_GIT_VERSION], [$sxemacs_git_version])
m4_define([SXEM4CS_CODENAME], [$sxemacs_codename])
m4_define([4UTOCONF_VERSION], [$autoconf_ver])
m4_define([4UTOHEADER_VERSION], [$autoheader_ver])
m4_define([4CLOCAL_VERSION], [$aclocal_ver])
m4_define([4UTOMAKE_VERSION], [$automake_ver])
m4_define([4IBTOOL_VERSION], [$libtool_ver])
EOF


# using libtoolize as we did before doesn't work anymore, so just mkdir --Horst
test -d libtld/m4 || mkdir -p libltdl/m4 
$AUTORECONF --force --verbose --install -Wall

# hack-o-matic.  Using gmp's config.{guess,sub} lets us have properer
# detected machine configurations --SY.
guess=$(grep GMP config.guess)
sub=$(grep GMP config.sub)
if test -z "${guess}"; then
    mv -f config.guess configfsf.guess
    cp configgmp.guess config.guess
fi
if test -z "${sub}"; then
    mv -f config.sub configfsf.sub
    cp configgmp.sub config.sub
fi

cd $olddir
