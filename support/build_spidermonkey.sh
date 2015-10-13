#!/bin/sh

CORE_TOP=`pwd`
export CORE_TOP

CURLBIN=`which curl`
if ! test -n "CURLBIN"; then
    display_error "Error: curl is required. Add it to 'PATH'"
    exit 1
fi

GUNZIP=`which gunzip`
UNZIP=`which unzip`
TAR=`which tar`
GNUMAKE=`which gmake 2>/dev/null || which make`
PATCHES=$CORE_TOP/support/patches
STATICLIBS=$CORE_TOP/.libs
DISTDIR=$CORE_TOP/.dists

# nspr sources
NSPR_VER=4.8.8
NSPR_DISTNAME=nspr-$NSPR_VER.tar.gz
NSPR_SITE=http://dl.refuge.io

# spidermonkey js sources
JS_VER=185-1.0.0
JS_REALVER=1.8.5
JS_DISTNAME=js$JS_VER.tar.gz
JS_SITE=http://dl.refuge.io
JSDIR=$STATICLIBS/js-$JS_REALVER
JS_LIBDIR=$STATICLIBS/js/lib
JS_INCDIR=$STATICLIBS/js/include


[ "$MACHINE" ] || MACHINE=`(uname -m) 2>/dev/null` || MACHINE="unknown"
[ "$RELEASE" ] || RELEASE=`(uname -r) 2>/dev/null` || RELEASE="unknown"
[ "$SYSTEM" ] || SYSTEM=`(uname -s) 2>/dev/null`  || SYSTEM="unknown"
[ "$BUILD" ] || VERSION=`(uname -v) 2>/dev/null` || VERSION="unknown"


CFLAGS="-g -O2 -Wall"
LDFLAGS="-lstdc++"
ARCH=
ISA64=
GNUMAKE=make
CC=gcc
CXX=g++
PATCH=patch
case "$SYSTEM" in
    Linux)
        ARCH=`(uname -p) 2>/dev/null`
        if [ "$ARCH" = "unknown" ]; then
            ARCH=`(uname -m) 2>/dev/null`
        fi
        ;;
    FreeBSD|OpenBSD|NetBSD)
        ARCH=`(uname -p) 2>/dev/null`
        GNUMAKE=gmake
        ;;
    Darwin)
        ARCH=`(uname -p) 2>/dev/null`
        ISA64=`(sysctl -n hw.optional.x86_64) 2>/dev/null`
        ;;
    Solaris)
        ARCH=`(uname -p) 2>/dev/null`
        GNUMAKE=gmake
        PATCH=gpatch
        ;;
    *)
        ARCH="unknown"
        ;;
esac


# TODO: add mirror & signature validation support
fetch()
{
    TARGET=$DISTDIR/$1
    if ! test -f $TARGET; then
        echo "==> Fetch $1 to $TARGET"
        $CURLBIN --progress-bar -L $2/$1 -o $TARGET
    fi
}

build_nspr()
{
    NSPR_CONFIGURE_ENV=""
    case "$SYSTEM" in
        Linux)
            if [ "$ARCH" = "x86_64" ]; then
                NSPR_CONFIGURE_ENV="--enable-64bit"
            fi
            CFLAGS="$CFLAGS -lpthread"
            ;;
        FreeBSD|OpenBSD|NetBSD)
            if [ "$ARCH" = "x86_64" ] || [ "$ARCH" = "amd64" ]; then
                NSPR_CONFIGURE_ENV+=--enable-64bit
            fi
            ;;
        Darwin)
            if [ "$ISA64" = "1" ]; then
                NSPR_CONFIGURE_ENV+=--enable-64bit
            fi
            ;;
    esac

    fetch $NSPR_DISTNAME $NSPR_SITE

    # clean the build
    rm -rf $STATICLIBS/nspr*

    echo "==> build nspr"
    cd $STATICLIBS
    $GUNZIP -c $DISTDIR/$NSPR_DISTNAME | $TAR xf -

    cd $STATICLIBS/nspr-$NSPR_VER/mozilla/nsprpub
    ./configure --disable-debug --enable-optimize \
        --prefix=$STATICLIBS/nsprpub $NSPR_CONFIGURE_ENV

    $GNUMAKE all
    $GNUMAKE install
}

build_js()
{

    fetch $JS_DISTNAME $JS_SITE

    # clean the build
    rm -rf $STATICLIBS/js*

    mkdir -p $JS_LIBDIR
    mkdir -p $JS_INCDIR

    cd $STATICLIBS
    $GUNZIP -c $DISTDIR/$JS_DISTNAME | $TAR -xf -

    echo "==> build js"
    cd $JSDIR/js/src
    $PATCH -p0 -i $PATCHES/js/patch-jsprf_cpp || echo "skipping patch"
    $PATCH -p0 -i $PATCHES/js/patch-configure || echo "skipping patch"

    env CFLAGS="$CFLAGS" LDFLAGS="$LDFLAGS" \
        CPPFLAGS="-DXP_UNIX -DJS_C_STRINGS_ARE_UTF8" \
        CC="$CC" CXX="$CXX" \
        ./configure --prefix=$STATICLIBS/js \
				    --disable-debug \
					--enable-optimize \
					--enable-static \
					--disable-shared-js \
					--disable-tests \
					--with-system-nspr \
					--with-nspr-prefix=$STATICLIBS/nsprpub && \
        $GNUMAKE all || exit 1

    mkdir -p $JS_INCDIR/js
    cp $JSDIR/js/src/*.h $JS_INCDIR
    cp $JSDIR/js/src/*.tbl $JS_INCDIR
    cp $JSDIR/js/src/libjs_static.a $JS_LIBDIR
}


do_setup()
{
    echo "==> spidermonkey (compile)"
    mkdir -p $DISTDIR
    mkdir -p $STATICLIBS
}

do_builddeps()
{
    if [ ! -f $STATICLIBS/nsprpub/lib/libnspr4.a ]; then
        build_nspr
    fi

    if [ ! -f $STATICLIBS/js/lib/libjs_static.a ]; then
        build_js
    fi
}


clean()
{
    rm -rf $STATICLIBS
    rm -rf $DISTDIR
}



usage()
{
    cat << EOF
Usage: $basename [command] [OPTIONS]

The $basename command compile Mozilla Spidermonkey and ICU statically
for couch_core.

Commands:

    all:        build couch_core static libs
    clean:      clean static libs
    -?:         display usage

Report bugs at <https://github.com/refuge/couch_core>.
EOF
}

if [ "x$1" = "x" ]; then
    do_setup
    do_builddeps
	exit 0
fi

case "$1" in
    all)
        shift 1
        do_setup
        do_builddeps
        ;;
    clean)
        shift 1
        clean
        ;;
    js)
        shift 1
        do_setup
        build_js
        ;;
    nspr)
        shift 1
        do_setup
        build_nspr
        ;;
    help|--help|-h|-?)
        usage
        exit 0
        ;;
    *)
        echo $basename: ERROR Unknown command $arg 1>&2
        echo 1>&2
        usage 1>&2
        echo "### $basename: Exitting." 1>&2
        exit 1;
        ;;
esac


exit 0