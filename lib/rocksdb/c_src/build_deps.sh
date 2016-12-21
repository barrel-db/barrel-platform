#!/bin/sh

# /bin/sh on Solaris is not a POSIX compatible shell, but /usr/bin/ksh is.
if [ `uname -s` = 'SunOS' -a "${POSIX_SHELL}" != "true" ]; then
    POSIX_SHELL="true"
    export POSIX_SHELL
    exec /usr/bin/ksh $0 $@
fi
unset POSIX_SHELL # clear it so if we invoke other scripts, they run as ksh as well

if [ `uname -s` = "FreeBSD" ]; then
    export CC="clang"
    export CXX="clang++"
    export CFLAGS="$CFLAGS -D_GLIBCXX_USE_C99"
    export CXXFLAGS="-std=c++11 -stdlib=libc++ -D_GLIBCXX_USE_C99"

fi

SCRIPTPATH=$( cd $(dirname $0) ; pwd -P )
SCRIPT=$SCRIPTPATH/${0##*/}
BASEDIR=$SCRIPTPATH
BUILD_CONFIG=$BASEDIR/rocksdb/make_config.mk

ROCKSDB_VSN="4.13"
SNAPPY_VSN="1.1.1"

set -e

if [ `basename $PWD` != "c_src" ]; then
    # originally "pushd c_src" of bash
    # but no need to use directory stack push here
    cd c_src
fi

# detecting gmake and if exists use it
# if not use make
# (code from github.com/tuncer/re2/c_src/build_deps.sh
which gmake 1>/dev/null 2>/dev/null && MAKE=gmake
MAKE=${MAKE:-make}

# Changed "make" to $MAKE

case "$1" in
    rm-deps)
        rm -rf rocksdb system snappy-$SNAPPY_VSN rocksdb-$ROCKSDB_VSN.tar.gz
        ;;

    clean)
        rm -rf system snappy-$SNAPPY_VSN
        if [ -d rocksdb ]; then
            (cd rocksdb && $MAKE clean)
        fi
        ;;

    test)
        export CFLAGS="$CFLAGS -I $BASEDIR/system/include"
        export CXXFLAGS="$CXXFLAGS -I $BASEDIR/system/include"
        export LDFLAGS="$LDFLAGS -L$BASEDIR/system/lib"
        export LD_LIBRARY_PATH="$BASEDIR/rocksdb:$BASEDIR/system/lib:$LD_LIBRARY_PATH"

        (cd rocksdb && $MAKE ldb_tests)

        ;;

    get-deps)
        if [ ! -d rocksdb ]; then
            ROCKSDBURL="https://gitlab.com/barrel-db/rocksdb/repository/archive.tar.gz?ref=v$ROCKSDB_VSN"
            ROCKSDBTARGZ="rocksdb-$ROCKSDB_VSN.tar.gz"
            echo Downloading $ROCKSDBURL...
            curl -L -o $ROCKSDBTARGZ $ROCKSDBURL
            tar -xzf $ROCKSDBTARGZ
            mv rocksdb-v$ROCKSDB_VSN-* rocksdb
        fi
        ;;

    *)
        if [ ! -d snappy-$SNAPPY_VSN ]; then
            tar -xzf snappy-$SNAPPY_VSN.tar.gz
            (cd snappy-$SNAPPY_VSN && ./configure --prefix=$BASEDIR/system --libdir=$BASEDIR/system/lib --with-pic)
        fi

        if [ ! -f system/lib/libsnappy.a ]; then
            (cd snappy-$SNAPPY_VSN && $MAKE && $MAKE install)
        fi

        export CFLAGS="$CFLAGS -I $BASEDIR/system/include"
        export CXXFLAGS="$CXXFLAGS -I $BASEDIR/system/include"
        export LDFLAGS="$LDFLAGS -L$BASEDIR/system/lib"
        export LD_LIBRARY_PATH="$BASEDIR/system/lib:$LD_LIBRARY_PATH"

        sh $SCRIPT get-deps
        if [ ! -f rocksdb/librocksdb.a ]; then
            (cd rocksdb && CXXFLAGS=-fPIC PORTABLE=1 $MAKE -j 3 static_lib)
        fi
        ;;
esac
