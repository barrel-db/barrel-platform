#!/bin/sh

# /bin/sh on Solaris is not a POSIX compatible shell, but /usr/bin/ksh is.
if [ `uname -s` = 'SunOS' -a "${POSIX_SHELL}" != "true" ]; then
    POSIX_SHELL="true"
    export POSIX_SHELL
    exec /usr/bin/ksh $0 $@
fi


unset POSIX_SHELL # clear it so if we invoke other scripts, they run as ksh as well

SCRIPTPATH=$( cd $(dirname $0) ; pwd -P )
SCRIPT=$SCRIPTPATH/${0##*/}
BASEDIR=$SCRIPTPATH

if ! $BASEDIR/barrel ping > /dev/null; then
    echo "Node is not running!"
    exit 1
fi

OUTPUT=`$BASEDIR/barrel rpc barrel_ctl $@`

echo "output is $OUTPUT"

if [[ $OUTPUT =~ "ERROR" ]]; then

    exit 1
fi