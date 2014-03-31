#!/bin/sh

REBAR_DEPS_DIR=$1
ACTION=$2

case $ACTION in
  compile)
    cd $REBAR_DEPS_DIR/ex_doc ; $REBAR_DEPS_DIR/elixir/bin/mix compile ;;
  clean)
    cd $REBAR_DEPS_DIR/ex_doc ; make clean ;;
  *) 
    echo "Unknow action $ACTION" ;;
esac

