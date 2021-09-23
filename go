#!/bin/bash

stack build
if [ $? -eq 0 ]; then
  stack exec blog rebuild
  if [ $? -eq 0 ] && [ -z $1 ]; then
    stack exec blog watch
  fi
fi
