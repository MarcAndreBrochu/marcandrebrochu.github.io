#!/bin/bash

stack build
stack exec blog rebuild
stack exec blog watch
