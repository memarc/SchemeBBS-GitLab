#!/bin/sh
if [ ! -d "data/sexp" ] 
then
  echo $"You haven't created any board"
  echo $"run \`./create-boards.sh BOARDNAME...'"
  exit 1
elif [ -z "$1" ]
then
  mit-scheme --args 8080 < bbs.scm
else
  mit-scheme --args $* < bbs.scm
fi
