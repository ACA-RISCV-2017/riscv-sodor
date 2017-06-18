#!/bin/sh

for f in `ls $1`; do
  case $f in
    rv32*)
      ;;
    *)
      echo "$f"
      tail -n19 "$1/$f" | head -n17
      echo ""
      ;;
  esac
done
