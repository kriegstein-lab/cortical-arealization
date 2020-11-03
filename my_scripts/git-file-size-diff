#!/bin/bash

# 2020-10-28 cse
# https://stackoverflow.com/questions/10845051/git-show-total-file-size-difference-between-two-commits
# git cat-file -s will output the size in bytes of an object in git. git diff-tree can tell you the differences between one tree and another.
# Putting this together into a script called git-file-size-diff located somewhere on your PATH will give you the ability to call git file-size-diff <tree-ish> <tree-ish>. 

# $ git file-size-diff --cached master
# -570    Makefile
# -134    git-gui.sh
# -1  lib/browser.tcl
# 931 lib/commit.tcl
# 18  lib/index.tcl
# total 244

USAGE='[--cached] [<rev-list-options>...]

Show file size changes between two commits or the index and a commit.'

. "$(git --exec-path)/git-sh-setup"
args=$(git rev-parse --sq "$@")
[ -n "$args" ] || usage
cmd="diff-tree -r"
[[ $args =~ "--cached" ]] && cmd="diff-index"
eval "git $cmd $args" | {
  total=0
  while read A B C D M P
  do
    case $M in
      M) bytes=$(( $(git cat-file -s $D) - $(git cat-file -s $C) )) ;;
      A) bytes=$(git cat-file -s $D) ;;
      D) bytes=-$(git cat-file -s $C) ;;
      *)
        echo >&2 warning: unhandled mode $M in \"$A $B $C $D $M $P\"
        continue
        ;;
    esac
    total=$(( $total + $bytes ))
    printf '%d\t%s\n' $bytes "$P"
  done
  echo total $total
}