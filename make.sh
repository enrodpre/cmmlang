#!/usr/bin/env zsh

# Advanced error handling function
handle_error() {
  local lineno=$1
  local command=$2
  local exitcode=$3

  if [[ $exitcode -eq 1 ]]; then
    exit 1
  fi

  echo "Error in script at line $lineno:"
  echo "Command '$command' exited with status $exitcode"

  exit exitcode
}

set -eE
trap 'handle_error "$LINENO" "$BASH_COMMAND" "$?"' ERR

TESTS="OFF"
RUN=false

# flags=(-DSAVE_PREPROCESSED=0 -DSAVE_ASSEMBLY=1 -DDEBUG_AST=0)

# export ASAN_OPTIONS=abort_on_error=1

while getopts "ad:ptr" opt; do
  case "$opt" in
  p)
    flags+=(-pg)
    ;;
  r)
    RUN=true
    ;;
  s)
    case "$OPTARG" in
    pre | preprocessed)
      flags+=(-DSAVE_PREPROCESSED=1)
      ;;
    asm | assembly)
      flags+=(-DSAVE_ASSEMBLY=1)
      ;;
    esac
    ;;
  t)
    TESTS="ON"
    ;;
  esac
done

export CMAKE_CUSTOM_FLAGS=$(
  IFS=" "
  echo "${flags[*]}"
)

shift $((OPTIND - 1))

cmake -B build -DBUILD_TESTS="$TESTS"
cmake --build build -j

if [[ $? -ne 0 ]]; then
  echo Compiler status - >$?
  exit $?
fi

if [[ "$RUN" = false ]]; then
  exit 0
fi

function setcurrfile() {
  echo "set args $1" >.gdb/file.gdb
}

SUCCESS_CODE=255
FILENAME="res"

if [[ $TESTS = ON ]]; then
  if [[ "$1" == "e2e" ]]; then
    # shift
    env -C build/test pytest e2e.py
    # make -C build/test test_e2e
  elif [[ "$1" == "unit" ]]; then
    make -C build/test test_unit
  elif [[ -n "$1" ]]; then
    setcurrfile build/test/unit_"$1"
    ./build/test/unit_$1
  else
    make -C build/test test
  fi
  exit $?
fi

echo
echo -e '----\033[0;32m COMPILING $FILENAME\033[0m'

[ -L CmmLang ] || ln -s ./build/CmmLang CmmLang

# Clear old files
[ -f "$FILENAME".preprocessed ] && rip "$FILENAME".preprocessed
[ -f "$FILENAME".asm ] && rip "$FILENAME".asm
[ -f "$FILENAME" ] && rip "$FILENAME"

setcurrfile "$1"

./CmmLang "$@"

result=$?
if [[ $result = 139 ]]; then
  echo -e "\033[1;31mCORE DUMPED\033[0m"
elif [[ $result = 0 ]]; then
  echo '---- \033[0;32mEXECUTING $FILENAME\033[0m'
  ./"$FILENAME"
  code=$?
  echo "Exit status -> " $code
  if [[ $code = $SUCCESS_CODE ]]; then
    echo -e "\033[1;32mOK\033[0m"
  else
    echo -e "\033[1;31mERROR\033[0m"
  fi
  exit $code
fi
