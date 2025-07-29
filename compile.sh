#!/usr/bin/env zsh

TARGET="standalone"

flags=(-DSAVE_PREPROCESSED=0 -DSAVE_ASSEMBLY=1 -DDEBUG_AST=0)

while getopts "ad:ptr" opt; do
  case "$opt" in
  d)
    case "$OPTARG" in
    src | source)
      flags+=(-DDEBUG_SRC=1)
      ;;
    token*)
      flags+=(-DDEBUG_TOKENS=1)
      ;;
    ast | nodes)
      flags+=(-DDEBUG_AST=1)
      ;;
    state)
      flags+=(-DDEBUG_STATE=1)
      ;;
    memory)
      flags+=(-DDEBUG_MEMORY=1)
      ;;
    esac
    ;;
  p)
    # Profiling flags with gprof
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
    export TARGET="test"
    ;;
  esac
done

export CMAKE_CUSTOM_FLAGS=$(
  IFS=" "
  echo "${flags[*]}"
)

# echo $CMAKE_CUSTOM_FLAGS

shift $((OPTIND - 1))

# echo "---- BUILDING $TARGET"
BUILD_DIR="build"
SOURCE_DIR="$TARGET"

cmake -B "$BUILD_DIR" -S "$SOURCE_DIR"
cmake --build "$BUILD_DIR" -j

if [[ $? -eq 0 ]]; then
  if [[ "$RUN" = true ]]; then
    source run.sh "$@"
  elif [[ "$TARGET" = "test" ]]; then
    source run.sh test "$@"
  fi
fi
