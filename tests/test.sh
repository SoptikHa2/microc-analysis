#!/usr/bin/env bash

CURRENT_DIR="$(pwd)"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null && pwd)"
cd "$SCRIPT_DIR" || exit

SUITES_DIR="$SCRIPT_DIR/suites"
ACTUALS_DIR="$SCRIPT_DIR/actual"
EXPECTED_DIR="$SCRIPT_DIR/expected"
BOOTSTRAP=0
FILTER=""
DIFF="diff"

# Check if running in an interactive shell
INTERACTIVE=0
if [ -t 0 ]; then
  INTERACTIVE=1
fi

if [ -t 1 ]; then
  if command -v delta >/dev/null; then
    DIFF='delta -s --word-diff-regex="."'
  fi
fi

[[ -d "$ACTUALS_DIR" ]] || mkdir -p "$ACTUALS_DIR"
[[ $BOOTSTRAP -eq 1 && ! -d "$ACTUALS_DIR" ]] && mkdir -p "$ACTUALS_DIR"

FAILED=0
OK=0
SKIP=0

check() {
  local name="$1"
  local actual="$2"
  local expected="$3"

  if [[ $BOOTSTRAP -eq 1 ]]; then
    echo "Bootstrapping $name in $expected"
    [[ -d $(dirname "$expected") ]] || mkdir -p "$(dirname "$expected")"
    cp "$actual" "$expected"
  fi

  failed=""

  if [[ ! -f "$expected" ]]; then
    echo "** MISSING: $name no expected file: $expected"
    touch "$expected"
    failed=1
    $DIFF "$actual" /dev/null
  else
    diff=$(diff "$actual" "$expected")

    if [[ -n "$diff" ]]; then
      echo "** FAILED: $name"
      $DIFF "$actual" "$expected"
      failed=1
    fi
  fi

  if [[ -n "$failed" ]]; then
    # In non-interactive mode (like CI), count as failed and continue
    if [[ $INTERACTIVE -eq 0 ]]; then
      FAILED=$((FAILED + 1))
    else
      # Interactive mode: prompt for action
      echo
      while true; do
        read -sn1 -p "Action: (a) accept, (r) reject, (s) skip, (q) quit" ans
        echo
        case "$ans" in
        a)
          echo "Updating '$expected' with '$actual'"
          cp "$actual" "$expected"
          break
          ;;
        r)
          FAILED=$((FAILED + 1))
          break
          ;;
        s)
          SKIP=$((SKIP + 1))
          break
          ;;
        q)
          exit 1
          ;;
        *)
          continue
          ;;
        esac
      done
    fi
  else
    echo "** OK: $name"
    OK=$((OK + 1))
  fi
}

do_test() {
  local name=$1
  shift
  local stdin_input="$1"
  shift
  local params=$*
  local actual="$ACTUALS_DIR/$name.out"
  local expected="$EXPECTED_DIR/$name.out"

  if [[ -n "$FILTER" && ! "$name" =~ $FILTER ]]; then
    echo "** SKIP: $name"
    SKIP=$((SKIP + 1))
    return 0
  fi

  [[ -d $(dirname "$actual") ]] || mkdir -p "$(dirname "$actual")"

  local cmd="$MICROC $params"
  echo "$cmd"

  {
    echo "---"
    echo "test: $params"
    echo "---"
    if [[ -n "$stdin_input" ]]; then
      printf '%s\n' "$stdin_input" | $cmd 2>&1
    else
      $cmd 2>&1
    fi
  } >"$actual"
  status=$?

  if [[ $status -eq 255 ]]; then
    echo "The microc command does not seem to work (error: 255)"
    rm "$actual"
    exit 255
  fi

  echo -e "\nexit: $status" >>"$actual"

  check "$name" "$actual" "$expected"
}

test_file() {
  local file=${1#"$(dirname "$SUITES_DIR")"/}
  local name="${file#*/}"
  shift
  local action=$1
  shift
  local stdin_input="$1"
  shift

  do_test "$name" "$stdin_input" "$action" "$file" "$@"
}

suite_task_1() {
  for file in $(find "$SUITES_DIR/task-1" -name '*.uc' | sort); do
    local args=()
    local stdin_input=""
    local header_lines=()
    # Read header lines starting with //
    while IFS= read -r line; do
      [[ $line == "//"* ]] || break
      header_lines+=("$line")
    done <"$file"

    for header in "${header_lines[@]}"; do
      if [[ $header == "// args: "* ]]; then
        args=(${header#"// args: "})
      elif [[ $header == "// stdin: "* ]]; then
        stdin_input=${header#"// stdin: "}
        # Transform spaces to newlines
        stdin_input=${stdin_input// /$'\n'}
      fi
    done

    test_file "$file" run "$stdin_input" "${args[@]}"
  done
}

suite_task_2() {
  for file in $(find "$SUITES_DIR/task-2" -name '*.uc' | sort); do
    test_file "$file" type "$stdin_input"
  done
}

suite() {
  case "$1" in
  all)
    suite_task_1
    suite_task_2
    suite_task_3
    suite_task_4
    ;;
  task-1)
    suite_task_1
    ;;
  task-2)
    suite_task_2
    ;;
  task-3)
    suite_task_3
    ;;
  task-4)
    suite_task_4
    ;;
  *)
    echo "Unknown suite"
    exit 1
    ;;
  esac

  echo "-----"
  echo "OK: $OK, FAILED: $FAILED, SKIP: $SKIP"

  if [[ $FAILED -eq 0 ]]; then
    exit 0
  else
    exit 1
  fi
}

usage() {

  while IFS= read -r f; do
    SUITE_FUNS+=("$f")
  done < <(compgen -A function | grep '^suite_')
  SUITE_NAMES=()
  for f in "${SUITE_FUNS[@]}"; do
    n=${f#suite_}
    n=${n//_/-}
    SUITE_NAMES+=("$n")
  done
  if ((${#SUITE_NAMES[@]})); then
    AVAILABLE_SUITES="(available: '$(
      IFS="', '"
      echo "${SUITE_NAMES[*]}"
    )' or 'all' to run everything)"
  else
    AVAILABLE_SUITES="(no suites found)"
  fi

  cat <<EOF
Usage: $0 [options]

Options:
  -h | -help        Show this help
  --bootstrap       Bootstrap the output
  --debug           Enable test harness debugging
  --filter REGEX    Only test cases that matches REGEX
  --microc PATH     Which microc binary to use
  --suite NAME      Which suite to run $AVAILABLE_SUITES
EOF
}

DEBUG=""

while (("$#")); do
  case "$1" in
  -h | --help)
    usage
    exit 0
    ;;
  --bootstrap)
    BOOTSTRAP=1
    shift
    ;;
  --debug)
    DEBUG=1
    shift
    ;;
  --microc)
    MICROC="$2"
    if [[ -z "$MICROC" ]]; then
      echo "Missing microc executable"
      exit 1
    fi
    shift 2
    ;;
  --filter)
    FILTER="$2"
    if [[ -z "$FILTER" ]]; then
      echo "Missing filter"
      exit 1
    fi
    shift 2
    ;;
  --suite)
    SUITE="$2"
    if [[ -z "$SUITE" ]]; then
      echo "Missing suite name $AVAILABLE_SUITES"
      exit 1
    fi
    shift 2
    ;;
  *)
    echo "Unknown option $1"
    exit 1
    ;;
  esac
done

[[ -n "$DEBUG" ]] && set -x

if [[ -z "$MICROC" ]]; then
  echo "Missing path to microc executable, provide using --microc or using MICROC environment variable"
  exit 1
fi

if [[ -z "$SUITE" ]]; then
  echo "Missing --suite option $AVAILABLE_SUITES"
  exit 1
fi

if [[ "$MICROC" != /* ]]; then
  MICROC="$CURRENT_DIR/$MICROC"
fi

# how one will work on linux/osx
MICROC=$(readlink -f "$MICROC" || realpath "$MICROC")

if [[ ! -f "$MICROC" || ! -x "$MICROC" ]]; then
  echo "$MICROC: not a valid executable"
  exit 1
fi

echo "Using microc: $MICROC"

suite "$SUITE"
