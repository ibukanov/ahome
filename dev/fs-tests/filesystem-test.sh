#!/bin/sh

set -e -u

run_test() {
  echo "Executing: $1"
  echo 3 > /proc/sys/vm/drop_caches
  /usr/bin/time -f 'Real time: %e' /bin/sh -c 'eval "$1"; sync' - "$1"
}

do_run() {
  echo "Removing files from the test directory ${test_dir}"
  find . -mindepth 1 -maxdepth 1 -exec rm -r '{}' +
  sync
  start_time="$(date +%s)"
  run_test 'tar xf "${SEED_TAR}"'
  run_test "git -C work status > /dev/null 2>&1"
  run_test 'find work -type f \
    \( -name "*.c" -o -name "*.cc" -o -name "*.cpp" \) -print0 \
    | xargs -P4 -n 100 -0 lz4 -k -m'
  echo "All tests time: $(($(date +%s) - start_time))"
}

test_dir="$1"
SEED_TAR="$(realpath "$2")"
export SEED_TAR

if ! test -e "${test_dir}"; then
  echo "Creating the test directory ${test_dir}"
  mkdir "${test_dir}"
fi
cd "${test_dir}"

for i in 1 2 3; do
  echo
  echo "Test run ${i}"
  do_run
done
