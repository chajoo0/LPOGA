#!/usr/bin/env bash

set -eu

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
TASK_RUNNER="${SCRIPT_DIR}/cluster/run_array_task.sh"

MODE="help"
DESIGN="all"
BATCH="all"
JOBS=1
DESIGN_SET=0
BATCH_SET=0

usage() {
  cat <<'EOF'
Usage:
  bash run_simulations_local.sh --plan [selection options]
  bash run_simulations_local.sh --all [--jobs N]
  bash run_simulations_local.sh --run --design DESIGN [--batch BATCH] [--jobs N]

Actions:
  --plan                 Show the selected tasks without starting them.
  --all                  Run the complete experiment (10 groups, 500 tasks).
  --run                  Run the subset selected by --design and --batch.

Selection options:
  --design DESIGN        factor, outcome, shock, cstar, or all.
  --batch BATCH          core, bootstrap, lava, oga, or all.
  --jobs N               Maximum concurrent local tasks (default: 1).

Calling this script without an action only prints this message. It never
starts simulation tasks implicitly.
EOF
}

die() {
  echo "Error: $*" >&2
  exit 2
}

while [ "$#" -gt 0 ]; do
  case "$1" in
    --plan)
      [ "${MODE}" = "help" ] || die "Specify only one of --plan, --all, or --run."
      MODE="plan"
      ;;
    --all)
      [ "${MODE}" = "help" ] || die "Specify only one of --plan, --all, or --run."
      MODE="run_all"
      DESIGN="all"
      BATCH="all"
      ;;
    --run)
      [ "${MODE}" = "help" ] || die "Specify only one of --plan, --all, or --run."
      MODE="run_subset"
      ;;
    --design)
      [ "$#" -ge 2 ] || die "--design requires a value."
      DESIGN="$2"
      DESIGN_SET=1
      shift
      ;;
    --batch)
      [ "$#" -ge 2 ] || die "--batch requires a value."
      BATCH="$2"
      BATCH_SET=1
      shift
      ;;
    --jobs)
      [ "$#" -ge 2 ] || die "--jobs requires a positive integer."
      JOBS="$2"
      shift
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      die "Unknown argument: $1"
      ;;
  esac
  shift
done

if [ "${MODE}" = "help" ]; then
  usage
  exit 0
fi

if [ "${MODE}" = "run_all" ] &&
   { [ "${DESIGN_SET}" -eq 1 ] || [ "${BATCH_SET}" -eq 1 ]; }; then
  die "--all cannot be combined with --design or --batch."
fi

case "${JOBS}" in
  ''|*[!0-9]*) die "--jobs must be a positive integer." ;;
esac
[ "${JOBS}" -ge 1 ] || die "--jobs must be a positive integer."

case "${DESIGN}" in
  factor|outcome|shock|cstar|all) ;;
  *) die "Unknown design: ${DESIGN}" ;;
esac

case "${BATCH}" in
  core|bootstrap|lava|oga|all) ;;
  *) die "Unknown batch: ${BATCH}" ;;
esac

if [ "${MODE}" = "run_subset" ] && [ "${DESIGN}" = "all" ]; then
  die "--run requires an explicit --design. Use --all for the complete run."
fi

ALL_RUNS='factor core
factor bootstrap
factor lava
outcome core
outcome bootstrap
outcome lava
shock core
shock bootstrap
shock lava
cstar oga'

GROUP_FILE="$(mktemp "${TMPDIR:-/tmp}/lpoga-local-groups.XXXXXX")"
TASK_FILE="$(mktemp "${TMPDIR:-/tmp}/lpoga-local-tasks.XXXXXX")"
trap 'rm -f "${GROUP_FILE}" "${TASK_FILE}"' EXIT

printf '%s\n' "${ALL_RUNS}" | while read -r run_design run_batch; do
  [ -n "${run_design}" ] || continue
  if { [ "${DESIGN}" = "all" ] || [ "${DESIGN}" = "${run_design}" ]; } &&
     { [ "${BATCH}" = "all" ] || [ "${BATCH}" = "${run_batch}" ]; }; then
    printf '%s %s\n' "${run_design}" "${run_batch}"
  fi
done > "${GROUP_FILE}"

GROUP_COUNT="$(wc -l < "${GROUP_FILE}" | tr -d ' ')"
[ "${GROUP_COUNT}" -ge 1 ] || die "Unsupported design/batch combination: ${DESIGN}/${BATCH}"
TASK_COUNT="$((GROUP_COUNT * 50))"

echo "Local simulation plan"
printf '%-12s %-12s %s\n' "Design" "Batch" "Task IDs"
while read -r run_design run_batch; do
  printf '%-12s %-12s %s\n' "${run_design}" "${run_batch}" "1-50"
done < "${GROUP_FILE}"
echo "Selected workload: ${GROUP_COUNT} task groups, ${TASK_COUNT} tasks; maximum ${JOBS} concurrent local tasks."

if [ "${MODE}" = "plan" ]; then
  echo "Preview only: no tasks were started."
  exit 0
fi

[ -f "${TASK_RUNNER}" ] || die "Missing task runner: ${TASK_RUNNER}"
command -v Rscript >/dev/null 2>&1 || die "Rscript was not found on PATH."
command -v xargs >/dev/null 2>&1 || die "xargs was not found on PATH."

while read -r run_design run_batch; do
  task_id=1
  while [ "${task_id}" -le 50 ]; do
    printf '%s %s %s\n' "${run_design}" "${run_batch}" "${task_id}"
    task_id=$((task_id + 1))
  done
done < "${GROUP_FILE}" > "${TASK_FILE}"

mkdir -p "${SCRIPT_DIR}/logs"
export SCRIPT_DIR TASK_RUNNER

echo "Starting ${TASK_COUNT} tasks with at most ${JOBS} running concurrently."
if ! xargs -n 3 -P "${JOBS}" sh -c '
  run_design="$1"
  run_batch="$2"
  task_id="$3"
  task_number="$(printf "%03d" "${task_id}")"
  log_file="${SCRIPT_DIR}/logs/local_${run_design}_${run_batch}_job${task_number}.log"
  bash "${TASK_RUNNER}" "${run_design}" "${run_batch}" "${task_id}" > "${log_file}" 2>&1
' sh < "${TASK_FILE}"; then
  echo "One or more simulation tasks failed. Inspect logs/local_*.log." >&2
  exit 1
fi

echo "Completed ${TASK_COUNT} tasks."
echo "Regenerate figures and tables with: Rscript run_analysis.R"
