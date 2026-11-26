#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
PROJECT_ROOT="$(dirname -- "${SCRIPT_DIR}")"
WORKER_SCRIPT="${SCRIPT_DIR}/sge_array_job.sh"
QSUB_BIN="${QSUB_BIN:-qsub}"

MODE="help"
DESIGN="all"
BATCH="all"
MAX_CONCURRENT=10
DESIGN_SET=0
BATCH_SET=0

usage() {
  cat <<'EOF'
Usage:
  bash cluster/submit_simulations.sh --plan [selection options]
  bash cluster/submit_simulations.sh --all [--max-concurrent N]
  bash cluster/submit_simulations.sh --submit --design DESIGN [--batch BATCH] [--max-concurrent N]

Actions:
  --plan                 Show the selected arrays without submitting jobs.
  --all                  Submit the complete simulation run (10 arrays, 500 tasks).
  --submit               Submit the subset selected by --design and --batch.

Selection options:
  --design DESIGN        factor, outcome, shock, cstar, or all.
  --batch BATCH          core, bootstrap, lava, oga, or all.
  --max-concurrent N     Maximum simultaneously running tasks in each array.

Calling this script without an action only prints this message. It never
submits jobs implicitly.
EOF
}

die() {
  echo "Error: $*" >&2
  exit 2
}

while [ "$#" -gt 0 ]; do
  case "$1" in
    --plan)
      [ "${MODE}" = "help" ] || die "Specify only one of --plan, --all, or --submit."
      MODE="plan"
      ;;
    --all)
      [ "${MODE}" = "help" ] || die "Specify only one of --plan, --all, or --submit."
      MODE="submit_all"
      DESIGN="all"
      BATCH="all"
      ;;
    --submit)
      [ "${MODE}" = "help" ] || die "Specify only one of --plan, --all, or --submit."
      MODE="submit_subset"
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
    --max-concurrent)
      [ "$#" -ge 2 ] || die "--max-concurrent requires a positive integer."
      MAX_CONCURRENT="$2"
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

if [ "${MODE}" = "submit_all" ] &&
   { [ "${DESIGN_SET}" -eq 1 ] || [ "${BATCH_SET}" -eq 1 ]; }; then
  die "--all cannot be combined with --design or --batch."
fi

case "${MAX_CONCURRENT}" in
  ''|*[!0-9]*) die "--max-concurrent must be a positive integer." ;;
esac
[ "${MAX_CONCURRENT}" -ge 1 ] || die "--max-concurrent must be a positive integer."

case "${DESIGN}" in
  factor|outcome|shock|cstar|all) ;;
  *) die "Unknown design: ${DESIGN}" ;;
esac

case "${BATCH}" in
  core|bootstrap|lava|oga|all) ;;
  *) die "Unknown batch: ${BATCH}" ;;
esac

if [ "${MODE}" = "submit_subset" ] && [ "${DESIGN}" = "all" ]; then
  die "--submit requires an explicit --design. Use --all for the complete run."
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

SELECTED_RUNS=""
while read -r run_design run_batch; do
  [ -n "${run_design}" ] || continue
  if { [ "${DESIGN}" = "all" ] || [ "${DESIGN}" = "${run_design}" ]; } &&
     { [ "${BATCH}" = "all" ] || [ "${BATCH}" = "${run_batch}" ]; }; then
    SELECTED_RUNS="${SELECTED_RUNS}${run_design} ${run_batch}
"
  fi
done <<EOF
${ALL_RUNS}
EOF

[ -n "${SELECTED_RUNS}" ] || die "Unsupported design/batch combination: ${DESIGN}/${BATCH}"

ARRAY_COUNT="$(printf '%s' "${SELECTED_RUNS}" | sed '/^[[:space:]]*$/d' | wc -l | tr -d ' ')"
TASK_COUNT="$((ARRAY_COUNT * 50))"

echo "Simulation submission plan"
echo "Project root: ${PROJECT_ROOT}"
printf '%-12s %-12s %s\n' "Design" "Batch" "Array tasks"
printf '%s' "${SELECTED_RUNS}" | while read -r run_design run_batch; do
  [ -n "${run_design}" ] || continue
  printf '%-12s %-12s %s\n' "${run_design}" "${run_batch}" "1-50"
done

if [ "${ARRAY_COUNT}" -eq 1 ]; then
  echo "Selected workload: 1 array, 50 tasks; maximum ${MAX_CONCURRENT} concurrent tasks per array."
else
  echo "Selected workload: ${ARRAY_COUNT} arrays, ${TASK_COUNT} tasks; maximum ${MAX_CONCURRENT} concurrent tasks per array."
fi

if [ "${MODE}" = "plan" ]; then
  echo "Preview only: no jobs were submitted."
  exit 0
fi

[ -f "${WORKER_SCRIPT}" ] || die "Missing worker script: ${WORKER_SCRIPT}"
command -v "${QSUB_BIN}" >/dev/null 2>&1 || die "qsub was not found. Run this command on an SGE login node."

mkdir -p "${PROJECT_ROOT}/logs"
SUBMITTED=0
printf '%s' "${SELECTED_RUNS}" | while read -r run_design run_batch; do
  [ -n "${run_design}" ] || continue
  job_name="lpoga_${run_design}_${run_batch}"
  job_id="$(${QSUB_BIN} -terse \
    -N "${job_name}" \
    -q long \
    -pe smp 1 \
    -t 1-50 \
    -tc "${MAX_CONCURRENT}" \
    -o "${PROJECT_ROOT}/logs" \
    -e "${PROJECT_ROOT}/logs" \
    -v "PROJECT_ROOT=${PROJECT_ROOT},SIM_DESIGN=${run_design},SIM_BATCH=${run_batch}" \
    "${WORKER_SCRIPT}")"
  echo "Submitted ${run_design}/${run_batch}: ${job_id}"
done

echo "Submitted ${ARRAY_COUNT} arrays (${TASK_COUNT} tasks)."
echo "After all arrays finish, run: Rscript run_analysis.R"
