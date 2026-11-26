#!/bin/bash -l
#$ -m n

set -euo pipefail

: "${PROJECT_ROOT:?PROJECT_ROOT was not supplied by submit_simulations.sh}"
: "${SIM_DESIGN:?SIM_DESIGN was not supplied by submit_simulations.sh}"
: "${SIM_BATCH:?SIM_BATCH was not supplied by submit_simulations.sh}"
: "${SGE_TASK_ID:?SGE_TASK_ID is not available}"

module purge
module load R/4.5.2/gcc/11.5.0

mkdir -p "${PROJECT_ROOT}/logs"
cd "${PROJECT_ROOT}"

exec bash cluster/run_array_task.sh "${SIM_DESIGN}" "${SIM_BATCH}" "${SGE_TASK_ID}"
