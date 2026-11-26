#!/usr/bin/env bash

set -eu

if [ "$#" -ne 3 ]; then
  echo "Usage: $0 {factor|outcome|shock|cstar} {core|bootstrap|lava|oga} TASK_ID" >&2
  exit 2
fi

DESIGN="$1"
BATCH="$2"
TASK_ID="$3"

case "${TASK_ID}" in
  ''|*[!0-9]*)
    echo "TASK_ID must be a positive integer." >&2
    exit 2
    ;;
esac

if [ "${TASK_ID}" -lt 1 ]; then
  echo "TASK_ID must be a positive integer." >&2
  exit 2
fi

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
PROJECT_ROOT="$(dirname -- "${SCRIPT_DIR}")"

export REPS_PER_JOB="${REPS_PER_JOB:-20}"
export NUM_HORIZONS="${NUM_HORIZONS:-12}"
export HAC_LAGS="${HAC_LAGS:-4}"
export FAVAR_BOOTSTRAPS="${FAVAR_BOOTSTRAPS:-100}"
export RSLP_DRAWS="${RSLP_DRAWS:-1000}"
export RSLP_DIMENSION="${RSLP_DIMENSION:-50}"
export RSLP_BOOTSTRAPS="${RSLP_BOOTSTRAPS:-100}"
export AIC_MAX_LAGS="${AIC_MAX_LAGS:-20}"

case "${DESIGN}:${BATCH}" in
  factor:core)
    export METHODS="ols:aic_bc:lalp:falp:oga:dlasso"
    export RUN_LABEL="unionvhat"
    JOB="job_factor_contamination.R"
    ;;
  factor:bootstrap)
    export METHODS="favar:rslp"
    export RUN_LABEL=""
    JOB="job_factor_contamination.R"
    ;;
  factor:lava)
    export METHODS="lava"
    export LAVA_BOOTSTRAPS="0"
    export RUN_LABEL="factor_lava_point"
    JOB="job_factor_contamination.R"
    ;;
  outcome:core)
    export METHODS="ols:aic_bc:lalp:falp:oga:dlasso"
    export OUTCOME_KZ="20"
    export OUTCOME_DECAY="1.5"
    export RUN_LABEL="kz20"
    JOB="job_outcome_nuisance.R"
    ;;
  outcome:bootstrap)
    export METHODS="favar:rslp"
    export OUTCOME_KZ="20"
    export OUTCOME_DECAY="1.5"
    export RUN_LABEL="kz20"
    JOB="job_outcome_nuisance.R"
    ;;
  outcome:lava)
    export METHODS="lava"
    export OUTCOME_KZ="20"
    export OUTCOME_DECAY="1.5"
    export LAVA_BOOTSTRAPS="0"
    export RUN_LABEL="kz20_lava_point"
    JOB="job_outcome_nuisance.R"
    ;;
  shock:core)
    export METHODS="ols:aic_bc:lalp:falp:oga:dlasso"
    export SHOCK_POLYNOMIAL_POWER="1.5"
    export RUN_LABEL="unionvhat"
    JOB="job_shock_contamination.R"
    ;;
  shock:bootstrap)
    export METHODS="favar:rslp"
    export SHOCK_POLYNOMIAL_POWER="1.5"
    export RUN_LABEL=""
    JOB="job_shock_contamination.R"
    ;;
  shock:lava)
    export METHODS="lava"
    export SHOCK_POLYNOMIAL_POWER="1.5"
    export LAVA_BOOTSTRAPS="0"
    export RUN_LABEL="poly150_lava_point"
    JOB="job_shock_contamination.R"
    ;;
  cstar:oga)
    export METHODS="oga"
    export SHOCK_POLYNOMIAL_POWER="1.5"
    export CSTAR_VALUES="${CSTAR_VALUES:-1.6,1.8,2.0,2.2,2.4}"
    export RUN_LABEL="cstar_robustness"
    JOB="job_shock_cstar_robustness.R"
    ;;
  *)
    echo "Unsupported design/batch combination: ${DESIGN}/${BATCH}" >&2
    exit 2
    ;;
esac

mkdir -p "${PROJECT_ROOT}/results/simulation" "${PROJECT_ROOT}/logs"
cd "${PROJECT_ROOT}"
exec Rscript "cluster/${JOB}" "${TASK_ID}"
