# Monetary policy empirical replication

This folder reproduces the monetary policy empirical application. The default run re-estimates double OGA, FALP, and FAVAR, uses the archived HDLP estimates from the full CRC run, and recreates the impulse response figure and OGA selection diagnostics.

The required empirical input and its open-format CSV copy are included in `data/`. Variable descriptions, transformation codes, source information, and the redistribution statement are given in [`data/README.md`](data/README.md).

## Reproducing the paper output

Run the following command from the `empirics/` directory:

```bash
Rscript run_analysis.R
```

Under the default setting, the script reads the archived HDLP estimates from `results/archived/`. These files contain only point estimates, interval bounds, horizons, specifications, and provenance fields. They do not contain input data, regression matrices, residuals, or package source code.

The default workflow reads the archived `.rds` files. Corresponding `.csv` files are also included for reference. They can be regenerated from the RDS files by running:

```bash
Rscript analysis/export_archived_hdlp_csv.R
```

## Contents

```text
README.md                  instructions, settings, and source information
LICENSE                    MIT license for original code in this folder
THIRD_PARTY_SOFTWARE.md    ownership and licenses of external software
install_packages.R         lists, installs, or checks pinned package versions
run_analysis.R             reproduces the empirical figures and tables
data/                      included empirical input and provenance statement
R/                         OGA, FALP, data-check, and result-helper functions
analysis/                  estimation, figure, table, and CSV-export scripts
results/archived/          HDLP estimates in RDS and CSV formats
results/estimates/         regenerated OGA, FALP, and FAVAR estimates
results/figures/           impulse response figure and selection heatmaps
results/tables/            OGA selection diagnostics
logs/                      runtime and R session information
```

## Inputs and provenance

The included empirical data files are:

```text
data/CDbmedium.RData         analysis input used by the R code
data/CDbmedium.csv           open-format copy of the complete data table
data/CDbmedium_series.csv    variable descriptions and transformations
```

`CDbmedium.RData` was obtained without modification from the following location in HDLPrepro 1.12, the replication package for Adamek, Smeekes, and Wilms (2024):

```text
HDLPrepro/inst/extdata/CDbmedium.RData
```

The source package is the [HDLPrepro GitHub repository](https://github.com/RobertAdamek/HDLPrepro), fixed at [commit `e9fe6071c5c91966cd7a6585a6f7bdafcb3624ba`](https://github.com/RobertAdamek/HDLPrepro/tree/e9fe6071c5c91966cd7a6585a6f7bdafcb3624ba) dated 8 March 2024. The package reports version 1.12 and package date 22 May 2023. The source data file was downloaded on 18 January 2026, and the repository was accessed on 21 September 2026.

The file is redistributed in this package with permission from the authors of the source replication package. The replication checks both file hashes and stops if either differs:

```text
SHA-256  a721ee8e6d07f59b8d163d1892846844875bae444165f07abd0d49a5c9e15411
MD5      90694a3aef6673b32784b6fc52b2a82c
```

The RData file contains 586 monthly observations from January 1960 through October 2008. It contains 122 transformed FRED-MD series: 67 slow variables, the federal funds rate, and 54 fast variables. The final `lag_FFR` column is the one-month lag of the transformed federal funds rate. `CDbmedium.csv` reproduces this complete rectangular table, including the dates and derived lag. `CDbmedium_series.csv` gives the series descriptions, slow/fast classification, transformation codes, and plain-language transformation definitions.

The underlying data come from the September 2021 vintage of FRED-MD, obtained through the replication materials of Adamek, Smeekes, and Wilms (2024), downloaded on 18 January 2026. Variable classifications and transformations follow their `processing_FREDMD.R` script. This replication uses their processed data file and does not repeat the upstream raw-data processing. Both FRED-MD and the source replication materials are cited in the References section.

To regenerate and verify the open-format data copy, run the following command from the `empirics/` directory:

```bash
Rscript analysis/export_empirical_data_csv.R
```

## Specification and implementation

The upstream package's `processing_FREDMD.R` documents the raw-data transformations and the fast/slow classification. This replication extracts:

- slow controls from `CDbmedium$slow_data`;
- fast controls from `CDbmedium$fast_data`;
- the monetary policy shock from `CDbmedium$FFR`;
- the federal funds rate, `INDPRO`, and `CPIAUCSL` as responses.

All methods use 13 lags and horizons 0–49. Industrial production and CPI responses are cumulative; the federal-funds-rate response is not. Double OGA always includes the 67 contemporaneous slow variables, applies OGA with HDAIC to the lagged information set, and uses the union of the shock- and outcome-equation selections. The intercept raises the corresponding design-matrix count to 68. Double OGA standard errors use a Parzen HAC estimator. FALP uses six factors. FAVAR uses 499 bootstrap draws and `set.seed(1)`.

## Rerunning the full analysis

To recompute the HDLP estimates and use the newly generated files immediately in Figure 4, run:

```bash
N_THREADS=16 Rscript run_analysis.R --run-hdlp
```

`N_THREADS` controls the number of parallel workers used by the HDLP routines. It may be set to another positive integer appropriate for the machine. If it is omitted, the script uses two fewer than the detected number of cores, with a minimum of one. The full run requires `desla`; all other methods are also re-estimated. Newly generated HDLP files are written to `results/estimates/`, while the archived files in `results/archived/` remain unchanged.

## Outputs

The paper and appendix exhibits map to the package output as follows.

| Paper exhibit | Package output |
|---|---|
| Main text, “Monetary shock results” (`fig:app_monetary`) | `results/figures/monetary_policy_irfs.pdf` |
| Online Appendix, “Number of selected covariates across horizons in the monetary policy application” (`tab:app_empiric_monetary_numselect`) | `results/tables/selection_count_table.tex` |
| Online Appendix, “Selected covariates — federal funds rate” (`fig:app_empiric_monetary_heat_ffr`) | `results/figures/oga_selection_heatmap_FFR.pdf`, `.png` |
| Online Appendix, “Selected covariates — industrial production” (`fig:app_empiric_monetary_heat_ip`) | `results/figures/oga_selection_heatmap_IP.pdf`, `.png` |
| Online Appendix, “Selected covariates — consumer price index” (`fig:app_empiric_monetary_heat_cpi`) | `results/figures/oga_selection_heatmap_CPI.pdf`, `.png` |

The combined heatmap and the CSV files in `results/tables/` are supporting outputs. `selection_counts_by_horizon.csv` records separate shock, outcome, union, 67-covariate, and 68-with-intercept counts for every response and horizon. `shock_selection.csv` and `outcome_selection_by_horizon.csv` contain the selected-variable identities.

The reproduced selection summary is:

| Response | Shock selected | Outcome median (range) | Union median (range) | Always included |
|---|---:|---:|---:|---:|
| FFR | 9 | 12 (6–28) | 21 (15–37) | 67 |
| IP | 9 | 11 (2–21) | 20 (11–30) | 67 |
| CPI | 9 | 17 (5–31) | 26 (13–40) | 67 |

The intercept is additionally included, so the corresponding design-matrix count is 68.

## Software and runtime

Two recorded environments were used. The `paper` profile is the Apple silicon environment used for the final Figure 4 PDF and the default workflow. The `hdlp` profile is the CRC environment used to produce the HDLP results. The package versions directly used by the replication are:

| Package | Role | `paper` profile, R 4.4.3 | `hdlp` profile, R 4.5.2 |
|---|---|---:|---:|
| digest | input-file checksum | 0.6.37 | 0.6.39 |
| ggplot2 | plotting | 3.5.2 | 4.0.2 |
| ggpattern | patterned confidence bands | 1.2.1 | 1.3.1 |
| magick | pattern rendering | 2.9.0 | 2.9.0* |
| patchwork | plot composition | 1.3.1 | 1.3.2 |
| desla | HDLP estimation | 0.3.0 | 0.3.1 |
| Deriv | archived HDLPrepro installation dependency | 4.2.0 | 4.3.6 |
| tsDyn | HDLPrepro and FAVAR dependency | 11.0.5.2 | 11.0.5.2 |
| HDLPrepro | FAVAR and upstream replication routines | 1.12 | 1.12 |
| remotes | installation helper only | 2.5.0 | 2.5.0 |

HDLPrepro is installed from the exact GitHub commit stated under “Inputs and provenance.” The complete loaded dependency environments, including transitive packages, are recorded in `logs/session_info.txt` and `logs/crc_hdlp_session_info.txt`. The installer pins the packages in the table; it does not claim to reconstruct an unrecorded lockfile for every transitive dependency.

`tsDyn` and `Deriv` are installed at the recorded versions before HDLPrepro. They are pinned explicitly because the versions required by the recorded environments are not resolved reliably from the current CRAN repository. Other HDLPrepro dependencies are installed through its package metadata.

From the `empirics/` directory, list a profile without changing the R library:

```bash
Rscript install_packages.R --list --profile paper
Rscript install_packages.R --list --profile hdlp
```

Install the recorded paper-output environment under R 4.4.3 and verify it with:

```bash
Rscript install_packages.R --profile paper
Rscript install_packages.R --check --profile paper
```

For full HDLP re-estimation, start R 4.5.2 and use:

```bash
Rscript install_packages.R --profile hdlp
Rscript install_packages.R --check --profile hdlp
N_THREADS=16 Rscript run_analysis.R --run-hdlp
```

The `hdlp` installer includes `magick` 2.9.0 because the current workflow proceeds from estimation to figure construction. The final Figure 4 PDF was rendered in the `paper` environment; `magick` was not loaded in the recorded CRC estimation session and does not affect the estimates.

Packages are installed from source when an exact archived version is unavailable as a binary. On Linux, `magick` requires the ImageMagick development libraries. `ggpattern` may require the system libraries used by `sf`, including GDAL, GEOS, PROJ, and UDUNITS. HDLPrepro dependencies may require CMake and standard compilation tools. These system libraries are supplied by the operating system or cluster module system and are not installed by the R script.

The archived HDLP estimates were generated on the University of Notre Dame CRC cluster using 16 threads. The run used Red Hat Enterprise Linux 9.7, R 4.5.2 compiled with GCC 11.5.0, and `set.seed(1)`.

The full run took an elapsed time of 9,844.3 seconds (approximately 2 hours 44 minutes). The complete records are in:

```text
logs/crc_hdlp_runtime.txt
logs/crc_hdlp_session_info.txt
```

The verified default run, which uses the archived HDLP estimates, took approximately 3.2 minutes on the Apple Silicon laptop. Runtime varies with the processor, memory, number of threads, package versions, and system load.

## References

Adamek, R., Smeekes, S., and Wilms, I. (2024). “Local Projection Inference in High Dimensions.” *The Econometrics Journal* 27(3), 323–342. <https://doi.org/10.1093/ectj/utae012>.

Adamek, R., Smeekes, S., and Wilms, I. (2024). *HDLPrepro: Replication Package for “Local Projection Inference in High Dimensions.”* GitHub repository, version 1.12, commit [`e9fe6071c5c91966cd7a6585a6f7bdafcb3624ba`](https://github.com/RobertAdamek/HDLPrepro/tree/e9fe6071c5c91966cd7a6585a6f7bdafcb3624ba), dated 8 March 2024. The file used here is `HDLPrepro/inst/extdata/CDbmedium.RData`; it was downloaded on 18 January 2026. The repository was accessed on 21 September 2026.

Federal Reserve Bank of St. Louis (2021). “FRED-MD: September 2021 Vintage.” FRED-MD database. <https://www.stlouisfed.org/research/economists/mccracken/fred-databases>. Processed copy obtained from the replication materials of Adamek, Smeekes, and Wilms on January 18, 2026.

McCracken, M. W., and Ng, S. (2016). “FRED-MD: A Monthly Database for Macroeconomic Research.” *Journal of Business & Economic Statistics* 34(4), 574–589. <https://doi.org/10.1080/07350015.2015.1086655>.

## Third-party material and license

The MIT license in `LICENSE` applies only to the original code in this folder. It does not apply to `data/CDbmedium.RData`, the Adamek et al. package, HDLPrepro, desla, or any other third-party material. The data file is included with permission from the authors of the source replication package. See `THIRD_PARTY_SOFTWARE.md` for ownership and license information.
