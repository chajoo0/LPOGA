# Included empirical data

This directory contains the empirical input in its original R format and in open CSV format:

```text
CDbmedium.RData         processed analysis input used by the R code
CDbmedium.csv           open-format copy of CDbmedium$data_all
CDbmedium_series.csv    description of every column in the CSV
```

`CDbmedium.RData` was obtained without modification from:

```text
HDLPrepro/inst/extdata/CDbmedium.RData
```

in HDLPrepro 1.12, the replication materials for Adamek, Smeekes, and Wilms (2024). The source package is available from the [HDLPrepro repository](https://github.com/RobertAdamek/HDLPrepro) and is fixed here at [commit `e9fe6071c5c91966cd7a6585a6f7bdafcb3624ba`](https://github.com/RobertAdamek/HDLPrepro/tree/e9fe6071c5c91966cd7a6585a6f7bdafcb3624ba), dated 8 March 2024; the article is available at <https://doi.org/10.1093/ectj/utae012>. The source data file was downloaded on 18 January 2026, and the repository was accessed on 21 September 2026. The source file is redistributed in this package with permission from the authors of the source replication package.

The replication verifies the following checksums before loading the file:

```text
SHA-256  a721ee8e6d07f59b8d163d1892846844875bae444165f07abd0d49a5c9e15411
MD5      90694a3aef6673b32784b6fc52b2a82c
```

The underlying data are from the September 2021 FRED-MD release downloaded by Adamek, Smeekes, and Wilms (2024). Their preprocessing produces the 586 monthly observations from January 1960 through October 2008 used here. The dataset contains 122 transformed series: 67 slow variables, the federal funds rate, and 54 fast variables. The `lag_FFR` column is a derived one-month lag of the transformed federal funds rate; its first observation uses the December 1959 value that precedes the exported estimation sample.

`CDbmedium_series.csv` records each CSV column in order. For the 122 FRED-MD series it gives the series name, description, economic category, slow/fast classification, and transformation used by Adamek, Smeekes, and Wilms (2024). The transformation codes are:

| Code | Transformation |
|---:|---|
| 1 | Level |
| 2 | First difference |
| 3 | Second difference |
| 4 | Natural logarithm |
| 5 | First difference of the natural logarithm |
| 6 | Second difference of the natural logarithm |
| 7 | Proportional change, `x[t] / x[t-1] - 1` |

The descriptions, transformation choices, and slow/fast classifications follow `processing_FREDMD.R` in the Adamek, Smeekes, and Wilms (2024) replication materials. The present replication does not repeat their raw-data processing. Run the following command from the `empirics/` directory to recreate `CDbmedium.csv` from the included RData file and verify it against the codebook:

```bash
Rscript analysis/export_empirical_data_csv.R
```

Permission to redistribute `CDbmedium.RData` does not place the data under the MIT license for the original replication code. See [`../README.md`](../README.md) for the complete citations.
