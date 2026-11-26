# Third-party software and material

This replication calls external R packages but does not redistribute their
source code.

| Component | Version used | License reported by package | Role |
|---|---:|---|---|
| HDLPrepro | 1.12, GitHub commit `e9fe6071c5c91966cd7a6585a6f7bdafcb3624ba` | GPL (>= 2) | FAVAR implementation and upstream replication support |
| desla | 0.3.0 (paper output); 0.3.1 (HDLP run) | GPL (>= 2) | optional debiased-Lasso/HDLP implementation |
| bigtime | 0.2.3 | GPL (>= 2) | HDLPrepro dependency |
| rrpack | 0.1-14 | GPL (>= 3) | HDLPrepro dependency |
| digest | 0.6.37 (paper output); 0.6.39 (HDLP run) | GPL (>= 2) | SHA-256 verification |
| ggplot2 | 3.5.2 (paper output); 4.0.2 (HDLP run) | MIT | plotting |
| ggpattern | 1.2.1 (paper output); 1.3.1 (HDLP run) | MIT | patterned confidence bands |
| magick | 2.9.0 | MIT | pattern rendering |
| patchwork | 1.3.1 (paper output); 1.3.2 (HDLP run) | MIT | plot composition |

The compact files in `results/archived/` are numerical HDLP outputs, not
package code. They contain only reported point estimates and interval bounds,
plus horizon, specification, and provenance fields. No rights are claimed in
HDLPrepro, desla, or FAVAR implementations.

The included `data/CDbmedium.RData` was obtained from the cited Adamek et al.
supplementary materials and is redistributed here with permission from the
authors of that source package. It remains third-party material and is not
covered by this folder's MIT license.
