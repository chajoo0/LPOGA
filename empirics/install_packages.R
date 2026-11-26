# Install or check the R packages used by the empirical replication.

arguments <- commandArgs(trailingOnly = TRUE)

usage <- function() {
  cat(
    "Usage:\n",
    "  Rscript install_packages.R [--profile paper|hdlp] [--list|--check]\n\n",
    "Profiles:\n",
    "  paper  R 4.4.3 environment used to render the final paper output\n",
    "  hdlp   R 4.5.2 environment used for the CRC HDLP re-estimation\n\n",
    "Actions:\n",
    "  no action  install missing or differently versioned packages\n",
    "  --list     print the required versions without changing the library\n",
    "  --check    compare the current R library with the selected profile\n",
    sep = ""
  )
}

read_profile <- function(arguments, default) {
  equals_form <- grep("^--profile=", arguments, value = TRUE)
  if (length(equals_form) > 1L) stop("Specify --profile only once.", call. = FALSE)
  if (length(equals_form) == 1L) return(sub("^--profile=", "", equals_form))

  position <- match("--profile", arguments)
  if (is.na(position)) return(default)
  if (position == length(arguments)) {
    stop("--profile must be followed by a profile name.", call. = FALSE)
  }
  arguments[position + 1L]
}

validate_arguments <- function(arguments) {
  recognized <- arguments %in% c("--list", "--check", "--help", "-h", "--profile") |
    grepl("^--profile=", arguments)
  profile_position <- match("--profile", arguments)
  if (!is.na(profile_position) && profile_position < length(arguments)) {
    recognized[profile_position + 1L] <- TRUE
  }
  unknown <- arguments[!recognized]
  if (length(unknown) > 0L) {
    stop("Unknown argument: ", paste(unknown, collapse = ", "), call. = FALSE)
  }
}

profiles <- list(
  paper = list(
    r_version = "4.4.3",
    cran = c(
      digest = "0.6.37",
      ggplot2 = "3.5.2",
      ggpattern = "1.2.1",
      magick = "2.9.0",
      patchwork = "1.3.1",
      desla = "0.3.0",
      Deriv = "4.2.0",
      tsDyn = "11.0.5.2"
    )
  ),
  hdlp = list(
    r_version = "4.5.2",
    cran = c(
      digest = "0.6.39",
      ggplot2 = "4.0.2",
      ggpattern = "1.3.1",
      magick = "2.9.0",
      patchwork = "1.3.2",
      desla = "0.3.1",
      Deriv = "4.3.6",
      tsDyn = "11.0.5.2"
    )
  )
)

hdlp_repository <- "RobertAdamek/HDLPrepro"
hdlp_version <- "1.12"
hdlp_commit <- "e9fe6071c5c91966cd7a6585a6f7bdafcb3624ba"
remotes_version <- "2.5.0"
cran_repository <- "https://cloud.r-project.org"

validate_arguments(arguments)

if ("--help" %in% arguments || "-h" %in% arguments) {
  usage()
  quit(status = 0L)
}

action_flags <- intersect(arguments, c("--list", "--check"))
if (length(action_flags) > 1L) {
  stop("Choose only one of --list and --check.", call. = FALSE)
}
action <- if (length(action_flags) == 0L) "install" else sub("^--", "", action_flags)
profile_name <- read_profile(arguments, default = "paper")

if (!profile_name %in% names(profiles)) {
  stop(
    "Unknown profile `", profile_name, "`. Choose one of: ",
    paste(names(profiles), collapse = ", "), ".",
    call. = FALSE
  )
}

profile <- profiles[[profile_name]]

print_plan <- function() {
  cat("Profile:", profile_name, "\n")
  cat("Required R:", profile$r_version, "\n")
  cat("Pinned CRAN packages:\n")
  for (package in names(profile$cran)) {
    cat("  ", package, " ", unname(profile$cran[[package]]), "\n", sep = "")
  }
  cat("Installer helper:\n")
  cat("  remotes ", remotes_version, "\n", sep = "")
  cat("Pinned GitHub package:\n")
  cat("  HDLPrepro ", hdlp_version, "\n", sep = "")
  cat("  Repository: https://github.com/", hdlp_repository, "\n", sep = "")
  cat("  Commit: ", hdlp_commit, "\n", sep = "")
}

installed_version <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) return(NA_character_)
  as.character(utils::packageVersion(package))
}

version_matches <- function(installed, required) {
  if (is.na(installed)) return(FALSE)
  base::numeric_version(installed) == base::numeric_version(required)
}

check_environment <- function() {
  problems <- character()
  current_r <- paste(R.version$major, R.version$minor, sep = ".")
  if (!identical(current_r, profile$r_version)) {
    problems <- c(
      problems,
      paste0("R: required ", profile$r_version, ", found ", current_r)
    )
  }

  for (package in names(profile$cran)) {
    required <- unname(profile$cran[[package]])
    installed <- installed_version(package)
    if (!version_matches(installed, required)) {
      found <- if (is.na(installed)) "not installed" else installed
      problems <- c(
        problems,
        paste0(package, ": required ", required, ", found ", found)
      )
    }
  }

  installed_hdlp <- installed_version("HDLPrepro")
  if (!version_matches(installed_hdlp, hdlp_version)) {
    found <- if (is.na(installed_hdlp)) "not installed" else installed_hdlp
    problems <- c(
      problems,
      paste0("HDLPrepro: required ", hdlp_version, ", found ", found)
    )
  } else {
    remote_sha <- utils::packageDescription("HDLPrepro", fields = "RemoteSha")
    if (is.na(remote_sha) || !startsWith(hdlp_commit, remote_sha)) {
      found <- if (is.na(remote_sha)) "not recorded" else remote_sha
      problems <- c(
        problems,
        paste0("HDLPrepro commit: required ", hdlp_commit, ", found ", found)
      )
    }
  }

  problems
}

if (identical(action, "list")) {
  print_plan()
  quit(status = 0L)
}

if (identical(action, "check")) {
  print_plan()
  problems <- check_environment()
  if (length(problems) == 0L) {
    cat("Environment check passed.\n")
    quit(status = 0L)
  }
  cat("Environment check failed:\n")
  cat(paste0("  - ", problems, collapse = "\n"), "\n")
  quit(status = 1L)
}

current_r <- paste(R.version$major, R.version$minor, sep = ".")
if (!identical(current_r, profile$r_version)) {
  stop(
    "This profile requires R ", profile$r_version, "; the current R is ",
    current_r, ". Start the required R version and rerun this script.",
    call. = FALSE
  )
}

if (!requireNamespace("remotes", quietly = TRUE)) {
  utils::install.packages("remotes", repos = cran_repository)
}
if (!version_matches(installed_version("remotes"), remotes_version)) {
  remotes::install_version(
    "remotes", version = remotes_version, repos = cran_repository,
    dependencies = NA, upgrade = "never"
  )
}

for (package in names(profile$cran)) {
  required <- unname(profile$cran[[package]])
  if (!version_matches(installed_version(package), required)) {
    remotes::install_version(
      package, version = required, repos = cran_repository,
      dependencies = NA, upgrade = "never"
    )
  }
}

remote_sha <- if (requireNamespace("HDLPrepro", quietly = TRUE)) {
  utils::packageDescription("HDLPrepro", fields = "RemoteSha")
} else {
  NA_character_
}
hdlp_is_current <- version_matches(installed_version("HDLPrepro"), hdlp_version) &&
  !is.na(remote_sha) && startsWith(hdlp_commit, remote_sha)

if (!hdlp_is_current) {
  remotes::install_github(
    hdlp_repository, ref = hdlp_commit,
    dependencies = NA, upgrade = "never"
  )
}

problems <- check_environment()
if (length(problems) > 0L) {
  stop(
    "Installation completed, but the environment check still reports:\n",
    paste0("  - ", problems, collapse = "\n"),
    call. = FALSE
  )
}

cat("Installation and environment check completed successfully.\n")
