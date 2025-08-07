
## ************************************************************************** ##
## *~* WORK IN PROGRESS *~*
## These functions attempt to provide a standardized and reusable structure
## for writing testthat tests for SpaDES modules.
## Eventually they may be developed into a new testing template for modules
## to make available to all users.
## ************************************************************************** ##

#' SpaDES test: Set up global options
#'
#' Set local global options for testing. See **Details**.
#'
#' This function was designed to be called in a \code{tests/testthat/setup.R} file.
#'
#' The defaults aim to provide an optimal environment for both standard
#' and interactive testing.
#'
#' @param reproducible.useMemoise       reproducible.useMemoise R package global option.
#' @param reproducible.verbose          reproducible.verbose R package global option.
#' @param Require.verbose               Require.verbose R package global option.
#' @param Require.cloneFrom             Require.cloneFrom R package global option.
#' @param spades.moduleCodeChecks       spades.moduleCodeChecks R package global option.
#' @param spades.moduleDocument         spades.moduleDocument R package global option.
#' @param SpaDES.project.updateRprofile SpaDES.project R package global option.
#'
#' @param teardownEnv environment. Optional. Environment to use for scoping.
#' The default for testing is the \code{testthat::teardown_env()}.
#'
#' @export
SpaDEStestSetGlobalOptions <- function(
    reproducible.useMemoise       = TRUE,
    reproducible.verbose          = if (testthat::is_testing()) -2,
    Require.verbose               = if (testthat::is_testing()) -2,
    Require.cloneFrom             = Sys.getenv("R_LIBS_USER"),
    spades.moduleCodeChecks       = if (testthat::is_testing()) FALSE,
    spades.moduleDocument         = FALSE,
    SpaDES.project.updateRprofile = FALSE,
    teardownEnv = if (testthat::is_testing()) testthat::teardown_env()){

  # Set global options
  localOptions <- list(
    reproducible.useMemoise       = reproducible.useMemoise,
    reproducible.verbose          = reproducible.verbose,
    Require.verbose               = Require.verbose,
    Require.cloneFrom             = Require.cloneFrom,
    spades.moduleCodeChecks       = spades.moduleCodeChecks,
    spades.moduleDocument         = spades.moduleDocument,
    SpaDES.project.updateRprofile = SpaDES.project.updateRprofile
  )
  localOptions <- localOptions[!sapply(localOptions, is.null)]
  #localOptions <- localOptions[!names(localOptions) %in% names(options())]

  if (!is.null(teardownEnv)){
    withr::local_options(localOptions, .local_envir = teardownEnv)
  }else{
    options(localOptions)
  }
}


#' SpaDES test: Set up directories
#'
#' List test data directories and set up a temporary directory structure
#' that will be removed on test teardown. See **Details**.
#'
#' This function was designed to be called in a \code{tests/testthat/setup.R} file.
#'
#' Module(s) will be copied to a temporary testing directory for testing.
#'
#' @param modulePath character.
#' By default, it is assumed that the working directory is a module directory.
#' Otherwise, provide a directory path (absolute or relative to the R project root)
#' that contains modules to be tested.
#' Set to NA to not copy any modules for testing.
#' @param modules character. Module(s) to copy for testing.
#' Defaults to the working directory module.
#' If \code{modulePath} is provided, all modules in this directory are included by default.
#' @param testPaths file or directory paths within the \code{tests/testthat}
#' directory to add to the file list.
#' By default a test data directory is included with location \code{tests/testthat/testdata}.
#' @param inputPath character. Optional alternative location of the input data directory.
#' Defaults to \code{getOption("spades.test.paths.inputs")}.
#' This allows users to speed up testing by allowing inputs to persist between test runs.
#' @param cachePath character. Optional alternative location of the cache directory.
#' Defaults to \code{getOption("spades.test.paths.cache")}.
#' This allows users to speed up testing by allowing cache to persist between test runs.
#' @param packagePath character. Optional alternative location of the R packages directory.
#' Defaults to \code{getOption("spades.test.paths.packages")}.
#' This allows users to speed up testing by allowing packages to persist between test runs.
#' @param tempDir character. Optional. Path to location of temporary test directory.
#' @param teardownEnv environment. Optional. Environment to use for scoping.
#' The default for testing is the \code{testthat::teardown_env()}.
#'
#' @return list of test paths
#' @export
SpaDEStestSetUpDirectories <- function(
    modulePath  = NULL,
    modules     = NULL,
    testPaths   = "testdata",
    inputPath   = getOption("spades.test.paths.inputs"),
    cachePath   = getOption("spades.test.paths.cache"),
    packagePath = getOption("spades.test.paths.packages"),
    tempDir     = tempdir(),
    teardownEnv = if (testthat::is_testing()) testthat::teardown_env()){

  # Set testing paths
  spadesTestPaths <- .test_directories(
    tempDir     = tempDir,
    testPaths   = testPaths,
    inputPath   = inputPath,
    cachePath   = cachePath,
    packagePath = packagePath
  )

  # Create temporary directories
  dir.create(spadesTestPaths$temp$root, recursive = TRUE)
  for (d in spadesTestPaths$temp) dir.create(d, showWarnings = FALSE)

  # Test module(s) in place if interactive
  if (interactive() & is.null(modulePath)){
    spadesTestPaths$modulePath  <- dirname(spadesTestPaths$RProj)
  }

  # Copy modules to temporary directory
  if (is.null(modulePath)){

    # R Project is a module
    modulePath <- dirname(spadesTestPaths$RProj)

    if (is.null(modules)){
      modules <- basename(spadesTestPaths$RProj)

      # Test module in place if interactive
      if (interactive()) spadesTestPaths$modulePath  <- dirname(spadesTestPaths$RProj)
    }

  }else if (!is.na(modulePath)){

    # R Project has a directory containing modules
    modulePathRel <- normalizePath(file.path(spadesTestPaths$RProj, modulePath), mustWork = FALSE)
    modulePath <- ifelse(file.exists(modulePathRel), modulePathRel, modulePath)
    if (is.null(modules)) modules <- list.dirs(modulePath, recursive = FALSE, full.names = FALSE)
  }

  # Copy module(s) to the temporary testing directory
  if (!is.na(modulePath)) for (module in modules){
    .copyModule(
      modulePath = modulePath,
      moduleName = module,
      destDir    = spadesTestPaths$temp$modules,
      overwrite  = TRUE
    )
  }

  # Remove temporary directories on test teardown
  ## NOTE: Temporary R packages loaded and/or attached to the environment
  ## may stop the library directory from being removed.
  if (!is.null(teardownEnv)){
    withr::defer({
      unlink(spadesTestPaths$temp$root, recursive = TRUE)
      if (file.exists(spadesTestPaths$temp$root)) warning(
        "Temporary test directory could not be removed: ",
        spadesTestPaths$temp$root, call. = FALSE)
    }, envir = teardownEnv, priority = "last")
  }

  # Restore library paths on teardown
  if (!is.null(teardownEnv)){
    libPathsInit <- .libPaths()
    withr::defer(.libPaths(libPathsInit), envir = teardownEnv, priority = "last")
  }

  # Return test directories
  spadesTestPaths
}

# Set test directory paths
.test_directories <- function(
    testPaths   = NULL,
    tempDir     = tempdir(),
    inputPath   = getOption("spades.test.paths.inputs"),
    cachePath   = getOption("spades.test.paths.cache"),
    packagePath = getOption("spades.test.paths.packages")){

  if (!is.null(inputPath))   inputPath   <- normalizePath(inputPath)
  if (!is.null(packagePath)) packagePath <- normalizePath(packagePath)
  if (!is.null(cachePath))   cachePath   <- normalizePath(cachePath)

  # Set R project root (module or R package)
  ## SpaDES will require absolute paths
  spadesTestPaths <- list(
    RProj = normalizePath(testthat::test_path("../.."))
  )

  # Set custom test paths
  for (testPath in testPaths){
    spadesTestPaths[[testPath]] <- normalizePath(testthat::test_path(testPath), mustWork = FALSE)
  }

  # Set temporary directory paths
  spadesTestPaths$temp <- list(
    root = file.path(tempDir, paste0("testthat-", basename(spadesTestPaths$RProj)))
  )
  spadesTestPaths$temp$modules  <- file.path(spadesTestPaths$temp$root, "modules")  # For shared modules
  spadesTestPaths$temp$inputs   <- file.path(spadesTestPaths$temp$root, "inputs")   # For shared inputs
  spadesTestPaths$temp$cache    <- file.path(spadesTestPaths$temp$root, "cache")    # For shared cache
  spadesTestPaths$temp$outputs  <- file.path(spadesTestPaths$temp$root, "outputs")  # For outputs
  spadesTestPaths$temp$projects <- file.path(spadesTestPaths$temp$root, "projects") # For project directories (temp)

  # Set shared project paths
  spadesTestPaths$projectPath <- spadesTestPaths$RProj
  spadesTestPaths$modulePath  <- spadesTestPaths$temp$modules
  spadesTestPaths$packagePath <- c(packagePath, .libPaths())[[1]]
  spadesTestPaths$cachePath   <- c(cachePath, spadesTestPaths$temp$cache)[[1]]
  spadesTestPaths$inputPath   <- c(inputPath, spadesTestPaths$temp$inputs)[[1]]
  spadesTestPaths$outputPath  <- spadesTestPaths$temp$outputs

  # Return
  spadesTestPaths
}

# Copy module files
.copyModule <- function(modulePath, moduleName, destDir,
                        include = c(paste0(moduleName, ".R"), "R", "data"),
                        overwrite = FALSE){

  modulePathFull <- file.path(modulePath, moduleName)
  if (!file.exists(modulePathFull)) stop(
    "Module directory not found: ", modulePathFull)

  # List module files
  modFiles <- file.info(list.files(modulePathFull, full.names = TRUE))
  modFiles$path <- row.names(modFiles)
  modFiles$name <- basename(modFiles$path)

  # Create module directory
  modDir <- file.path(destDir, moduleName)
  if (file.exists(modDir) & overwrite) unlink(modDir, recursive = TRUE)
  if (file.exists(modDir)) stop(
    "Module already exists at destination. Try overwrite = TRUE: ", modDir)
  dir.create(modDir)

  # Copy module files
  copyFiles <- subset(modFiles, name %in% include)

  if (nrow(copyFiles) == 0) stop(
    "Module files not found in directory: ", modulePathFull)

  copySuccess <- c()
  for (i in 1:nrow(copyFiles)){
    copySuccess[copyFiles[i,]$name] <- suppressWarnings({
      if (!copyFiles[i,]$isdir){
        file.copy(copyFiles[i,]$path, file.path(modDir, copyFiles[i,]$name))
      }else{
        file.copy(copyFiles[i,]$path, modDir, recursive = TRUE)
      }
    })
  }
  if (any(!copySuccess)) stop(
    "Module file(s) failed to copy:\n- ",
    paste(file.path(moduleName, names(copySuccess)[!copySuccess]), collapse = "\n- ")
  )
}

#' SpaDES test: Muffle output
#'
#' A wrapper of \code{\link{withCallingHandlers}}
#' that intends to handle output, messages, and innocuous warnings from calls to
#' \code{\link[SpaDES.core]{simInit}},
#' \code{\link[SpaDES.core]{spades}},
#' and \code{\link[SpaDES.project]{setupProject}}.
#' All warnings can be silenced by setting \code{suppressWarnings = TRUE}
#' or \code{option("spades.test.suppressWarnings" = TRUE)}.
#'
#' @param expr expression to be evaluated inside \code{\link{withCallingHandlers}}.
#' @param suppressOutput logical. Sink output to a temporary file.
#' @param handleConditions logical. If FALSE, the expression will be evaluated
#' as normal outside of \code{\link{withCallingHandlers}}.
#' This is the default for interactive testing.
#' @param suppressWarnings logical. Suppress all warnings.
#' Default is FALSE or \code{getOption("spades.test.suppressWarnings")}
#' @param ... optional additional arguments to \code{\link{withCallingHandlers}}.
#'
#' @export
SpaDEStestMuffleOutput <- function(
    expr, ...,
    suppressOutput   = testthat::is_testing(),
    handleConditions = testthat::is_testing(),
    suppressWarnings = getOption("spades.test.suppressWarnings", default = FALSE)){

  # Sink output and messages to file
  if (suppressOutput){
    tempSink <- tempfile("local_output_sink_", fileext = ".txt")
    withr::local_output_sink(tempSink)
    withr::defer(unlink(tempSink))
  }

  if (handleConditions | suppressWarnings){

    withCallingHandlers(
      expr,
      message               = function(c) tryInvokeRestart("muffleMessage"),
      packageStartupMessage = function(c) tryInvokeRestart("muffleMessage"),
      warning = function(w){
        if (suppressWarnings){
          tryInvokeRestart("muffleWarning")
        }else{
          if (grepl("^package ['\u2018]{1}[a-zA-Z0-9.]+['\u2019]{1} was built under R version [0-9.]+$", w$message)){
            tryInvokeRestart("muffleWarning")
          }
        }
      },
      ...
    )

  }else expr
}

