
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
    reproducible.verbose          = if (testthat::is_testing()) -2,
    Require.verbose               = if (testthat::is_testing()) -2,
    Require.cloneFrom             = Sys.getenv("R_LIBS_USER"),
    spades.moduleCodeChecks       = if (testthat::is_testing()) FALSE,
    spades.moduleDocument         = FALSE,
    SpaDES.project.updateRprofile = FALSE,
    teardownEnv = if (testthat::is_testing()) testthat::teardown_env()){

  # Set global options
  localOptions <- list(
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
#' Set up a temporary directory structure. See **Details**.
#'
#' This function was designed to be called in a \code{tests/testthat/setup.R} file.
#'
#' This function will create a temporary directory structure
#' that will be removed on test teardown.
#' \code{\link[SpaDES.project]{setupProject}} is called to initialize modules and R packages.
#'
#' @param testPaths file or directory paths within the \code{tests/testthat}
#' directory to add to the file list.
#' By default a test data directory is included with location \code{tests/testthat/testdata}.
#' @param modulePath character.
#' By default, it is assumed the R project is a single module to be tested.
#' Otherwise, provide a directory path (relative to the R project root)
#' that contains modules to be tested.
#' Ignored if \code{moduleRepos} is provided.
#' @param copyModules logical.
#' If TRUE, local modules will be copied to the temporary test directories before testing.
#' Ignored if \code{moduleRepos} is provided.
#' @param moduleRepos character. Github repository locations of modules to test.
#' @param require character. Additional R packages to require
#' @param teardownEnv environment. Optional. Environment to use for scoping.
#' The default for testing is the \code{testthat::teardown_env()}.
#' @param tempDir character. Optional. Path to location of temporary test directory.
#' @param ... passed to \code{\link[SpaDES.project]{setupProject}}
#'
#' @return list of test paths
#' @export
SpaDEStestSetUpDirectories <- function(
    testPaths   = "testdata",
    modulePath  = NULL,
    moduleRepos = NULL,
    require     = NULL,
    copyModules = testthat::is_testing(),
    teardownEnv = if (testthat::is_testing()) testthat::teardown_env(),
    tempDir     = tempdir(),
    ...){

  # List test paths and temporary directories
  spadesTestPaths <- .test_directories(tempDir = tempDir, testPaths = testPaths)

  # Create temporary directories
  for (d in spadesTestPaths$temp) dir.create(d)

  if (is.null(moduleRepos)){

    if (is.null(modulePath)){

      # R Project is a module
      modulePath <- dirname(spadesTestPaths$RProj)
      modules <- basename(spadesTestPaths$RProj)

    }else{

      # R Project has a directory containing modules
      modulePath <- file.path(spadesTestPaths$RProj, modulePath)
      modules <- list.files(modulePath)
    }

    # Copy module(s) to the temporary testing directory
    if (copyModules){

      for (module in modules){
        .copyModule(
          modulePath = modulePath,
          moduleName = module,
          destDir    = spadesTestPaths$temp$modules
        )
      }
    }else{

      # If not copying module to temporary location: set module location in place
      spadesTestPaths$temp$modules <- modulePath
    }
  }else modules <- moduleRepos

  # Get initial library state
  libPathsInit <- .libPaths()

  # Use setupProject to set up modules and R package library
  projectPath <- file.path(spadesTestPaths$temp$projects, "setup")
  dir.create(projectPath)
  withr::local_dir(projectPath)

  setupList <- SpaDEStestMuffleOutput(
    SpaDES.project::setupProject(

      restart = FALSE,
      updateRprofile = FALSE,

      require = c("testthat", require),

      modules = modules,
      paths   = list(
        projectPath = projectPath,
        inputPath   = spadesTestPaths$temp$inputs,
        packagePath = spadesTestPaths$temp$packages,
        modulePath  = spadesTestPaths$temp$modules,
        cachePath   = file.path(projectPath, "cache"),
        outputPath  = file.path(projectPath, "outputs")
      )
    ),
    ...
  )

  # Restore library paths on teardown
  if (!is.null(teardownEnv)){
    withr::defer(.libPaths(libPathsInit), envir = teardownEnv, priority = "last")
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

  # Return test directories
  spadesTestPaths
}

# Set test directory paths
.test_directories <- function(testPaths = NULL, tempDir = tempdir()){

  spadesTestPaths <- list()

  # Set R project root (module or R package)
  ## SpaDES will require absolute paths
  spadesTestPaths$RProj <- normalizePath(testthat::test_path("../.."))

  # Set custom test paths
  for (testPath in testPaths){
    spadesTestPaths[[testPath]] <- normalizePath(testthat::test_path(testPath), mustWork = FALSE)
  }

  # Set temporary directory paths
  spadesTestPaths$temp <- list(
    root = file.path(tempDir, paste0("testthat-", basename(spadesTestPaths$RProj)))
  )
  spadesTestPaths$temp$packages <- file.path(spadesTestPaths$temp$root, "packages") # R package library
  spadesTestPaths$temp$inputs   <- file.path(spadesTestPaths$temp$root, "inputs")   # For shared inputs
  spadesTestPaths$temp$modules  <- file.path(spadesTestPaths$temp$root, "modules")  # For shared modules
  spadesTestPaths$temp$cache    <- file.path(spadesTestPaths$temp$root, "cache")    # For shared cache
  spadesTestPaths$temp$projects <- file.path(spadesTestPaths$temp$root, "projects") # For project directories
  spadesTestPaths$temp$outputs  <- file.path(spadesTestPaths$temp$root, "outputs")  # For function test outputs

  # Return
  spadesTestPaths
}

# Copy module files
.copyModule <- function(modulePath, moduleName, destDir,
                        include = c(paste0(moduleName, ".R"), "R", "data")){

  modulePathFull <- file.path(modulePath, moduleName)
  if (!file.exists(modulePathFull)) stop(
    "Module directory not found: ", modulePathFull)

  # List module files
  modFiles <- file.info(list.files(modulePathFull, full.names = TRUE))
  modFiles$path <- row.names(modFiles)
  modFiles$name <- basename(modFiles$path)

  # Create module directory
  modDir <- file.path(destDir, moduleName)
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

