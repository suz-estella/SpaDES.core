
## ************************************************************************** ##
## *~* WORK IN PROGRESS *~*
## These functions attempt to provide a standardized and reusable structure
## for writing testthat tests for SpaDES modules.
## Eventually they may be developed into a new testing template for modules
## to make available to all users.
## ************************************************************************** ##

#' SpaDES test: Set up environment
#'
#' Set local global options and sink output to file. See **Details**.
#'
#' This function was designed to be called in a \code{tests/testthat/setup.R} file
#' After \code{\link{SpaDEStestSetUpDirectories}}.
#' The defaults aim to provide an optimal environment for both standard and
#' interactive testing.
#'
#' @param localOutputSink logical. Sink output to a temporary file
#' @param localMessageSink logical. Sink messages to a temporary file
#'
#' @param reproducible.verbose          reproducible.verbose R package global option.
#' @param Require.verbose               Require.verbose R package global option.
#' @param Require.cloneFrom             Require.cloneFrom R package global option.
#' @param spades.moduleCodeChecks       spades.moduleCodeChecks R package global option.
#' @param spades.moduleDocument         spades.moduleDocument R package global option.
#' @param SpaDES.project.updateRprofile SpaDES.project R package global option.
#'
#' @param spadesTestPaths list. Optional.
#' List of test paths where \code{spadesTestPaths$temp$root} is the location
#' of the output and message sink files.
#' @param teardownEnv environment. Optional. Environment to set up.
#' The default for testing is the \code{testthat::teardown_env()}.
#'
#' @export
SpaDEStestSetUpEnvironment <- function(

    # Sink output and messages
    localOutputSink  = testthat::is_testing(),
    localMessageSink = FALSE,

    # Set SpaDES R package global options
    reproducible.verbose          = if (testthat::is_testing()) -2,
    Require.verbose               = if (testthat::is_testing()) -2,
    Require.cloneFrom             = Sys.getenv("R_LIBS_USER"),
    spades.moduleCodeChecks       = if (testthat::is_testing()) FALSE,
    spades.moduleDocument         = FALSE,
    SpaDES.project.updateRprofile = FALSE,

    spadesTestPaths = .test_directories(),
    teardownEnv     = if (testthat::is_testing()) testthat::teardown_env() else parent.frame(2)){

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

  withr::local_options(localOptions, .local_envir = teardownEnv)

  # Sink output and messages to file
  if (localOutputSink)  withr::local_output_sink(
    file.path(spadesTestPaths$temp$root, "local_output_sink.txt"),
    .local_envir = teardownEnv)

  if (localMessageSink) withr::local_message_sink(
    file.path(spadesTestPaths$temp$root, "local_message_sink.txt"),
    .local_envir = teardownEnv)
}


#' SpaDES test: Set up directories
#'
#' Set up a temporary directory structure. See **Details**.
#'
#' This function was designed to be called in a \code{tests/testthat/setup.R} file.
#'
#' This function will create a temporary directory structure
#' that will be removed on test teardown.
#' The RProject module will be copied to the "modules" sub-directory
#' to be available to tests.
#' An R package "library" sub-directory will be initialized
#' and the initial library paths are restored on teardown.
#'
#' @param testPaths file or directory paths within the \code{tests/testthat}
#' directory to add to the file list.
#' By default a test data directory is included with location \code{tests/testthat/testdata}.
#' @param copyModule logical. If TRUE, the RProject must be a module.
#' It will be copied to the temporary test directories before testing.
#' @param modules character. if \code{copyModule = TRUE}, copy these modules.
#' By default, only the RProject module is copied.
#' If other modules are on the list, they must also be located in the RProject parent directory.
#' @param teardownEnv environment. Optional. Environment to set up.
#' The default for testing is the \code{testthat::teardown_env()}.
#' @param tempDir character. Optional. Path to location of temporary test directory.
#'
#' @return list of test paths
#' @export
SpaDEStestSetUpDirectories <- function(
    testPaths   = "testdata",
    copyModule  = testthat::is_testing(), modules = NULL,
    teardownEnv = if (testthat::is_testing()) testthat::teardown_env() else parent.frame(2),
    tempDir     = tempdir()){

  # List test paths and temporary directories
  spadesTestPaths <- .test_directories(testPaths = testPaths)

  # Create temporary directories
  for (d in spadesTestPaths$temp) dir.create(d)

  if (!copyModule) spadesTestPaths$temp$modules <- dirname(spadesTestPaths$RProj)

  # Remove temporary directories on test teardown
  withr::defer({
    unlink(spadesTestPaths$temp$root, recursive = TRUE)
    if (file.exists(spadesTestPaths$temp$root)) warning(
      "Temporary test directory could not be removed: ",
      spadesTestPaths$temp$root, call. = FALSE)
  }, envir = teardownEnv, priority = "last")

  # Copy module(s) to the temporary testing directory
  if (copyModule){

    if (is.null(modules)) modules <- basename(spadesTestPaths$RProj)

    for (module in modules){
      .copyModule(
        moduleDir  = dirname(spadesTestPaths$RProj),
        moduleName = module,
        destDir    = spadesTestPaths$temp$modules
      )
    }
  }

  # Restore library paths after testing
  ## This likely should be where setupProject() is called (inside test_that),
  ## but the packages that are left attached after running SpaDES stops it
  libPathsInit <- .libPaths()
  withr::local_libpaths(libPathsInit, .local_envir = teardownEnv)

  # Install "testthat" with dependencies into the project R packages directory
  ## This prevents dependencies from not being found when .libPaths() changes
  withr::with_options(
    c(Require.cloneFrom = .libPaths()[1]),
    Require::Require("testthat", libPaths = spadesTestPaths$temp$libPath,
                     dependencies = TRUE, verbose = -2)
  )

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
    spadesTestPaths[[testPath]] <- normalizePath(testthat::test_path(testPath))
  }

  # Set temporary directory paths
  spadesTestPaths$temp <- list(
    root = file.path(tempDir, paste0("testthat-", basename(spadesTestPaths$RProj)))
  )
  spadesTestPaths$temp$inputs   <- file.path(spadesTestPaths$temp$root, "inputs")   # For shared inputs
  spadesTestPaths$temp$outputs  <- file.path(spadesTestPaths$temp$root, "outputs")  # For function test outputs
  spadesTestPaths$temp$modules  <- file.path(spadesTestPaths$temp$root, "modules")  # For modules
  spadesTestPaths$temp$libPath  <- file.path(spadesTestPaths$temp$root, "library")  # R package library
  spadesTestPaths$temp$projects <- file.path(spadesTestPaths$temp$root, "projects") # For project directories

  # Return
  spadesTestPaths
}

# Copy module files
.copyModule <- function(moduleDir, moduleName, destDir,
                        include = c(paste0(moduleName, ".R"), "R", "data")){

  modulePath <- file.path(moduleDir, moduleName)
  if (!file.exists(modulePath)) stop(
    "Module directory not found: ", modulePath)

  # List module files
  modFiles <- file.info(list.files(modulePath, full.names = TRUE))
  modFiles$path <- row.names(modFiles)
  modFiles$name <- basename(modFiles$path)

  # Create module directory
  modDir <- file.path(destDir, moduleName)
  dir.create(modDir)

  # Copy module files
  copyFiles <- subset(modFiles, name %in% include)

  if (nrow(copyFiles) == 0) stop(
    "Module files not found in directory: ",
    file.path(modulePath, moduleName))

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

#' SpaDES test: Muffle conditions
#'
#' A wrapper of \code{\link{withCallingHandlers}}
#' that intends to handle messages and warnings that are not relevant to
#' test success from calls to
#' \code{\link[SpaDES.core]{simInit}},
#' \code{\link[SpaDES.core]{spades}},
#' and \code{\link[SpaDES.project]{setupProject}}.
#' All warnings can be silenced by setting \code{suppressWarnings = TRUE}
#' or \code{option("spades.test.suppressWarnings" = TRUE)}.
#'
#' @param expr expression to be evaluated inside \code{\link{withCallingHandlers}}.
#' @param handleConditions logical. If FALSE, the expression will be evaluated
#' as normal outside of \code{\link{withCallingHandlers}}.
#' This is the default for interactive testing.
#' @param suppressWarnings logical. Suppress all warnings.
#' Default is FALSE or \code{getOption("spades.test.suppressWarnings")}
#' @param ... optional additional arguments to \code{\link{withCallingHandlers}}.
#'
#' @export
SpaDEStestMuffleConditions <- function(
    expr, ...,
    handleConditions = testthat::is_testing(),
    suppressWarnings = getOption("spades.test.suppressWarnings", default = FALSE)){

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

