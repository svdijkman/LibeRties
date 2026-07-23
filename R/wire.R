.ls_wire_names <- function(x) {
  value <- names(x)
  if (is.null(value)) character() else as.character(value)
}

.ls_wire_pack <- function(x, depth = 0L) {
  if (depth > 50L) .ls_stop("Wire payload nesting exceeds 50 levels.")
  next_depth <- depth + 1L
  if (is.null(x)) return(list(type = "null"))
  if (is.function(x) || is.environment(x) || is.language(x) ||
      typeof(x) %in% c("externalptr", "weakref")) {
    .ls_stop("Wire payload contains an executable or pointer-backed R object.")
  }
  if (is.data.frame(x)) {
    return(list(
      type = "data.frame", names = names(x), rows = nrow(x),
      columns = lapply(unclass(x), .ls_wire_pack, depth = next_depth)
    ))
  }
  if (is.matrix(x)) {
    return(list(
      type = "matrix", dimensions = as.integer(dim(x)),
      dimnames = .ls_wire_pack(dimnames(x), next_depth),
      values = .ls_wire_pack(as.vector(x), next_depth)
    ))
  }
  if (is.list(x)) {
    return(list(
      type = "list", named = !is.null(names(x)), names = .ls_wire_names(x),
      values = lapply(x, .ls_wire_pack, depth = next_depth)
    ))
  }
  type <- typeof(x)
  if (!type %in% c("logical", "integer", "double", "character")) {
    .ls_stop("Unsupported wire value type: ", type, ".")
  }
  if (type == "double" && any(!is.finite(x) & !is.na(x))) {
    .ls_stop("Wire payload numeric values must be finite or NA.")
  }
  missing <- which(is.na(x))
  values <- x
  if (length(missing)) {
    replacement <- switch(type, logical = FALSE, integer = 0L, double = 0, character = "")
    values[missing] <- replacement
  }
  list(
    type = type, names = .ls_wire_names(x), length = length(x),
    missing = as.integer(missing), values = unname(values)
  )
}

.ls_wire_vector <- function(x) {
  if (is.null(x)) return(list())
  if (is.list(x)) return(x)
  as.list(x)
}

.ls_wire_character <- function(x) {
  as.character(unlist(.ls_wire_vector(x), use.names = FALSE))
}

.ls_wire_integer <- function(x) {
  values <- unlist(.ls_wire_vector(x), use.names = FALSE)
  integers <- suppressWarnings(as.integer(values))
  if (length(values) && (anyNA(integers) || any(as.numeric(values) != integers))) {
    .ls_stop("Wire payload contains an invalid integer value.")
  }
  integers
}

.ls_wire_unpack <- function(x, depth = 0L) {
  if (depth > 50L) .ls_stop("Wire payload nesting exceeds 50 levels.")
  if (!is.list(x)) .ls_stop("Wire payload node must be an object.")
  type <- as.character(x$type %||% "")
  if (length(type) != 1L || !nzchar(type)) .ls_stop("Wire payload node has no type.")
  next_depth <- depth + 1L
  if (identical(type, "null")) return(NULL)
  if (identical(type, "list")) {
    values <- lapply(.ls_wire_vector(x$values), .ls_wire_unpack, depth = next_depth)
    named <- isTRUE(x$named)
    names <- .ls_wire_character(x$names)
    if (named) {
      if (length(names) != length(values)) .ls_stop("Wire list names have the wrong length.")
      names(values) <- names
    } else if (length(names)) {
      .ls_stop("Unnamed wire list unexpectedly contains names.")
    }
    return(values)
  }
  if (identical(type, "data.frame")) {
    names <- .ls_wire_character(x$names)
    columns <- lapply(.ls_wire_vector(x$columns), .ls_wire_unpack, depth = next_depth)
    rows <- .ls_wire_integer(x$rows)
    if (length(rows) != 1L || rows < 0L || length(names) != length(columns) ||
        any(lengths(columns) != rows)) {
      .ls_stop("Wire data frame dimensions are inconsistent.")
    }
    if (anyDuplicated(names)) .ls_stop("Wire data frame contains duplicate column names.")
    names(columns) <- names
    return(as.data.frame(columns, stringsAsFactors = FALSE, check.names = FALSE,
                         optional = TRUE))
  }
  if (identical(type, "matrix")) {
    dimensions <- .ls_wire_integer(x$dimensions)
    if (length(dimensions) != 2L || any(dimensions < 0L)) {
      .ls_stop("Wire matrix dimensions are invalid.")
    }
    values <- .ls_wire_unpack(x$values, next_depth)
    if (length(values) != prod(dimensions)) .ls_stop("Wire matrix dimensions are inconsistent.")
    result <- matrix(values, nrow = dimensions[[1L]], ncol = dimensions[[2L]])
    dimnames <- .ls_wire_unpack(x$dimnames, next_depth)
    if (!is.null(dimnames)) {
      if (!is.list(dimnames) || length(dimnames) != 2L) .ls_stop("Wire matrix dimnames are invalid.")
      dimnames(result) <- dimnames
    }
    return(result)
  }
  if (!type %in% c("logical", "integer", "double", "character")) {
    .ls_stop("Unknown wire value type: ", type, ".")
  }
  declared_length <- .ls_wire_integer(x$length)
  if (length(declared_length) != 1L || declared_length < 0L) {
    .ls_stop("Wire vector length is invalid.")
  }
  raw_values <- unlist(.ls_wire_vector(x$values), use.names = FALSE)
  result <- switch(
    type,
    logical = as.logical(raw_values),
    integer = .ls_wire_integer(raw_values),
    double = as.numeric(raw_values),
    character = as.character(raw_values)
  )
  if (length(result) != declared_length) .ls_stop("Wire vector length is inconsistent.")
  missing <- .ls_wire_integer(x$missing)
  if (length(missing) && (any(missing < 1L) || any(missing > declared_length) || anyDuplicated(missing))) {
    .ls_stop("Wire vector missing-value positions are invalid.")
  }
  if (length(missing)) result[missing] <- NA
  names <- .ls_wire_character(x$names)
  if (length(names)) {
    if (length(names) != declared_length) .ls_stop("Wire vector names have the wrong length.")
    names(result) <- names
  }
  result
}

.ls_model_wire_fields_v1 <- c(
  "INPUT", "OUTPUT", "ADVAN", "TRANS", "SS", "DOSECMP", "OBSCMP", "PRED", "ERROR",
  "DES", "THETAS", "OMEGAS", "SIGMAS", "COVARIATES", "USE_ODE",
  "ODE_CONTROL", "IOV", "LIK_CONFIG", "SOLVER", "ERROR_TYPE", "GRAPH",
  "LAYOUT", "LANGUAGE"
)

.ls_model_contract <- function(model) {
  if (!requireNamespace("LibeRation", quietly = TRUE)) {
    .ls_stop("LibeRation is required to serialize a remote model.")
  }
  if (utils::packageVersion("LibeRation") < "0.8.1") {
    .ls_stop("Remote model contract v2 requires LibeRation 0.8.1 or newer.")
  }
  tryCatch(
    LibeRation::nm_model_to_contract(model, version = 2L),
    error = function(e) .ls_stop("Remote model serialization failed: ", conditionMessage(e))
  )
}

.ls_restore_model_v1 <- function(model_fields) {
  if (!requireNamespace("LibeRation", quietly = TRUE)) {
    .ls_stop("LibeRation is required to validate a remote model.")
  }
  graph <- model_fields$GRAPH
  if (is.list(graph) && all(c("compartments", "flows") %in% names(graph))) {
    model_fields$GRAPH <- tryCatch(
      LibeRation::nm_matrix_model(
        graph$compartments, graph$flows, graph$observations,
        graph$inputs, graph$layout
      ),
      error = function(e) .ls_stop("Remote matrix graph validation failed: ", conditionMessage(e))
    )
  }
  tryCatch(
    do.call(LibeRation::nm_model, model_fields),
    error = function(e) .ls_stop("Remote model validation failed: ", conditionMessage(e))
  )
}

.ls_restore_model <- function(contract) {
  if (!requireNamespace("LibeRation", quietly = TRUE)) {
    .ls_stop("LibeRation is required to validate a remote model.")
  }
  if (is.list(contract) && identical(as.character(contract$schema %||% ""),
                                     "liberation.model")) {
    return(tryCatch(
      LibeRation::nm_model_from_contract(contract),
      error = function(e) .ls_stop("Remote model validation failed: ", conditionMessage(e))
    ))
  }
  .ls_restore_model_v1(contract)
}

#' Convert a LibeR job to the non-executable JSON wire contract
#' @param job A [ls_job()] object containing an `nm_model`.
#' @return A JSON-compatible typed object.
#' @export
ls_job_to_wire <- function(job) {
  if (!inherits(job, "liber_job")) .ls_stop("`job` must be created by ls_job().")
  if (startsWith(job$type, "library_")) {
    return(list(
      schema = "liber.job.wire", version = 2L, type = job$type,
      label = job$label, created = job$created,
      payload = .ls_wire_pack(job$data), arguments = .ls_wire_pack(job$arguments)
    ))
  }
  if (!inherits(job$model, "nm_model")) {
    .ls_stop("Remote wire jobs require a serializable LibeRation nm_model.")
  }
  list(
    schema = "liber.job.wire", version = 2L, type = job$type,
    label = job$label, created = job$created,
    model = .ls_wire_pack(.ls_model_contract(job$model)), data = .ls_wire_pack(job$data),
    arguments = .ls_wire_pack(job$arguments)
  )
}

#' Rebuild and validate a LibeR job from the JSON wire contract
#'
#' The transmitted expression IR is never trusted. The semantic model fields
#' are compiled again through [LibeRation::nm_model()] on the receiving host.
#'
#' @param payload Parsed JSON-compatible payload.
#' @return A validated `liber_job`.
#' @export
ls_job_from_wire <- function(payload) {
  version <- suppressWarnings(as.integer(payload$version %||% NA_integer_))
  if (!is.list(payload) || !identical(as.character(payload$schema), "liber.job.wire") ||
      length(version) != 1L || is.na(version) || !version %in% c(1L, 2L)) {
    .ls_stop("Unsupported or invalid LibeR JSON wire contract.")
  }
  type <- as.character(payload$type)
  allowed <- c("simulate", "estimate", "estimate_sequence", "individualise", "regimen", "optimal_design",
               "library_triage", "library_parse",
               "library_index", "library_dual_extract", "library_assess",
               "library_adjudicate")
  if (length(type) != 1L || !type %in% allowed) {
    .ls_stop("Unsupported wire job type.")
  }
  arguments <- .ls_wire_unpack(payload$arguments)
  if (!is.list(arguments) || (length(arguments) && is.null(names(arguments)))) {
    .ls_stop("Wire job arguments must be a named list.")
  }
  label <- as.character(payload$label %||% "")
  created <- as.character(payload$created %||% "")
  if (length(label) != 1L || is.na(label) || nchar(label, type = "bytes") > 1024L ||
      length(created) != 1L || is.na(created) || nchar(created, type = "bytes") > 128L) {
    .ls_stop("Wire job label or timestamp is invalid.")
  }
  if (startsWith(type, "library_")) {
    literature_payload <- .ls_wire_unpack(payload$payload)
    if (!is.list(literature_payload) || is.null(literature_payload$metadata)) {
      .ls_stop("Wire literature payload is invalid.")
    }
    job <- ls_library_job(type, literature_payload, arguments, label)
    job$created <- created
    return(job)
  }
  model_payload <- .ls_wire_unpack(payload$model)
  if (version == 1L) {
    if (!is.list(model_payload) || is.null(names(model_payload)) ||
        length(setdiff(names(model_payload), .ls_model_wire_fields_v1))) {
      .ls_stop("Wire model contains invalid semantic fields.")
    }
    required <- c("INPUT", "ADVAN", "PRED", "THETAS")
    if (length(setdiff(required, names(model_payload)))) {
      .ls_stop("Wire model is missing required semantic fields.")
    }
  } else if (!is.list(model_payload) ||
             !identical(as.character(model_payload$schema %||% ""), "liberation.model")) {
    .ls_stop("Wire model does not contain a LibeRation model contract.")
  }
  model <- .ls_restore_model(model_payload)
  data <- .ls_wire_unpack(payload$data)
  if (identical(type, "optimal_design")) {
    if (!is.list(data) || !identical(data$schema, "liberality.design")) {
      .ls_stop("Wire optimal-design payload must be a LibeRality design.")
    }
  } else if (!is.data.frame(data)) .ls_stop("Wire job data must be a data frame.")
  job <- ls_job(type, model, data, arguments, label)
  job$created <- created
  job
}

#' Encode a LibeR job as JSON
#' @param job A [ls_job()] object.
#' @export
ls_job_encode <- function(job) {
  jsonlite::toJSON(
    ls_job_to_wire(job), auto_unbox = TRUE, null = "null", digits = 17,
    dataframe = "columns", POSIXt = "ISO8601", force = TRUE
  )
}

#' Decode and validate a LibeR JSON job
#' @param json UTF-8 JSON text or raw bytes.
#' @param max_bytes Maximum accepted encoded payload size.
#' @export
ls_job_decode <- function(json, max_bytes = 100 * 1024^2) {
  if (is.raw(json)) json <- rawToChar(json)
  json <- as.character(json)
  if (length(json) != 1L || is.na(json) || nchar(json, type = "bytes") > max_bytes) {
    .ls_stop("Encoded job exceeds the accepted JSON payload size.")
  }
  payload <- tryCatch(
    jsonlite::fromJSON(json, simplifyVector = FALSE),
    error = function(e) .ls_stop("Invalid job JSON: ", conditionMessage(e))
  )
  ls_job_from_wire(payload)
}

#' Encode a safe result object for remote transport
#' @param result Serializable result without executable or pointer-backed values.
#' @export
ls_result_to_wire <- function(result) {
  attributes <- attributes(result)
  attributes <- attributes[setdiff(names(attributes), c("names", "row.names", "dim", "dimnames"))]
  transported <- result
  if (inherits(transported, c("nm_fit", "nm_individual_fit")) &&
      inherits(transported$model, "nm_model")) {
    transported$model <- .ls_model_contract(transported$model)
  }
  list(
    schema = "liber.result.wire", version = 2L,
    result = .ls_wire_pack(transported), attributes = .ls_wire_pack(attributes)
  )
}

#' @rdname ls_result_to_wire
#' @export
ls_result_encode <- function(result) {
  jsonlite::toJSON(
    ls_result_to_wire(result),
    auto_unbox = TRUE, null = "null", digits = 17, force = TRUE
  )
}

#' Rebuild a result from its parsed wire representation
#' @param payload Parsed result wire object.
#' @export
ls_result_from_wire <- function(payload) {
  version <- suppressWarnings(as.integer(payload$version %||% NA_integer_))
  if (!is.list(payload) ||
      !identical(as.character(payload$schema), "liber.result.wire") ||
      length(version) != 1L || is.na(version) || !version %in% c(1L, 2L)) {
    .ls_stop("Unsupported or invalid LibeR result wire contract.")
  }
  result <- .ls_wire_unpack(payload$result)
  attributes <- .ls_wire_unpack(payload$attributes %||% list(type = "list", named = FALSE,
                                                              names = list(), values = list()))
  if (!is.list(attributes) || length(setdiff(names(attributes),
                                             c("class", "solver", "state_names", "id_levels")))) {
    .ls_stop("Remote result contains unsupported attributes.")
  }
  if (!is.null(attributes$class)) {
    classes <- as.character(attributes$class)
    allowed <- c("data.frame", "nm_dataset", "nm_fit", "nm_individual_fit",
                 "lator_assessment", "lator_regimen_comparison", "matrix", "array")
    extra <- setdiff(classes, allowed)
    schema <- if (is.list(result)) as.character(result$schema %||% "") else ""
    schema_class <-
      (grepl("^liberality\\.", schema) && all(grepl("^lity_", extra))) ||
      (grepl("^liberator\\.", schema) && all(grepl("^lator_", extra))) ||
      (grepl("^liberary\\.", schema) && all(grepl("^(liberary|library)_", extra)))
    if (length(extra) && !schema_class) .ls_stop("Remote result contains an unsupported class.")
  }
  for (name in names(attributes)) attr(result, name) <- attributes[[name]]
  if (inherits(result, "nm_fit")) {
    required <- c("method", "objective", "theta", "omega", "sigma", "eta", "model", "data")
    if (!is.list(result) || length(setdiff(required, names(result)))) {
      .ls_stop("Remote nm_fit result is missing required fields.")
    }
    model_payload <- if (version == 1L) {
      result$model[intersect(.ls_model_wire_fields_v1, names(result$model))]
    } else result$model
    if (version == 1L &&
        length(setdiff(c("INPUT", "ADVAN", "PRED", "THETAS"), names(model_payload)))) {
      .ls_stop("Remote nm_fit contains an invalid model.")
    }
    result$model <- .ls_restore_model(model_payload)
    data <- as.data.frame(result$data, stringsAsFactors = FALSE)
    data[grep("^\\.", names(data), value = TRUE)] <- NULL
    result$data <- tryCatch(
      LibeRation::nm_dataset(data),
      error = function(e) .ls_stop("Remote nm_fit contains invalid data: ", conditionMessage(e))
    )
    result$eta <- as.matrix(result$eta)
    if (length(result$theta) != nrow(result$model$THETAS) ||
        length(result$sigma) != nrow(result$model$SIGMAS) ||
        length(result$omega) != nrow(result$model$OMEGAS) ||
        nrow(result$eta) != length(unique(result$data$ID))) {
      .ls_stop("Remote nm_fit parameter dimensions are inconsistent.")
    }
  }
  if (inherits(result, "nm_individual_fit")) {
    required <- c("eta", "eta_covariance", "model", "data", "predictions")
    if (!is.list(result) || length(setdiff(required, names(result)))) {
      .ls_stop("Remote individual-fit result is missing required fields.")
    }
    model_payload <- if (version == 1L) {
      result$model[intersect(.ls_model_wire_fields_v1, names(result$model))]
    } else result$model
    result$model <- .ls_restore_model(model_payload)
    data <- as.data.frame(result$data, stringsAsFactors = FALSE)
    data[grep("^\\.(ID_INDEX|source_row|generated|sort_priority)$", names(data))] <- NULL
    result$data <- LibeRation::nm_dataset(data)
    result$eta <- as.numeric(result$eta)
    result$eta_covariance <- as.matrix(result$eta_covariance)
    if (!identical(dim(result$eta_covariance), c(length(result$eta), length(result$eta)))) {
      .ls_stop("Remote individual-fit covariance dimensions are inconsistent.")
    }
  }
  if (length(class(result)) && any(grepl("^lity_", class(result))) &&
      requireNamespace("LibeRality", quietly = TRUE) &&
      "lity_contract_restore" %in% getNamespaceExports("LibeRality")) {
    result <- LibeRality::lity_contract_restore(result)
  }
  result
}

#' Decode a remote result object
#' @param json UTF-8 result JSON.
#' @param max_bytes Maximum accepted result size.
#' @export
ls_result_decode <- function(json, max_bytes = 500 * 1024^2) {
  if (is.raw(json)) json <- rawToChar(json)
  json <- as.character(json)
  if (length(json) != 1L || is.na(json) || nchar(json, type = "bytes") > max_bytes) {
    .ls_stop("Encoded result exceeds the accepted JSON payload size.")
  }
  payload <- tryCatch(jsonlite::fromJSON(json, simplifyVector = FALSE), error = identity)
  if (inherits(payload, "error")) {
    .ls_stop("Unsupported or invalid LibeR result wire contract.")
  }
  ls_result_from_wire(payload)
}
