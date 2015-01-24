
NOT_AVAILABLE = NA
attr(NOT_AVAILABLE, "not_available") = TRUE

GlobalOption = setRefClass("GlobalOption",
    fields = list(
    	name = "character",
        default_value = "ANY",  # default_value
		value = "ANY",           # setted value
		real_value = "ANY",  # is value is a function to be executed, it is the returned value
		length = "numeric",
		class = "character",
		validate = "function",
		filter = "function",
		read.only = "logical",
		private = "logical",
		visible = "logical",
		"__generated_namespace__" = "environment"),
   methods = list(

initialize = function(...) {
	callSuper(...)
},
# get current value
get = function(calling_ns = parent.frame(), read.only = NULL, enforce_visible = FALSE) {

	.self$refresh()

	if(!.self$visible && !enforce_visible) {
		return(NOT_AVAILABLE)
	}

	if(is.null(read.only)) {
		return(.self$real_value)
	} else {
		if(! identical(.self$`__generated_namespace__`, calling_ns)) {
			if(.self$private) {
				return(NOT_AVAILABLE)
			}
		}

		if(read.only) {
			if(.self$read.only) {
				return(.self$real_value)
			}
		} else {
			if(!.self$read.only) {
				return(.self$real_value)
			}
		}

	}

	return(NOT_AVAILABLE)
},

# set and refresh current value
set = function(opt_value = NULL, calling_ns = parent.frame()) {

	# test on read only
	if(.self$read.only) {
		stop(paste("'", .self$name, "' is a read-only option.\n", sep = ""))
	}
				
	# test on private
	# in option function generation and calling are in the same namespace, then private options can be modified
	if( (!identical(.self$`__generated_namespace__`, calling_ns)) && .self$private) {
		stop(paste("'", .self$name, "' is a private option and it can only be modified inside '", env2txt(.self$`__generated_namespace__`), "' namespace.\n", sep = ""))
	}
	
	if(is.function(opt_value) && length(intersect(.self$class, "function")) == 0) {
		value_fun = opt_value
		opt_value = value_fun()
	}
				
	# test on value length
	if(length(.self$length)) {
		if(!(length(opt_value) %in% .self$length)) {
			stop(paste("Length of '", .self$name, "' should be one of ", paste(.self$length, collapse = ", "), ".\n", sep = ""))
		}
	}

	# test on classes of the values
	if(length(.self$class)) {
		if(!any(sapply(.self$class, function(cl) inherits(opt_value, cl)))) {
			stop(paste("Class of '", .self$name, "' should be one of '", paste(.self$class, collapse = ", "), "'.\n", sep = ""))
		}
	}
				
	# test on validate function
	if(!.self$validate(opt_value)) stop("Didn't pass the validation. Your option is invalid.\n")

	# filter on data
	opt_value = .self$filter(opt_value)
				
	# check filtered value again
	# test on value length
	if(length(.self$length)) {
		if(!(length(opt_value) %in% .self$length)) {
			stop(paste("Length of filtered '", .self$name, "' should be one of ", paste(.self$length, collapse = ", "), "\n", sep = ""))
		}
	}

	# test on classes of the values
	if(length(.self$class)) {
		if(!any(sapply(.self$class, function(cl) inherits(opt_value, cl)))) {
			stop(paste("Class of filtered '", .self$name, "' should be one of '", paste(.self$class, collapse = ", "), "'.\n", sep = ""))
		}
	}
				
	# finally, all values are correct
	if(exists("value_fun")) {
		value <<- value_fun
	} else {
		value <<- opt_value
	}
				
	.self$refresh()
},

# set to default value
reset = function(calling_ns = parent.frame()) {
	if(identical(.self$`__generated_namespace__`, calling_ns)) {
		# read-only options cannot be reset
		if(! .self$read.only) {
			.self$value = .self$default_value
		}
	} else {
		# read-only and private options can not be reset
		if(! (.self$read.only || .self$private) ) {
			.self$value = .self$default_value
		}
	}

	.self$refresh()
},

refresh = function() {
	if(inherits(.self$value, "function") && !("function" %in% .self$class)) {
		.self$real_value = .self$value()
	} else {
		.self$real_value = .self$value
	}
}

)
)
