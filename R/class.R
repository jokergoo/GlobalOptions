
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
		failed_msg = "character",
		filter = "function",
		read.only = "logical",
		private = "logical",
		visible = "logical",
		description = "character",
		"__generated_namespace__" = "environment"),

	methods = list(

		initialize = function(...) {
			obj = callSuper(...)
			if(length(obj$failed_msg) == 0) {
				obj$failed_msg = "Your option is invalid."
			}
			return(obj)
		},

		# get current value
		get = function(calling_ns = parent.frame(), read.only = NULL, enforce_visible = FALSE) {

			# in case the value is an executable function
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

			return(NOT_AVAILABLE)  # a special NA which means invalid NA
		},

		# set and refresh current value
		set = function(opt_value = NULL, calling_ns = parent.frame(), initialize = FALSE) {

			if(is.function(opt_value) && length(intersect(.self$class, "function")) == 0) {
				value_fun = opt_value
				opt_value = value_fun()
			}

			if(initialize) { # do not do checking
				if(exists("value_fun")) {
					value <<- value_fun
				} else {
					value <<- opt_value
				}
				
				# in case the function is executable
				.self$refresh()
				return(NULL)
			}	

			# test on read only
			if(.self$read.only) {
				stop(paste("'", .self$name, "' is a read-only option.\n", sep = ""))
			}
						
			# test on private
			# in option function generation and calling are in the same namespace, then private options can be modified
			if( (!identical(.self$`__generated_namespace__`, calling_ns)) && .self$private) {
				stop(paste("'", .self$name, "' is a private option and it can only be modified inside '", env2txt(.self$`__generated_namespace__`), "' namespace while not '", env2txt(calling_ns), "'.\n", sep = ""))
			}
					
			# test on value length
			if(length(.self$length)) {
				if(!(length(opt_value) %in% .self$length)) {
					if(length(.self$length) == 1) {
						stop(paste("Length of '", .self$name, "' should be ", .self$length, ".", sep = ""))
					} else {
						stop(paste("Length of '", .self$name, "' should be one of ", paste(.self$length, collapse = ", "), ".\n", sep = ""))
					}
				}
			}

			# test on classes of the values
			if(length(.self$class)) {
				if(!any(sapply(.self$class, function(cl) inherits(opt_value, cl)))) {
					if(length(.self$class)) {
						stop(paste("Class of '", .self$name, "' should be '", .self$class, "'.\n", sep = ""))
					} else {
						stop(paste("Class of '", .self$name, "' should be one of '", paste(.self$class, collapse = ", "), "'.\n", sep = ""))
					}
				}
			}
						
			# test on validate function
			failed_msg_ = paste(strwrap(paste(.self$name, " didn't pass the validation. ", .self$failed_msg, "\n", sep = "")), collapse = "\n")
			failed_msg_ = paste0(failed_msg_, "\n")
			if(!.self$validate(opt_value)) stop(failed_msg_)

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
			
			# in case the function is executable
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
		},

		copy = function (shallow = FALSE) {
		    def <- .refClassDef
		    value_ <- new(def)
		    vEnv <- as.environment(value_)
		    selfEnv <- as.environment(.self)
		    for (field in names(def@fieldClasses)) {
		        if (shallow)
		            base::assign(field, base::get(field, envir = selfEnv), envir = vEnv)  # get here will conflict with the `get` in this reference class
		        else {
		            current <- base::get(field, envir = selfEnv)
		            if (is(current, "envRefClass"))
		                current <- current$copy(FALSE)
		            base::assign(field, current, envir = vEnv)
		        }
		    }
		    value_
		},

		fields = function() {
			names(.refClassDef@fieldClasses)
		},

		show = function() {
			fd = .self$fields()
			fd = setdiff(fd, "value")
			df = data.frame("Field" = fd,
				"Value" = sapply(fd, function(x) value2text(.self[[x]], x)),
				check.names = FALSE,
				stringsAsFactors = FALSE)
			df[[1]][ df[1] == "real_value" ] = "current_value"
			print(df, row.names = FALSE)
		}

	)
)

value2text = function(v, field, width = 40) {
	if(is.null(v)) {
		"NULL"
	} else if(is.function(v)) {
		"a user-defined function"
	} else if(length(v) == 0) {
		if(field %in% c("length", "class")) {
			"no limit"
		} else {
			paste0(class(v)[1], "(0)")
		}
	} else if(is.environment(v)) {
		env2txt(v)
	} else if(identical(v, "")) {
		"\"\""
	} else if(is.matrix(v)) {
		nr = nrow(v)
		nc = ncol(v)
		paste("a matrix with", nrow(v), ifelse(nr == 1, "row", "rows,"), ncol(v), ifelse(nc == 1, "column", "columns"))
	} else if(is.factor(v)) {
		nl = nlevels(v)
		paste("a factor with", nlevels(v), ifelse(nl == 1, "level", "levels"))
	} else if(is.atomic(v)) {
		toString(v, width = width)
	} else if(is.data.frame(v)) {
		nr = nrow(v)
		nc = ncol(v)
		paste("a data frame with", nrow(v), ifelse(nr == 1, "row", "rows,"), ncol(v), ifelse(nc == 1, "column", "columns"))
	} else if(is.list(v)) {
		paste("a list with", length(v), "elements")
	} else {
		paste0("a (", paste(class(v), collapse = ", "), ") object")
	}
}


# == title
# Get value of other options
#
# == param
# -opt_name name of the option, can be quoted or not quoted.
# -name_is_character whether ``opt_name`` is a character, only used internally.
#
# == details
# When setting one option, the value can be dependent on other option names.
# The current value of other option can be accessed by ``v(nm)`` or ``v$nm``.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# opt = set_opt(a = 1, b = function() .v$a*2)
# opt$b
# opt(a = 2); opt$b
.v = function(opt_name, name_is_character = NA) {
	if(is.na(name_is_character)) {
		opt_name = substitute(opt_name)
	} else if(name_is_character) {
		opt_name = opt_name
	}
	n = 1
	while(TRUE) {
		e = parent.frame(n = n)
		if(exists("options", envir = e, inherits = FALSE)) {
			break
		}
		n = n + 1
		if(n > 50) {
			stop("Cannot find the correct environment.")
		}
	}
	get("options", envir = e)[[opt_name]]$real_value
}

class(.v) = "InternalOptionValue"

# == title
# Get value of other options
#
# == param
# -x should always be ``.v``
# -nm name of the option
#
# == details
# ``.v$nm`` is basically a short version of ``.v(nm)``.
#
# == seealso
# `.v`
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
"$.InternalOptionValue" = function(x, nm) {
	x(nm, name_is_character = TRUE)
}
