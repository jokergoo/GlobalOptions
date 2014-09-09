
# == title
# Produce a function which can get or set global options
#
# == param
# -... specification of options, see 'details' section
#
# == detail
# The most simple way is to construct an option function (e.g. ``foo.options()``) as:
#
#     foo.options = setGlobalOptions(
#         "a" = 1,
#         "b" = "text"
#     )
#
# Then users can get or set the options by 
#
#     foo.options()
#     foo.options("a")
#     foo.options(c("a", "b"))
#     foo.options("a", "b")
#     foo.options("a" = 2)
#     foo.options("a" = 2, "b" = "new_text")
#
# Options can be reset to their default values by:
#
#     foo.options(RESET = TRUE)
#
# The value for each option can be set as a list which may contain more control of the option:
#
#     foo.options = setGlobalOptions(
#         "a" = list(.value = 1,
#                    .length = 1,
#                    .class = "numeric",
#                    .validate = function(x) x > 0),
#     )
#
# ``.length``, ``.class`` and ``.validate`` will be used to check users' input. Please note ``.validate`` function
# should only returns a logical value.
#
# For more detailed explanation, please go to the vignette.
#
setGlobalOptions = function(...) {
	args = list(...)
	
	if(any(is.null(names(args))) || any(names(args) == "")) {
		stop("You should provide named arguments\n")
	}
    
	# format the options
	options = vector("list", length = length(args))

	if("RESET" %in% names(args)) {
		stop("Don't use 'RESET' as the option name.\n")
	}
	
	if("READ.ONLY" %in% names(args)) {
		stop("Don't use 'READ.ONLY' as the option name.\n")
	}
	
	names(options) = names(args)
	for(i in seq_along(args)) {
	
		arg = args[[i]]
		# if it is an advanced setting
		if(is.list(arg) && length(setdiff(names(arg), c(".value", ".class", ".length", ".validate", ".filter", ".read.only"))) == 0) {
			default_value = arg[[".value"]]
			value         = default_value
			length        = arg[[".length"]]
			class         = arg[[".class"]]
			if(is.null(arg[[".validate"]])) {
				validate = function(x) TRUE
			} else {
				validate = arg[[".validate"]]
			}
			if(is.null(arg[[".filter"]])) {
				filter = function(x) x
			} else {
				filter = arg[[".filter"]]
			}
			read.only     = ifelse(is.null(arg[[".read.only"]]), FALSE, arg[[".read.only"]])
		} else {
			if(is.list(arg) && length(intersect(names(arg), c(".value", ".class", ".length", ".validate", ".filter", ".read.only"))) > 0 &&
				length(setdiff(names(arg), c(".value", ".class", ".length", ".validate", ".filter", ".read.only"))) > 0) {
				warning(paste("Your defintion for '", names(args)[i], "' is mixed. It should only contain .value, .class, .length, .validate, .filter, .read.only.\nIgnore the setting and use the whole list as the default value.\n", sep = ""))
			}
			default_value = arg
			value = arg
			length = NULL
			class = NULL
			validate = function(x) TRUE
			filter = function(x) x
			read.only = FALSE
		}

		# create an OPT object inside functions
		assign("OPT", NULL, envir = environment(validate))
		assign("OPT", NULL, envir = environment(filter))
		if(is.function(default_value) && length(intersect(class, "function")) == 0) {
			assign("OPT", NULL, envir = environment(default_value))
			assign("OPT", NULL, envir = environment(value))		
		}

		options[[i]] = list(default_value = default_value,
		                    value         = value,
		                    length        = length,
							class         = class,
							validate      = validate,
							filter        = filter,
							read.only     = read.only)
	}
	
	sth.par = function(..., RESET = FALSE, READ.ONLY = NULL) {
	
		# first we need a copy of `options`
		options2 = options
		
		# reset the options
		if(RESET) {
			for(i in seq_along(options2)) {
				options2[[i]][["value"]] = options[[i]][["default_value"]]
			}
			# assign to original environment
			options <<- options2
			return(invisible(NULL))
		}
		
		args = list(...)
		
		if(length(args) == 1 && is.null(names(args)) && is.null(args[[1]])) {
			return(NULL)
		}
		
		# if settings are stored in one object and send this object
		if(length(args) == 1 && is.list(args[[1]]) && is.null(names(args))) {
			args = args[[1]]
		}

		OPT = getOPT(options)

		# getting all options
		if(length(args) == 0) {
			val = lapply(options, getOptionValue, OPT)
			if(is.null(READ.ONLY)) {
				return(val)
			} else {
				l = sapply(options, function(x) x$read.only)
				if(READ.ONLY) {
					return(val[l])
				} else {
					return(val[!l])
				}
			}
		}
		
		# getting part of the options
		if(is.null(names(args))) {
			args = unlist(args)
			
			if(length(setdiff(args, names(options)))) {
				stop(paste("No such option(s):", paste(setdiff(args, names(options)), collapse = ""), "\n"))
			}
			
			if(length(args) == 1) {
				val = getOptionValue(options[[args]], OPT)
				if(is.null(READ.ONLY)) {
					return(val)
				} else {
					l = options[[args]]$read.only
					if(READ.ONLY) {
						return(val[l])
					} else {
						return(val[!l])
					}
				}
			} else {
				val = lapply(options[args], getOptionValue, OPT)
				if(is.null(READ.ONLY)) {
					return(val)
				} else {
					l = sapply(options[args], function(x) x$read.only)
					if(READ.ONLY) {
						return(val[l])
					} else {
						return(val[!l])
					}
				}
			}
		}
		
		# set the options
		name = names(args)
		option.names = names(options)
		if(any(name == "")) {
			stop("When setting options, all arguments should be named.\n")
		} else {
			for(i in seq_along(args)) {
				
				# if there are names which are not defined in options
				if(sum(name[i] %in% option.names) == 0) {
					stop(paste("No such option: '", name[i], "\n", sep = ""))
				}
				
				# now we can do validating on the values
				length = options[[ name[i] ]][["length"]]
				class = options[[ name[i] ]][["class"]]
				validate = options[[ name[i] ]][["validate"]]
				filter = options[[ name[i] ]][["filter"]]
				read.only = options[[ name[i] ]][["read.only"]]
				
				# test on read only
				if(read.only) {
					stop(paste("'", name[i], "' is a read-only option.\n", sep = ""))
				}

				OPT = getOPT(options2)
				e = environment(validate)
				unlockBinding("OPT", e)
				assign("OPT", OPT, envir = e)
				e = environment(filter)
				unlockBinding("OPT", e)
				assign("OPT", OPT, envir = e)

				# user's value
				value = args[[ name[i] ]]

				if(is.function(value) && length(intersect(class, "function")) == 0) {
					e = environment(value)
					#unlockBinding("OPT", e)
					assign("OPT", OPT, envir = e)
					value = value()
				}
				
				# test on value length
				if(!is.null(length)) {
					if(!(length(value) %in% length)) {
						stop(paste("Length of '", name[i], "' should be one of ", paste(length, collapse = ", "), "\n", sep = ""))
					}
				}

				# test on classes of the values
				if(!is.null(class)) {
					if(length(intersect(class(value), class)) == 0) {
						stop(paste("Class of '", name[i], "' should be one of '", paste(class, collapse = ", "), "'.\n", sep = ""))
					}
				}
				
				# test on validate function
				if(!validate(value)) {
					stop("Your option is invalid.\n")
				}

				# filter on data
				value = filter(value)
				
				# finally, all values are correct
				options2[[ name[i] ]][["value"]] = value
			}
		}
		# assign to original environment
		options <<- options2
		return(invisible(NULL))
	}
	
	return(sth.par)
}

# get the real option value based on settings
getOptionValue = function(x, OPT) {
	if(is.function(x$value)) {

		# if the value is specified as 'function' class
		if(length(intersect(x$class, "function"))) {
			return(x$value)
		} else {
			e = environment(x$value)
			unlockBinding("OPT", e)
			assign("OPT", OPT, envir = e)
			return(x$value())
		}
	} else {
		return(x$value)
	}
}

getOPT = function(options) {
	OPT = vector("list", length = length(options))
	names(OPT) = names(options)

	for(i in seq_along(options)) {
		x = options[[i]]
		if(is.function(x$value) && length(intersect(x$class, "function")) == 0) {
			e = environment(x$value)
			unlockBinding("OPT", e)
			assign("OPT", OPT, envir = e)
			OPT[[i]] = x$value()
		} else {
			OPT[[i]] = x$value
		}
	}
	return(OPT)
}