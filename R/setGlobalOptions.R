
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
# The value for each option can be set as a list which contains more controls of the option:
#
#     foo.options = setGlobalOptions(
#         "a" = list(.value = 1,
#                    .length = 1,
#                    .class = "numeric",
#                    .validate = function(x) x > 0)
#     )
#
# The different fields in the list can be used to filter or validate the option values.
#
# -.value The default value.
# -.length The valid length of the option value. It can be a vector, the check will be passed if one of the length fits.
# -.class The valid class of the option value. It can be a vector, the check will be passed if one of the classes fits.
# -.validate Validation function. The input parameter is the option value and should return a single logical value.
# -.filter Filtering function. The input parameter is the option value and it should return a filtered option value.
# -.read.only Logical. The option value can not be modified if it is set to ``TRUE``.
# -.visible Logical. Whether the option is visible to users.
# -.private Logical. The option value can only be modified in the same namespace where the option function is created.
#
# For more detailed explanation, please go to the vignette.
#
setGlobalOptions = function(...) {

	# the environment where the function is called
	.envoking_env = parent.frame()
	
	# whether the environment where foo.options() is called is the same as the 
	# environment where foo.options() is generated.
	envokedInTheSameNamespace = function(ns) {	
		identical(ns, options[["__generatedNamespace__"]][["value"]])
	}
	
	args = list(...)
	
	if(any(is.null(names(args))) || any(names(args) == "")) {
		stop("You should provide named arguments.\n")
	}
	
	if("__generatedNamespace__" %in% names(args)) {
		stop("Don't use '__generatedNamespace__' as the option name.\n")
	}
	
	args[["__generatedNamespace__"]] = list(.value = topenv(.envoking_env),
	                                       .read.only = TRUE,
										   .visible = FALSE)

	# format the options
	options = vector("list", length = length(args))

	if("RESET" %in% names(args)) {
		stop("Don't use 'RESET' as the option name.\n")
	}
	
	if("READ.ONLY" %in% names(args)) {
		stop("Don't use 'READ.ONLY' as the option name.\n")
	}
	
	if("LOCAL" %in% names(args)) {
		stop("Don't use 'LOCAL' as the option name.\n")
	}
	
	names(options) = names(args)
	
	
	for(i in seq_along(args)) {
	
		arg = args[[i]]
		# if it is an advanced setting
		if(is.list(arg) && length(setdiff(names(arg), c(".value", ".class", ".length", ".validate", ".filter", ".read.only", ".private", ".visible"))) == 0) {
			default_value = arg[[".value"]]
			value = default_value
			length = arg[[".length"]]
			class = arg[[".class"]]
			if(is.null(arg[[".validate"]])) {
				validate = function(x) TRUE
			} else {
				if(is.function(arg[[".validate"]])) {
					validate = arg[[".validate"]]
				} else {
					stop(paste("'.validate' field in", names(args)[i], "should be a function.\n"))
				}
			}
			if(is.null(arg[[".filter"]])) {
				filter = function(x) x
			} else {
				if(is.function(arg[[".filter"]])) {
					filter = arg[[".filter"]]
				} else {
					stop(paste("'.filter' field in", names(args)[i], "should be a function.\n"))
				}
			}
			read.only = ifelse(is.null(arg[[".read.only"]]), FALSE, arg[[".read.only"]])
			private = ifelse(is.null(arg[[".private"]]), FALSE, arg[[".private"]])
			visible = ifelse(is.null(arg[[".visible"]]), TRUE, arg[[".visible"]])
		} else {
			if(is.list(arg) && length(intersect(names(arg), c(".value", ".class", ".length", ".validate", ".filter", ".read.only", ".private", ".visible"))) > 0 &&
				length(setdiff(names(arg), c(".value", ".class", ".length", ".validate", ".filter", ".read.only", ".private", ".visible"))) > 0) {
				warning(paste("Your definition for '", names(args)[i], "' is mixed. It should only contain\n.value, .class, .length, .validate, .filter, .read.only, .private, .visible.\nIgnore the setting and use the whole list as the default value.\n", sep = ""))
			}
			default_value = arg
			value = arg
			length = NULL
			class = NULL
			validate = function(x) TRUE
			filter = function(x) x
			read.only = FALSE
			private = FALSE
			visible = TRUE
		}

		options[[i]] = list(default_value = default_value,
		                    value         = value,
		                    length        = length,
							class         = class,
							validate      = validate,
							filter        = filter,
							read.only     = read.only,
							private       = private,
							visible       = visible)
	}
	
	# place to store `options` and `OPT`
	options_env = new.env()
	assign("options", options, envir = options_env)
	
	OPT_env = new.env()
	OPT = vector("list", length = length(options))
	names(OPT) = names(options)
	assign("OPT", OPT, envir = OPT_env)
	
	# create a pool to store copies of local options
	#local_options_db = list(.tmp = NULL)
	#local_env = as.environment(local_options_db)  # environment where local options is stored
	
	sth.par = function(..., RESET = FALSE, READ.ONLY = NULL) {
		# the environment where foo.options() is called
		.envoking_env = parent.frame()
		ns = topenv(.envoking_env)  # top package where foo.options() is called
		
		get_options = function() {
			get("options", envir = options_env)
		}
			
		set_options = function(options) {
			assign("options", options, envir = options_env)
		}
			
		get_OPT = function() {
			get("OPT", envir = OPT_env)
		}
		
		# each time execute foo.options(), fixed value should be refreshed, which means
		# if value is a dynamic function, each time the current fixed value is generated
		refresh_fixed_option_values = function() {
			options = get_options()
			OPT = get_OPT()
			for(i in names(options)) {
				x = options[[i]]
				if(is.null(x$value)) {
					OPT[i] = list(NULL)
				} else if(is.function(x$value) && length(intersect(x$class, "function")) == 0) {
					value_fun = x$value
					value_fun = insertEnvBefore(value_fun, OPT_env)
					tryCatch({ OPT[[i]] = value_fun();
					           attr(OPT[[i]], "FUN") = value_fun},
						finally = deleteEnvBefore(value_fun))
				} else {
					OPT[[i]] = x$value
				}
				assign("OPT", OPT, OPT_env)
			}
		}
				
		# first we need a copy of `options`
		options2 = get_options()
		refresh_fixed_option_values()
		
		# reset the options
		if(RESET) {
			for(i in seq_along(options2)) {
				if(envokedInTheSameNamespace(ns)) {
					# read-only options cannot be reset
					if(! options2[[i]][["read.only"]]) {
						options2[[i]][["value"]] = options2[[i]][["default_value"]]
					}
				} else {
					# read-only and private options can not be reset
					if(! (options2[[i]][["read.only"]] && options2[[i]][["private"]]) ) {
						options2[[i]][["value"]] = options2[[i]][["default_value"]]
					}
				}
			}
			# assign to original environment
			set_options(options2)
			return(invisible(NULL))
		}
		
		#if(is.logical(LOCAL)) {
		#	return(invisible(NULL))
		#}
		
		
		args = list(...)
		
		if(length(args) == 1 && is.null(names(args)) && is.null(args[[1]])) {
			return(NULL)
		}
		
		# if settings are stored in one object and send this object
		if(length(args) == 1 && is.list(args[[1]]) && is.null(names(args))) {
			args = args[[1]]
		}

		# getting all options
		if(length(args) == 0) {
			val = get_OPT()
			# only returns visible options
			l_visible = sapply(options2, function(x) x[["visible"]])
			if(is.null(READ.ONLY)) {
				return(val[l_visible])
			} else {
				l1 = sapply(options2, function(x) x[["read.only"]])
				l2 = sapply(options2, function(x) x[["private"]])
				
				# in the same namespace, then private is not read only
				if(envokedInTheSameNamespace(ns)) {
					if(READ.ONLY) {
						return(val[l_visible & l1])
					} else {
						return(val[l_visible & (!l1)])
					}
				} else {
					if(READ.ONLY) {
						return(val[l_visible & (l1 | l2)])
					} else {
						return(val[l_visible & (!(l1 | l2))])
					}
				}
			}
		}
		
		# getting part of the options
		if(is.null(names(args))) {
			args = unlist(args)
			
			if(length(setdiff(args, names(options2)))) {
				stop(paste("No such option(s):", paste(setdiff(args, names(options2)), collapse = ""), "\n"))
			}
			
			if(length(args) == 1) {
				val = get_OPT()[[args]]
				
				if(is.null(READ.ONLY)) {
					return(val)
				} else {
					l1 = options2[[args]][["read.only"]]
					l2 = options2[[args]][["private"]]
					
					# in the same namespace, then private is not read only
					if(envokedInTheSameNamespace(ns)) {
						if(READ.ONLY) {
							return(val[l_visible & l1])
						} else {
							return(val[l_visible & (!l1)])
						}
					} else {
						if(READ.ONLY) {
							return(val[l_visible & (l1 | l2)])
						} else {
							return(val[l_visible & (!(l1 | l2))])
						}
					}
				}
			} else {
				val = get_OPT()[args]
				l_visible = sapply(options[args], function(x) x[["visible"]])
				
				if(is.null(READ.ONLY)) {
					return(val[l_visible])
				} else {
					l1 = sapply(options[args], function(x) x[["read.only"]])
					l2 = sapply(options[args], function(x) x[["private"]])
					
					# in the same namespace, then private is not read only
					if(envokedInTheSameNamespace(ns)) {
						if(READ.ONLY) {
							return(val[l_visible & l1])
						} else {
							return(val[l_visible & (!l1)])
						}
					} else {
						if(READ.ONLY) {
							return(val[l_visible & (l1 | l2)])
						} else {
							return(val[l_visible & (!(l1 | l2))])
						}
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
				private = options[[ name[i] ]][["private"]]
				
				# test on read only
				if(read.only) {
					stop(paste("'", name[i], "' is a read-only option.\n", sep = ""))
				}
				
				# test on private
				# in option function generation and calling are in the same namespace, then private options can be modified
				if( (!envokedInTheSameNamespace(ns)) && private) {
					stop(paste("'", name[i], "' is a private option and it can only be modified inside '", env2txt(options[["__envokingNamespace__"]][["value"]]), "' namespace.\n", sep = ""))
				}
				
				# user's value
				value = args[[ name[i] ]]
				
				if(!is.null(attr(value, "FUN"))) {
					value = attr(value, "FUN")
				}
				if(is.function(value) && length(intersect(class, "function")) == 0) {
					value_fun = value
					value_fun = insertEnvBefore(value_fun, OPT_env)
					tryCatch({ value = value_fun() },
						finally = deleteEnvBefore(value_fun))
				}
				
				# test on value length
				if(!is.null(length)) {
					if(!(length(value) %in% length)) {
						stop(paste("Length of '", name[i], "' should be one of ", paste(length, collapse = ", "), ".\n", sep = ""))
					}
				}

				# test on classes of the values
				if(!is.null(class)) {
					if(!any(sapply(class, function(cl) inherits(value, cl)))) {
						stop(paste("Class of '", name[i], "' should be one of '", paste(class, collapse = ", "), "'.\n", sep = ""))
					}
				}
				
				# test on validate function
				validate = insertEnvBefore(validate, OPT_env)
				tryCatch({ if(!validate(value)) stop("Your option is invalid.\n") },
						finally = deleteEnvBefore(validate))
				

				# filter on data
				filter = insertEnvBefore(filter, OPT_env)
				tryCatch({ value = filter(value) },
						finally = deleteEnvBefore(filter))
				
				
				# check filtered value again
				# test on value length
				if(!is.null(length)) {
					if(!(length(value) %in% length)) {
						stop(paste("Length of filtered '", name[i], "' should be one of ", paste(length, collapse = ", "), "\n", sep = ""))
					}
				}

				# test on classes of the values
				if(!is.null(class)) {
					if(!any(sapply(class, function(cl) is(value, cl)))) {
						stop(paste("Class of filtered '", name[i], "' should be one of '", paste(class, collapse = ", "), "'.\n", sep = ""))
					}
				}
				
				# finally, all values are correct
				if(exists("value_fun")) {
					options2[[ name[i] ]][["value"]] = value_fun
					rm(value_fun)
				} else {
					options2[[ name[i] ]][["value"]] = value
				}
				
				set_options(options2)
				refresh_fixed_option_values()
		
			}
		}
		# assign to original environment
		
		return(invisible(NULL))
	}
	
	return(sth.par)
}

env2txt = function(env) {
	if(identical(env, emptyenv())) {
		return("R_EmptyEnv")
	} else if(identical(env, .GlobalEnv)){
		return("R_GlobalEnv")
	} else if(isNamespace(env)) {
		return(getNamespaceName(env))
	} else if(!is.null(attr(env, "name"))) {
		return(attr(env, "name"))
	} else {
		return(get_env_str(env))
	}
}

insertEnvBefore = function(fun, e) {
	oe = environment(fun)
	ope = parent.env(oe)
	parent.env(oe) = e
	parent.env(e) = ope
	return(fun)
}

deleteEnvBefore = function(fun) {
	oe = environment(fun)
	parent.env(oe) = parent.env(parent.env(oe))
	return(fun)
}

print_env_stack = function(e, depth = Inf) {
	if(is.function(e)) {
		env = environment(e)
	} else {
		env = e
	}
	i_depth = 0
	while(!identical(env, emptyenv()) && i_depth < depth) {
		cat(env2txt(env), "\n")
		env = parent.env(env)
		i_depth = i_depth + 1
	}
}

# with_sink is copied from testthat package
with_sink = function (connection, code, ...) 
{
    sink(connection, ...)
    on.exit(sink())
    code
}

get_env_str = function(env) {
	temp = file()
	with_sink(temp, print(env))
	output <- paste0(readLines(temp, warn = FALSE), collapse = "\n")
	close(temp)
	return(output)
}

