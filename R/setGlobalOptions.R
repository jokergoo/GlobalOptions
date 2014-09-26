
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
# The different fields in the list can be used to filter or validate the option values.
#
# -.value The default value.
# -.length The valid length of the option value. It can be a vector, the check will be passed if one of the length fits.
# -.class The valid class of the option value. It can be a vector, the check will be passed if one of the class fits.
# -.validate Validation function. The input parameter is the option value and should return a single logical value.
# -.filter Filtering function. The input parameter is the option value and it should return a filtered option value.
# -.read.only Logical. The option value will not be modified if it is set to ``TRUE``.
# -.visible Logical.
# -.private Logical. The option value can only be modified in the same namespace where the option function is created.
#
# For more detailed explanation, please go to the vignette.
#
setGlobalOptions = function(...) {

	.envoking_env = parent.frame()
	
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
    
        # create an OPT object inside functions  
		e = environment(validate)
		#if(exists("OPT", envir = e)) {
		#	lockBinding("OPT", e)
		#	unlockBinding("OPT", e)
		#}
		assign("OPT", NULL, envir = e)
		#lockBinding("OPT", e)
		
		
		e = environment(filter)
		#if(exists("OPT", envir = e)) {
		#	lockBinding("OPT", e)
		#	unlockBinding("OPT", e)
		#}
		assign("OPT", NULL, envir = e)
		#lockBinding("OPT", e)
		
		
		if(is.function(default_value) && length(intersect(class, "function")) == 0) {
			e = environment(default_value)
			#if(exists("OPT", envir = e)) {
			#	lockBinding("OPT", e)
			#	unlockBinding("OPT", e)
			#}
			assign("OPT", NULL, envir = e)
			#lockBinding("OPT", e)
			
			# same for environment of default_value and value 		
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
	
	# create a pool to store copies of local options
	#local_options_db = list(.tmp = NULL)
	#local_env = as.environment(local_options_db)  # environment where local options is stored
	
	sth.par = function(..., RESET = FALSE, READ.ONLY = NULL) {
		.envoking_env = parent.frame()
		ns = topenv(.envoking_env)  # top package the option function is used
		
		#local_options_name = env2txt(ns);
		#local_options_name = gsub(":", "_", local_options_name)  # to make it a valid variable name
		
		options_env = parent.env(environment()) # environment where global options is stored
		
		############# no supported yet ######################
		#LOCAL = NULL
		#is_in_local_mode = function(LOCAL) {
		#	if(is.null(LOCAL)) {
		#		# local_options_name exists means it is already in local mode
		#		if(exists(local_options_name, envir = local_env)) {
		#			return(TRUE)
		#		} else {
		#			return(FALSE)
		#		}
		#	} else if(LOCAL) {  # set to local mode
		#		assign(local_options_name, options, envir = local_env)  # or set to default values ???
		#		return(TRUE)
		#	} else {  # cancel local mode
		#		if(exists(local_options_name, envir = local_env)) {  # just in case it is called in non-local mode
		#			# delete the local option
		#			rm(local_options_name, envir = local_env)
		#			return(FALSE)
		#		}
		#		return(FALSE)
		#	}
		#}
		#######################################
		
		# re-define get_options and set_options
		#if(is_in_local_mode(LOCAL)) {
		#	
		#	get_options = function() {
		#		get(local_options_name, envir = local_env)
		#	}
		#	
		#	set_options = function(options) {
		#		assign(local_options_name, options, envir = local_env)
		#	}
		#	
		#} else {
		
			get_options = function() {
				get("options", envir = options_env)
			}
			
			set_options = function(options) {
				assign("options", options, envir = options_env)
			}
		#}
		
		# first we need a copy of `options`
		options2 = get_options()
		
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

		OPT = getOPT(options2)

		# getting all options
		if(length(args) == 0) {
			val = lapply(options2, getOptionValue, OPT)
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
				val = getOptionValue(options2[[args]], OPT)
				
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
				val = lapply(options[args], getOptionValue, OPT)
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
				
				OPT = getOPT(options2)
				e1 = environment(validate)
				#if(bindingIsLocked("OPT", e1)) 
				unlockBinding("OPT", e1)
				assign("OPT", OPT, envir = e1)

				e2 = environment(filter)
				#if(bindingIsLocked("OPT", e2)) 
				unlockBinding("OPT", e2)
				assign("OPT", OPT, envir = e2)

				# user's value
				value = args[[ name[i] ]]

				if(is.function(value) && length(intersect(class, "function")) == 0) {
					value_fun = value
					e3 = environment(value)
					assign("OPT", OPT, envir = e3)
					#lockBinding("OPT", e3)
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
					if(!any(sapply(class, function(cl) is(value, cl)))) {
						stop(paste("Class of '", name[i], "' should be one of '", paste(class, collapse = ", "), "'.\n", sep = ""))
					}
				}
				
				# test on validate function
				if(!validate(value)) {
					stop("Your option is invalid.\n")
				}

				# filter on data
				value = filter(value)
				
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
				} else {
					options2[[ name[i] ]][["value"]] = value
				}
				
			}
		}
		# assign to original environment
		set_options(options2)
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
			if(is.function(x$default_value)) unlockBinding("OPT", e)
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
			if(is.function(x$default_value)) unlockBinding("OPT", e)
			assign("OPT", OPT, envir = e)
			OPT[[i]] = x$value()
		} else {
			OPT[[i]] = x$value
		}
	}
	return(OPT)
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
		return("CallStack")
	}
}
