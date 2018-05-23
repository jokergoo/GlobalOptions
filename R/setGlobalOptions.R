
# == title
# Produce a function which can get or set global options
#
# == param
# -... specification of options, see 'details' section
#
# == detail
# The function has a short name `set_opt`.
#
# The most simple way is to construct an option function (e.g. ``opt()``) as:
#
#     opt = set_opt(
#         "a" = 1,
#         "b" = "text"
#     )
#
# Then users can get or set the options by 
#
#     opt()
#     opt("a")
#     opt$a
#     opt[["a"]]
#     opt(c("a", "b"))
#     opt("a", "b")
#     opt("a" = 2)
#     opt$a = 2
#     opt[["a"]] = 2
#     opt("a" = 2, "b" = "new_text")
#
# Options can be reset to their default values by:
#
#     opt(RESET = TRUE)
#
# The value for each option can be set as a list which contains more configurations of the option:
#
#     opt = set_opt(
#         "a" = list(.value = 1,
#                    .length = 1,
#                    .class = "numeric",
#                    .validate = function(x) x > 0)
#     )
#
# The different fields in the list can be used to filter or validate the option values.
#
# -``.value`` The default value.
# -``.length`` The valid length of the option value. It can be a vector, the check will be passed if one of the length fits.
# -``.class`` The valid class of the option value. It can be a vector, the check will be passed if one of the classes fits.
# -``.validate`` Validation function. The input parameter is the option value and should return a single logical value.
# -``.failed_msg`` Once validation failed, the error message that is printed.
# -``.filter`` Filtering function. The input parameter is the option value and it should return a filtered option value.
# -``.read.only`` Logical. The option value can not be modified if it is set to ``TRUE``.
# -``.visible`` Logical. Whether the option is visible to users.
# -``.private`` Logical. The option value can only be modified in the same namespace where the option function is created.
# -``.synonymous`` a single option name which should have been already defined ahead of current option. The option specified will be shared by current option.
# -``.description`` a short text for describing the option. The description is only used when printing the object.
#
# For more detailed explanation, please go to the vignette.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# # please go to the vignette
setGlobalOptions = function(...) {

	# the environment where the function is called
	envoking_env = parent.frame()
	
	args = list(...)
	
	if(any(is.null(names(args))) || any(names(args) == "")) {
		stop("You should provide named arguments.")
	}
	
	
	if("RESET" %in% names(args)) {
		stop("Don't use 'RESET' as the option name.")
	}
	
	if("READ.ONLY" %in% names(args)) {
		stop("Don't use 'READ.ONLY' as the option name.")
	}

	if("LOCAL" %in% names(args)) {
		stop("Don't use 'LOCAL' as the option name.")
	}

	add_opt = function(arg, name, envoking_env, calling_ns = NULL) {

		if(is.list(arg)) {
			if(".synonymous" %in% names(arg)) {
				if(is.null(options[[ arg[[".synonymous"]] ]])) {
					stop(paste0("Option ", arg[[".synonymous"]], " has not been created yet."))
				}
				opt = options[[ arg[[".synonymous"]] ]]
				return(opt)
			}
		}

		# if it is an advanced setting
		if(is.list(arg) && length(setdiff(names(arg), c(".value", ".class", ".length", ".validate", ".failed_msg", ".filter", ".read.only", ".private", ".visible", ".description"))) == 0) {
			default_value = arg[[".value"]]
			length = if(is.null(arg[[".length"]])) numeric(0) else arg[[".length"]]
			class = if(is.null(arg[[".class"]])) character(0) else arg[[".class"]]
			if(is.null(arg[[".validate"]])) {
				validate = function(x) TRUE
			} else {
				if(is.function(arg[[".validate"]])) {
					validate = arg[[".validate"]]
				} else {
					stop(paste("'.validate' field in", name, "should be a function.\n"))
				}
			}
			failed_msg = ifelse(is.null(arg[[".failed_msg"]]), "Your option is invalid.", arg[[".failed_msg"]][1])
			if(is.null(arg[[".filter"]])) {
				filter = function(x) x
			} else {
				if(is.function(arg[[".filter"]])) {
					filter = arg[[".filter"]]
				} else {
					stop(paste("'.filter' field in", name, "should be a function.\n"))
				}
			}
			read.only = ifelse(is.null(arg[[".read.only"]]), FALSE, arg[[".read.only"]])
			private = ifelse(is.null(arg[[".private"]]), FALSE, arg[[".private"]])
			visible = ifelse(is.null(arg[[".visible"]]), TRUE, arg[[".visible"]])
			description = ifelse(is.null(arg[[".description"]]), "", arg[[".description"]])
		} else {
			if(is.list(arg) && 
				length(intersect(names(arg), c(".value", ".class", ".length", ".validate", "failed_msg", ".filter", ".read.only", ".private", ".visible", ".synonymous", ".description"))) > 0 &&
				length(setdiff(names(arg), c(".value", ".class", ".length", ".validate", "failed_msg", ".filter", ".read.only", ".private", ".visible", ".synonymous", ".description"))) > 0) {
				warning(paste("Your definition for '", name, "' is mixed. It should only contain\n.value, .class, .length, .validate, .failed_msg, .filter, .read.only, .private, .visible, .synonymous, .description. Ignore the setting and use the whole list as the default value.\n", sep = ""))
			}
			default_value = arg
			length = numeric(0)
			class = character(0)
			validate = function(x) TRUE
			failed_msg = "Your option is invalid."
			filter = function(x) x
			read.only = FALSE
			private = FALSE
			visible = TRUE
			description = ""
		}

		opt = GlobalOption$new(
			name          = name,
			default_value = default_value,
			value         = default_value,
		    length        = length,
			class         = class,
			validate      = validate,
			failed_msg    = failed_msg,
			filter        = filter,
			read.only     = read.only,
			private       = private,
			visible       = visible,
			description   = description,
			"__generated_namespace__" = topenv(envoking_env))

		if(!is.null(calling_ns)) {
			opt$set(default_value, calling_ns)
		} else {
			opt$set(default_value, calling_ns, initialize = TRUE)
		}
		return(opt)
	}

	# format the options
	options = vector("list", length = length(args))

	opt_names = names(args)
	names(options) = opt_names
	
	for(i in seq_along(args)) {
		options[[i]] = add_opt(args[[i]], opt_names[i], envoking_env)
	}

	local_options = NULL
	local_options_start_env = NULL
	
	opt_fun = function(..., RESET = FALSE, READ.ONLY = NULL, LOCAL = FALSE) {
		# the environment where foo.options() is called
		calling_ns = topenv(parent.frame())  # top package where foo.options() is called
		
		e = environment()
		if(!missing(LOCAL) && !LOCAL) {
			local_options_start_env <<- NULL
			local_options <<- NULL
			options = options
			# cat("enforce to be global mode.\n")
			return(invisible(NULL))
		} else if(LOCAL) {
			# check whether there is already local_options initialized
			if(is.null(parent.env(e)$local_options_start_env)) {
				local_options_start_env <<- parent.frame() # parent envir is where opt_fun is called
				local_options <<- lapply(options, function(opt) opt$copy())
			} else if(!is.parent.frame(parent.env(e)$local_options_start_env, parent.frame())) {
				local_options_start_env <<- parent.frame() # parent envir is where opt_fun is called
				local_options <<- lapply(options, function(opt) opt$copy())
			}
			options = local_options
			# cat("under local mode: ", get_env_str(local_options_start_env), "\n")
			return(invisible(NULL))
		} else {

			# if local_options_start_env exists, it probably in local mode
			if(!is.null(parent.env(e)$local_options_start_env)) {
				 # if calling frame is offspring environment of local_options_start_env
				if(identical(parent.env(e)$local_options_start_env, parent.frame())) {
					options = local_options
					# cat("in a same environment, still under local mode.\n")
				} else if(is.parent.frame(parent.env(e)$local_options_start_env, parent.frame())) {
					options = local_options
					# cat("in child environment, still under local mode.\n")
				} else {
					local_options_start_env <<- NULL
					local_options <<- NULL
					under_local_mode = FALSE
					options = options
					# cat("leave the local mode, now it is global mode.\n")
				}
			} else {
				options = options
				# cat("under global mode.\n")
			}
		}

		if(RESET) {
			for(i in seq_along(options)) {
				options[[i]]$reset(calling_ns)
			}
			return(invisible(NULL))
		}

		args = list(...)

		# input value is NULL
		if(length(args) == 1 && is.null(names(args)) && is.null(args[[1]])) {
			return(NULL)
		}
		
		# if settings are stored in one object and send this object
		if(length(args) == 1 && is.list(args[[1]]) && is.null(names(args))) {
			args = args[[1]]
		}

		# refresh all 
		# lapply(options[intersect(names(args), names(options))], function(opt) opt$refresh())
		
		# getting all options
		if(length(args) == 0) {
			opts = lapply(options, function(opt) opt$get(calling_ns, read.only = READ.ONLY))

			# some NULL are valid value, some NULL means do not output this option
			opts = opts[sapply(opts, function(opt) is.null(attr(opt, "not_available")))]

			return(opts)
		}
		
		# getting part of the options
		if(is.null(names(args))) {
			args = unlist(args)
			
			if(length(setdiff(args, names(options)))) {
				stop(paste("No such option(s):", paste(setdiff(args, names(options)), collapse = "")))
			}
			
			opts = lapply(options[args], function(opt) opt$get(calling_ns, read.only = READ.ONLY, enforce_visible = TRUE))
			opts = opts[sapply(opts, function(opt) is.null(attr(opt, "not_available")))]

			if(length(args) == 1) {
				opts = opts[[1]]
			}
			return(opts)
		}
		
		# set the options
		name = names(args)
		option.names = names(options)
		if(any(name == "")) {
			stop("When setting options, all arguments should be named.")
		} else {

			# first check on copy
			for(i in seq_along(args)) {
					
				# if there are names which are not defined in options, create one
				if(sum(name[i] %in% option.names) == 0) {
					stop(paste("No such option: '", name[i], "\n", sep = ""))
					# options[[ name[i] ]] <<- add_opt(args[[ name[i] ]], name[i], envoking_env, calling_ns)
				} else {
					# user's value
					value = args[[ name[i] ]]	
					options[[ name[i] ]]$set(value, calling_ns)
				}
			}
		}
		
		return(invisible(NULL))
	}

	class(opt_fun) = "GlobalOptionsFun"
	return(opt_fun)
}

# == title
# Print the GlobalOptionsFun object
#
# == param
# -x the option object returned by `set_opt` or `setGlobalOptions`.
# -... other arguments
#
# == author
# z.gu@dkfz.de
#
print.GlobalOptionsFun = function(x, ...) {
	lt = x()
	options = get("options", envir = environment(x))
	options = options[names(lt)]

	df = data.frame("Option" = names(options), 
		"Value" = sapply(options, function(opt) value2text(opt$real_value, width = Inf)),
		"Description" = sapply(options, function(opt) value2text(opt$description, width = Inf)),
		check.names = FALSE,
		stringsAsFactors = FALSE)
	if(all(df$Description == "\"\"")) {
		df$Description = NULL
	}
	max_nchar = sapply(df, function(x) max(nchar(x)))
	max_nchar = pmax(max_nchar, nchar(colnames(df)))

	if(sum(max_nchar) + length(max_nchar) <= getOption("width")) {
		print(df, row.names = FALSE)
	} else {
		df = lapply(df, function(x) sapply(x, function(y) toString(y, width = round(getOption("width")/ncol(df)))))
		df = do.call("data.frame", df)
		print(df, row.names = FALSE)
	}
}


# == title
# Get a single GlobalOption object
# 
# == param
# -x the option object returned by `set_opt` or `setGlobalOptions`.
# -nm a single name of the option.
#
# == details
# This function is only used internally.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# opt = set_opt(a = 1, b = "b")
# opt["a"]
# opt["b"]
"[.GlobalOptionsFun" = function(x, nm) {
	options = get("options", envir = environment(x))
	if(length(nm) > 1) {
		stop("The index can only be length of 1.\n")
	}
	options[[nm]]
}

# == title
# Print all fields of a single option
#
# == param
# -opt the option object returned by `set_opt` or `setGlobalOptions`.
# -opt_name a single name of the option.
#
# == details
# Actually this function is identical to ``opt[opt_name]``.
#
# == author
# z.gu@dkfz.de
#
# == example
# opt = set_opt(a = 1, b = "b")
# dump_opt(opt, "a")
# dump_opt(opt, "b")
dump_opt = function(opt, opt_name) {
	if(length(opt_name) > 1) {
		stop("The option name can only be length of 1.\n")
	}
	opt[opt_name]
}

# == title
# Get option value by subset operator
#
# == param
# -x the option object returned by `set_opt` or `setGlobalOptions`.
# -nm a single option name.
#
# == details
# ``opt[["a"]]`` is same as ``opt("a")`` or ``opt$a``.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# opt = set_opt(a = 1)
# opt[["a"]]
"[[.GlobalOptionsFun" = function(x, nm) {
	if(is.numeric(nm)) {
		stop("The index should only be option name.\n")
	}
	if(length(nm) > 1) {
		stop("The index can only be length of 1.\n")
	}
	x(nm)
}

# == title
# Set option value by subset operator
#
# == param
# -x the option object returned by `set_opt` or `setGlobalOptions`.
# -nm a single option name.
# -value the value which is assigned to the option.
#
# == details
# ``opt[["a"]] = 1`` is same as ``opt("a" = 1)`` or ``opt$a = 1``.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# opt = set_opt(a = 1)
# opt[["a"]] = 2
# opt$a
"[[<-.GlobalOptionsFun" = function(x, nm, value) {
	if(is.numeric(nm)) {
		stop("The index should only be option names.\n")
	}
	if(length(nm) > 1) {
		stop("The index can only be length of 1.\n")
	}
	
	lt = list(value)
	names(lt) = nm

	assign(".__temp_opt__.", x, envir = parent.frame())
	do.call(".__temp_opt__.", lt, envir = parent.frame())
	rm(".__temp_opt__.", envir = parent.frame())

	return(x)
}

# == title
# The .DollarNames method for the GlobalOptionsFun class
#
# == param
# -x the object returned by `set_opt` or `setGlobalOptions`.
# -pattern pattern, please ignore it.
#
# == details
# This makes the option object looks like a list that it allows
# option name completion after ``$``.
#
# == author
# z.gu@dkfz.de
#
.DollarNames.GlobalOptionsFun = function(x, pattern = "") {
	options = get("options", envir = environment(x))
	names(options)
}

# == title
# Produce a function which can get or set global options
# 
# == param
# -... all go to `setGlobalOptions`
#
# == details
# This is just a short name for `setGlobalOptions`.
#
# == author
# z.gu@dkfz.de
set_opt = function(...) {}
set_opt = setGlobalOptions

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
	oe = environment(fun)  # where `fun` is defined
	environment(fun) = e
	parent.env(e) = oe
	return(fun)
}

deleteEnvBefore = function(fun) {
	environment(fun) = parent.env(environment(fun))
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

is.parent.env = function(p, e) {
	while(1) {
		e = parent.env(e)
		
		if(identical(e, emptyenv())) {
			return(FALSE)
		}
		if(identical(p, e)) {
			return(TRUE)
		}
	}
	return(FALSE)
}

is.parent.frame = function(p, e) {
	if(identical(p, e)) {
		return(FALSE)
	}

	i = 1 + 1
	while(!is_top_env(e)) {
		e = parent.frame(n = i)
		if(identical(p, e)) {
			return(TRUE)
		}
		i = i + 1
	}
	return(FALSE)
}

is_top_env = function(e) {
	if(identical(e, .GlobalEnv)) {
		return(TRUE)
	} else if(isNamespace(e)) {
		return(TRUE)
	} else {
		return(FALSE)
	}
}

# with_sink is copied from testthat package
with_sink = function (connection, code, ...) {
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


stop = function(msg) {
	base::stop(paste(strwrap(msg), collapse = "\n"), call. = FALSE)
}

warning = function(msg) {
	base::warning(paste(strwrap(msg), collapse = "\n"), call. = FALSE)
}

# == title
# Get option value by dollar symbol
#
# == param
# -x the object returned by `set_opt` or `setGlobalOptions`.
# -nm a single option name.
#
# == details
# ``opt$a`` is same as ``opt("a")``.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# opt = set_opt(a = 1)
# opt$a
"$.GlobalOptionsFun" = function(x, nm) {
	x(nm)
}

# == title
# Set option value by dollar symbol
#
# == param
# -x the object returned by `set_opt` or `setGlobalOptions`.
# -nm a single option name.
# -value the value which is assigned to the option.
#
# == details
# ``opt$a = 1`` is same as ``opt("a" = 1)``.
#
# Note you cannot reconfigurate the option by assigning a configuration list.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# opt = set_opt(a = 1)
# opt$a = 2
# opt$a
"$<-.GlobalOptionsFun" = function(x, nm, value) {
	lt = list(value)
	names(lt) = nm

	assign(".__temp_opt__.", x, envir = parent.frame())
	do.call(".__temp_opt__.", lt, envir = parent.frame())
	rm(".__temp_opt__.", envir = parent.frame())

	return(x)
}

