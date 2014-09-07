
# == title
# Produce a function which can get or set global options
#
# == param
# -... see 'details' section
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
# And users can reset their default values by:
#
#     foo.options(RESET = TRUE)
#
# The value for each option can be set as a list which may contains more control of the options:
#
#     foo.options = setGlobalOptions(
#         "a" = list(.value = 1,
#                    .class = "numeric",
#                    .validate = function(x) x > 0),
#         "b" = "text"
#     )
#
# ``.class`` and ``.validate`` will be used to check users' input. Please note ``.validate`` function
# should only returns a logical value.
setGlobalOptions = function(...) {
	args = list(...)
	
	if(any(is.null(names(args)))) {
		stop("You should provide named arguments\n")
	}
    
	# format the options
	options = vector("list", length = length(args))
	names(options) = names(args)
	for(i in seq_along(args)) {
	
		arg = args[[i]]
		if(is.list(arg) && length(setdiff(names(arg), c(".value", ".class", ".validate"))) == 0) {
			default_value = arg[[".value"]]
			value = default_value
			class = arg[[".class"]]
			validate = arg[[".validate"]]
		} else {
			default_value = arg
			value = arg
			class = NULL
			validate = NULL
		}
		options[[i]] = list(default_value = default_value,
		                    value = value,
							class = class,
							validate = validate)
	}
	
	
	
	sth.par = function(..., RESET = FALSE) {
	
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
		if(length(args) == 1 && is.list(args[[1]]) && is.null(names(args))) {
			args = args[[1]]
		}
		
		if(length(args) == 0) {
			return(lapply(options, function(x) x[["value"]]))
		}
		
		# just get the options
		if(all(is.null(names(args)))) {
			args = unlist(args)
			
			if(length(setdiff(args, names(options)))) {
				stop(paste("No such option(s):", paste(setdiff(args, names(options)), collapse = ""), "\n"))
			}
			
			if(length(args) == 1) {
				return(options[[args]][["value"]])
			} else {
				return(lapply(options[args], function(x) x[["value"]]))
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
				class = options[[ name[i] ]][["class"]]
				validate = options[[ name[i] ]][["validate"]]
				value = args[[ name[i] ]]
				
				# test on classes of the values
				if(!is.null(class)) {
					for(k in class) {
						if(!is(value, k)) {
							stop(paste("Values of '", name[i], "' should be of class '", k, "'.\n", sep = ""))
						}
					}
				}
				
				# test on validate function
				if(!is.null(validate)) {
					if(!validate(value)) {
						stop("Your option is invalid.\n")
					}
				}
				
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
