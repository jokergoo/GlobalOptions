library(testthat)
library(GlobalOptions)

context("Test `GlobalOptions`")

"%==%" = function(x, y) {
	expect_that(x, is_identical_to(y))
}

"%err%" = function(x, y) {
	expect_that(x, throws_error(y))
}


opt = set_opt(
	a = 1,
	b = "text"
)

test_that("get option values", {
	opt() %==% list(a = 1, b = "text")
	opt("a") %==% 1
	opt[["a"]] %==% 1
	opt$a %==% 1
	opt("b") %==% "text"
	opt("c") %err% "No such option"
	opt(c("a", "b")) %==% list(a = 1, b = "text")
	opt("a", "b") %==% list(a = 1, b = "text")
	opt(c("a", "b", "c")) %err% "No such option"
	opt("a", "b", "c") %err% "No such option"
})

test_that("set option values", {
	opt("a" = 2)
	opt("a") %==% 2

	opt$a = 4
	opt$a %==% 4

	opt[["a"]] = 6
	opt$a %==% 6
	
	opt(RESET = TRUE)
	opt("a") %==% 1
	
	opt("a" = 2, "b" = "str")
	opt("a") %==% 2
	opt("b") %==% "str"
	
	opt(RESET = TRUE)
	op = opt()
	opt("a" = 2, "b" = "str")
	opt(op)
	opt("a") %==% 1
	opt("b") %==% "text"
	
	opt("c" = 1) %err% "No such option"
	opt(1, "b" = "a") %err% "When setting options, all arguments should be named"
	opt(list(1, "b" = "a")) %err% "When setting options, all arguments should be named"
	opt("a" = 1, "c" = 1) %err% "No such option"
})

test_that("testing valus are also list", {
	opt("a" = list(a = 1, b = 2))
	expect_that(opt("a"), is_identical_to(list(a = 1, b = 2)))

})

# testing if advanced setting is not mixed
test_that("testing on mixed setting", {
	expect_that(opt <- set_opt(
	a = list(.value = 1,
	         length = 1,
	         class = "numeric")
	), gives_warning("mixed"))
	expect_that(opt("a"), is_identical_to(
		list(.value = 1,
	         length = 1,
	         class = "numeric")
	))
})


# testing .length and .class
opt = set_opt(
	a = list(.value = 1,
	         .length = 1,
	         .class = "numeric")
)

test_that("tesing on .length and .class ", {
	expect_that(opt(), is_identical_to(list(a = 1)))
	expect_that(opt(a = 1:3), throws_error("Length of .* should be"))
	expect_that(opt(a = "text"), throws_error("Class of .* should be"))		
})

# testing read.only
opt = set_opt(
	a = list(.value = 1,
	         .read.only = TRUE),
	b = 2
)

test_that("tesing on .read.only ", {
	expect_that(opt(), is_identical_to(list(a = 1, b = 2)))
	expect_that(opt(a = 2), throws_error("is a read-only option"))
	expect_that(opt(READ.ONLY = TRUE), is_identical_to(list(a = 1)))
	expect_that(opt(READ.ONLY = FALSE), is_identical_to(list(b = 2)))
})

opt = set_opt(
	a = list(.value = 1,
		     .validate = function(x) x > 0,
		     .failed_msg = "'a' should be a positive number.")
)

test_that("testing on .failed_msg", {
	expect_that(opt(a = -1), throws_error("positive"))
})

# testing .validate and .filter
opt = set_opt(
	a = list(.value = 1,
	         .validate = function(x) x > 0 && x < 10,
	         .filter = function(x) c(x, x))
)

test_that("tesing on .validate and .filter ", {
	expect_that(opt(), is_identical_to(list(a = c(1))))
	opt(a = 2)
	expect_that(opt(), is_identical_to(list(a = c(2, 2))))
	expect_that(opt(a = 20), throws_error("Your option is invalid"))
})

# test value after filter
opt = set_opt(
	a = list(.value = 1,
	         .length = 1,
	         .filter = function(x) c(x, x))
)
test_that("testing on validation of filtered value", {
	expect_that(opt(a = 2), throws_error("Length of filtered"))
})

# testing if .value is a function
opt = set_opt(
	a = list(.value = 1),
	b = list(.value = 2,
		     .class = "function"),
	c = list(.value = function(x) 3,
		     .class = "numeric")
)

test_that("testing if '.value' is set as a function", {
	#expect_that(opt(), is_identical_to(list(a = 1, b = 2, c = 3)))
	opt(a = function(x) 1)
	expect_that(opt("a"), is_identical_to(1))
	opt(b = function(x) 2)
	expect_that(body(opt("b")), is_identical_to(2))
	expect_that(opt(c = function(x) "text"), throws_error("Class of .* should be"))

})

# testing if.value is a function and uses OPT
opt = set_opt(
	a = 1,
	b = function() .v$a * 2
)

test_that("tesing if '.value' is a function and using other option values", {
	expect_that(opt("b"), is_identical_to(2))
	opt(a = 2)
	expect_that(opt("b"), is_identical_to(4))
	opt(RESET = TRUE)
	expect_that(opt("b"), is_identical_to(2))
})

# testing if.validate and .filter use OPT
opt = set_opt(
	a = 1,
	b = list(.value = 2,
	     .validate = function(x) {
	     	if(.v$a > 0) x > 0
	     	else x < 0
	     },
	     .filter = function(x) {
	     	x + .v$a
	     })
)

test_that("tesing '.validate' and '.filter' using other option values", {
	opt(a = 1, b = 2)
	expect_that(opt("b"), is_identical_to(3))
	expect_that(opt(a = 1, b = -1), throws_error("Your option is invalid"))
	expect_that(opt(a = -1, b = 1), throws_error("Your option is invalid"))
})

# test in input value is NULL
opt = set_opt(
	a = 1
)

test_that("tesing if input value is NULL", {
	expect_that(opt(NULL), is_identical_to(NULL))
	opt(a = NULL)
	expect_that(opt("a"), is_identical_to(NULL))
})

## test if .value is invisible
opt = set_opt(
	a = list(.value = 1,
	         .visible = FALSE),
	b = 1
)

test_that("testing if '.value' is visible", {
	expect_that(opt(), is_identical_to(list(b = 1)))
	expect_that(opt("a"), is_identical_to(1))
	opt(a = 2)
	expect_that(opt("a"), is_identical_to(2))
})

############################################

opt = set_opt(
	a = 1
)

f1 = function() {
	opt(LOCAL = TRUE)
	opt(a = 2)
	return(opt("a"))
}

f1() # 2


f2 = function() {
	opt(LOCAL = TRUE)
	opt(a = 4)
	return(opt("a"))
}

f2() # 4

test_that("testing local mode", {
	expect_that(f1(), is_identical_to(2))
	expect_that(f2(), is_identical_to(4))
	expect_that(opt$a, is_identical_to(1))

	opt(LOCAL = TRUE)
	opt(a = 4)
	expect_that(opt("a"), is_identical_to(4))
	opt(LOCAL = FALSE)
	expect_that(opt("a"), is_identical_to(1))
})


opt = setGlobalOptions(
	a = 1
)

f1 = function() {
	opt(LOCAL = TRUE)
	opt(a = 2)
	return(f2())
}

f2 = function() {
	opt("a")
}

f1()  # 2

test_that("testing local mode 2", {
	expect_that(f1(), is_identical_to(2))
	expect_that(opt("a"), is_identical_to(1))
})

opt = set_opt(
	a = 1
)

opt(LOCAL = TRUE)
opt(a = 2)

f1 = function() {
	return(opt("a"))
}

f1()

test_that("testing local mode 3", {
	expect_that(f1(), is_identical_to(2))
})

opt = set_opt(
	a = list(.value = 1,
		     .private = TRUE)
)
require(stats)
ns = getNamespace("stats")
environment(opt)$options$a$`__generated_namespace__` = ns
test_that("testing private", {
	expect_that(opt$a <- 2, throws_error("is a private option"))
})


##########################################

opt = set_opt(a = NULL)
opt$a = 1
opt$a = NULL
test_that("testing set value to NULL", {
	expect_that(opt$a, is_identical_to(NULL))
})

##########################################

opt = set_opt(a = 1, b = list(".synonymous" = "a"))
test_that("test .synonymous", {
	expect_that(opt$a, is_identical_to(opt$b))
	opt(a = 2)
	expect_that(opt$a, is_identical_to(opt$b))
	expect_that(opt$a, is_identical_to(2))
	opt(b = 3)
	expect_that(opt$a, is_identical_to(opt$b))
	expect_that(opt$a, is_identical_to(3))

	expect_that(opt <- set_opt(a = 1, b = list(".synonymous" = "c"), c = 1), 
		throws_error("has not been created yet"))
})

#### test ADD
opt = set_opt(a = 1)
test_that("test ADD", {
	expect_that(opt$b <- 1, throws_error("No such option"))
	opt(b = 1, ADD = TRUE)
	expect_that(opt$b, is_identical_to(1))

	opt(c = list(.value = "a", .class = "character"), ADD = TRUE)
	expect_that(opt$c <- 1, throws_error("should be"))

	opt(d = list(.value = 1, .class = "numeric"),
		e = list(.value = "a", .class = "character"),
		ADD = TRUE)
	expect_that(opt$d, is_identical_to(1))
	expect_that(opt$e, is_identical_to("a"))
})
