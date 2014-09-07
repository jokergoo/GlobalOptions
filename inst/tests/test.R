context("Test `GlobalOptions`")

foo.options = setGlobalOptions(
	a = 1,
	b = "text"
)

test_that("get option values", {
	opt = foo.options()
	expect_that(opt, is_identical_to(list(a = 1, b = "text")))
	
	opt = foo.options("a")
	expect_that(opt, is_identical_to(1))
	
	opt = foo.options("b")
	expect_that(opt, is_identical_to("text"))
	
	expect_that(foo.options("c"), throws_error("No such option"))
	
	opt = foo.options(c("a", "b"))
	expect_that(opt, is_identical_to(list(a = 1, b = "text")))
	
	opt = foo.options("a", "b")
	expect_that(opt, is_identical_to(list(a = 1, b = "text")))
	
	expect_that(foo.options(c("a", "b", "c")), throws_error("No such option"))
	expect_that(foo.options("a", "b", "c"), throws_error("No such option"))
})

test_that("set option values", {
	foo.options("a" = 2)
	opt = foo.options("a")
	expect_that(opt, is_identical_to(2))
	
	foo.options(RESET = TRUE)
	opt = foo.options("a")
	expect_that(opt, is_identical_to(1))
	
	foo.options("a" = 2, "b" = "str")
	opt = foo.options("a")
	expect_that(opt, is_identical_to(2))
	opt = foo.options("b")
	expect_that(opt, is_identical_to("str"))
	
	foo.options(RESET = TRUE)
	op = foo.options()
	foo.options("a" = 2, "b" = "str")
	foo.options(op)
	opt = foo.options("a")
	expect_that(opt, is_identical_to(1))
	opt = foo.options("b")
	expect_that(opt, is_identical_to("text"))
	
	expect_that(foo.options("c" = 1), throws_error("No such option"))
	expect_that(foo.options(1, "b" = "a"), throws_error("When setting options, all arguments should be named"))
	expect_that(foo.options(list(1, "b" = "a")), throws_error("When setting options, all arguments should be named"))
	expect_that(foo.options("a" = 1, "c" = 1), throws_error("No such option"))
})



foo.options = setGlobalOptions(
	a = list(.value = 1,
	         .class = "numeric",
			 .validate = function(x) x > 0),
	b = list(.value = "text",
	         .class = "character")
)

test_that("get option values (customized)", {
	opt = foo.options()
	expect_that(opt, is_identical_to(list(a = 1, b = "text")))
	
	opt = foo.options("a")
	expect_that(opt, is_identical_to(1))
	
	opt = foo.options("b")
	expect_that(opt, is_identical_to("text"))
	
	expect_that(foo.options("c"), throws_error("No such option"))
	
	opt = foo.options(c("a", "b"))
	expect_that(opt, is_identical_to(list(a = 1, b = "text")))
	
	opt = foo.options("a", "b")
	expect_that(opt, is_identical_to(list(a = 1, b = "text")))
})

test_that("set option values (customized)", {
	foo.options("a" = 2)
	opt = foo.options("a")
	expect_that(opt, is_identical_to(2))
	
	foo.options(RESET = TRUE)
	opt = foo.options("a")
	expect_that(opt, is_identical_to(1))
	
	foo.options("a" = 2, "b" = "str")
	opt = foo.options("a")
	expect_that(opt, is_identical_to(2))
	opt = foo.options("b")
	expect_that(opt, is_identical_to("str"))
	
	expect_that(foo.options("a" = "a"), throws_error("should be of class"))
	expect_that(foo.options("a" = -1), throws_error("Your option is invalid"))
	expect_that(foo.options("a" = "a", b = "str"), throws_error("should be of class"))
})
