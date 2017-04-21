Renv=new.env(parent=globalenv())
Renv$obj<-rpois(50, lambda = 0.3)
FLenv<-as.FL(Renv)

test_that("croston's test",{
  result = eval_expect_equal({
    obj<-croston(obj, alpha=0.9, h=7)
  },Renv,FLenv,
  tolerance=1e-4,
  expectation=c("obj"),
  verbose=F)
})