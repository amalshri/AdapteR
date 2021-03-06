Renv = new.env(parent = globalenv())

Renv$var1 =  rnorm(10)
Renv$prob1 = c(0.1, 0.5, 1, 2)/100

FLenv = as.FL(Renv)

#test failed . Errors showing problem in FL Cast Functions and FL Matrix Arithematic.
#Asana Ticket - https://app.asana.com/0/143316600934101/146934264360563
test_that("quantile: r vector probs",{
    result = eval_expect_equal({
        test10 = quantile(var1)
        test11 = quantile(var1,probs = prob1)
    },Renv,FLenv,
    tolerance=1e-2/length(Renv$var1))
})

#Test failed 
## All methods wont obviously match R output
#Asana Ticket - https://app.asana.com/0/143316600934101/146934264360563
# test_that("Check for quantile function with different types",{
#           result = lapply(1:9 ,function(x){eval_expect_equal({
#                                            paste0("test",x) = quantile(var1,  prob1, type = x)
#                                            },Renv,FLenv)
#                                            })
#           print(result)
#     })
test_that("quantile: FLVector probs",{
    result = eval_expect_equal({
        prob2 = c(0.1, 0.5, 1, 2, 5, 10, 50, NA)/100
        test12 = quantile(var1,probs = prob2)
    },Renv,FLenv,
    tolerance=1e-2/length(Renv$var1))
})
