context("Infrastructure")

test_that("check of north carolina I", {
  
  male <- letters[1:4]
  female <- letters[6:7]
  set <- 3 #sets
  reps <-2 #reps
  out <- design_carolina(set= set, r = reps, male = male, female= female, type =1)
  expect_true(is.data.frame(out$book))
  
})



test_that("check of north carolina II", {
  
  male <- letters[1:4]
  female <- letters[6:7]
  set <- 3 #sets
  reps <-2 #reps
  out <- design_carolina(set= set, r = reps, male = male, female= female, type =1)
  expect_true(is.data.frame(out$book))
  
})


# test_that("check NULL values in Replication parameters for NCI",{
#   
#   male <- letters[1:4]
#   female <- letters[1:2]
#   set <- 3 #sets
#   reps <-NULL #reps
#   out <- design_carolinaI(set= set, r = reps, male = male, female= female)
#   expect_true(is.null(out))
#   
# })


test_that("Check number of males with NA values in NCI",{
  
  male <- letters[1]
  female <- letters[1:2]
  set <- 3 #sets
  reps <- NA #reps
  out <- design_carolina( set = set, r = reps, male = male, female = female, type =1 )
  expect_true(is.null(out$book))
  
})

test_that("Check number of males with NA values in NCII",{
  
  male <- letters[1:2]
  female <- letters[1]
  set <- 3 #sets
  reps <- NA #reps
  out <- design_carolina( set = set, r = reps, male = male, female = female, type =2 )
  expect_true(is.null(out$book))
  
})






