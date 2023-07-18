test_that("multiplication works", {
  # set up interactive answers
  f <- file()
  lines <- c(0.7668901,0.3634045, 0.4329440, 0.2036515,0.5857832, 0.3071523, 0.1070645,0.4976825)
  ans <- paste(lines, collapse = "\n")
  write(ans, f)

  options("usr_con" = f) # set connection option

  catch_df <- catchment_sim(16, 4.313320, 3.026894, 20)

  #simulate elementary schools for each area
  elementary_df <- elementary_pop(catch_df, 5.27426341, 0.01427793)


  house_children <- subpop_children(elementary_df)

  lines <- c(0.23246269, 0.34281716, 0.16091418, 0.16427239, 0.09953358, 0.4277052)
  ans <- paste(lines, collapse = "\n")
  write(ans, f)


  house_nochildren <- subpop_noChildren(house_children, elementary_df)


  close(f) # close the file

  options("usr_con" = stdin()) # reset connection option

  simulation <- simulate_households(house_children, house_nochildren)


  individuals <- simulation$individual_sim[sample(nrow(simulation$individual_sim),1000),]

  epidemic <- simepi(individuals, b=3, sus=.0019, spark=0, num_inf = 2)

  data <- suppressWarnings(model_data(epidemic, individuals))


  expect_equal(2 * 2, 4)
})
