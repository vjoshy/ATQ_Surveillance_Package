
test_that("Sub population of households with Children", {


  # set up interactive answers
  f <- file()
  lines <- c(0.7668901,0.3634045, 0.4329440, 0.2036515,0.5857832, 0.3071523, 0.1070645,0.4976825)
  ans <- paste(lines, collapse = "\n")
  write(ans, f)

  options("usr_con" = f) # set connection option

  catch_df <- catchment_sim(16, 4.313320, 3.026894, 20)

  #simulate elementary schools for each area
  elementary_df <- elementary_pop(catch_df, 5.27426341, 0.01427793)

  # simulate household with children and assign them to elementary school

  output <- capture_output_lines({
    result <- subpop_children(elementary_df)
  })


  mean_children1 <- round(mean(aggregate(result$num_elem_child ~ result$schoolID, FUN="sum")[,2]))
  mean_children2 <- round(mean(elementary_df$schoolPop))


  close(f) # close the file

  options("usr_con" = stdin()) # reset connection option

  # tests
  expect_equal(mean_children1, mean_children1)
})
