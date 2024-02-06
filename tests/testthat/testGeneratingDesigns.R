# library(skpr)
# set.seed(1)
#
# # if(Sys.getenv("SKPR_FULL_FACTORIAL_TEST") == "TRUE") {
# #One numeric
# num_two = c(1,-1)
# #One numeric with centerpoints
# num_center = c(1,0,-1)
# #One numeric with 100 points
# num_many = seq(-1,1,length.out = 1000)
# #One numeric that's scaled very large
# num_large = c(-1, 0.5, 0, 0.5, 1)*1e10
# #One numeric that's scaled very small
# num_small = c(-1, 0.5, 0, 0.5, 1)/1e10
#
# #One integers
# int_two = c(1L, -1L)
# #One integer with centerpoints
# int_center = c(1L, 0L, -1L)
# #One integer with 100 points
# int_many = 1L:100L
# #One integer that's scaled very large
# int_large = c(1L,16777216L)
#
# #One 2-level categorical
# fac_two = factor(c("a","b"))
# #One 3-level categorical
# fac_three = factor(c("a","b","c"))
# #One 4-level categorical
# fac_tour = factor(c("a","b","c","d"))
# #One 50-level categorical
# fac_big = factor(as.character(sprintf("a%i", 1:50)))
#
# #One 2-level categorical
# char_two = c("a","b")
# #One 3-level categorical
# char_three = c("a","b","c")
# #One 4-level categorical
# char_tour = c("a","b","c","d")
# #One 50-level categorical
# char_big = as.character(sprintf("a%i", 1:50))
#
#
# #Factors associated with errors
# #Constant numeric
# error_num_const = 1
# #Constant integer
# error_int_one = 1L
# #Single factor
# error_fac_one = factor("a")
#
# basic_cand_set = expand.grid(a=num_two, b=fac_two, c= int_two, d=char_two, stringsAsFactors = FALSE)
# three_cand_set = expand.grid(a=num_center, b=fac_three, c= int_center, d=char_three, stringsAsFactors = FALSE)
# scaled_cand_set = expand.grid(a=num_large, b=num_small, c= int_two, d=char_tour, stringsAsFactors = FALSE)
# many_cand_set = expand.grid(a=num_many, b=int_many, c= int_two, d=fac_big, stringsAsFactors = FALSE)
#
# gen_design_args = expand.grid(candidateset = list(basic_cand_set, three_cand_set, scaled_cand_set),
#                               trials = c(10,50,100),
#                               model = c(~., ~.*., ~(a + b + c + d), ~(a + b + c + d)^2, ~a + c + b:d, ~. - a),
#                               # parallel = c(FALSE, FALSE),
#                               randomized = c(TRUE, FALSE),
#                               contrast = c(contr.simplex, contr.sum, contr.treatment),
#                               optimality = c( "D", "I", "A", "T", "E"),
#                               KEEP.OUT.ATTRS = FALSE,
#                               stringsAsFactors = FALSE)
#
# # #These take extra long--do them separately
# # gen_design_args_g_alias = expand.grid(candidateset = list(basic_cand_set),
# #                                  trials = c(10,50,100,200),
# #                                  model = c(~., ~.*., ~(a + b + c + d), ~(a + b + c + d)^2),
# #                                  # parallel = c(TRUE, FALSE),
# #                                  randomized = c(TRUE, FALSE),
# #                                  contrast = c(contr.simplex, contr.sum, contr.treatment),
# #                                  optimality = c( "G", "ALIAS"),
# #                                  KEEP.OUT.ATTRS = FALSE,
# #                                  stringsAsFactors = FALSE)
# #
# # ptm = proc.time()[3]
# result_test = list()
# # result_eval_test = list()
# # result_eval_mc_test = list()
#
# # for(i in 1:nrow(gen_design_args)) {
# #   print(i)
# #   print(sprintf("Total time elapsed: %f", proc.time()[3]- ptm))
# #   result_test[[i]] = tryCatch({
# #   do.call(gen_design, args = unlist(as.list(gen_design_args[i,]), recursive = FALSE))
# #   }, error = function(e) {
# #     print(e)
# #     e
# #   })
# #   if(inherits(result_test[[i]], "data.frame")) {
# #     result_eval_test[[i]] = tryCatch({
# #       eval_design(result_test[[i]])
# #     }, error = function(e) e)
# #   } else {
# #     result_eval_test[[i]] = FALSE
# #   }
# #   # if(inherits(result_test[[i]], "data.frame")) {
# #   #   result_eval_mc_test[[i]] = tryCatch({
# #   #     eval_design_mc(result_test[[i]], nsim = 100)
# #   #   }, error = function(e) e)
# #   # } else {
# #   #   result_eval_mc_test[[i]] = FALSE
# #   # }
# # }
# # proc.time()[3]- ptm
#
# # error_gen = unlist(lapply(result_test, inherits, "error"))
# # error_eval = unlist(lapply(result_eval_test, inherits, "error"))
# # # error_eval_mc = unlist(lapply(result_eval_mc_test, inherits, "error"))
# #
# # error_message_gen = vector("character", length = length(error_gen))
# # error_message_eval = vector("character", length = length(error_gen))
# # # error_message_eval_mc = vector("character", length = length(error_gen))
#
# # for(i in 1:length(error_gen)) {
# #   error_message_gen[i] = ifelse(error_gen[[i]], as.character(result_test[[i]]), "")
# #   error_message_eval[i] = ifelse(error_eval[[i]], as.character(result_eval_test[[i]]), "")
# #   # error_message_eval_mc[i] = ifelse(error_eval_mc[[i]], as.character(result_eval_mc_test[[i]]), "")
# #
# # }
#
# # gen_design_args_descriptive = expand.grid(candidateset = c("basic_cand_set", "three_cand_set", "scaled_cand_set"),
# #                               trials = c(10,50,100),
# #                               model = as.character(c(~., ~.*., ~(a + b + c + d), ~(a + b + c + d)^2, ~a + c + b:d, ~. - a)),
# #                               # parallel = c(FALSE, FALSE),
# #                               timer = c("TRUE", "FALSE"),
# #                               randomized = c("TRUE", "FALSE"),
# #                               contrast = c("contr.simplex", "contr.sum", "contr.treatment"),
# #                               optimality = c( "D", "I", "A", "T", "E"),
# #                               KEEP.OUT.ATTRS = FALSE,
# #                               stringsAsFactors = FALSE)
# #
# # final_test_results = cbind(gen_design_args_descriptive[seq_len(length(error_gen)),],data.frame(has_gen_error  = error_gen,
# #                                                       has_eval_error = error_gen & error_eval,
# #                                                       # has_eval_mc_error = error_gen & error_eval_mc,
# #                                                       error_msg_gen = error_message_gen,
# #                                                       error_msg_eval = error_message_eval,
# #                                                       # error_msg_eval_mc = error_message_eval_mc,
# #                                                       i = seq_len(length(error_gen))))
#
#
# # saveRDS(final_test_results,"final_test_results.Rds")
# library(dplyr)
# library(skpr)
#
# testthat::test_that("Checking gen_design() and eval_design() errors as expected...", {
#   system.file("tests","testthat", "final_test_results.Rds", package = "skpr")
#   final_test_results = readRDS("final_test_results.Rds")
#   final_test_results$error_gen_msg_regex = factor(final_test_results$error_msg_gen)
#   levels(final_test_results$error_gen_msg_regex) = c("",
#                                                      "non-singular design within given number of repeats",
#                                                      "non-singular design within given number of repeats",
#                                                      "introduced a perfect correlation","Too few runs")
#   final_test_results$error_gen_msg_regex = as.character(final_test_results$error_gen_msg_regex)
#   final_test_results$error_gen_msg_regex = factor(final_test_results$error_msg_gen)
#   levels(final_test_results$error_gen_msg_regex) = c("",
#                                                      "non-singular design within given number of repeats",
#                                                      "non-singular design within given number of repeats",
#                                                      "introduced a perfect correlation","Too few runs")
#   for(i in seq_len(nrow(gen_design_args))) {
#     set.seed(2023)
#     if(final_test_results$has_gen_error[i]) {
#       expect_error(do.call("gen_design", args = unlist(as.list(gen_design_args[i,]), recursive = FALSE)),
#                    regexp = final_test_results$error_gen_msg_regex[i])
#     } else {
#       if(final_test_results$has_eval_error[i]) {
#         temp_design = NA
#         expect_success({temp_design <<- do.call("gen_design", args = unlist(as.list(gen_design_args[i,]), recursive = FALSE))})
#         expect_error(eval_design(result_test[[i]]), regexp = final_test_results[i,]$error_msg_eval)
#       } else {
#         expect_success(eval_design(result_test[[i]]))
#       }
#     }
#   }
# })
