#
#
#
# AB_t2n_test_function = function(input_list) {
#   result = rep(0, 5)
#
#   temp_list = input_list; temp_list$N <- NULL
#   result[1] = do.call(AB_t2n, temp_list)$N
#
#   temp_list = input_list; temp_list$percent_B <- NULL
#   result[2] = do.call(AB_t2n, temp_list)$percent_B
#
#   temp_list = input_list; temp_list$mean_diff <- NULL
#   result[3] = do.call(AB_t2n, temp_list)$mean_diff
#
#   temp_list = input_list; temp_list$sig_level <- NULL
#   result[4] = do.call(AB_t2n, temp_list)$sig_level
#
#   temp_list = input_list; temp_list$power <- NULL
#   result[5] = do.call(AB_t2n, temp_list)$power
#
#   comparison = abs(c(result[1] / input_list$N - 1,
#                      result[2] / input_list$percent_B - 1,
#                      result[3] / input_list$mean_diff - 1,
#                      result[4] / input_list$sig_level - 1,
#                      result[5] / input_list$power - 1))
#   max(comparison)
# }
#
# a = list(N = 31817,
#         percent_B = .3,
#            mean_diff = .11,
#            sd_A = 2,
#            sd_B = 5,
#            sig_level = .05,
#            power = .8,
#            alternative = 'two_sided')
#
# b = list(N = 3000,
#          percent_B = .4,
#          mean_diff = .15,
#          sd_A = 1,
#          sd_B = 2,
#          sig_level = .05,
#          power = .8,
#          alternative = 'two_sided')
#
# AB_t2n_test_function(b)
#
#
# AB_t2n(N = 3000, percent_B = .3, mean_diff = .15, sd_A = 1,
#        sd_B = 2, sig_level = .05,
#        alternative = 'two_sided')
#
#
#
# b = a; b$power = NULL; b$percent_B = .5
#
# blah = rep(0, 40)
# for(i in seq(1:40)) {
#   b$percent_B = i / 40
#   blah[i] = do.call(AB_t2n, b)$power
# }
#
# c(1:10000)
#
# do.call(AB_t2n, b)$power
#
#
# seq(1, 20)/ 40
#
#
# # no power
# AB_t2n(N = 3000, percent_B = .4, mean_diff = .15, sd_A = 1,
#        sd_B = 2, sig_level = .05,
#        alternative = 'two_sided')
#
# # no significance
# AB_t2n(N = 3000, percent_B = .4, mean_diff = .15, sd_A = 1,
#        sd_B = 2, power = 0.6714653,
#        alternative = 'two_sided')
#
# # no N
# AB_t2n(percent_B = .4, mean_diff = .15, sd_A = 1,
#        sd_B = 2, power = 0.6714653, sig_level = .05,
#        alternative = 'two_sided')
#
# # no mean_diff
# AB_t2n(N = 3000, percent_B = .4, sd_A = 1,
#        sd_B = 2, power = 0.6714653, sig_level = .05,
#        alternative = 'two_sided')
#
# # no percent_B
# AB_t2n(N = 3000, mean_diff = .15, sd_A = 1,
#        sd_B = 2, power = 0.6714653, sig_level = .05,
#        alternative = 'two_sided')
#
# # search grid for mean_diff for tests
#
#
#
# ###
# AB_t2n(percent_B = .2204, N = 5000, sd_A = 1,
#        sd_B = 2, sig_level = .05, mean_diff = .15,
#        alternative = 'two_sided')
#
#
#
#
# AB_t2n(N = 1000, percent_B = .5, mean_diff = .1, sd_A = 1,
#        sd_B = 2, power = .8,
#        alternative = 'two_sided')
#
#
#
# AB_t2n(N = 10000, prop_B = .05, mean_diff = 1, sd_A = 1,
#        sd_B = 2, power = .8,
#        alternative = 'two_sided')
#
#
#
#
#
#
#
# asdf = function(percent_B) eval(power_eval) - power
# asdf(.5)
#
# N_B = N * percent_B
# N_A = N - N_B
# df_ws <- (sd_A ^ 2 / N_A + sd_B ^ 2 / N_B) ^ 2 / (
#   (sd_A ^ 2 / N_A) ^ 2 / (N_A - 1) + (sd_B ^ 2 / N_B) ^ 2 / (N_B - 1)
# )
# t_stat <- mean_diff / sqrt((sd_A ^ 2) / N_A + (sd_B ^ 2) / N_B)
#
# qu <- qt(sig_level / 2, df= df_ws, lower = FALSE)
# pt(qu, df = df_ws, ncp = t_stat, lower = FALSE) +
#   pt(-qu, df = df_ws, ncp = t_stat, lower = TRUE)
#
#
#
#
#
# AB_t2n(N = 3000, percent_B = .4, mean_diff = .15, sd_A = 1,
#        sd_B = 2, sig_level = .05,
#        alternative = 'two_sided')
#
#
# #
# # asdf = function(a, b){
# #   a ^ b
# # }
# #
# # do.call(asdf, list(a=2, b=4))
# # asdf = list(a = 10, b=20,c = 30)
# # asdf2 = asdf; asdf$b = NULL
#
# blah = data.frame(a = 1:10, b = 2:11)
#
AB_t2n_prop(prop_A = .2, prop_B = .25,
            N = 3000, percent_B = .3,
            sig_level = .05, alternative = 'two_sided')

AB_t2n_prop(prop_A = .2, N = 3000, percent_B = .3,
            power = .8, sig_level = .05,
            alternative = 'two_sided')
