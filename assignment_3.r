library(Diagnostics)



learn <- function(cases) {
  # order: P(Pn), P(Te |Pn), P(VTB), P(TB| VTB), P(Sm), P(LC |Sm), P(BR |Sm), P(XR |Pn, VTB, LC), P(Dy |LC, BR)
  pn = table(cases$Pn) + 1
  pn = pn / sum(pn)
  
  #special handling for temperature
  te__pn = generate_te_model(cases)
  
  vtb = table(cases$VTB) + 1
  vtb = vtb / sum(vtb)
  
  tb__vtb = table(cases$VTB, cases$TB) + 1
  for (i in 1:2) {
    tb__vtb[i, ] = tb__vtb[i, ] / sum(tb__vtb[i, ])
  }
  
  sm = table(cases$Sm) + 1
  sm = sm / sum(sm)
  
  lc__sm =  table(cases$Sm, cases$LC)  + 1
  for (i in 1:2) {
    lc__sm[i, ] = lc__sm[i, ] / sum(lc__sm[i, ])
  }
  
  br__sm = table(cases$Sm, cases$Br)  + 1
  for (i in 1:2) {
    br__sm[i, ] = br__sm[i, ] / sum(br__sm[i, ])
  }
  
  xr__pn_tb_lc = table(cases$Pn, cases$TB, cases$LC, cases$XR)  + 1
  for (i in 1:2) {
    for (j in 1:2) {
      for (k in 1:2) {
        xr__pn_tb_lc[i, j, k, ] = xr__pn_tb_lc[i, j, k, ] / sum(xr__pn_tb_lc[i, j, k, ])
      }
    }
  }
  
  dy__lc_br = table(cases$LC, cases$Br, cases$Dy) + 1
  for (i in 1:2) {
    for (j in 1:2) {
      dy__lc_br[i, j, ] = dy__lc_br[i, j, ] / sum(dy__lc_br[i, j, ])
    }
  }
  
  model = list()
  model$pn = pn
  model$te__pn = te__pn
  model$vtb = vtb
  model$tb__vtb = tb__vtb
  model$sm = sm
  model$lc__sm = lc__sm
  model$br__sm  = br__sm
  model$xr__pn_tb_lc = xr__pn_tb_lc
  model$dy__lc_br = dy__lc_br
  
  return(model)
}

generate_te_model <- function(cases) {
  cases0 = list()
  cases1 = list()
  
  for (i in 1:length(cases$Te)) {
    if (cases$Pn[i] == 0) {
      cases0 = append(cases0, cases$Te[i])
    } else{
      cases1 = append(cases1, cases$Te[i])
    }
  }
  cases0 = as.vector(unlist(cases0))
  cases1 =  as.vector(unlist(cases1))
  
  l0 = list()
  l0$mean = mean(cases0)
  l0$sd = sd(cases0)
  l1 = list()
  l1$mean = mean(cases1)
  l1$sd = sd(cases1)
  
  return(list(l0, l1)) # access with var_name[[1]]$sd, var_name[[2]]$sd, etc
}

diagnose <- function(model, cases) {
  samples_number = 1500
  burn_period = 0.1
  
  for (i in 1:length(cases[,1])){
    case = cases[i,]
    samples = generate_samples(cases[i,], samples_number, burn_period, model)
    case = make_predictions(case, samples)
    cases[i,] = case
  }
  return((c(cases$Pn, cases$TB, cases$LC, cases$Br)))
}

evaluate_var_prob <- function(samples, var_name) {
  samples_count = length(samples[,1])
  var_column = samples[var_name]
  var_true_count = sum(var_column)
  true_prob = var_true_count / samples_count
  return(true_prob)
}

make_predictions <- function(case, samples) {
  
  case["Pn"] = evaluate_var_prob(samples, "Pn")
  case["TB"] = evaluate_var_prob(samples, "TB")
  case["LC"] = evaluate_var_prob(samples, "LC")
  case["Br"] = evaluate_var_prob(samples, "Br")
  
  return(case)
}

invert <- function(val) {
  if (val == 0) {
    return(1)
  } else {
    return(0)
  }
}

calc_conditional_probs <- function(sample, model) {
  Pn = sample$Pn
  Te = sample$Te
  VTB = sample$VTB
  TB = sample$TB
  Sm = sample$Sm
  LC = sample$LC
  Br = sample$Br
  XR = sample$XR
  Dy = sample$Dy
  return(
    model$pn[Pn + 1] *
      dnorm(
        mean = model$te__pn[[Pn + 1]]$mean,
        sd = model$te__pn[[Pn + 1]]$sd,
        x = Te
      ) *
      model$vtb[VTB + 1] *
      model$tb__vtb[VTB + 1, TB + 1] *
      model$sm[Sm + 1] *
      model$lc__sm[Sm + 1, LC + 1] *
      model$br__sm[Sm + 1, Br + 1] *
      model$xr__pn_tb_lc[Pn + 1, TB + 1, LC + 1, XR + 1] *
      model$dy__lc_br[LC + 1, Br + 1, Dy + 1]
  )
}

propose_value_for_unknown <- function(sample, unknown_var, model) {
  old_sample = sample
  new_sample = sample
  new_sample[unknown_var] = invert(new_sample[unknown_var])
  p_old = calc_conditional_probs(old_sample, model)
  p_new = calc_conditional_probs(new_sample, model)
  
  if (p_new > p_old) {
    return(new_sample)
  } else {
    threshold = p_new / p_old
    random = runif(1, 0, 1)
    if (random < threshold) {
      return(new_sample)
    } else {
      return(old_sample)
    }
  }
  
}

generate_one_sample <- function(samples, number, model) {
  sample = samples[number,]
  if (number == 1) {  #first sample, assign unknown variables to random values
    sample$Pn = sample(0:1, 1)
    sample$TB = sample(0:1, 1)
    sample$LC = sample(0:1, 1)
    sample$Br = sample(0:1, 1)
  } else {
    prev_sample = samples[number - 1,]
    sample$Pn = prev_sample$Pn
    sample$TB = prev_sample$TB
    sample$LC = prev_sample$LC
    sample$Br = prev_sample$Br
  }
  
  sample = propose_value_for_unknown(sample, "Pn", model)
  sample = propose_value_for_unknown(sample, "TB", model)
  sample = propose_value_for_unknown(sample, "LC", model)
  sample = propose_value_for_unknown(sample, "Br", model)
  
  return(sample)
}

generate_samples <- function(case, samples_number, burn_period, model) {
  
  samples = hist
  samples = samples[1:samples_number,]  # samples_number cannot be more than hist size, maybe fix it later
  samples$Te = case$Te
  samples$VTB = case$VTB
  samples$Sm = case$Sm
  samples$XR = case$XR
  samples$Dy = case$Dy
  
  for (i in 1:samples_number) {
    samples[i,] = generate_one_sample(samples, i, model)
  }
  
  samples_burned = samples_number * burn_period
  samples = samples[samples_burned:length(samples[,1]),]
  
  return(samples)
}
#run_count = 10
#runs = vector(length = run_count)
runDiagnostics(learn, diagnose, verbose = 2)
#for (i in 1:run_count) {
#  runs[i] = runDiagnostics(learn, diagnose, verbose = 2)
#}
# print("Result: ")
# cat("mean: ", mean(runs), "\n")
# cat("sd: ", sd(runs), "\n")
# cat("worse than 0.01345: ", length(runs[runs > 0.01345]), "\n")
# cat("worse than 0.013375: ", length(runs[runs > 0.013375]), "\n")