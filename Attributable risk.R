cb.1 <- cb.lin(bj)
mod.1 <- mod(bj, "tot", cb.1, 6)
attrdl(bj$ave_pm, basis = cb.1, bj$tot, mod.1, cen = 7.5, type = "an", dir = "forw")

cb <- cb.lin(bj)
mod.1 <- mod(bj, "tot", cb, 6)
attrdl(bj$ave_pm, cb, bj$tot, mod.1, cen = 7.5, type = "an", dir = "forw")


int <- function(a = c(), b = c()){
  out <- paste0(round(a, 0), "(", round(b[1], 0), ",", round(b[2], 0), ")")
  return(out)
}

int <- function(a = c(), b = c()){
  out <- paste0(round(a, 4)*100, "(", round(b[1], 4)*100, ",", round(b[2], 4)*100, ")")
  return(out)
}


### Curve to fit year-round data
attr.lin <- function(data = c(), cold = c(), warm = c(), cause = "tot", cen = c()){
  cb <- cb.lin(data = data)
  model <- mod(data = data, cause = cause, cb = cb, df = 6)
  # year-round
  an.year <- attrdl(x = data$ave_pm, basis = cb, cases = data[, cause], model = model, 
         cen = cen, type = "an", dir = "forw")
  ci.year <- quantile(attrdl(x = data$ave_pm, basis = cb, cases = data[, cause], model = model,
                        cen = cen, type = "an", dir = "forw", sim = T, nsim = 1000),
                 c(0.025, 0.975))
  out.year <- int(a = an.year, b = ci.year)
  # cold season
  an.cold <- attrdl(x = cold$ave_pm, basis = cb, cases = cold[, cause], model = model, 
                    cen = 7.5, type = "an", dir = "forw")
  ci.cold <- quantile(attrdl(x = cold$ave_pm, basis = cb, cases = cold[, cause], model = model,
                             cen = cen, type = "an", dir = "forw", sim = T, nsim = 1000),
                      c(0.025, 0.975))
  out.cold <- int(a = an.cold, b = ci.cold)
  # warm season
  an.warm <- attrdl(x = warm$ave_pm, basis = cb, cases = warm[, cause], model = model, 
                    cen = cen, type = "an", dir = "forw")
  ci.warm <- quantile(attrdl(x = warm$ave_pm, basis = cb, cases = warm[, cause], model = model,
                             cen = cen, type = "an", dir = "forw", sim = T, nsim = 1000),
                      c(0.025, 0.975))
  out.warm <- int(a = an.warm, b = ci.warm)
  output <- rbind(out.year, out.cold, out.warm)
  return(output)
}

attr.spl <- function(data = c(), cold = c(), warm = c(), cause = "tot", cen = c()){
  cb <- cb.spl(data = data, knots = c(75, 150))
  mod <- mod(data = data, cause = cause, cb = cb, df = 6)
  # year-round
  an.year <- attrdl(x = data$ave_pm, basis = cb, cases = data[, cause], model = mod, 
               cen = cen, type = "an", dir = "forw")
  ci.year <- quantile(attrdl(x = data$ave_pm, basis = cb, cases = data[, cause], model = mod,
                        cen = cen,  type = "an", dir = "forw", sim = T, nsim = 1000),
                 c(0.025, 0.975))
  out.year <- int(a = an.year, b = ci.year)
  # cold season
  an.cold <- attrdl(x = cold$ave_pm, basis = cb, cases = cold[, cause], model = mod, 
                    cen = cen, type = "an", dir = "forw")
  ci.cold <- quantile(attrdl(x = cold$ave_pm, basis = cb, cases = cold[, cause], model = mod,
                             cen = cen, type = "an", dir = "forw", sim = T, nsim = 1000),
                      c(0.025, 0.975))
  out.cold <- int(a = an.cold, b = ci.cold)
  # warm season
  an.warm <- attrdl(x = warm$ave_pm, basis = cb, cases = warm[, cause], model = mod, 
                    cen = cen, type = "an", dir = "forw")
  ci.warm <- quantile(attrdl(x = warm$ave_pm, basis = cb, cases = warm[, cause], model = mod,
                             cen = cen, type = "an", dir = "forw", sim = T, nsim = 1000),
                      c(0.025, 0.975))
  out.warm <- int(a = an.warm, b = ci.warm)
  output <- rbind(out.year, out.cold, out.warm)
  return(output)
}

tab.tot <- matrix(NA, nrow = 3, ncol = 6)
colnames(tab.tot) <- c("lin-7.5", "lin-25", "lin-75", "spl-7.5", "spl-25", "spl-75")
rownames(tab.tot) <- c("year-round", "cold season", "warm season")
tab.tot[, 1] <- attr.lin(data = bj, cold = bj.cold, warm = bj.warm, cen = 7.5)
tab.tot[, 2] <- attr.lin(data = bj, cold = bj.cold, warm = bj.warm, cen = 25)
tab.tot[, 3] <- attr.lin(data = bj, cold = bj.cold, warm = bj.warm, cen = 75)
tab.tot[, 4] <- attr.spl(data = bj, cold = bj.cold, warm = bj.warm, cen = 7.5)
tab.tot[, 5] <- attr.spl(data = bj, cold = bj.cold, warm = bj.warm, cen = 25)
tab.tot[, 6] <- attr.spl(data = bj, cold = bj.cold, warm = bj.warm, cen = 75)
saveRDS(tab.tot, file = "af_curve fit year-round data.rds")

readRDS("af_curve fit year-round data.rds")
readRDS("an_curve fit year-round data.rds")



af <- readRDS("AF for total death.rds")
an <- readRDS("AN for total death.rds")
af


cb <- cb.lin(bj)
model <- mod(bj, "tot", cb, 6)
attrdl(bj.cold$ave_pm, cb, bj.cold$tot, model, dir = "forw")
