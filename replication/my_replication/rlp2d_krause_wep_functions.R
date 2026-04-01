ia.plot.df <- function (model, effect, moderator, interaction, type, varcov = "default", 
          minimum = "min", maximum = "max", incr = "default", num_points = 50, 
          conf = 0.95, mean = FALSE, median = FALSE, alph = 80) 
{
  if (type == "binary") {
    minimum = 0
    maximum = 1
    incr = 1
  }
  makeTransparent <- function(someColor, alpha = alph) {
    newColor <- col2rgb(someColor)
    apply(newColor, 2, function(curcoldata) {
      rgb(red = curcoldata[1], green = curcoldata[2], 
          blue = curcoldata[3], alpha = alpha, maxColorValue = 255)
    })
  }
  if (identical(varcov, "default")) {
    covMat = vcov(model)
  }
  else {
    covMat = varcov
  }
  mod_frame = model.frame(model)
  beta_1 = model$coefficients[[effect]]
  beta_3 = model$coefficients[[interaction]]
  if (minimum == "min") {
    min_val = min(mod_frame[[moderator]])
  }
  else {
    min_val = minimum
  }
  if (maximum == "max") {
    max_val = max(mod_frame[[moderator]])
  }
  else {
    max_val = maximum
  }
  if (min_val > max_val) {
    stop("Error: Minimum moderator value greater than maximum value.")
  }
  if (incr == "default") {
    increment = (max_val - min_val)/(num_points - 1)
  }
  else {
    increment = incr
  }
  x_2 <- seq(from = min_val, to = max_val, by = increment)
  delta_1 = beta_1 + beta_3 * x_2
  var_1 = covMat[effect, effect] + (x_2^2) * covMat[interaction, 
                                                    interaction] + 2 * x_2 * covMat[effect, interaction]
  se_1 = sqrt(var_1)
  z_score = qnorm(1 - ((1 - conf)/2))
  upper_bound = delta_1 + z_score * se_1
  lower_bound = delta_1 - z_score * se_1
  max_y = max(upper_bound)
  min_y = min(lower_bound)
  data <- as.data.frame(cbind(x_2, delta_1, upper_bound, lower_bound))
  if (type == "binary") {
    data[1, 1] <- moderator
    data[2, 1] <- stringr::str_remove(interaction, paste0(effect, 
                                                          ":"))
  }
  return(data)
}


ia.plot <- function (data, orig.data, model, moderator, interaction, y.name, 
                     x.name, binw, type, incl.beta) 
{
  data <- as.data.frame(data)
  lower.lim = min(data[, "lower_bound"], na.rm = T)
  upper.lim = max(data[, "upper_bound"], na.rm = T)
  x.max = max(data[, "x_2"], na.rm = T)
  if (incl.beta == TRUE) {
    if (type == "continuous" | type == "binary") {
      stars <- ifelse(model[interaction, 4] < 0.1, "*", 
                      "")
      stars <- ifelse(model[interaction, 4] < 0.05, "**", 
                      stars)
      stars <- ifelse(model[interaction, 4] < 0.01, "***", 
                      stars)
      coef <- paste(round(model[interaction, 1], 3), stars, 
                    sep = "")
    }
  }
  library(ggplot2)
  if (type == "continuous") {
    p <- ggplot() + geom_line(data = data, aes(x = x_2, 
                                               y = delta_1)) + geom_line(data = data, aes(x = x_2, 
                                                                                          y = upper_bound), linetype = 2) + geom_line(data = data, 
                                                                                                                                      aes(x = x_2, y = lower_bound), linetype = 2) + geom_hline(yintercept = 0, 
                                                                                                                                                                                                linetype = 3) + geom_dotplot(data = orig.data, aes_string(x = moderator), 
                                                                                                                                                                                                                             y = lower.lim - 0.02, binaxis = "x", binwidth = binw, 
                                                                                                                                                                                                                             color = "black", fill = "black", dotsize = 0.1, 
                                                                                                                                                                                                                             stackratio = 2.4) + scale_y_continuous(limits = c(lower.lim - 
                                                                                                                                                                                                                                                                                 0.02, upper.lim + 0.02), name = y.name) + scale_x_continuous(name = x.name) + 
      theme_bw(base_size = 12, base_family = "serif") + 
      theme(axis.title.y = element_text(vjust = 0.75), 
            axis.title.x = element_text(vjust = 0), panel.grid = element_blank(), 
            legend.position = "right", panel.border = element_blank(), 
            axis.line.x = element_line(), axis.line.y = element_line())
    if (incl.beta == TRUE) {
      p + geom_text(x = max(data$x_2) - 0.25, y = upper.lim + 
                      0.02, aes(label = paste("β", "=", coef)), hjust = 1, 
                    vjust = 1)
    }
  }
  if (type == "binary") {
    p <- ggplot() + geom_errorbar(data = data, aes(x = factor(x_2), 
                                                   ymin = lower_bound, ymax = upper_bound), width = 0.2) + 
      geom_point(data = data, aes(x = factor(x_2), y = delta_1)) + 
      geom_hline(yintercept = 0, linetype = 3) + scale_y_continuous(limits = c(lower.lim - 
                                                                                 0.02, upper.lim + 0.02), name = y.name) + scale_x_discrete(name = x.name) + 
      theme_bw(base_size = 20, base_family = "serif") + 
      theme(axis.title.y = element_text(vjust = 0.75), 
            axis.title.x = element_text(vjust = 0), panel.grid = element_blank(), 
            legend.position = "right", panel.border = element_blank(), 
            axis.line.x = element_line(), axis.line.y = element_line())
    if (incl.beta == TRUE) {
      p + geom_text(x = Inf, y = Inf, aes(label = paste("β", 
                                                        "=", coef)), hjust = 1, vjust = 1, size = 5.5)
    }
  }
  if (type == "categorical") {
    p <- ggplot() + geom_errorbar(data = data, aes(x = factor(x_2), 
                                                   ymin = lower_bound, ymax = upper_bound), width = 0.2) + 
      geom_point(data = data, aes(x = factor(x_2), y = delta_1)) + 
      geom_hline(yintercept = 0, linetype = 3) + scale_y_continuous(limits = c(lower.lim - 
                                                                                 0.02, upper.lim + 0.02), name = y.name) + scale_x_discrete(name = x.name) + 
      theme_bw(base_size = 20, base_family = "serif") + 
      theme(axis.title.y = element_text(vjust = 0.75), 
            axis.title.x = element_text(vjust = 0), panel.grid = element_blank(), 
            legend.position = "right", panel.border = element_blank(), 
            axis.line.x = element_line(), axis.line.y = element_line())
  }
  return(p)
}


tw.cl.lm <- function (data, dv, cntrls, ia, fe, c1, c2, formula = NULL, 
                      demeaned = FALSE, ...) 
{
  if (is.null(formula)) {
    if (!is.null(fe) & is.null(ia)) {
      data <- na.omit(data[, c(dv, cntrls, fe, c1, c2)])
      if (demeaned == TRUE) {
        library(dplyr)
        library(magrittr)
        data %<>% group_by(.dots = fe) %>% mutate_at(vars(-one_of(fe, 
                                                                  c1, c2)), funs(as.numeric(as.character(.)))) %>% 
          mutate_if(is.numeric, funs(. - mean(., na.rm = T))) %>% 
          ungroup() %>% as.data.frame()
        formula = paste0(dv, "~", paste(cntrls, collapse = "+"))
      }
      else {
        formula = paste0(dv, "~", paste(cntrls, collapse = "+"), 
                         "+ factor(", fe, ")")
      }
    }
    if (!is.null(fe) & !is.null(ia)) {
      ia.cntrl1 <- stringr::str_extract(ia, "\\*.*$")
      ia.cntrl1 <- stringr::str_remove(ia.cntrl1, "\\*")
      ia.cntrl1 <- trimws(ia.cntrl1)
      ia.cntrl2 <- stringr::str_extract(ia, "^.*\\*")
      ia.cntrl2 <- stringr::str_remove(ia.cntrl2, "\\*")
      ia.cntrl2 <- trimws(ia.cntrl2)
      data <- na.omit(data[, c(dv, cntrls, fe, ia.cntrl1, 
                               ia.cntrl2, c1, c2)])
      if (demeaned == TRUE) {
        library(dplyr)
        library(magrittr)
        data %<>% group_by(.dots = fe) %>% mutate_at(vars(-one_of(ia.cntrl1, 
                                                                  ia.cntrl2, fe, c1, c2)), funs(as.numeric(as.character(.)))) %>% 
          mutate_if(is.numeric, funs(. - mean(., na.rm = T))) %>% 
          ungroup() %>% as.data.frame()
        formula = paste0(dv, "~", paste(cntrls, collapse = "+"), 
                         " + ", ia)
      }
      else {
        formula = paste0(dv, "~", paste(cntrls, collapse = "+"), 
                         "+ factor(", fe, ")", " + ", ia)
      }
    }
    if (is.null(fe) & !is.null(ia)) {
      ia.cntrl1 <- stringr::str_extract(ia, "\\*.*$")
      ia.cntrl1 <- stringr::str_remove(ia.cntrl1, "\\*")
      ia.cntrl1 <- trimws(ia.cntrl1)
      ia.cntrl2 <- stringr::str_extract(ia, "^.*\\*")
      ia.cntrl2 <- stringr::str_remove(ia.cntrl2, "\\*")
      ia.cntrl2 <- trimws(ia.cntrl2)
      data <- na.omit(data[, c(dv, cntrls, fe, ia.cntrl1, 
                               ia.cntrl2, c1, c2)])
      formula = paste0(dv, "~", paste(cntrls, collapse = "+"), 
                       " + ", ia)
    }
    if (is.null(fe) & is.null(ia)) {
      data <- na.omit(data[, c(dv, cntrls, c1, c2)])
      formula = paste0(dv, "~", paste(cntrls, collapse = "+"))
    }
  }
  m <- lm(formula, data)
  if (is.null(c1)) {
    vcv <- multiwayvcov::cluster.vcov(m, cbind(data[, c2]))
  }
  if (is.null(c2)) {
    vcv <- multiwayvcov::cluster.vcov(m, cbind(data[, c1]))
  }
  if (!is.null(c1) & !is.null(c2)) {
    vcv <- multiwayvcov::cluster.vcov(m, cbind(data[, c1], 
                                               data[, c2]))
  }
  se <- lmtest::coeftest(m, vcv)
  newList <- list(lm = m, vcv = vcv, tw.se = se)
  return(newList)
}
