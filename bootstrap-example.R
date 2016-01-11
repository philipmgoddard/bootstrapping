##############################################
# basic bootstrapping function 

bstrap <- function(df, samples = nrow(df)) {
  # sample row indices with replacement
  inBagRows <- sample(nrow(df), nrow(df), replace = TRUE)
  # define the in bag and out of bag rows
  inBag <- df[inBagRows, ]
  OOB <- df[-inBagRows, ]
  
  # output
  return(list(inSample = inBag, 
              outSample = OOB))
}

##############################################
##############################################
# now to test this function
library(ggplot2)

#################################################
# simulate some data - quadratic with normal noise 
x <- seq(1, 10, 0.1)
y <- 2 * x^2 + x + rnorm(x, 0, 10)
sim_data <- data.frame(x1 = x,
                       x2 = x^2,
                       obs = y)

#################################################
# example- a little plot of in vs out of bag points
example <- bstrap(sim_data)

plotData <- do.call(rbind.data.frame, example)
plotData$group <- unlist(lapply(rownames(plotData),
                                function(x) strsplit(x, split = "[.]")[[1]][1]))

# warning- you probably wont have my plotting color scheme loaded...
plot1 <- ggplot(plotData, aes(x1, obs)) +
  geom_point(aes(color = factor(group)), size = 3, alpha = 0.6) +
  theme_bw() +
  scale_color_manual(values = philTheme()[c(1, 4)], name = "Group") +
  theme(legend.position = c(0.2, 0.7),
        text = element_text(size = 18))

#################################################
# now, for actual bootstrapping we want to repeat that process many times.
# Do 250 resamples, fit a lm with linear and linear + quad terms

resamples <- data.frame(rmse = rep(NA, 500),
                        model = rep(c("lin", "quad"), 250))

for (i in seq(1, 500, 2)) {
  tmp <- bstrap(sim_data)
  fit <- lm(obs ~ x1, data = tmp[[1]])
  fit2 <- lm(obs ~ x1 + x2, data = tmp[[1]])
  OOB = tmp[[2]]
  
  linPred <- predict(fit, OOB[, 1, drop = FALSE])
  quadPred <- predict(fit2, OOB[, 1:2, drop = FALSE])
  correct <- OOB[, 3, drop = TRUE]
                           
  # rmse of out of bag samples
  resamples[i, 1] <- sqrt(mean((linPred - correct) ^ 2))
  resamples[i + 1, 1] <- sqrt(mean((quadPred - correct) ^ 2))
}

#################################################
# plot of resamples- histograms
plot2 <- ggplot(resamples, aes(x = rmse)) +
  geom_histogram(aes(color = model,
                     fill = model),
                 alpha = 0.4,
                 position = "identity",
                 binwidth = 1) +
  scale_color_manual(values = philTheme()) +
  scale_fill_manual(values = philTheme()) +
  theme_bw() + 
  theme(legend.position = c(0.1, 0.7),
        text = element_text(size = 18))  


# boxplots
plot3 <- ggplot(resamples, aes(x = model, y = rmse)) +
  geom_boxplot(aes(color = model,
                   fill = model),
               alpha = 0.4,
               position = "identity",
               binwidth = 1) +
  scale_color_manual(values = philTheme()) +
  scale_fill_manual(values = philTheme()) +
  theme_bw() + 
  theme(legend.position = "none",
        text = element_text(size = 18))

# and finally a t test... it would be advisable to reject the null
# (the two sample means are not equal)
# select paired = TRUE as for each sample, both models are fit
t.test(resamples[resamples$model == "lin", 1],
       resamples[resamples$model == "quad", 1],
       paired = TRUE,
       alternative = "two.sided")
