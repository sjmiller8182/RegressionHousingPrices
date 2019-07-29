#' Print Typical Regression Fit Plots
#' 
#' @description
#' Plots QQ plot of residuals, histogram of residuals,
#' residuals vs predicted values, and studentized 
#' residuals vs predicted values. Depends on tidyverse
#' and gridExtra packages being loaded.
#'
#' @param data The true values corresponding to the input.
#' @param model The predicted/fitted values of the model.

basic.fit.plots <- function(data, model) {
	
	# depends on
	require(tidyverse)
	require(gridExtra)

	# get predicted values
	data$Predicted <- predict(model, data)
	# get residuals
	data$Resid <- model$residuals
	# get studentized residuals
	data$RStudent <- rstudent(model = model)

	# create qqplot of residuals with reference line
	qqplot.resid <- data %>% 
	  ggplot(aes(sample = Resid)) +
	  geom_qq() + geom_qq_line() +
	  labs(subtitle = 'QQ Plot of Residuals',
	       x = 'Theoretical Quantile',
	       y = 'Acutal Quantile')
	
	# create histogram of residuals
	hist.resid <- data %>% 
	  ggplot(aes(x = Resid)) +
	  geom_histogram(bins = 15) + 
	  labs(subtitle = 'Histogram of Residuals',
	       x = 'Residuals',
	       y = 'Count')

	# create scatter plot of residuals vs predicted values
	resid.vs.pred <- data %>% 
	  ggplot(aes(x = Predicted, y = Resid)) +
	  geom_point() +
	  geom_abline(slope = 0) + 
	  labs(subtitle = 'Residuals vs Prediction',
	       x = 'Predicted Value',
	       y = 'Residual')

	# create scatter plot of studentized 
	# residuals vs predicted values
	rStud.vs.pred <- data %>% 
	  ggplot(aes(x = Predicted, y = RStudent)) +
	  geom_point() +
	  geom_abline(slope = 0) + 
  	  geom_abline(slope = 0, intercept = -2) + 
  	  geom_abline(slope = 0, intercept = 2) + 
	  labs(subtitle = 'Studentized Residuals vs Prediction',
	       x = 'Predicted Value',
	       y = 'RStudent')
	
	# add all four plots to grid as
	# qqplot           histogram
	# resid vs pred    RStud vs pred
	grid.arrange(qqplot.resid,
		     hist.resid,
		     resid.vs.pred,
		     rStud.vs.pred, 
		     nrow = 2,
		     top = 'Fit Assessment Plots')
}
