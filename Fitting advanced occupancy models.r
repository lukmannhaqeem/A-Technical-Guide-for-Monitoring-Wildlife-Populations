### Fitting occupancy models with covariate effects 

#load new data 
gibbon <- read.csv("gibbon.csv", comment = "#", header = TRUE)
str(civet)

#re-organise input data
umf <- unmarkedFrameOccu(
	y = gibbon[, 5:7],								
	siteCovs = data.frame(elevation = gibbon$elevation, forest = gibbon$forest),	
	obsCovs = list(duration = gibbon[, 8:10], day = gibbon[, 11:13]))
summary(umf)

#So far, you have a simple model that does not include covariate effects. You can try
#fitting occupancy models that include covariate effects if you have many sampling sites.
#In some situations, perhaps habitat conditions and sampling efforts might explain the
#variation in detection and occupancy better than simply assuming them to be constant. One
#way to find out is to create a set of models, each containing a different combination of
#your covariates. This example generates 13 more models using every possible combination
#of site and observation covariates. This does take some time to run, and hence we
#recommend choosing carefully candidate model sets based on your knowledge of the
#species/system instead of this approach. 

#So here are the models written in the unmarked notation.

#variations in detection (psi~1)
m2 <- occu(formula = ~day ~1, data = umf)
m3 <- occu(formula = ~duration ~1, data = umf)
m4 <- occu(formula = ~day+duration ~1, data = umf)

#variations in occupancy (p~1)
m5 <- occu(formula = ~1 ~elevation, data = umf)
m6 <- occu(formula = ~1 ~forest, data = umf)
m7 <- occu(formula = ~1 ~elevation+forest, data = umf)

#variations in both detection and occupancy
m8 <- occu(formula = ~day ~elevation, data = umf)
m9 <- occu(formula = ~duration ~elevation, data = umf)
m10 <- occu(formula = ~day ~forest, data = umf)
m11 <- occu(formula = ~duration ~forest, data = umf)
m12 <- occu(formula = ~day ~elevation+forest, data = umf)
m13 <- occu(formula = ~duration ~elevation+forest, data = umf)
m14 <- occu(formula = ~day+duration ~elevation+forest, data = umf)

### Choosing 'best' model

#So you could fit any model, but in practice, you do not usually know what the truth is in
#this case. You have a different hypothesis about what you think might drive occupancy and
#what might variation in detection. If you sit down and think of all the ways things in
#nature could vary, you would potentially end up with many competing models. 

#Once you have created several possible models, you can use the Akaike Information
#Criterion (AIC) to compare them. In plain words, AIC is a single number score that is used
#to determine which of the models is most likely to be the best model for a given data. It
#estimates models relatively, meaning that AIC scores are only useful in comparison with
#other AIC scores for the *same data*. A lower AIC score is better and will be the
#better-fit model.

#You can run the aictab() function from package AICcmodavg to calculate AIC
#on the list of models you have created and do the comparison. The code will produce the
#following output table:

library(AICcmodavg)
fit.list <- aictab(c(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14))
fit.list

#A quick look at the AIC scores reveal that model m10 is considered best for this
#gibbon data. Model m10 included forest cover as predictor of occupancy, and sampling
#duration as predictor of detection. 

### Making predictions

#Now what you want to do is the inference and prediction part. now what you have identified
#the top model (model that receives more support than the rest of the models) what you want
#to do is understand what the model is telling us.

#Probably the best part of modeling is that the model can be used to predict expected
#detection and occupancy when given new data. For example, you could ask "*what is the
#occupancy of a sampling site if it has 40% forest cover?*" or "*what is the detection
#probability of a sampling site if sampling is conducted on Day 60?*" For prediction of a
#single data point, you can use the plogis() function as well to calculate probability like
#the example shown below. Since model m10 have covariates, you need to factor in
#the covariate effect and the value of interest in your calculation.  

#what is the occupancy of a sampling site if it has 40% forest cover?
predict.occupancy <- plogis(m10@opt$par[1] + m10@opt$par[2]*40)
predict.occupancy 

#what is the detection of a sampling site if sampling is done on Day 60?
predict.detection <- plogis(m10@opt$par[3] + m10@opt$par[4]*60)
predict.detection 
 
#You can also illustrate by plotting the predictions of detection and occupancy over the
#range of covariate values observed. To predict a range of data points, you will need to
#use the predict() function instead. The predict() function returns the predictions with
#standard errors and confidence intervals. This information can be used to create plots.

##predict occupancy across forest cover range
#prepare new data
pred.forest <- data.frame(
	forest = seq(min(umf@siteCovs$forest, na.rm = TRUE),	# min forest cover
				max(umf@siteCovs$forest, na.rm = TRUE),		# max forest cover
				length = 100))					# number of points in range
#predict occupancy with new data
pred.for <- predict(
	m10, 									# model to predict from
	type = "state", 							# sub model to predict from
	newdata = pred.forest, 							# new values to use for predictions
	append = TRUE) 								# add newdata to prediction data frame
#setup forest cover plot
library(ggplot2)
gpred.forest <- ggplot() +
	geom_line(aes(x = pred.for$forest, y = pred.for$Predicted), color = "blue", size = 1) +
	geom_ribbon(aes(x = pred.for$forest, ymin = pred.for$lower, ymax = pred.for$upper),
				fill = "skyblue", alpha = 0.1) +
	ylab("Pr(Occupancy)") +
	xlab("Forest cover (%)") + 
	ylim(0, 1) +
	theme_light()

##predict detection across sampling day range
#prepare new data
pred.day <- data.frame(
	day = seq(min(umf@obsCovs$day, na.rm = TRUE),	# first day
			  max(umf@obsCovs$day, na.rm = TRUE),			# last day
			  length = 100))					# number of points in range
#predict detection with new data
pred.d <- predict(
	m10, 									# model to predict from
	type = "det", 			    					# sub model to predict from
	newdata = pred.day, 							# new values to use for predictions
	append = TRUE) 								# add newdata to prediction data frame
#setup forest cover plot
gpred.day <- ggplot() +
	geom_line(aes(x = pred.d$day, y = pred.d$Predicted), color = "blue", size = 1) +
	geom_ribbon(aes(x = pred.d$day, ymin = pred.d$lower, ymax = pred.d$upper),
				fill = "skyblue", alpha = 0.1) +
	ylab("Pr(Detection)") +
	xlab("Day (since survey started)") + 
	ylim(0, 1) +
	theme_light()

#plot predictions side-by-side	
library(cowplot)
plot_grid(gpred.forest, gpred.day, nrow = 1)
 
#You can also illustrate by mapping the predictions of occupancy. For prediction
#over a surface, you will need spatial forest cover data of the whole FMU. You can get such
#data through websites such as Global Forest Watch (www.globalforestwatch.org), although
#you may need to do some fiddling before the data can be used for analysis. Now load
#in the data you want to predict occupancy for. This data needs to have two columns for
#longitude and latitude values for each site/pixel and a column for the covariate value
#(each row is one site/pixel).

sector <- read.csv("sector.csv", header = TRUE)
siteCovs.map <- data.frame(forest = sector$forest)
head(siteCovs.map)

#Like before, you will need to use the predict() function to estimate occupancy,
#standard errors, and confidence intervals for each site/pixel. 

predict.occupancy <- predict(
	m10,        								# model to predict from 
	type = "state",         						# sub model to predict from
	newdata = siteCovs.map,							# new values to use for predictions
	append = TRUE)      							# add newdata to prediction data frame

#save predictions
save(predict.occupancy, file = "predict.occupancy.RData")

#setup elevation map
elevation.map <- ggplot() +
	geom_tile(data = sector, aes(x = easting, y = northing, fill = elevation)) +
		scale_fill_gradient(low = "white", high = "#E69F00") +
	xlab("easting") +
	ylab("northing") +
	coord_fixed() +
	theme_light() +
	theme(plot.title = element_text(hjust = 0.5)) +
	theme(legend.position = "bottom") +
	geom_point(aes(x = gibbon$easting, y = gibbon$northing), colour = "cyan", shape = 0,
			   alpha = 1, size = 1) +
	geom_point(aes(x = 430000, y = 590000), colour = "magenta", size = 3) 	

#setup forest cover map
forest.map <- ggplot() +
	geom_tile(data = sector, aes(x = easting, y = northing, fill = forest)) +
		scale_fill_gradient(low = "white", high = "#009E73") +
	xlab("easting") +
	ylab("northing") +
	coord_fixed() +
	theme_light() +
	theme(plot.title = element_text(hjust = 0.5)) +
	theme(legend.position = "bottom") +
	geom_point(aes(x = gibbon$easting, y = gibbon$northing), colour = "cyan", shape = 0,
			   alpha = 1, size = 1) +
	geom_point(aes(x = 430000, y = 590000), colour = "magenta", size = 3) 

#setup occupancy map
#load prediction output
load("predict.occupancy.RData")
#copy predictions' Mean and SE into sector
sector$predicted.occupancy <- predict.occupancy$Predicted
sector$uncertainty <- predict.occupancy$SE
occupancy.predicted <- ggplot() +
	geom_tile(data = sector, aes(x = easting, y = northing, fill = predicted.occupancy)) +
	scale_fill_viridis(option = "inferno") +
	xlab("easting") +
	ylab("northing") +
	coord_fixed() +
	theme_light() +
	theme(plot.title = element_text(hjust = 0.5)) +
	theme(legend.position = "bottom") +
	geom_point(aes(x = gibbon$easting, y = gibbon$northing), colour = "cyan", shape = 0,
			   alpha = 1, size = 1) +
	geom_point(aes(x = 430000, y = 590000), colour = "magenta", size = 3) 	

#setup occupancy's standard error (uncertainty) map
occupancy.uncertainty <- ggplot() +
	geom_tile(data = sector, aes(x = easting, y = northing, fill = uncertainty)) +
	scale_fill_viridis(option = "inferno") +
	xlab("easting") +
	ylab("northing") +
	coord_fixed() +
	theme_light() +
	theme(plot.title = element_text(hjust = 0.5)) +
	theme(legend.position = "bottom") +
	geom_point(aes(x = gibbon$easting, y = gibbon$northing), colour = "cyan", shape = 0,
			   alpha = 1, size = 1) +
	geom_point(aes(x = 430000, y = 590000), colour = "magenta", size = 3) 		
#plot maps side-by-side
plot_grid(elevation.map, forest.map, nrow = 1)	
plot_grid(occupancy.predicted, occupancy.uncertainty, nrow = 1)	

#As detection is imperfect, we expect the true gibbon occupancy in the FMU to be higher
#than these values. Still, without estimating the detection rate directly, it is difficult
#to say how much higher. Also, remember that there was a lot of variation in detection that
#the model did not explain. So the map shows the best estimate of occupancy based on the
#best model. Not accounting for the discrepancy between naïve occupancy and forecasts from
#the occupancy model can have profound implications for management decisions. If
#non-detections of species are unaccounted for, the total area occupied by the species, and
#therefore the potential conservation value of the area, will be underestimated.

