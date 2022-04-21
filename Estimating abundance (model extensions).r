...continued

### Fitting occupancy models with covariate effects 

So far, you have a simple model that does not include covariate effects. You can try
fitting occupancy models that include covariate effects if you have many sampling sites.
In some situations, perhaps habitat conditions and sampling efforts might explain the
variation in detection and occupancy better than simply assuming them to be constant. One
way to find out is to create a set of models, each containing a different combination of
your covariates.This example generates 13 more models using every possible combination
of site and observation covariates. This does take some time to run, and hence we
recommend choosing carefully candidate model sets based on your knowledge of the
species/system instead of this approach. 

So here are the models written in the unmarked notation.

#variations in detection (lambda~1)
m2 <- pcount(~day ~1, K = 100, data = umf)
m3 <- pcount(~duration ~1, K = 100, data = umf)
m4 <- pcount(~day+duration ~1, K = 100, data = umf)

#variations in lambda (p~1)
m5 <- pcount(~1 ~elevation, K = 100, data = umf)
m6 <- pcount(~1 ~elevation, K = 100, data = umf)
m7 <- pcount(~1 ~elevation+forest, K = 100, data = umf)

#variations in both detection and occupancy
m8 <- pcount(~day ~elevation, K = 100, data = umf)
m9 <- pcount(~duration ~elevation, K = 100, data = umf)
m10 <- pcount(~day ~forest, K = 100, data = umf)
m11 <- pcount(~day ~forest, K = 100, data = umf)
m12 <- pcount(~day+duration ~elevation, K = 100, data = umf)
m13 <- pcount(~day+duration ~forest, K = 100, data = umf)
m14 <- pcount(~day+duration ~elevation+forest, K = 100, data = umf)

### Choosing 'best' model

You can run the aictab() function from package AICcmodavg to calculate AIC
on the list of models you have created and do the comparison. The code will produce the
following output table:

library(AICcmodavg)
fit.list <- aictab(c(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14))
fit.list

A quick look at the AIC scores reveal that model m9 is considered best for this data. Model
m9 included forest cover as predictor of occupancy, and day as predictor of detection. 

### Making predictions

The model can be used to predict expected detection and occupancy when given new data. For
example, you could ask "*what is the occupancy of a sampling site if it has 40% forest
cover?*" or "*what is the detection probability of a sampling site if sampling is conducted
on Day 60?*" For prediction of a single data point, you can use the plogis() function as
well to calculate probability like the example shown below. Since model m10 have covariates,
you need to factor in the covariate effect and the value of interest in your calculation.

#what is the expected count of a sampling site if it was located at 1500m?
predict.count <- exp(m9@opt$par[1] + m9@opt$par[2]*1500)
predict.count 

#what is the detection probability of a sampling site if the duration was 60 minutes?
predict.detection <- plogis(m9@opt$par[3] + m9@opt$par[4]*60)
predict.detection

You can also illustrate by plotting the predictions of detection and occupancy over the
range of covariate values observed. To predict a range of data points, you will need to
use the predict() function instead. The predict() function returns the predictions with
standard errors and confidence intervals. This information can be used to create plots.

##predict abundance across elevation range
#prepare new data
pred.elev <- data.frame(
	elevation = seq(min(umf@siteCovs$elevation, na.rm = TRUE),	# min elevation cover
                    max(umf@siteCovs$elevation, na.rm = TRUE),  # max elevation cover
                    length = 100),                              # number of points in range
    forest = rep(mean(umf@siteCovs$forest), length = 100))		
#predict count with new data
pred.elevation <- predict(
	m9, 						# model to predict from
	type = "state", 			# sub model to predict from
	newdata = pred.elev, 	    # new values to use for predictions
	append = TRUE) 				# add newdata to prediction data frame
#setup elevation plot
gpred.elevation <- ggplot() +
	geom_line(aes(x = pred.elevation$elevation, y = pred.elevation$Predicted),
              color = "blue", size = 1) +
	geom_ribbon(aes(x = pred.elevation$elevation, ymin = pred.elevation$lower,
                ymax = pred.elevation$upper), fill = "skyblue", alpha = 0.1) +
	ylab("Count") +
	xlab("Elevation (m)") + 
	ylim(0, 30) +    
	theme_light()
   
##predict detection probability across duration range
#prepare new data
pred.dur <- data.frame(
	day = rep(mean(umf@obsCovs$day, na.rm = TRUE), length = 100),
    duration = seq(min(umf@obsCovs$duration, na.rm = TRUE),	# min sampling duration
			       max(umf@obsCovs$duration, na.rm = TRUE),	# max sampling duration
				   length = 100))							# number of points in range
#predict detection with new data
pred.duration <- predict(
	m9, 						# model to predict from
	type = "det", 				# sub model to predict from
	newdata = pred.dur, 	    # new values to use for predictions
	append = TRUE) 				# add newdata to prediction data frame
#setup duration plot
gpred.duration <- ggplot() +
	geom_line(aes(x = pred.duration$duration, y = pred.duration$Predicted),
              color = "blue", size = 1) +
	geom_ribbon(aes(x = pred.duration$duration, ymin = pred.duration$lower,
                ymax = pred.duration$upper), fill = "skyblue", alpha = 0.1) +
	ylab("Pr(Detection)") +
	xlab("Sampling duration (minutes)") + 
	ylim(0, 1) +
	theme_light()
#plot detection predictions side-by-side	
plot_grid(gpred.elevation, gpred.duration, nrow = 1)

You can also illustrate by mapping the predictions of occupancy. For prediction
over a surface, you will need spatial forest cover data of the whole FMU. You can get such
data through websites such as [Global Forest Watch](www.globalforestwatch.org), although
you may need to do some fiddling before the data can be used for analysis. Now load
in the data you want to predict occupancy for. This data needs to have two columns for
longitude and latitude values for each site/pixel and a column for the covariate value
(each row is one site/pixel).

sector <- read.csv("sector.csv", header = TRUE)
siteCovs.map <- data.frame(elevation = sector$elevation)
head(siteCovs.map)

Like before, you will need to use the \texttt{predict()} function to estimate abundance,
standard errors, and confidence intervals for each site/pixel. 
predict.count <- predict(
	m9,        					    # model to predict from 
	type = "state",         		# sub model to predict from
	newdata = siteCovs.map,			# new values to use for predictions
	append = TRUE)      			# add newdata to prediction data frame

#save predictions
save(predict.count, file = "predict.count.RData")

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
	geom_point(aes(x = pig$easting, y = pig$northing), colour = "cyan", shape = 0,
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
	geom_point(aes(x = pig$easting, y = pig$northing), colour = "cyan", shape = 0,
			   alpha = 1, size = 1) +
	geom_point(aes(x = 430000, y = 590000), colour = "magenta", size = 3) 	

#setup abundance map
#load prediction output
load("predict.count.RData")
#copy columns Predict and SE into sector
sector$predicted.count <- predict.count$Predicted
sector$uncertainty <- predict.count$SE
count.predicted <- ggplot() +
	geom_tile(data = sector, aes(x = easting, y = northing, fill = predicted.count)) +
	scale_fill_viridis(option = "inferno") +
	xlab("easting") +
	ylab("northing") +
	coord_fixed() +
	theme_light() +
	theme(plot.title = element_text(hjust = 0.5)) +
	theme(legend.position = "bottom") +
	geom_point(aes(x = pig$easting, y = pig$northing), colour = "cyan", shape = 0,
			   alpha = 1, size = 1) +
	geom_point(aes(x = 430000, y = 590000), colour = "magenta", size = 3) 	

#setup abundance's standard error (uncertainty) map
count.uncertainty <- ggplot() +
	geom_tile(data = sector, aes(x = easting, y = northing, fill = uncertainty)) +
	scale_fill_viridis(option = "inferno") +
	xlab("easting") +
	ylab("northing") +
	coord_fixed() +
	theme_light() +
	theme(plot.title = element_text(hjust = 0.5)) +
	theme(legend.position = "bottom") +
	geom_point(aes(x = pig$easting, y = pig$northing), colour = "cyan", shape = 0,
			   alpha = 1, size = 1) +
	geom_point(aes(x = 430000, y = 590000), colour = "magenta", size = 3) 		
#plot maps side-by-side
plot_grid(elevation.map, forest.map, nrow = 1)	
plot_grid(count.predicted, count.uncertainty, nrow = 1)	
