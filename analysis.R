dat <- read.csv(file.choose())


nrow(subset(dat, correct=='True'))
nrow(subset(dat, correct=='False'))

# Amount of fact introduced
unique(dat$fact_id)

#Threshold for items to be considered as learned
threshold = 0.5
nrow(subset(dat, alpha>threshold))
learned <- subset(dat, alpha>threshold)

#Amount of learned facts
length(unique(learned$fact_id))

