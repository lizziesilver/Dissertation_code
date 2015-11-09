eval <- read.csv("output.txt")
library(plyr)
comp1 <- ddply(eval, .(nodes), summarize,
					  atpr=mean(atpr), afpr=mean(afpr), 
				      atdr=mean(atdr), otpr=mean(otpr), 
				      ofpr=mean(ofpr), otnr=mean(otnr),
				      ofnr=mean(ofnr), otdr=mean(otdr))

comp2 <- ddply(eval, .(solo), summarize,
					  atpr=mean(atpr), afpr=mean(afpr), 
				      atdr=mean(atdr), otpr=mean(otpr), 
				      ofpr=mean(ofpr), otnr=mean(otnr),
				      ofnr=mean(ofnr), otdr=mean(otdr))

comp3 <- ddply(eval, .(den), summarize,
					  atpr=mean(atpr), afpr=mean(afpr), 
				      atdr=mean(atdr), otpr=mean(otpr), 
				      ofpr=mean(ofpr), otnr=mean(otnr),
				      ofnr=mean(ofnr), otdr=mean(otdr))

comp4 <- ddply(eval, .(mods), summarize,
					  atpr=mean(atpr), afpr=mean(afpr), 
				      atdr=mean(atdr), otpr=mean(otpr), 
				      ofpr=mean(ofpr), otnr=mean(otnr),
				      ofnr=mean(ofnr), otdr=mean(otdr))

comp5 <- ddply(eval, .(mods, solo), summarize,
					  atpr=mean(atpr), afpr=mean(afpr), 
				      atdr=mean(atdr), otpr=mean(otpr), 
				      ofpr=mean(ofpr), otnr=mean(otnr),
				      ofnr=mean(ofnr), otdr=mean(otdr))

comp6 <- ddply(eval, .(mods, solo, nodes), summarize,
					  atpr=mean(atpr), afpr=mean(afpr), 
				      atdr=mean(atdr), otpr=mean(otpr), 
				      ofpr=mean(ofpr), otnr=mean(otnr),
				      ofnr=mean(ofnr), otdr=mean(otdr))

plot(comp1)