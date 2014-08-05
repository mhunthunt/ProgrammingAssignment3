rankall <- function(outcome, num = "best") {

## Read outcome data
data <- read.csv("D:/Coursera Work/ProgrammingAssignment3/outcome-of-care-measures.csv", colClasses = "character")
outcomes <- c('heart attack'=11, 'heart failure'=17, 'pneumonia'=23)

## Check that outcome is valid
if (!(outcome %in% names(outcomes))) {
	stop('invalid outcome')
}

## For each state, find the hospital of the given rank

##get required data
HospitalName <- as.character(data[,2])
StateName <- as.character(data[,7])
OutcomeScores <- suppressWarnings(as.numeric(data[,outcomes[outcome]]))

mydata<-data.frame(HospitalName, StateName, OutcomeScores)
mydata<-mydata[complete.cases(mydata[,3]),]
mydata<- mydata[with(mydata, order(StateName, OutcomeScores, HospitalName)), ]

##list of hosptals at given rank (or empty)

states<-levels(mydata[,2])
hospitals <- vector()

for (state in states) {
		d <- subset(mydata, mydata[,2] == state)

		if (num != "best" && num != "worst" && num > nrow(d)) {
			hospitalName <- "<NA>"
		} else {
			if (num == "best") {
				hospitalName <- as.character(d[[1,1]])
			} else if (num == "worst") {
				hospitalName <- as.character(d[[nrow(d),1]])
			} else {
				hospitalName <- as.character(d[[num,1]])
			}
		}
		hospitals <- append(hospitals, hospitalName)
	}

##put into dataframe
output <- data.frame(hospital=hospitals, state=states)
return(output)

}
