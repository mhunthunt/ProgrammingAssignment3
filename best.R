best <- function(state, outcome) {


## Read outcome data

data <- read.csv("D:/Coursera Work/ProgrammingAssignment3/outcome-of-care-measures.csv", colClasses = "character")
outcomes <- c('heart attack'=11, 'heart failure'=17, 'pneumonia'=23)

## Check that state and outcome are valid

if (!(state %in% data[,7])) {
	stop('invalid state')
} else if (!(outcome %in% names(outcomes))) {
	stop('invalid outcome')
}

## Return hospital name in that state with lowest 30-day death rate

##get required data

StateData <- subset(data,data[,7]==state)

HospitalName <- as.character(StateData[,2])
OutcomeScores <- suppressWarnings(as.numeric(StateData[,outcomes[outcome]]))

mydata<-data.frame(HospitalName,OutcomeScores)
mydata<- mydata[with(mydata, order(OutcomeScores,HospitalName)), ]

as.character(mydata[1,1])

}
