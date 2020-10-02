
#####################
#####Execution ######
#####################

source("LoadingCodes.R")         # Load all the training data and required functions

str="TestExamples/Average.csv"   # change this to try other examples

test=read.csv(str,sep="\t")

main(test)                        # Function inside file "LoadingCodes.R"


