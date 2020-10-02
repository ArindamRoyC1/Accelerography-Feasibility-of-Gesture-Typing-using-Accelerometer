# ArindamRoyC1-Accelerography-Feasibility-of-Gesture-Typing-using-Accelerometer
We have constructed gestures, that are easy to remember, not cumbersome to reproduce and easily identifiable, for the entire English alphabet and provide an algorithm to identify the gestures, even when they are constructed continuously. We tackle the problem statistically, taking into account the problem of randomness in the hand movement gestures of users, and achieve an average accuracy of 97.33% with the entire English alphabet. 

The Repository has four files:
  * _TrainData_ : Consists of .rds files for all the letters. They are used as the training set.
  * _TestExamples_ : Contains csv files with the accelarometer data representing different words typed continuously.
  * _LodaingCodes.R_ : Contains all the functions and loads the Training Data
  * _Excecute.R_ : Open it to try out the test examples given. Run the four lines of R code in this file to Execute the entire procedure for a particular test data. 
  
 
 For details of procedures and methodologies, check out our paper:
 arXiv:2003.14310
  
  
