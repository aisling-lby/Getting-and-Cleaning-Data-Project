# Codebook

The tidy data contains the average for each variable for each activity and each subject. Only variables names contain std or mean were calculated. 

## Procedure
- Training data and test data were merged to create one data set
- Only the measurements on the mean and std for each measurement were kept.
- Descriptive activity names were replaced in the data set.
- Change some variables name. For example, column names start with t will be replaced with Time; column names start with f will be replaced with Frequency, column names containing Acc, Gyro, Mag, tBody will be replaced with Accelerometer, Gyroscope, Magnitude, TimeBody respectively. () will be removed.
- Tidy data was created based on the data set above, it was a summary with the average of each variable for each activity and each subject.

## Variables
1. activity_label, including WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING
2. subject
3. TimeBodyAccelerometer-mean-X
4. TimeBodyAccelerometer-mean-Y
5. TimeBodyAccelerometer-mean-Z
6. TimeBodyAccelerometer-std-X                     
7. TimeBodyAccelerometer-std-Y                     
8. TimeBodyAccelerometer-std-Z                     
9. TimeGravityAccelerometer-mean-X                 
10. TimeGravityAccelerometer-mean-Y                 
11. TimeGravityAccelerometer-mean-Z                 
12. TimeGravityAccelerometer-std-X                  
13. TimeGravityAccelerometer-std-Y                  
14. TimeGravityAccelerometer-std-Z                  
15. TimeBodyAccelerometerJerk-mean-X                
16. TimeBodyAccelerometerJerk-mean-Y                
17. TimeBodyAccelerometerJerk-mean-Z                
18. TimeBodyAccelerometerJerk-std-X                 
19. TimeBodyAccelerometerJerk-std-Y                 
20. TimeBodyAccelerometerJerk-std-Z                 
21. TimeBodyGyroscope-mean-X                        
22. TimeBodyGyroscope-mean-Y                        
23. TimeBodyGyroscope-mean-Z                        
24. TimeBodyGyroscope-std-X                         
25. TimeBodyGyroscope-std-Y                         
26. TimeBodyGyroscope-std-Z                         
27. TimeBodyGyroscopeJerk-mean-X                    
28. TimeBodyGyroscopeJerk-mean-Y                    
29. TimeBodyGyroscopeJerk-mean-Z                    
30. TimeBodyGyroscopeJerk-std-X                     
31. TimeBodyGyroscopeJerk-std-Y                     
32. TimeBodyGyroscopeJerk-std-Z                     
33. TimeBodyAccelerometerMagnitude-mean             
34. TimeBodyAccelerometerMagnitude-std              
35. TimeGravityAccelerometerMagnitude-mean          
36. TimeGravityAccelerometerMagnitude-std           
37. TimeBodyAccelerometerJerkMagnitude-mean         
38. TimeBodyAccelerometerJerkMagnitude-std          
39. TimeBodyGyroscopeMagnitude-mean                 
40. TimeBodyGyroscopeMagnitude-std                  
41. TimeBodyGyroscopeJerkMagnitude-mean             
42. TimeBodyGyroscopeJerkMagnitude-std              
43. FrequencyBodyAccelerometer-mean-X               
44. FrequencyBodyAccelerometer-mean-Y               
45. FrequencyBodyAccelerometer-mean-Z               
46. FrequencyBodyAccelerometer-std-X                
47. FrequencyBodyAccelerometer-std-Y                
48. FrequencyBodyAccelerometer-std-Z                
49. FrequencyBodyAccelerometerJerk-mean-X           
50. FrequencyBodyAccelerometerJerk-mean-Y           
51. FrequencyBodyAccelerometerJerk-mean-Z           
52. FrequencyBodyAccelerometerJerk-std-X            
53. FrequencyBodyAccelerometerJerk-std-Y            
54. FrequencyBodyAccelerometerJerk-std-Z            
55. FrequencyBodyGyroscope-mean-X                   
56. FrequencyBodyGyroscope-mean-Y                   
57. FrequencyBodyGyroscope-mean-Z                   
58. FrequencyBodyGyroscope-std-X                    
59. FrequencyBodyGyroscope-std-Y                    
60. FrequencyBodyGyroscope-std-Z                    
61. FrequencyBodyAccelerometerMagnitude-mean        
62. FrequencyBodyAccelerometerMagnitude-std         
63. FrequencyBodyBodyAccelerometerJerkMagnitude-mean
64. FrequencyBodyBodyAccelerometerJerkMagnitude-std 
65. FrequencyBodyBodyGyroscopeMagnitude-mean        
66. FrequencyBodyBodyGyroscopeMagnitude-std         
67. FrequencyBodyBodyGyroscopeJerkMagnitude-mean    
68. FrequencyBodyBodyGyroscopeJerkMagnitude-std  
