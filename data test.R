y <- c(85, 90, 88, 70, 75, 92, 78, 85, 80, 82)
grp <- c(1, 1, 1, 2, 2, 3, 3, 3, 3, 3)

mu <- c(87, 72, 81)  # Example: Classroom 1 mean = 87, Classroom 2 mean = 72, Classroom 3 mean = 81

mu_grp_i <- mu[grp]

mu_grp_i

data_set <- data.frame(StudentID = 1:10, Score = y, Classroom = grp, ClassroomMean = mu_grp_i)

print(data_set)
