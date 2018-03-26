library(tidyr)

# Reads data into two data frames, separating time data
data = read.csv(file="C:/Users/rache/Desktop/Capital\ One/sfpd_dispatch_data_subset.csv", header=FALSE, sep=",",
                colClasses=c(rep("NULL", 6), rep("character", 7), "NULL", "character", rep("NULL", 21)))
all_data = read.csv(file="C:/Users/rache/Desktop/Capital\ One/sfpd_dispatch_data_subset.csv", header=FALSE, sep=",",
                    colClasses=c(rep("character", 6), rep("NULL", 7), "character", "NULL", rep("character", 21)))

# Names columns that will be split for time
names = data[1,]
colnames(data) = names

# Splits time cells to separate the time from date and time zone
for(i in 1:length(names)) {
  temp_data = separate(data, col = names[i], into=c("Date", "Time", "Time Zone"), sep=" ")
  temp_vec = temp_data[i]
  all_data = cbind(temp_vec, all_data)
  all_data = cbind(temp_data[i + 1], all_data)
  all_data = cbind(temp_data[i + 2], all_data)
}

#count = 0
#for(i in all_data[,52]) {
#  if (all_data[i, 52] == "Non Life-threatening") {
#    count = count + 1
#  }
#}
#print(count)