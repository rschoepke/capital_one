library(tidyr)

# Reads data into two data frames, separating time data
data = read.csv(file="C:/Users/rache/Desktop/Capital\ One/sfpd_dispatch_data_subset.csv", header=FALSE, sep=",",
                colClasses=c(rep("NULL", 6), rep("character", 7), "NULL", "character", rep("NULL", 21)))
all_data = read.csv(file="C:/Users/rache/Desktop/Capital\ One/sfpd_dispatch_data_subset.csv", header=FALSE, sep=",",
                    colClasses=c(rep("character", 6), rep("NULL", 7), "character", "NULL", rep("character", 21)))

# Names columns that will be split for time
names = c("received_timestamp", "entry_timestamp", "dispatch_timestamp", "response_timestamp", "on_scence_timestamp",
          "transport_timestamp", "hospital_timestamp", "available_timestamp")
colnames(data) = names

data = data[-1,]
all_data = all_data[-1,]

# Splits time cells to separate the time from date and time zone
for(i in 1:length(names)) {
  temp_data = separate(data, col = names[i], into=c("Date", "Time", "Time Zone"), sep=" ")
  temp_vec = temp_data[i]
  all_data = cbind(temp_vec, all_data)
  all_data = cbind(temp_data[i + 1], all_data)
  all_data = cbind(temp_data[i + 2], all_data)
}
print(all_data)
