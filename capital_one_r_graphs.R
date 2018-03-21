library(tidyr)
library(plyr)

# Read data into two data frames, separating time data
data = read.csv(file="C:/Users/rache/Desktop/Capital\ One/sfpd_dispatch_data_subset.csv", header=FALSE, sep=",",
                colClasses=c(rep("NULL", 6), rep("character", 7), "NULL", "character", rep("NULL", 21)))
all_data = read.csv(file="C:/Users/rache/Desktop/Capital\ One/sfpd_dispatch_data_subset.csv", header=FALSE, sep=",",
                    colClasses=c(rep("character", 6), rep("NULL", 7), "character", "NULL", rep("character", 21)))

# Name columns
names = c("received_timestamp", "entry_timestamp", "dispatch_timestamp", "response_timestamp", "on_scence_timestamp",
          "transport_timestamp", "hospital_timestamp", "available_timestamp")
colnames(data) = names

other_names = c("call_number", "unit_id", "incident_number", "call_type", "call_date", "watch_date", "call_final_disposition",
                "address", "city", "zipcode_of_incident", "battalion", "station_area", "box", "original_priority", "priority",
                "priority", "als_unit", "call_type_group", "number of alarms", "unit_type", "unit_sequence_in_call_dispatch",
                "fire_prevention_district", "supervisor_district", "neighborhood_district", "location", "row_id", "latitude",
                "logitude")
colnames(all_data) = other_names

# Remove the row with titles of columns
data = data[-1,]
all_data = all_data[-1,]

# Split time cells to separate the time from date and time zone
for(i in 1:length(names)) {
  temp_data = separate(data, col = names[i], into=c("Date", "Time", "Time Zone"), sep=" ")
  temp_vec = temp_data[i]
  all_data = cbind(temp_vec, all_data)
  all_data = cbind(temp_data[i + 1], all_data)
  all_data = cbind(temp_data[i + 2], all_data)
}

# Create graph showing frequency of emergencies by zipcode and save to png file
vector = unlist(as.numeric(all_data$zipcode_of_incident), use.names = FALSE)
counts = table(vector)
setwd("C:/Users/rache/Desktop/Capital\ One")
png("freq_of_zipcodes.png")
barplot(counts, xlab = "Zipcodes", ylab = "Frequency", las = 2, cex.names = 0.8, cex.axis = 0.8, col =  "firebrick",
        main = "Frequency of Emergencies by Zipcode")
dev.off()

#Create a dataframe to also hold their corresponding start and end times
#For each element in the vector, call the function
#Function: run through the column of the data frame, find the difference between start and end times
#Add this difference to a sum and increase the count for that member
#Store average in dataframe
#Move to next element

# Make vector of all the different emergency types
temp_vector = unlist(all_data$call_type, use.names = FALSE)
emergency_types = unique(temp_vector)

# Make vectors of all the call received and fulfilled times
temp_times = unlist(all_data[,2], use.names = FALSE)
received_times = c()
fulfilled_times = c()

for(i in 1:length(temp_times)) {
  # Received
  time = all_data[i,23]
  time2 = as.numeric(unlist(strsplit(time, ":")))
  mins = time2[1] * 60 + time2[2] + time2[3] / 60
  received_times = c(received_times, mins)
  
  #Fulfilled
  time = all_data[i, 11]
  time2 = as.numeric(unlist(strsplit(time, ":")))
  mins = time2[1] * 60 + time2[2] + time2[3] / 60
  fulfilled_times = c(fulfilled_times, mins)
}

averages = c()

for(i in emergency_types) {
  sum = 0
  count = 0
  for (j in 1:length(received_times)) {
    if (i == all_data[j, 28]) {
      # Account for times after midnight
      if (!is.na(fulfilled_times[j]) & fulfilled_times[j] < received_times[j]) {
        add_to_sum = (1440 - received_times[j]) + fulfilled_times[j]
      }
      else if (!is.na(fulfilled_times[j])) {
        add_to_sum = fulfilled_times[j] - received_times[j]
      }
      else {
        add_to_sum = 0
        count = count - 1
      }
      # For finding the average
      sum = sum + add_to_sum
      count = count + 1
    }
  }
  average = sum / count
  averages = c(averages, average)
}

graph_data = data.frame(emergency_types, averages)

setwd("C:/Users/rache/Desktop/Capital\ One")
png("response_by_type.png")
mar.default <- c(5,4,4,2) + 0.1
par(mar = mar.default + c(7, 0, 0, 0))
barplot(graph_data[,2], xlab="", ylab="Average Respnse Time", 
     main="Average Response Time by Emergency Type", las = 2, cex.names = 0.8, 
     cex.axis = 0.8, names.arg = graph_data[,1], col = "firebrick")
mtext("Emergency Type", side=1, line=10, cex.lab = 0.8)
dev.off()
