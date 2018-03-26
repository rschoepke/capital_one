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

# Make vector of all the different emergency types
temp_vector = unlist(all_data$call_type, use.names = FALSE)
emergency_types = unique(temp_vector)

# Make vectors of all the call received and fulfilled times
temp_times = unlist(all_data[,2], use.names = FALSE)
received_times = c()
fulfilled_times = c()

# Store received times and fulfilled times into vectors
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

# Find the average response time for each emergency type
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
      # Blank fulfilled times
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

# Create data frame of emergency types and their averages
graph_data = data.frame(emergency_types, averages)

# Graph and save average response times
setwd("C:/Users/rache/Desktop/Capital\ One")
png("response_by_type.png")
mar.default <- c(5,4,4,2) + 0.1
par(mar = mar.default + c(7, 0, 0, 0))
barplot(graph_data[,2], xlab="", ylab="Average Respnse Time", 
     main="Average Response Time by Emergency Type", las = 2, cex.names = 0.8, 
     cex.axis = 0.8, names.arg = graph_data[,1], col = "firebrick")
mtext("Emergency Type", side=1, line=10, cex.lab = 0.8)
dev.off()

# Make vector of all the different zipcodes
temp_vector = unlist(all_data$zipcode_of_incident, use.names = FALSE)
zipcodes = unique(temp_vector)

# Find the average response time for each emergency type
averages = c()
for(i in zipcodes) {
  sum = 0
  count = 0
  for (j in 1:length(received_times)) {
    if (i == all_data[j, 34]) {
      # Account for times after midnight
      if (!is.na(fulfilled_times[j]) & fulfilled_times[j] < received_times[j]) {
        add_to_sum = (1440 - received_times[j]) + fulfilled_times[j]
      }
      else if (!is.na(fulfilled_times[j])) {
        add_to_sum = fulfilled_times[j] - received_times[j]
      }
      # Blank fulfilled times
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

# Create data frame of zipcodes and their averages
graph_data = data.frame(zipcodes, averages)

# Graph and save average response times
setwd("C:/Users/rache/Desktop/Capital\ One")
png("response_by_zipcode.png")
barplot(graph_data[,2], xlab="Zipcode", ylab="Average Respnse Time", 
        main="Average Response Time by Zipcode", las = 2, cex.names = 0.8, 
        cex.axis = 0.8, names.arg = graph_data[,1], col = "firebrick")
dev.off()

freq = c()
for (i in emergency_types) {
  count = 0
  for (j in 1:length(all_data[,28])) {
    if(all_data[j, 28] == i) {
      count = count + 1
    }
  }
  freq = c(freq, count)
}

# Graph and save average response times
setwd("C:/Users/rache/Desktop/Capital\ One")
png("incident_freq.png")
pie(x = freq, labels = c(emergency_types[1], emergency_types[2], emergency_types[3],
    emergency_types[4]), main = "Incident Type Frequency", col = rainbow(length(freq)))
legend("topright", emergency_types, cex = 0.65,
       fill = rainbow(length(freq)))
dev.off()

# Make vector of all the different emergency types
temp_vector = unlist(all_data$unit_type, use.names = FALSE)
dispatch_types = unique(temp_vector)

#time1_vec = c()
#time2_vec = c()
#time3_vec = c()
#time4_vec = c()
#time5_vec = c()
#time6_vec = c()
#time7_vec = c()
#time8_vec = c()
#time9_vec = c()
#time10_vec = c()
#time11_vec = c()
#time12_vec = c()
#
#output_matrix = matrix(nrow = length(zipcodes), ncol = 12)

#zip_index = 1
#for (i in zipcodes) {
#  for (j in 1:length(all_data[,44])) {
#    if (received_times[j] < 120) {
#      time1_vec = c(time1_vec, all_data[j, 44])
#    }
#    else if (received_times[j] < 240) {
#      time2_vec = c(time2_vec, all_data[j, 44])
#    }
#    else if (received_times[j] < 360) {
#      time3_vec = c(time3_vec, all_data[j, 44])
#    }
#    else if (received_times[j] < 480) {
#      time4_vec = c(time4_vec, all_data[j, 44])
#    }
#    else if (received_times[j] < 600) {
#      time5_vec = c(time5_vec, all_data[j, 44])
#    }
#    else if (received_times[j] < 720) {
#      time6_vec = c(time6_vec, all_data[j, 44])
#    }
#    else if (received_times[j] < 840) {
#      time7_vec = c(time7_vec, all_data[j, 44])
#    }
#    else if (received_times[j] < 960) {
#      time8_vec = c(time8_vec, all_data[j, 44])
#    }
#    else if (received_times[j] < 1080) {
#      time9_vec = c(time9_vec, all_data[j, 44])
#    }
#    else if (received_times[j] < 1200) {
#      time10_vec = c(time10_vec, all_data[j, 44])
#    }
#    else if (received_times[j] < 1320) {
#      time11_vec = c(time11_vec, all_data[j, 44])
#    }
#    else {
#      time12_vec = c(time12_vec, all_data[j, 44])
#    }
#  }
#  temp_time1 = table(time1_vec)
#  max_time1 = names(temp_time1[temp_time1 == max(temp_time1)])
#output_matrix[zip_index, 1] = max_time1
  
#  temp_time2 = table(time2_vec)
#  max_time2 = names(temp_time2[temp_time2 == max(temp_time2)])
#  output_matrix[zip_index, 2] = max_time2
  
#  temp_time3 = table(time3_vec)
#  max_time3 = names(temp_time3[temp_time3 == max(temp_time3)])
#  output_matrix[zip_index, 3] = max_time3
  
#  temp_time4 = table(time4_vec)
#  max_time4 = names(temp_time4[temp_time4 == max(temp_time4)])
#  output_matrix[zip_index, 4] = max_time4
  
#  temp_time5 = table(time5_vec)
#  max_time5 = names(temp_time5[temp_time5 == max(temp_time5)])
#  output_matrix[zip_index, 5] = max_time5
  
#  temp_time6 = table(time6_vec)
#  max_time6 = names(temp_time6[temp_time6 == max(temp_time6)])
#  output_matrix[zip_index, 6] = max_time5
  
#  temp_time7 = table(time7_vec)
#  max_time7 = names(temp_time7[temp_time7 == max(temp_time7)])
#  output_matrix[zip_index, 7] = max_time7
  
#  temp_time8 = table(time8_vec)
#  max_time8 = names(temp_time8[temp_time8 == max(temp_time8)])
#  output_matrix[zip_index, 8] = max_time8
  
#  temp_time9 = table(time9_vec)
#  max_time9 = names(temp_time9[temp_time9 == max(temp_time9)])
#  output_matrix[zip_index, 9] = max_time9
  
#  temp_time10 = table(time10_vec)
#  max_time10 = names(temp_time10[temp_time10 == max(temp_time10)])
#  output_matrix[zip_index, 10] = max_time10
  
#  temp_time11 = table(time11_vec)
# max_time11 = names(temp_time11[temp_time11 == max(temp_time11)])
#  output_matrix[zip_index, 11] = max_time11
  
#  temp_time12 = table(time12_vec)
#  max_time12 = names(temp_time12[temp_time12 == max(temp_time12)])
#  output_matrix[zip_index, 12] = max_time12
  
#  zip_index = zip_index + 1
#}

#print(output_matrix)


#temp_time = table(all_data$unit_type)
#max_time = names(temp_time[temp_time == max(temp_time)])
#print(max_time)

#output = data.frame(all_data[,3], received_times, all_data$zipcode_of_incident, all_data$unit_type)
#write.csv(output, file = "javascript_input.csv")

for (i in zipcodes) {
  time1 = c()
  time2 = c()
  time3 = c()
  for (j in 1:length(received_times)) {
    if (received_times[j] < 480 && i == all_data[j, 34]) {
      time1 = c(time1, all_data[j, 44])
    }
    else if (received_times[j] < 960 && i == all_data[j, 34]) {
      time2 = c(time2, all_data[j, 44])
    }
    else if (received_times[j] < 1440 && i == all_data[j, 34]) {
      time3 = c(time3, all_data[j, 44])
    }
  }
  print(i)
  table1 = table(time1)
  print(table1)
  table2 = table(time2)
  print(table2)
  table3 = table(time3)
  print(table3)
}

