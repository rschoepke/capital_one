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

vector = unlist(as.numeric(all_data$zipcode_of_incident), use.names = FALSE)
counts = table(vector)
setwd("C:/Users/rache/Desktop/Capital\ One")
png("freq_of_zipcodes.png")
barplot(counts, xlab = "Zipcodes", ylab = "Frequency", las = 2, cex.names = 0.8, cex.axis = 0.8, col =  "firebrick",
        main = "Frequency of Emergencies by Zipcode")
dev.off()
