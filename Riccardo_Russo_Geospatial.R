# Load necessary libraries
library(readr)
library(dplyr)  
library(sf)
library(spdep)
library(tmap)
library(ggplot2)
library(leaflet)
library(boot)
library(leaps)
library(spatialreg)
library(viridis)
library(leaflet.extras)

setwd("C:/Users/ricca/Documents/geospatial/progetto_geospatial/")
listings_path <- "listings.csv.gz"
listings_df <- read_csv(listings_path, col_types = cols())  
listings_df<- filter(listings_df, neighbourhood_cleansed=="Lecce")

neighborhoods_path <- "neighbourhoods.csv"
neighborhoods_df <- read_csv(neighborhoods_path, col_types = cols())

neighborhoods_path_gjson <- "neighbourhoods.geojson"
neighborhoods_sf <- st_read(neighborhoods_path_gjson)
neighborhoods_sf <- st_make_valid(neighborhoods_sf)
neighborhoods_sf<- filter(neighborhoods_sf, neighbourhood=="Lecce")

listings_df<- select(listings_df,id, host_response_rate, host_has_profile_pic,
                     host_identity_verified, room_type, host_listings_count,
                     host_total_listings_count, neighbourhood_cleansed, 
                     accommodates,bathrooms_text, bedrooms , beds, price, 
                     minimum_nights, maximum_nights, number_of_reviews, 
                     review_scores_rating, review_scores_location,review_scores_value, 
                     host_acceptance_rate, longitude, latitude)

listings_df$price <- as.numeric(gsub("[$,]", "", listings_df$price))
listings_df$host_response_rate[listings_df$host_response_rate=="N/A"] = 0
listings_df$host_acceptance_rate[listings_df$host_acceptance_rate=="N/A"] = 0
listings_df$host_response_rate <- as.numeric(gsub("[%,]", "", listings_df$host_response_rate))/100
listings_df$host_acceptance_rate <- as.numeric(gsub("[%,]", "", listings_df$host_acceptance_rate))/100
listings_df$bathrooms_text[listings_df$bathrooms_text %in% c("Half-bath", "Private half-bath", "Shared half-bath")] <- 0.5
listings_df$bathrooms_text <- as.numeric(sub("([0-9.]+).*", "\\1", listings_df$bathrooms_text))



plot(neighborhoods_sf$geometry)
perimeter <- st_union(st_boundary(neighborhoods_sf$geometry))
perimeter <- st_cast(st_union(st_geometry(neighborhoods_sf)), "POLYGON")
grid <- st_make_grid(perimeter, cellsize = c(0.007, 0.007), what= "polygons", square=F)
result_grid_neighborhoods <- st_intersection(perimeter, grid)
plot(result_grid_neighborhoods, main = "Intersection of Perimeter and Grid")

listings_sf <- st_as_sf(listings_df, coords = c("longitude", "latitude"), crs = st_crs(result_grid_neighborhoods))
result_grid_neighborhoods <- st_make_valid(result_grid_neighborhoods)
listings_sf$GRID_ID <- st_within(listings_sf, result_grid_neighborhoods)


summary_data <- listings_sf %>%
  group_by(GRID_ID) %>%
  summarise(host_response_rate = median(host_response_rate, na.rm = TRUE),
            host_acceptance_rate = median(host_acceptance_rate, na.rm = TRUE),
            host_listings_count = median(host_listings_count, na.rm = TRUE),
            accommodates = median(accommodates, na.rm = TRUE),
            bathrooms_text = median(bathrooms_text, na.rm = TRUE),
            bedrooms = median(bedrooms, na.rm = TRUE),
            beds = median(beds, na.rm = TRUE),
            minimum_nights = median(minimum_nights, na.rm = TRUE),
            maximum_nights = median(maximum_nights, na.rm = TRUE),
            number_of_reviews = median(number_of_reviews, na.rm = TRUE),
            review_scores_rating = median(review_scores_rating, na.rm = TRUE),
            review_scores_location = median(review_scores_location, na.rm = TRUE),
            review_scores_value = median(review_scores_value, na.rm = TRUE),
            price = median(price, na.rm = TRUE),
            geometry = st_centroid(st_union(geometry)),
            host_has_profile_pic = names(table(host_has_profile_pic))[which.max(table(host_has_profile_pic))],
            host_identity_verified = names(table(host_identity_verified))[which.max(table(host_identity_verified))],
            room_type = names(table(room_type))[which.max(table(room_type))],
            bnb_density= n()
  )

any(is.na(summary_data))
summary_data$review_scores_rating <- ifelse(is.na(summary_data$review_scores_rating), 0, summary_data$review_scores_rating)
summary_data$review_scores_location <- ifelse(is.na(summary_data$review_scores_location), 0, summary_data$review_scores_location)
summary_data$review_scores_value <- ifelse(is.na(summary_data$review_scores_value), 0, summary_data$review_scores_value)
summary_data$bedrooms <- ifelse(is.na(summary_data$bedrooms), median(summary_data$bedrooms, na.rm = TRUE), summary_data$bedrooms)
summary_data$beds <- ifelse(is.na(summary_data$beds), median(summary_data$beds, na.rm = TRUE), summary_data$beds)
any(is.na(summary_data))


tm_polygons <- tm_shape(result_grid_neighborhoods) +
  tm_borders(col = "lightblue") 

tm_points <- tm_shape(summary_data) +
  tm_bubbles(size = 0.05, col = "red")

tm_polygons + tm_points


summary_data <- summary_data %>%
  mutate(
    latitude = st_coordinates(summary_data)[, "Y"],
    longitude = st_coordinates(summary_data)[, "X"]
  ) 

summary_data= as_tibble(summary_data)
summary_data= summary_data%>%select(-geometry)
result_grid_neighborhoods= as_tibble(result_grid_neighborhoods)
result_grid_neighborhoods = mutate(result_grid_neighborhoods, GRID_ID = row_number())
result_grid_neighborhoods <- st_sf(geometry = result_grid_neighborhoods)


merged_data <- merge(result_grid_neighborhoods, summary_data, by= "GRID_ID")



#ROOM TYPE 
room_counts <- table(merged_data$room_type)
room_percentages <- prop.table(room_counts) * 100
print(room_percentages)
room_means <- tapply(merged_data$price, merged_data$room_type, mean, na.rm = TRUE)

room_pal <- colorFactor("Set3", merged_data$room_type)
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = merged_data,
    fillColor = ~room_pal(room_type),
    fillOpacity = 0.7,
    color = "white",
    weight = 1,
    label = ~paste("Room Type: ", room_type, "Mean Price: $", round(room_means[room_type], 2)),
    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), direction = "auto")
  ) %>%
  addLegend(
    "bottomright",
    pal = room_pal,
    values = merged_data$room_type,
    title = "Room Type",
    opacity = 0.7
  )




#DENSITA'
density_breaks <- breaks <- pretty(merged_data$bnb_density)
density_pal <- colorNumeric("YlOrRd", domain = merged_data$bnb_density)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = merged_data,
    fillColor = ~density_pal(bnb_density),
    fillOpacity = 0.7,
    color = "white",
    weight = 1,
    label = ~paste("Density: ", round(bnb_density, 1)),
    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), direction = "auto")
  ) %>%
  addLegend(
    "bottomright",
    pal = density_pal,
    values = merged_data$bnb_density,
    title = "Density",
    opacity = 0.7
  )



#LOCATION
breaks <- pretty(merged_data$review_scores_location)
pal <- colorNumeric("OrRd", breaks)
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%  # Choose a tile provider (you can change it)
  addPolygons(
    data = merged_data,
    fillColor = ~colorQuantile("OrRd", review_scores_location)(review_scores_location),
    fillOpacity = 0.7,
    color = "white",
    weight = 1,
    label = ~paste("score: ", round(review_scores_location,1)),
    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), direction = "auto")
  ) %>%addAwesomeMarkers(
    lng = c(18.172443, 18.172540, 18.1791296, 18.168876, 18.16896, 18.172918),
    lat = c(40.353178, 40.352554, 40.3531905, 40.351987, 40.356463, 40.354711),
    popup = c("Piazza Sant'Oronzo", "Anfiteatro romano", "Piazza Mazzini", "Piazza Del Duomo", "Porta Napoli", "Basilica di Santa Croce"),
    clusterOptions = markerClusterOptions()
  )%>%
  addLegend(
    "bottomright",  # You can adjust the position as needed (e.g., "topright", "bottomleft", "topleft")
    pal = pal,
    values = merged_data$review_scores_location,
    title = "review_scores_location",
    opacity = 0.7
  )




#polygons and points
gdf_polygons <- st_geometry(result_grid_neighborhoods)
gdf_points <- st_as_sf(summary_data, coords = c("longitude", "latitude"))
coords <- st_geometry(gdf_points)
st_crs(coords) <- st_crs("+proj=longlat +datum=WGS84")


#with k=1 
knn1 <- knn2nb(knearneigh(coords, k = 1))
plot(st_geometry(result_grid_neighborhoods), border = "lightblue")
plot(knn1, coords, add = TRUE, pch = 20, cex = 0.1, col = "red")

#With k = 5
knn5 <- knn2nb(knearneigh(coords, k = 5))
plot(st_geometry(result_grid_neighborhoods), border = "lightblue")
plot(knn5, coords, add = TRUE, pch = 20, cex = 0.1, col = "red")

#CRITICAL CUT OFF 4.11 KM
max_distance <- max(unlist(nbdists(knn1, coords))) 
max_distance

dnb4 <- dnearneigh(coords, 0, max_distance+0.001)
dnb5 <- dnearneigh(coords, 0, max_distance+1)
dnb6 <- dnearneigh(coords, 0, max_distance+2)

plot(st_geometry(result_grid_neighborhoods), border="lightblue") 
title(main="d nearest neighbours, d = 4-6 km") 
plot(dnb4, coords, add=TRUE, col="blue", pch = 20, cex = 0.1)
plot(dnb5, coords, add=TRUE, col="red", pch = 20, cex = 0.1)
plot(dnb6, coords, add=TRUE, col="green", pch = 20, cex = 0.1)

### spatial weights

dnb4.listw <- nb2listw(dnb4,style="W",zero.policy=F)
dnb5.listw <- nb2listw(dnb5,style="W",zero.policy=F)
dnb6.listw <- nb2listw(dnb6,style="W",zero.policy=F)


### free-form spatial weight matrices 

distM <- st_distance(coords)
class(distM) <- "matrix" 

W1 <- 1/(1+distM)
diag(W1) <- 0
W2 <- 1/(1+distM^2)
diag(W2) <- 0
W3 <- 1 / (1 + abs(distM))
diag(W3) <- 0


#weight matrix -> "listw" 
listW1s <- mat2listw(W1, style= "W",zero.policy=F)
listW2s <- mat2listw(W2, style= "W",zero.policy=F)
listW3s <- mat2listw(W3, style= "W",zero.policy=F)


### The Moran's I test of spatial autocorrelation 
moran.test(merged_data$price, listW1s, randomisation=FALSE)
moran.test(merged_data$price, listW2s, randomisation=FALSE)
moran.test(merged_data$price, listW3s, randomisation=FALSE)

moran.mc(merged_data$price, listW1s, nsim=999)



##########################################
merged_data$host_identity_verified <- as.factor(merged_data$host_identity_verified)
merged_data$room_type <- as.factor(merged_data$room_type)
merged_data$host_has_profile_pic <- as.factor(merged_data$host_has_profile_pic)

pso <- st_sfc(st_point(c(18.172443, 40.353178)), crs = 4326)  
merged_data_sf <- st_as_sf(merged_data, coords = c("longitude", "latitude"), crs = 4326)
merged_data_sf$distance_from_pso <- st_distance(merged_data_sf, pso)
merged_data_sf$distance_from_pso <- as.numeric(merged_data_sf$distance_from_pso) / 1000
merged_data$distance_from_pso= merged_data_sf$distance_from_pso

frig <- st_sfc(st_point(c(18.253591,40.4303882)), crs = 4326)  
merged_data_sf <- st_as_sf(merged_data, coords = c("longitude", "latitude"), crs = 4326)
merged_data_sf$distance_from_frig <- st_distance(merged_data_sf, frig)
merged_data_sf$distance_from_frig <- as.numeric(merged_data_sf$distance_from_frig) / 1000
merged_data$distance_from_frig= merged_data_sf$distance_from_frig




linear_model <- lm(price ~ host_response_rate+ host_listings_count+
                     accommodates +bedrooms+beds+number_of_reviews+
                     review_scores_rating+host_identity_verified+
                     room_type +review_scores_location + review_scores_value 
                   + host_has_profile_pic + maximum_nights + minimum_nights
                   + bathrooms_text +distance_from_pso  +distance_from_frig, merged_data)

summary(linear_model)

merged_data$studres <- rstudent(linear_model)
qpal <- colorQuantile("OrRd", merged_data$studres, n=4) 
leaflet(merged_data) %>%
  addPolygons(stroke = FALSE, fillOpacity = .8, smoothFactor = 0.2, color = ~qpal(studres)) %>%
  addTiles() 


linear_model.lmx <- lm(price ~ host_response_rate+ host_listings_count+
                         accommodates +bedrooms+beds+number_of_reviews+
                         review_scores_rating+host_identity_verified+
                         room_type +review_scores_location + review_scores_value 
                       + host_has_profile_pic + maximum_nights + minimum_nights
                       + bathrooms_text + distance_from_pso+distance_from_frig , merged_data, x=TRUE)
MoraneI.boot <- function(var, i, ...) {
  var <- var[i]
  lmres <- lm(var ~ linear_model.lmx$x - 1)
  return(moran(x=residuals(lmres), ...)$I)	  
}
boot1 <- boot(residuals(linear_model.lmx),statistic=MoraneI.boot,  
              R=999, sim="permutation", listw=listW1s,  
              n=length(listW1s$neighbours), S0=Szero(listW1s))
ti <- (boot1$t0 - median(boot1$t))/sqrt(var(boot1$t))  
boot1
plot(boot1)


##########################################
### Moran scatterplot  

mplot <- moran.plot(merged_data$price, listw=listW1s, main="Moran scatterplot")
grid()

merged_data$hat_value <- mplot$hat
breaks <- pretty(merged_data$hat_value)
pal <- colorNumeric("OrRd", breaks)

leaflet(merged_data) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~colorQuantile("OrRd", hat_value)(hat_value),
    fillOpacity = 0.7,
    color = "white",
    weight = 1,
    label = ~paste("Hat Value:", round(hat_value, 2)),
    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), direction = "auto")
  ) %>%
  addLegend(
    "bottomright",
    pal = pal,
    values = merged_data$hat_value,
    title = "Hat Value",
    opacity = 0.7
  )

lev_res_sq <- hatvalues(linear_model) * residuals(linear_model)^2
threshold <- 2 * mean(lev_res_sq)
influential_indices <- which(lev_res_sq > threshold)
plot(lev_res_sq, pch = 20, col = "green", main = "Leverage-Residual Squared Plot")
abline(h = threshold, col = "red", lty = 2)  
points(influential_indices, lev_res_sq[influential_indices], pch = 20, col = "blue")

mplot <- moran.plot(merged_data$price, listw=listW1s, main="Moran scatterplot", 
                    return_df=F)
hotspot <- as.numeric(row.names(as.data.frame(summary(mplot))))
merged_data$wx <- lag.listw(listW1s, merged_data$price)
merged_data$quadrant <- rep("None", length(merged_data$price))
for(i in 1:length(hotspot))  {
  if (merged_data$price[hotspot[i]]>median(merged_data$price) & merged_data$wx[hotspot[i]]> median(merged_data$wx)) 
    merged_data$quadrant[hotspot[i]] <- "HH" 
  if (merged_data$price[hotspot[i]]>median(merged_data$price) & merged_data$wx[hotspot[i]]< median(merged_data$wx)) 
    merged_data$quadrant[hotspot[i]] <- "HL" 
  if (merged_data$price[hotspot[i]]<median(merged_data$price) & merged_data$wx[hotspot[i]]< median(merged_data$wx)) 
    merged_data$quadrant[hotspot[i]] <- "LL" 
  if (merged_data$price[hotspot[i]]<median(merged_data$price) & merged_data$wx[hotspot[i]]> median(merged_data$wx)) 
    merged_data$quadrant[hotspot[i]] <- "LH" 
}
table(merged_data$quadrant)

breaks <- levels(merged_data$quadrant)
leaflet(merged_data) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~colorFactor(viridis(4), quadrant)(quadrant),
    fillOpacity = 0.7,
    color = "white",
    weight = 1,
    label = ~paste("Quadrant:", quadrant),
    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), direction = "auto")
  ) %>%
  addLegend(
    "bottomright",
    pal = colorFactor(viridis(4), merged_data$quadrant),
    values = ~quadrant,
    title = "Quadrant",
    opacity = 0.7
  )

##########################################
### The Local Moran's I 

lmI <- localmoran(merged_data$price, listW1s)
head(lmI)
merged_data$lmI <- lmI[,1]
breaks <- c(-1, seq(-0.5, 0.5, by = 0.1), 1)  # Adjust breaks based on your data range
color_palette <- colorRampPalette(c("red", "white", "green"))(length(breaks) - 1)

leaflet(merged_data) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~colorBin(color_palette, domain = breaks)(lmI),
    fillOpacity = 0.7,
    color = "white",
    weight = 1,
    label = ~paste("Local Moran's I:", round(lmI, 2)),
    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), direction = "auto")
  ) %>%
  addLegend(
    "bottomright",
    pal = colorBin(color_palette, domain = breaks),
    values = lmI,
    title = "Local Moran's I",
    opacity = 0.7
  )

merged_data$locmpv <- p.adjust(lmI[, "Pr(z != E(Ii))"], "bonferroni")
breaks <- c(0, 0.0005, 0.001, 0.005, 0.01, 0.05, 0.1, 0.2, 0.5, 0.75, 1)
leaflet(merged_data) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~colorBin("YlOrRd", domain = breaks)(locmpv),
    fillOpacity = 0.7,
    color = "white",
    weight = 1,
    label = ~paste("Adjusted p-value:", round(locmpv, 4)),
    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), direction = "auto")
  ) %>%
  addLegend(
    "bottomright",
    pal = colorBin("YlOrRd", domain = breaks),
    values = merged_data$locmpv,
    title = "Adjusted p-value",
    opacity = 0.7
  )

merged_data$quadrant2 <- attr(lmI, "quadr")$median
merged_data$quadrant2[merged_data$locmpv>0.005] <- NA
breaks <- levels(merged_data$quadrant2)

leaflet(merged_data) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~colorFactor(viridis(4), quadrant2)(quadrant2),
    fillOpacity = 0.7,
    color = "white",
    weight = 1,
    label = ~paste("Quadrant:", quadrant2),
    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), direction = "auto")
  ) %>%
  addLegend(
    "bottomright",
    pal = colorFactor(viridis(4), merged_data$quadrant2),
    values = ~quadrant2,
    title = "Quadrant",
    opacity = 0.7
  )


###Choosing the best specification
starting_model <- lm(price ~ host_response_rate+ host_listings_count+
                       accommodates +bedrooms+beds+number_of_reviews+
                       review_scores_rating+host_identity_verified+
                       room_type +review_scores_location + review_scores_value 
                     + host_has_profile_pic + maximum_nights + minimum_nights
                     + bathrooms_text +distance_from_pso +distance_from_frig, merged_data)

lmtest_starting_model <- lm.LMtests(starting_model, listW1s, 
                                    test=c("all"))
summary(lmtest_starting_model)


# check if h0 is rejected in favor of SAR, SEM, or both
if (lmtest_starting_model$LMerr$p.value < 0.05 | lmtest_starting_model$LMlag$p.value < 0.05 |
    lmtest_starting_model$RLMerr$p.value < 0.05 | lmtest_starting_model$RLMlag$p.value < 0.05) {
  
  # Estimate SDM
  SDM <- lagsarlm(price ~ host_response_rate+ host_listings_count+
                    accommodates +bedrooms+beds+number_of_reviews+
                    review_scores_rating+host_identity_verified+
                    room_type +review_scores_location + review_scores_value 
                  + host_has_profile_pic + maximum_nights + minimum_nights
                  + bathrooms_text + distance_from_pso, merged_data, listw=listW1s,
                  type="mixed")
  
  SAR <- lagsarlm(price ~ host_response_rate+ host_listings_count+
                    accommodates +bedrooms+beds+number_of_reviews+
                    review_scores_rating+host_identity_verified+
                    room_type +review_scores_location + review_scores_value 
                  + host_has_profile_pic + maximum_nights + minimum_nights
                  + bathrooms_text + distance_from_pso, merged_data, listw=listW1s)
  
  SEM <- errorsarlm(price ~ host_response_rate+ host_listings_count+
                      accommodates +bedrooms+beds+number_of_reviews+
                      review_scores_rating+host_identity_verified+
                      room_type +review_scores_location + review_scores_value 
                    + host_has_profile_pic + maximum_nights + minimum_nights
                    + bathrooms_text + distance_from_pso, merged_data, listw=listW1s)
  # Step 4: LRT to verify if SDM can be reduced to SAR
  lrt_sar <- anova(SDM, SAR)
  
  # Step 4: LRT to verify if SDM can be reduced to SEM
  lrt_sem <- anova(SDM, SEM)
  
  print(lrt_sar)
  print(lrt_sem)
  
  # Step 5: if both restrictions are rejected
  if (lrt_sar$`p-value`[2] < 0.05 & lrt_sem$`p-value`[2] < 0.05) {
    cat("SDM best describes the data.\n")
  } else {
    # Step 6: if only SAR or SEM restriction cannot be rejected
    if (lrt_sar$`p-value`[2] >= 0.05) {
      cat("SAR best describes the data.\n")
    } 
    else if (lrt_sem$`p-value`[2] >= 0.05) {
      cat("SEM best describes the data.\n")
    } else {
      cat("SDM is better.\n")
    }
  }
  
  # Step 7: If it points to SEM, estimate SDEM
  if (lrt_sem$`p-value`[2] >= 0.05) {
    SDEM <- errorsarlm(price ~ host_response_rate+ host_listings_count+
                         accommodates +bedrooms+beds+number_of_reviews+
                         review_scores_rating+host_identity_verified+
                         room_type +review_scores_location + review_scores_value 
                       + host_has_profile_pic + maximum_nights + minimum_nights
                       + bathrooms_text + distance_from_pso , merged_data, listw=listW1s, etype = "emixed")
    summary(SDEM)
    # check if θ is statistically significant
    anova(SDEM, SEM)
  }
  
} else {
  # Step 8: last chance estimate LDM model
  LDM <- lmSLX(price ~ host_response_rate+ host_listings_count+
                 accommodates +bedrooms+beds+number_of_reviews+
                 review_scores_rating+host_identity_verified+
                 room_type +review_scores_location + review_scores_value 
               + host_has_profile_pic + maximum_nights + minimum_nights
               + bathrooms_text + distance_from_pso, merged_data, listw=listW1s)
  # if θ is statistically significant
  summary(LDM)
}



SAR <- lagsarlm(price ~ host_response_rate+ host_listings_count+
                  accommodates +bedrooms+beds+number_of_reviews+
                  review_scores_rating+host_identity_verified+
                  room_type +review_scores_location + review_scores_value 
                + host_has_profile_pic + maximum_nights + minimum_nights
                + bathrooms_text  + distance_from_pso +distance_from_frig, merged_data, listw=listW1s)
summary(SAR)

#review_scores_location
SAR <- lagsarlm(price ~ host_response_rate+ host_listings_count+
                  accommodates +bedrooms+beds+number_of_reviews+
                  review_scores_rating+host_identity_verified+
                  room_type  + review_scores_value 
                + host_has_profile_pic + maximum_nights + minimum_nights
                + bathrooms_text  + distance_from_pso +distance_from_frig, merged_data, listw=listW1s)
summary(SAR)

#room_type
SAR <- lagsarlm(price ~ host_response_rate+ host_listings_count+
                  accommodates +bedrooms+beds+number_of_reviews+
                  review_scores_rating+host_identity_verified+
                  review_scores_value 
                + host_has_profile_pic + maximum_nights + minimum_nights
                + bathrooms_text  + distance_from_pso +distance_from_frig, merged_data, listw=listW1s)
summary(SAR)

#minimum_nights maximum_nights
SAR <- lagsarlm(price ~ host_response_rate+ host_listings_count+
                  accommodates +bedrooms+beds+number_of_reviews+
                  review_scores_rating+host_identity_verified+
                  review_scores_value 
                + host_has_profile_pic  
                + bathrooms_text  + distance_from_pso +distance_from_frig, merged_data, listw=listW1s)
summary(SAR)



leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%  
  addPolygons(
    data = merged_data,
    fillColor = ~colorQuantile("OrRd", price)(price),
    fillOpacity = 0.7,
    color = "white",
    weight = 1,
    label = ~paste("Price: $", round(price,1)),
    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), direction = "auto")
  ) 



impSAR <- impacts(SAR, listw=listW1s, R=100)
summary(impSAR, zstats=TRUE, short=TRUE)



