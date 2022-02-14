# #Package Coverage Test:
# library(rstudioapi)
# library(NCmisc)
# list.functions.in.file(rstudioapi::getSourceEditorContext()$path, alphabetic = TRUE)

#### Athlete Results Plot, Filtered to 16km
athlete_results_plot <- function(results, athlete) {
  
   gap <- hms::as_hms("00:00:00")
   
  # Filter and Organize primary data frame
  athlete_tt_results <- results %>% 
                             dplyr::filter(rider_name == athlete & hour(time) < 2) %>% 
                             dplyr::arrange(time) %>% 
                             dplyr::mutate(event = case_when(event = str_detect(event, "Loburn") ~ "Loburn TT",
                                                      TRUE ~ event)) %>% 
                            dplyr::mutate(time_segment_gap = time - gap)  %>% 
                            dplyr:: mutate(event = paste0 ( event , " - " , as.character(dist_km) , "km" )  )

  tt_results_plot <- ggplot(athlete_tt_results, aes(x=date, y=time)) +
                              geom_segment(aes(x=date, xend=date, y=0, yend = time), colour = "black") +
                              geom_point(aes(fill = event), colour = "black", pch = 21, size = 4) +
                              labs(title = NULL , x = NULL, y = NULL, color = "Event\n") +
                              scale_y_time(breaks = scales::breaks_width("5 min"), limits = c(0, NA)) +
                              scale_x_datetime(breaks = scales::breaks_width("1 year"), date_labels = "%Y")
  
  tt_results_plot <- ggplotly(tt_results_plot)

  return(tt_results_plot)
}


event_results_plot <- function(results, date, gender_filter, dist) {
  
  # Filter and Organize primary data frame
  date_filter <- ymd(str_sub(date,1,10))
  event_tt_results_filtered <- results %>% 
                      dplyr::filter(date == date_filter & hour(time) < 2) %>% 
                      # filter(dist_km == dist) %>% 
                      dplyr::mutate(dist_km_char = as.character(dist_km)) %>%
                      dplyr::arrange(dist_km) %>%
                      dplyr::group_by(dist_km) %>% 
                      dplyr::arrange(time, .by_group = TRUE) %>% 
                      dplyr::ungroup() %>% 
                      dplyr::mutate(row_order = row_number()) %>% 
                      dplyr::mutate(rider_name = as.factor(rider_name) %>% fct_reorder(time, min))

  if (gender_filter != "both") {
    event_tt_results_plot <- event_tt_results_filtered %>% dplyr::filter(gender == gender_filter)
  }
    
    # event_tt_results_plot <- event_results_ggplot(event_tt_results_plot)
    
    event_tt_results_plot <-event_results_ggplot_lolipop(event_tt_results_plot)
    return(event_tt_results_plot)
}

# event_results_ggplot <- function(event_results_filtered) {
#   
#   y_max_w_buffer <- max(event_results_filtered$time) * 1.05
#   y_min_w_buffer <- min(event_results_filtered$time) * 0.8
#   y_limits <- c( y_min_w_buffer , y_max_w_buffer)
#   
#   event_results_plot <- ggplot(data = event_results_filtered, aes( x=rider_name_abbv, y = time)) + 
#     geom_point(aes(colour = dist_km_char, size = 4)) +
#     geom_segment(aes(x=rider_name_abbv, xend=rider_name_abbv, y=0, yend=time)) +
#     scale_x_discrete(labels = NULL, breaks = NULL) +
#     scale_y_time(breaks = scales::breaks_width("2 min"), limits = y_limits) +  
#     geom_richtext(  aes(label = paste0(time," ",rider_name_abbv)), fill = "white", size = 5, angle = 90, hjust="top", nudge_y = -50) +
#     # geom_richtext(  aes(label = time), fill = "white",  size = 4, angle = 90,hjust="bottom", nudge_y = +60) +
#     guides(size = "none", colour = guide_legend(override.aes = list(size=10))) +
#     theme(legend.position = c(0.1 , 0.9),
#           legend.background = element_rect(fill = "white", color = "black")) +
#     labs(title = NULL , x = NULL, y = NULL, color = "Distance (km)")
#   
#   return(event_results_plot)
# }

event_results_ggplot_lolipop <- function(event_results_filtered) {
  
  y_max_w_buffer <- max(event_results_filtered$time) 
  y_min_w_buffer <- min(event_results_filtered$time)
  y_limits <- c( y_min_w_buffer , y_max_w_buffer)
  
  event_results_plot <- event_results_filtered %>% 
    ggplot(aes(x = rider_name, y = time)) +
    geom_segment(aes(x = rider_name, xend = rider_name, y = y_min_w_buffer, yend = time, colour = dist_km_char), size = 1) + 
    geom_point(aes(colour = dist_km_char), size = 4) +
    # rotate_x_text(45)+
    scale_y_time(breaks = scales::breaks_width("1 min"), limits = y_limits) +
    guides(size = "none", colour = guide_legend(override.aes = list(size=10)))+
    labs(title = NULL , x = NULL, y = NULL, color = "Distance (km)") +
    theme(axis.text = element_text(size = 12 , face="bold"),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          panel.grid.major.y = element_line(color = "red",
                                            size = 0.5,
                                            linetype = "dashed"))+
    scale_color_manual(values=c("red3", "blue", "green"))

  return(event_results_plot)
}

