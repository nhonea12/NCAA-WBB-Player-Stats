library(tidyverse)
library(shiny)
library(bslib)
library(shinycssloaders) # to create a loading graphic while charts are being created
library(wehoop) # women's basketball data
library(tidyverse)
library(jsonlite) # to read in data from Torvik
library(stringi) # to remove accents from player names
library(hexbin) # for hexagonal shot charts
library(gt)
library(gtExtras)
library(magick) # needed for gtUtils
library(gtUtils)

# read data (data manipulation in data.R)
player_stats_total       <- readRDS("Data/player_stats_total.rds")
player_stats_vs_tournament <- readRDS("Data/player_stats_vs_tournament.rds")
wbb_shots                <- readRDS("Data/wbb_shots.rds")
averages                 <- readRDS("Data/averages.rds")

# create the court points
##Draw court (from https://raw.githubusercontent.com/Henryjean/NBA-Court/refs/heads/main/CourtDimensions.R) (dimensions changed from NBA to college)
width = 50
height = 94/2
key_height = 19
inner_key_width = 12
outer_key_width = 12 #Edited to same as inner key, as no outer key in college
backboard_width = 6
backboard_offset = 4
neck_length = 0.5
hoop_radius = 0.75
hoop_center_y = backboard_offset + neck_length + hoop_radius
three_point_radius = 22.1458333 #Edited to college 3 point line
three_point_side_radius = 21.65625 #Edited to college 3 point line
three_point_side_height = 9.86458333 #Edited to college 3 point line

circle_points = function(center = c(0, 0), radius = 1, npoints = 360) {
  angles = seq(0, 2 * pi, length.out = npoints)
  return(data.frame(x = center[1] + radius * cos(angles),
                    y = center[2] + radius * sin(angles)))
}

court_points = data.frame(
  x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2, 
        outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2,
        -backboard_width / 2, backboard_width / 2, 
        0, 0),
  y = c(height, 0, 0, height, height, 0, key_height, key_height, 0,
        backboard_offset, backboard_offset, 
        backboard_offset, backboard_offset + neck_length),
  desc = c(rep("perimeter", 5), rep("outer_key", 4), rep("backboard", 2),
           rep("neck", 2))
)

# define foul circle
foul_circle = circle_points(center = c(0, key_height), radius = inner_key_width / 2)
foul_circle_top = filter(foul_circle, y > key_height) %>% mutate(desc = "foul_circle_top")
foul_circle_bottom = filter(foul_circle, y < key_height) %>% mutate(desc = "foul_circle_bottom") #Not included in this chart as no bottom ft circle in college

# define hoop
hoop = circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>% mutate(desc = "hoop") 
restricted = circle_points(center = c(0, hoop_center_y), radius = 4) %>%
  filter(y >= hoop_center_y) %>%
  mutate(desc = "restricted")

# define 3-point line
three_point_circle = circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>% filter(y >= three_point_side_height)
three_point_line = data.frame(
  x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
  y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
  desc = "three_point_line"
)

# combine all court points into one object that can be used
court_points = rbind(court_points , foul_circle_top, hoop, restricted, three_point_line)

# altered ggplot theme from the F5
theme_f5 <- function (font_size = 9) { 
  theme_minimal(base_size = font_size, base_family = "roboto") %+replace% 
    theme(
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite"), 
      panel.grid.minor = element_blank(), 
      plot.title = element_text(hjust = 0, size = 14, face = 'bold'), 
      plot.subtitle = element_text(color = 'gray65', hjust = 0, margin=margin(2.5,0,10,0), size = 9), 
      plot.caption = element_text(color = 'gray65', margin=margin(-2,0,0,0), hjust = 1, size = 6)
    )
}


# below functions used for hexagonal charts
hex_bounds <- function(x, binwidth) {
  c(
    plyr::round_any(min(x), binwidth, floor) - 1e-6,
    plyr::round_any(max(x), binwidth, ceiling) + 1e-6
  )
}

calculate_hex_coords = function(shots, binwidths) {
  xbnds = hex_bounds(shots$loc_x, binwidths[1])
  xbins = diff(xbnds) / binwidths[1]
  ybnds = hex_bounds(shots$loc_y, binwidths[2])
  ybins = diff(ybnds) / binwidths[2]
  
  hb = hexbin(
    x = shots$loc_x,
    y = shots$loc_y,
    xbins = xbins,
    xbnds = xbnds,
    ybnds = ybnds,
    shape = ybins / xbins,
    IDs = TRUE
  )
  
  shots = mutate(shots, hexbin_id = hb@cID)
  
  hexbin_stats = shots  |> 
    group_by(hexbin_id) |>
    summarize(
      hex_attempts = n(),
      hex_pct = mean(shot_made_numeric),
      hex_points_scored = sum(shot_made_numeric * shot_value),
      hex_points_per_shot = mean(shot_made_numeric * shot_value),
      .groups = "drop"
    )
  
  hexbin_ids_to_zones = shots |>
    dplyr::group_by(hexbin_id, shot_zone_range, shot_zone_area) |>
    summarize(attempts = n(), .groups = "drop") |>
    arrange(hexbin_id, desc(attempts)) |>
    group_by(hexbin_id) |>
    filter(row_number() == 1) |>
    select(hexbin_id, shot_zone_range, shot_zone_area)
  
  hexbin_stats = inner_join(hexbin_stats, hexbin_ids_to_zones, by = "hexbin_id")
  
  # from hexbin package, see: https://github.com/edzer/hexbin
  sx = hb@xbins / diff(hb@xbnds)
  sy = (hb@xbins * hb@shape) / diff(hb@ybnds)
  dx = 1 / (2 * sx)
  dy = 1 / (2 * sqrt(3) * sy)
  origin_coords = hexcoords(dx, dy)
  
  hex_centers = hcell2xy(hb)
  
  hexbin_coords = bind_rows(lapply(1:hb@ncells, function(i) {
    tibble(
      x = origin_coords$x + hex_centers$x[i],
      y = origin_coords$y + hex_centers$y[i],
      center_x = hex_centers$x[i],
      center_y = hex_centers$y[i],
      hexbin_id = hb@cell[i]
    )
  }))
  
  inner_join(hexbin_coords, hexbin_stats, by = "hexbin_id")
}

# min_radius_factor controls the difference in sizes between hex bins
# binwidths control the overall size of the bins
calculate_hexbins_from_shots = function(shots, league_averages = averages, binwidths = c(2.5, 2.5), min_radius_factor = 0.4, fg_diff_limits = c(-0.12, 0.12), fg_pct_limits = c(0.2, 0.7), pps_limits = c(0.5, 1.5)) {
  shots <- tibble::as_tibble(as.data.frame(shots))
  league_averages <- tibble::as_tibble(as.data.frame(league_averages))
  
  if (nrow(shots) == 0) {
    return(list())
  }
  
  grouped_shots = shots |> 
    group_by(shot_zone_range, shot_zone_area)
  
  zone_stats = grouped_shots |>
    summarize(
      zone_attempts = n(),
      zone_pct = mean(shot_made_numeric),
      zone_points_scored = sum(shot_made_numeric * shot_value),
      zone_points_per_shot = mean(shot_made_numeric * shot_value),
      .groups = "drop"
    )
  
  league_zone_stats = league_averages |>
    group_by(shot_zone_range, shot_zone_area) |>
    summarize(league_pct = sum(fgm) / sum(fga), .groups = "drop")
  
  hex_data = calculate_hex_coords(shots, binwidths = binwidths)
  
  join_keys = c("shot_zone_area", "shot_zone_range")
  
  hex_data = hex_data |>
    inner_join(zone_stats, by = join_keys) |>
    inner_join(league_zone_stats, by = join_keys)
  
  max_hex_attempts = max(hex_data$hex_attempts)
  
  hex_data = mutate(hex_data,
                    radius_factor = min_radius_factor + (1 - min_radius_factor) * log(hex_attempts + 1) / log(max_hex_attempts + 1),
                    adj_x = center_x + radius_factor * (x - center_x),
                    adj_y = center_y + radius_factor * (y - center_y),
                    bounded_fg_diff = pmin(pmax(zone_pct - league_pct, fg_diff_limits[1]), fg_diff_limits[2]),
                    bounded_fg_pct = pmin(pmax(zone_pct, fg_pct_limits[1]), fg_pct_limits[2]),
                    bounded_points_per_shot = pmin(pmax(zone_points_per_shot, pps_limits[1]), pps_limits[2]))
  
  list(
    hex_data = hex_data, 
    fg_diff_limits = fg_diff_limits, 
    fg_pct_limits = fg_pct_limits, 
    pps_limits = pps_limits, 
    player_name = shots$athlete_display_name[1], 
    team_name = shots$team_location[1], 
    player_number = shots$athlete_jersey[1], 
    player_headshot = shots$athlete_headshot_href[1], 
    player_position = shots$athlete_position_name[1],
    fgm = shots |> filter((grepl("made", text) | grepl("makes", text))) |> nrow(),
    fga = nrow(shots),
    fgm3 = shots |> filter((grepl("Three", text) | grepl("three", text)) & (grepl("made", text) | grepl("makes", text))) |> nrow(),
    fga3 = shots |> filter(grepl("Three", text) | grepl("three", text)) |> nrow()
  )
}


percent_formatter = function(x) {
  scales::percent(x, accuracy = 1)
}

points_formatter = function(x) {
  scales::comma(x, accuracy = 0.01)
}

# function to create the hexagonal shot charts from hexagonal shot data
generate_hex_chart = function(hex_data, base_court = court_points, metric = "bounded_fg_diff", alpha_range = c(0.85, 0.98)) {
  if (length(hex_data) == 0) {
    return(base_court)
  }
  
  if (metric == "bounded_fg_diff") {
    fill_limit = hex_data$fg_diff_limits
    fill_label = "FG% vs. National Avg"
    label_formatter = percent_formatter
  } else if (metric == "bounded_fg_pct") {
    fill_limit = hex_data$fg_pct_limits
    fill_label = "FG%"
    label_formatter = percent_formatter
  } else if (metric == "bounded_points_per_shot") {
    fill_limit = hex_data$pps_limits
    fill_label = "Points Per Shot"
    label_formatter = points_formatter
  } else {
    stop("invalid metric")
  }
  
  ggplot() + 
    geom_path(data = court_points,
              aes(x = x, y = y, group = desc),
              color = "black", linewidth = .4,
              , inherit.aes = FALSE) +
    geom_polygon(
      data = hex_data$hex_data,
      aes(
        x = adj_x,
        y = adj_y,
        group = hexbin_id,
        fill = !!sym(metric),
        alpha = hex_attempts
      ),
      # size = court_theme$hex_border_size,
      # color = court_theme$hex_border_color
    ) +
    # custom theme
    theme_f5()  +
    # set opacity limits
    #scale_alpha_continuous(range = c(0.4, 1)) +
    # set y-axis limits
    scale_y_continuous(limits = c(-2.5, 45), oob = scales::oob_squish) +
    # set x-axis limits
    scale_x_continuous(limits = c(-30, 30), oob = scales::oob_squish) + 
    # theme tweaks
    theme(line = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(), 
          plot.margin = margin(.25, 0, 0.25, 0, "lines"),
          plot.title = element_text(face = 'bold', hjust= .5, vjust = 0, family = "roboto", size = 25),
          plot.subtitle = element_text(face = 'bold', hjust= .5, vjust = 0, family = "roboto", size = 15, lineheight = 0.25),
          plot.caption = element_text(size = 20)) + 
    scale_fill_viridis_c(
      paste0(fill_label, "   "),
      limit = fill_limit,
      labels = label_formatter
    ) +
    scale_alpha_continuous(guide = FALSE, range = alpha_range, trans = "sqrt") +
    theme(legend.text = element_text(size = rel(4)), 
          legend.title = element_text(size = rel(6))) + 
    guides(
      fill = guide_colourbar(position = "bottom", barwidth = 24, barheight = 6)
    ) #+ 
  # labs(
  #   title = ifelse(!is.na(hex_data$player_headshot),
  #                  paste0("<img src = '", hex_data$player_headshot, "' height = 50>",
  #                         "<span style='font-size: 40pt'>",
  #                         hex_data$player_name,
  #                         " Hex Chart</span>"),
  #                  paste0(hex_data$player_name, " Hex Chart")
  #   ),
  #   subtitle = paste0("#", hex_data$player_number, ", ", hex_data$player_position)
  #   ) + 
  # annotate("label",
  #          x = 17, y = 41,
  #          label = paste0("FG: ", hex_data$fgm, "/", hex_data$fga, " \n 3 FG: ", hex_data$fgm3, "/", hex_data$fga3),
  #          size = 14,
  #          color = "black",
  #          fill = "floralwhite",
  #          fontface = "bold",
  #          lineheight = 1, # Controls line spacing (default is 1)
  #          label.padding = unit(0.2, "lines"),  # Reduces padding inside the label (less space between text and label border)
  #          linewidth = 2  # increase border thickness
  # ) +
  # theme(
  #   plot.title = ggtext::element_markdown()
  # )
}

# create function to automatically plot hex charts (don't have to worry about having to generate hex data and then hex chart, all in one step with this)
plot_hex_chart <- function(player = "Tilda Trygger", team = "NC State"){
  
  # filter to only get the player shots desired
  player_shots <- wbb_shots |> 
    filter(
      athlete_display_name == player,
      team_location == team
    )
  
  # generate hex data for the player
  player_hex_data <- calculate_hexbins_from_shots(shots = player_shots)
  
  # plot the hexagonal shot chart
  generate_hex_chart(hex_data = player_hex_data)
}

# Helper function to avoid repeating this pattern
stat_header <- function(label, output_id) {
  tags$p(tags$strong(
    style = "display: inline-flex; align-items: baseline; gap: 0;",
    label,
    " (",
    textOutput(output_id, inline = TRUE),
    tags$span("\u00a0Games)")# non-breaking space before "Games"
  ))
}



# now the Shiny app
# user interface for the Shiny app
ui <- fluidPage(
  # sidebar panel of the user interface
  sidebarLayout(
    sidebarPanel(
      h2("Select Team and Player"),
      h5("Team and player names must match what is seen on ESPN.com"),
      # allow user to set the team name
      textInput(
        inputId = "team_name",
        label = "Team:",
        value = "NC State"
      ),
      # allow the user to set the player name
      textInput(
        inputId = "player_name",
        label = "Player:",
        value = "Tilda Trygger"
      ),
      # require action button to see a new player
      actionButton(
        inputId = "action_button",
        label = "See Player Stats!"
      )
    ),
    mainPanel(
      page_fluid(
        layout_columns(
          # card for player headshot and team logo
          card(
            style = "padding: 0; overflow: visible;",  # allow circle to overflow
            #shinycssloaders::withSpinner(
              tags$div(
                style = "display: flex; justify-content: center; padding-bottom: 50px;",  # space for circle
                uiOutput("player_headshot")
              )
            #)
          ),
          # card for info on the player (category names in bold)
          card(
            tags$p(tags$strong("Player: "), textOutput("player", inline = TRUE)),
            tags$p(tags$strong("Team: "), textOutput("team", inline = TRUE)),
            tags$p(tags$strong("Role: "), textOutput("role", inline = TRUE)),
            tags$p(tags$strong("Height: "), textOutput("height", inline = TRUE)),
            tags$p(tags$strong("Class in 2027: "), textOutput("class27", inline = TRUE)),
            tags$p(tags$strong("Hometown: "), textOutput("hometown", inline = TRUE))
          ),
          # card for hexagonal shot chart of player
          card(
            style = "padding: 0; overflow: visible;",
            shinycssloaders::withSpinner(
              plotOutput(
                outputId = "shot_chart",
                width = "100%",
                height = "300px"
              )
            )
          ),
          col_widths = c(4, 3, 5)
        ),
        card(
          card_header("Basic Stats"),
          stat_header("Player Stats", "gp_total_basic"),
          gt_output(outputId = "basic_player_stats"),
          stat_header("Player Stats vs. Tournament Teams", "gp_tourny_basic"),
          gt_output(outputId = "basic_player_stats_vs_good")
        ),
        card(
          card_header("Shooting Splits"),
          stat_header("Shooting Splits", "gp_total_shooting"),
          gt_output(outputId = "shooting_splits_1"),
          gt_output(outputId = "shooting_splits_2"),
          stat_header("Shooting Splits vs. Tournament Teams", "gp_tourny_shooting"),
          gt_output(outputId = "shooting_splits_vs_good")
        ),
        card(
          card_header("Advanced Stats"),
          stat_header("Advanced Stats", "gp_total_advanced"),
          gt_output(outputId = "advanced_player_stats"),
          stat_header("Advanced Stats vs. Top 100 Teams", "gp_top_100_advanced"),
          gt_output(outputId = "advanced_player_stats_vs_good")
        ),
        card(
          card_header("Player Impact Metrics"),
          stat_header("Player Impact Metrics", "gp_total_metric"),
          gt_output(outputId = "player_metrics"),
          stat_header("Player Impact Metrics vs. Top 100 Teams", "gp_top_100_metric"),
          gt_output(outputId = "player_metrics_vs_good")
        )
      )
    )
  )
)

# server for the Shiny app
server <- function(input, output, session) {
  # subset the data to only the specified player's stats, only update when the action button is pressed
  player_stats <- eventReactive(input$action_button, {
    player_stats_total |> 
      filter(team_location == input$team_name, 
             athlete_display_name == input$player_name)
  })
  
  # subset the data to only the specified player's stats vs good teams, only update when the action button is pressed
  player_stats_vs_good_teams <- eventReactive(input$action_button, {
    player_stats_vs_tournament |> 
      filter(team_location == input$team_name, 
             athlete_display_name == input$player_name)
  })
  
  # subset the data to only the specified player's shots, only update when the action button is pressed
  player_shots <- eventReactive(input$action_button, {
    wbb_shots |> 
      filter(team_location == input$team_name, 
             athlete_display_name == input$player_name)
  })
  
  # create hexagonal shot data for hexagonal shot charts
  player_hexbin_data <- eventReactive(input$action_button, {
    calculate_hexbins_from_shots(shots = player_shots(), league_averages = averages)
  })
  
  output$player_headshot <- renderUI({
    # assuming these come from reactive expressions or input values
    headshot_url        <- player_stats()$athlete_headshot_href
    team_logo_url       <- player_stats()$team_logo
    team_color          <- player_stats()$team_color
    team_alternate_color <- player_stats()$team_alternate_color
    
    tags$div(
      style = "position: relative; display: inline-block; width: 350px;",
      tags$img(src = headshot_url, width = "350"),
      tags$div(
        style = paste0(
          "position: absolute; bottom: -42px; right: 38px;",
          "width: 90px; height: 90px;",
          "background-color: #", team_alternate_color, ";",
          "border-radius: 50%;",
          "border: 2px solid #", team_color, ";"
        ),
        tags$img(
          src = team_logo_url,
          width = "72",
          style = "display: block; margin: auto; padding-top: 6px;"
        )
      )
    )
  })
  
  # reactive information about the player in the form of text
  # player name
  output$player  <- renderText({
    player_stats()$athlete_display_name
  })
  # player's team
  output$team  <- renderText({
    player_stats()$team_location
  })
  
  # player role
  output$role  <- renderText({
    player_stats()$role
  })
  
  # player height
  output$height  <- renderText({
    player_stats()$height
  })
  
  # player class in 2026-27
  output$class27  <- renderText({
    player_stats()$class2027
  })
  
  # player's hometown
  output$hometown  <- renderText({
    player_stats()$hometown
  })
  
  # reactive plot of player shots
  output$shot_chart <- renderPlot({
    # require the number of shots by a player to be greater than 0 for the plot to render
    req(
      player_shots(), 
      nrow(player_shots()) > 0
    )
    ggplot() + 
      geom_path(data = court_points,
                aes(x = x, y = y, group = desc),
                color = "black", linewidth = .25,
                , inherit.aes = FALSE) +
      coord_fixed(clip = 'off') + 
      geom_polygon(
        data = player_hexbin_data()$hex_data,
        aes(
          x = adj_x,
          y = adj_y,
          group = hexbin_id,
          fill = !!sym("bounded_fg_diff"),
          alpha = hex_attempts
        )
        # size = court_theme$hex_border_size,
        # color = court_theme$hex_border_color
      ) +
      # custom theme
      theme_f5()  +
      # set opacity limits
      #scale_alpha_continuous(range = c(0.4, 1)) +
      # set y-axis limits
      scale_y_continuous(limits = c(-2.5, 45), oob = scales::oob_squish) +
      # set x-axis limits
      scale_x_continuous(limits = c(-30, 30), oob = scales::oob_squish) + 
      # theme tweaks
      theme(line = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            plot.margin = margin(.25, 0, 0.25, 0, "lines"),
            plot.title = element_text(face = 'bold', hjust= .5, vjust = 0, family = "roboto", size = 40),
            plot.subtitle = element_text(face = 'bold', hjust= .5, vjust = 0, family = "roboto", size = 25, lineheight = 0.25),
            plot.caption = element_text(size = 5)) + 
      scale_fill_viridis_c(
        paste0("FG% vs. National Avg.", "   "),
        limit = player_hexbin_data()$fg_diff_limits,
        labels = percent_formatter,
        guide = guide_colorbar(barwidth = 15)
      ) +
      scale_alpha_continuous(guide = FALSE, range = c(0.85, 0.98), trans = "sqrt") +
      theme(legend.text = element_text(size = 12), 
            legend.title = element_text(face = 'bold', size = 18)
      ) + 
      guides(
        fill = guide_colourbar(position = "bottom")
      ) + 
      # labs(
      #   title = ifelse(!is.na(player_hexbin_data()$player_headshot),
      #                  paste0("<img src = '", player_hexbin_data()$player_headshot, "' height = 50>",
      #                         "<span style='font-size: 40pt'>",
      #                         player_hexbin_data()$player_name,
      #                         " Hex Chart</span>"),
      #                  paste0(player_hexbin_data()$player_name, " Hex Chart")
      #   ),
      #   subtitle = paste0("#", player_hexbin_data()$player_number, ", ", player_hexbin_data()$player_position)
      # ) + 
      # # annotate("label", 
      #          x = 19.5, y = 42, 
      #          label = paste0("FG: ", nrow(player_shots() %>% filter((grepl("made", text) | grepl("makes", text)))), "/", nrow(player_shots()), " \n 3 FG: ", nrow(player_shots() %>% filter((grepl("Three", text) | grepl("three", text)) & (grepl("made", text) | grepl("makes", text)))), "/", nrow(player_shots() %>% filter((grepl("Three", text) | grepl("three", text))))),
      #          size = 8, 
      #          color = "black", 
      #          fill = "floralwhite", 
      #          fontface = "bold",
      #          lineheight = 0.8, # Controls line spacing (default is 1)
      #          label.padding = unit(0.2, "lines"),  # Reduces padding inside the label (less space between text and label border)
      # ) + 
      theme(
        plot.title = ggtext::element_markdown()
      )
  }) # end of renderPlot
  
  # number of games played by player (need a different reactive text value for each time it is used)
  # total games played
  output$gp_total_basic    <- renderText({
    player_stats()$GP
  })
  output$gp_total_shooting <- renderText({
    player_stats()$GP
  })
  output$gp_total_advanced <- renderText({
    player_stats()$GP
  })
  output$gp_total_metric  <- renderText({
    player_stats()$GP
  })
  
  # games played vs tournamet teams
  output$gp_tourny_basic    <- renderText({
    player_stats_vs_good_teams()$GP
  })
  output$gp_tourny_shooting <- renderText({
    player_stats_vs_good_teams()$GP
  })
  
  # games played vs top 100 teams by Torvik
  output$gp_top_100_advanced    <- renderText({
    player_stats_vs_good_teams()$games_played
  })
  output$gp_top_100_metric <- renderText({
    player_stats_vs_good_teams()$games_played
  })
  
  # tables of player stats
  # basic player stats for the whole season
  output$basic_player_stats <- render_gt({
    player_stats() |> 
      select(
        MIN_pg, PTS_pg, REB_pg, OREB_pg, DREB_pg, AST_pg, BLK_pg, STL_pg, TO_pg, FG_per, FG3_per, FT_per
      ) |> # turn into a nice gt table
      gt() |> 
      # format column names
      cols_label(
        MIN_pg = "MIN",
        PTS_pg = "PTS",
        OREB_pg = "OREB",
        DREB_pg = "DREB",
        AST_pg = "AST",
        BLK_pg = "BLK",
        FG_per = "FG%", 
        FG3_per = "3FG%",
        REB_pg = "TREB",
        FT_per = "FT%",
        STL_pg = "STL",
        TO_pg = "TO"
      ) |> 
      gt_theme_ncaa()
  })
  
  # basic player stats vs tournament teams
  output$basic_player_stats_vs_good <- render_gt({
    player_stats_vs_good_teams() |> 
      select(
        MIN_pg, PTS_pg, REB_pg, OREB_pg, DREB_pg, AST_pg, BLK_pg, STL_pg, TO_pg, FG_per, FG3_per, FT_per
      ) |> # turn into a nice gt table
      gt() |> 
      # format column names
      cols_label(
        MIN_pg = "MIN",
        PTS_pg = "PTS",
        OREB_pg = "OREB",
        DREB_pg = "DREB",
        AST_pg = "AST",
        BLK_pg = "BLK",
        FG_per = "FG%", 
        FG3_per = "3FG%",
        REB_pg = "TREB",
        FT_per = "FT%",
        STL_pg = "STL",
        TO_pg = "TO"
      ) |> 
      gt_theme_ncaa()
  })
  
  # tables of shooting splits
  # basic player stats for the whole season
  output$shooting_splits_1 <- render_gt({
    player_stats() |> 
      select(
        FGM, FGA, FG_per, FGM2, FGA2, FG2_per, FGM3, FGA3, FG3_per, FTM, FTA, FT_per, EFG_per
      ) |> # turn into a nice gt table
      gt() |> 
      # format column names
      cols_label(
        FGM = "FG",
        FGA = "FGA",
        FG_per = "FG%", 
        FGM2 = "FG2",
        FGA2 = "FGA2",
        FG2_per = "FG2%",
        FGM3 = "3FG",
        FGA3 = "3FGA",
        FG3_per = "3FG%", 
        FTM = "FT",
        FTA = "FTA",
        FT_per = "FT%",
        EFG_per = "EFG%"
      ) |> 
      gt_theme_ncaa()
  })
  
  output$shooting_splits_2 <- render_gt({
    player_stats() |> 
      mutate(
        rim_fg_per = rim_fg_per |> round(3),
        non_rim2_fg_per = non_rim2_fg_per |> round(3)
      ) |> 
      select(
        rim_fgm, rim_fga, rim_fg_per, non_rim2_fgm, non_rim2_fga, non_rim2_fg_per
      ) |> # turn into a nice gt table
      gt() |> 
      # format column names
      cols_label(
        rim_fgm = "Short 2 FG",
        rim_fga = "Short 2 FGA",
        rim_fg_per = "Short 2 FG%",
        non_rim2_fgm = "Long 2 FG",
        non_rim2_fga = "Long 2 FGA",
        non_rim2_fg_per = "Long 2 FG%"
      ) |> 
      gt_theme_ncaa()
  })
  
  # shooting splits vs tournament teams
  output$shooting_splits_vs_good <- render_gt({
    player_stats_vs_good_teams() |> 
      select(
        FGM, FGA, FG_per, FGM2, FGA2, FG2_per, FGM3, FGA3, FG3_per, FTM, FTA, FT_per, EFG_per
      ) |> # turn into a nice gt table
      gt() |> 
      # format column names
      cols_label(
        FGM = "FG",
        FGA = "FGA",
        FG_per = "FG%", 
        FGM2 = "FG2",
        FGA2 = "FGA2",
        FG2_per = "FG2%",
        FGM3 = "3FG",
        FGA3 = "3FGA",
        FG3_per = "3FG%", 
        FTM = "FT",
        FTA = "FTA",
        FT_per = "FT%",
        EFG_per = "EFG%"
      ) |> 
      gt_theme_ncaa()
  })
  
  # tables of advanced player stats
  # advanced player stats for the whole season
  output$advanced_player_stats <- render_gt({
    player_stats() |> 
      mutate(
        TS_per = (ts_per/100) |> round(digits = 3)
      ) |> 
      select(
        usage, 
        AST_TO_ratio,
        EFG_per,
        TS_per,
        dr_per,
        or_per,
        stl_per,
        blk_per,
        to_per,
        fouls_per_40
      ) |> # turn into a nice gt table
      gt() |> 
      # format column names
      cols_label(
        usage = "Usage", 
        AST_TO_ratio = "AST/TO",
        EFG_per = "EFG%",
        TS_per = "TS%",
        dr_per = "DREB%",
        or_per = "OREB%",
        stl_per = "STL%",
        blk_per = "BLK%",
        to_per = "TO%",
        fouls_per_40 = "Fouls/40"
      ) |> 
      gt_theme_ncaa()
  })
  
  # advanced player stats vs tournament teams
  output$advanced_player_stats_vs_good <- render_gt({
    player_stats_vs_good_teams() |> 
      mutate(
        TS_per = (ts_per/100) |> round(digits = 3)
      ) |> 
      select(
        usage, 
        AST_TO_ratio,
        EFG_per,
        TS_per,
        dr_per,
        or_per,
        stl_per,
        blk_per,
        to_per,
        fouls_per_40
      ) |> # turn into a nice gt table
      gt() |> 
      # format column names
      cols_label(
        usage = "Usage", 
        AST_TO_ratio = "AST/TO",
        EFG_per = "EFG%",
        TS_per = "TS%",
        dr_per = "DREB%",
        or_per = "OREB%",
        stl_per = "STL%",
        blk_per = "BLK%",
        to_per = "TO%",
        fouls_per_40 = "Fouls/40"
      ) |> 
      gt_theme_ncaa()
  })
  
  # tables of all in one player metrics
  # player metrics for the whole season
  output$player_metrics <- render_gt({
    player_stats() |> 
      mutate(
        porpagatu = round(porpagatu, 2), 
        d_porpagatu = round(d_porpagatu, 2),
        total_prpg = round(total_prpg, 2),
        OBPM_torvik = round(OBPM_torvik, 2),
        DBPM_torvik = round(DBPM_torvik, 2),
        BPM_torvik = round(BPM_torvik, 2)
      ) |> 
      select(
        porpagatu, 
        d_porpagatu,
        total_prpg,
        OBPM_torvik,
        DBPM_torvik,
        BPM_torvik
      ) |> # turn into a nice gt table
      gt() |> 
      # format column names
      cols_label(
        porpagatu = "Off. PRPG",
        d_porpagatu = "Def. PRPG",
        total_prpg = "Total PRPG",
        OBPM_torvik = "Off. BPM",
        DBPM_torvik = "Def. BPM",
        BPM_torvik = "BPM"
      ) |> 
      gt_theme_ncaa()
  })
  
  # player metrics vs tournament teams
  output$player_metrics_vs_good <- render_gt({
    player_stats_vs_good_teams() |> 
      mutate(
        porpagatu = round(porpagatu, 2), 
        d_porpagatu = round(d_porpagatu, 2),
        total_prpg = round(total_prpg, 2),
        OBPM_torvik = round(OBPM_torvik, 2),
        DBPM_torvik = round(DBPM_torvik, 2),
        BPM_torvik = round(BPM_torvik, 2)
      ) |> 
      select(
        porpagatu, 
        d_porpagatu,
        total_prpg,
        OBPM_torvik,
        DBPM_torvik,
        BPM_torvik
      ) |> # turn into a nice gt table
      gt() |> 
      # format column names
      cols_label(
        porpagatu = "Off. PRPG",
        d_porpagatu = "Def. PRPG",
        total_prpg = "Total PRPG",
        OBPM_torvik = "Off. BPM",
        DBPM_torvik = "Def. BPM",
        BPM_torvik = "BPM"
      ) |> 
      gt_theme_ncaa()
  })
}

#run the application
shinyApp(ui = ui, server = server)