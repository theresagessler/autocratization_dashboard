# ==============================================================================
# Autocratization Episodes Dashboard
# Draft v2 - run prepare_data.R first to generate episode_data.rds
# ==============================================================================

library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

# ------------------------------------------------------------------------
# Load precomputed data (no V-Dem fetching / get_stock() at runtime -
# sessions start instantly)
# ------------------------------------------------------------------------
data_path <- "episode_data.rds"
if (!file.exists(data_path)) {
  stop("episode_data.rds not found. Run prepare_data.R first (with the ",
       "working directory set to this folder) to generate it.")
}
bundle <- readRDS(data_path)

episode_data         <- bundle$episode_data
country_trajectories <- bundle$country_trajectories
regime_switches      <- bundle$regime_switches
regime_levels        <- bundle$regime_levels
regime_colors        <- bundle$regime_colors
experience_levels    <- bundle$experience_levels
experience_colors    <- bundle$experience_colors
params               <- bundle$params

all_countries <- sort(unique(episode_data$country_name))
year_bounds <- range(c(episode_data$episode_start_year, episode_data$episode_end_year))

# Round slider bounds out to clean numbers so the default evenly-spaced tick
# marks land on round values instead of awkward data-driven fractions (e.g.
# a raw max decline of 0.64 produces ticks like 0.07, 0.14, 0.21...; rounded
# up to 0.7 the same slider ticks at 0, 0.1, 0.2, ... 0.7).
round_up_to <- function(x, nearest) ceiling(x / nearest) * nearest
round_down_to <- function(x, nearest) floor(x / nearest) * nearest

year_min_rounded <- round_down_to(year_bounds[1], 10)
year_max_rounded <- round_up_to(year_bounds[2], 10)
duration_max_rounded <- round_up_to(max(episode_data$episode_length), 5)
decline_max_rounded <- round_up_to(max(abs(episode_data$total_decline)), 0.1)

color_by_choices <- c(
  "Regime at episode start" = "regime",
  "Democratic experience (get_stock)" = "experience_getstock",
  "Democratic experience (zero-fill)" = "experience_zerofill"
)

# Combine a classification label + its numeric value into one display string,
# e.g. "Low (0.2-0.4)  ·  0.352" - used for both experience columns in the table
combine_label_value <- function(label, value) {
  ifelse(is.na(label), NA_character_,
         paste0(sprintf("%.3f", value), "  ·  ", as.character(label)))
}

# ------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------
ui <- page_sidebar(
  title = "Autocratization Episodes Dashboard",
  theme = bs_theme(version = 5, primary = "#1c5cab"),

  sidebar = sidebar(
    width = 300,
    selectizeInput("countries", "Countries",
                    choices = all_countries, selected = NULL,
                    multiple = TRUE,
                    options = list(placeholder = "All countries")),
    # ticks = FALSE on all three: ionRangeSlider's auto grid-tick labels are
    # a plain even subdivision of [min, max], which only lands on round
    # numbers by coincidence - with min_decline's floor fixed at 0.01, every
    # tick inherits that offset and can never be round. Suppressing the grid
    # row avoids ever displaying an ugly non-round tick; the exact current
    # value still shows on the handle badge regardless of this setting.
    sliderInput("year_range", "Episode start year range",
                min = year_min_rounded, max = year_max_rounded,
                value = c(1950, year_max_rounded), sep = "", step = 1,
                ticks = FALSE),
    sliderInput("min_duration", "Minimum episode duration (years)",
                min = params$MIN_EPISODE_LENGTH,
                max = duration_max_rounded,
                value = params$MIN_EPISODE_LENGTH, step = 1,
                ticks = FALSE),
    sliderInput("min_decline", "Minimum total decline (absolute value)",
                min = 0.01,
                max = decline_max_rounded,
                value = round(abs(params$TOTAL_DECLINE_THRESHOLD), 2), step = 0.01,
                ticks = FALSE),
    hr(),
    radioButtons("color_by", "Color episodes by",
                 choices = color_by_choices, selected = "regime"),
    uiOutput("category_filter_ui"),
    hr(),
    helpText(
      "Shows every detected decline episode in the Electoral Democracy Index ",
      "(v2x_polyarchy), from any starting regime - not filtered to any fixed ",
      "minimum decline or duration at detection time. The duration and decline ",
      "sliders above apply entirely on top of that: they default to a ",
      "commonly-used threshold (", abs(params$TOTAL_DECLINE_THRESHOLD),
      " index points) but can be moved down to see smaller, more marginal ",
      "declines too. See ", strong("About / Methodology"),
      " for the full detection rules. Data generated ",
      format(bundle$generated_at, "%Y-%m-%d %H:%M"), "."
    )
  ),

  layout_columns(
    col_widths = c(3, 3, 3, 3),
    value_box(title = "Episodes shown", value = textOutput("n_episodes"),
               theme = "primary", height = "130px"),
    value_box(title = "Countries", value = textOutput("n_countries"),
               theme = "secondary", height = "130px"),
    value_box(title = "Median duration", value = textOutput("median_duration"),
               theme = "info", height = "130px"),
    value_box(title = "Median decline", value = textOutput("median_decline"),
               theme = "dark", height = "130px")
  ),

  navset_card_tab(
    nav_panel(
      "Timeline",
      plotOutput("timeline_plot", height = "auto")
    ),
    nav_panel(
      "Episodes active per year",
      plotOutput("stacked_plot", height = "500px")
    ),
    nav_panel(
      "Episode table",
      helpText("Click a row to see that country's full Electoral Democracy ",
               "Index trajectory in a popup, with the selected episode highlighted."),
      DTOutput("episode_table")
    ),
    nav_panel(
      "About / Methodology",
      div(
        style = "max-width: 760px;",
        h4("What counts as an autocratization episode?"),
        p("An episode begins the first year the Electoral Democracy Index ",
          "(v2x_polyarchy) drops by at least ", abs(params$EPISODE_START_DECLINE),
          " points, and ends either when the index rises by at least ",
          params$EPISODE_END_INCREASE, " in a single year, or after ",
          params$EPISODE_END_STAGNATION, " consecutive years where the change ",
          "stays within that same small band (i.e. the decline has stalled)."),
        p(strong("Every"), " detected decline episode is included here - there is ",
          "no fixed minimum decline or duration applied at detection time. ",
          "(An earlier version of this dashboard restricted the view to ",
          "\"confirmed events\" - at least ", abs(params$TOTAL_DECLINE_THRESHOLD),
          " index points over at least ", params$MIN_EPISODE_LENGTH,
          " year(s) - but that's now purely something the sidebar sliders do, ",
          "not a filter baked into the data itself.) Episodes starting from ",
          strong("any"), " regime count - both backsliding within an existing ",
          "democracy and declines starting from an already-autocratic baseline ",
          "are included; the ", strong("Regime at episode start"),
          " classification (below) is how to tell these apart, not a filter ",
          "on which episodes appear."),
        p("The ", strong("sidebar's duration and decline sliders"),
          " default to that same ", abs(params$TOTAL_DECLINE_THRESHOLD),
          "-point / ", params$MIN_EPISODE_LENGTH, "-year threshold (a commonly ",
          "used cutoff for what counts as substantively meaningful backsliding), ",
          "but can be moved in either direction - lower them to see smaller, ",
          "more marginal declines, or raise them to narrow the view further."),
        tags$ul(
          tags$li(strong("A known limitation: "), "a single borderline year can ",
                  "flip whether two declines merge into one episode or stay separate ",
                  "(e.g. a +", params$EPISODE_END_INCREASE,
                  " uptick right at the end threshold). This mostly ",
                  "affects where an episode's boundary falls, not whether a real ",
                  "decline gets detected at all.")
        ),

        h4("Regime classification"),
        p("Each episode is classified by where the index stood ", strong("at its start"),
          ", on a four-level ordinal scale (V-Dem-style bins at 0.25 / 0.5 / 0.75): ",
          paste(regime_levels, collapse = " → "), ". Within an episode, a hollow ",
          "dot on the timeline marks a year where this classification crossed one of ",
          "those boundaries."),

        h4("Democratic experience: two methods, shown side by side"),
        p("\"Democratic experience\" is a discounted cumulative measure of how much ",
          "sustained democratic history a country had accumulated ", em("going into"),
          " the episode (the year before it started) - not just its instantaneous ",
          "index value. Both methods use the same underlying recursion ",
          "(demstock's ", code("get_stock()"), ", discount weight ",
          params$STOCK_WEIGHT, ", gaps filled up to ", params$FILL_YEARS, " years):"),
        tags$ul(
          tags$li(strong("get_stock() (official): "), "the standard demstock calculation. ",
                  "It has one important limitation: if a country has even one historical ",
                  "gap in its data longer than the fill window, the accumulated value ",
                  "goes blank from that point ", strong("forever"), " - even decades later, ",
                  "with perfectly complete data. This silently affects a handful of ",
                  "countries, including some with real, substantively important recent ",
                  "episodes (Honduras, El Salvador, Palestine/West Bank)."),
          tags$li(strong("Zero-fill (alternative): "), "the same recursion and scaling, ",
                  "but every missing year is treated as a democracy score of 0 and the ",
                  "calculation keeps running instead of breaking. This never produces a ",
                  "blank value - the accumulated stock just decays gradually (at the ",
                  "model's own ", round((1 - params$STOCK_WEIGHT) * 100, 1),
                  "%-per-year rate) across a gap, rather than freezing or vanishing.")
        ),
        p("The two methods will usually agree for countries with continuous historical ",
          "coverage. Where they disagree, that disagreement is itself informative - it ",
          "flags a country whose democratic-experience value should be read with the ",
          "data gap in mind."),

        h4("Data"),
        p("Source: V-Dem (Varieties of Democracy), accessed via the ",
          code("vdemdata"), " and ", code("demstock"), " R packages. ",
          "Historical German/Italian sub-national entities (e.g. Baden, Tuscany, ",
          "Saxony) are excluded throughout, matching how demstock itself treats them. ",
          "Dashboard data generated ",
          format(bundle$generated_at, "%Y-%m-%d %H:%M"), "."),
        p(em("This is a draft dashboard; see the companion Quarto documents for the ",
             "full derivation, worked examples, and sensitivity checks behind every ",
             "choice described here."))
      )
    )
  )
)

# ------------------------------------------------------------------------
# Server
# ------------------------------------------------------------------------
server <- function(input, output, session) {

  # Which label/color/level set is active, based on the "color by" choice
  active_label_col <- reactive({
    switch(input$color_by,
      regime = "regime_label",
      experience_getstock = "experience_label_getstock",
      experience_zerofill = "experience_label_zerofill"
    )
  })

  active_levels <- reactive({
    if (input$color_by == "regime") regime_levels else experience_levels
  })

  active_colors <- reactive({
    if (input$color_by == "regime") regime_colors else experience_colors
  })

  active_legend_title <- reactive({
    switch(input$color_by,
      regime = "Regime at episode start",
      experience_getstock = "Democratic experience (get_stock)",
      experience_zerofill = "Democratic experience (zero-fill)"
    )
  })

  # Category checkboxes update to match whichever classification is active
  output$category_filter_ui <- renderUI({
    checkboxGroupInput("categories", "Show categories",
                        choices = active_levels(),
                        selected = active_levels())
  })

  filtered_data <- reactive({
    req(input$categories)
    d <- episode_data %>%
      filter(
        episode_start_year >= input$year_range[1],
        episode_start_year <= input$year_range[2],
        episode_length >= input$min_duration,
        abs(total_decline) >= input$min_decline
      )
    if (!is.null(input$countries) && length(input$countries) > 0) {
      d <- d %>% filter(country_name %in% input$countries)
    }
    label_col <- active_label_col()
    d <- d %>% filter(as.character(.data[[label_col]]) %in% input$categories |
                        (is.na(.data[[label_col]]) & "NA" %in% input$categories))
    d %>%
      mutate(episode_label = fct_reorder(episode_label, episode_start_year))
  })

  output$n_episodes <- renderText({ nrow(filtered_data()) })
  output$n_countries <- renderText({ n_distinct(filtered_data()$country_name) })
  output$median_duration <- renderText({
    d <- filtered_data()
    if (nrow(d) == 0) return("-")
    paste(round(median(d$episode_length), 1), "yrs")
  })
  output$median_decline <- renderText({
    d <- filtered_data()
    if (nrow(d) == 0) return("-")
    round(median(d$total_decline), 2)
  })

  output$timeline_plot <- renderPlot({
    d <- filtered_data()
    validate(need(nrow(d) > 0, "No episodes match the current filters."))

    label_col <- active_label_col()
    switches <- regime_switches %>% filter(episode_id %in% d$episode_id) %>%
      left_join(d %>% select(episode_id, episode_label), by = "episode_id")

    # Labels sit next to each episode's own line rather than on a shared
    # y-axis - at 139+ rows a y-axis label column either gets illegibly
    # small or forces the plot absurdly wide. Needs left-side room scaled
    # to the longest label ("Country Name (YYYY-YYYY)"), since the
    # earliest-starting episode has the least natural space to its left.
    x_range <- range(c(d$episode_start_year, d$episode_end_year))
    max_chars <- max(nchar(as.character(d$episode_label)))
    left_pad <- max_chars * diff(x_range) * 0.011

    ggplot(d, aes(y = episode_label, color = .data[[label_col]])) +
      geom_segment(aes(x = episode_start_year, xend = episode_end_year,
                        yend = episode_label),
                   linewidth = 1.8, lineend = "round") +
      geom_text(aes(x = episode_start_year, label = episode_label),
                hjust = 1, nudge_x = -diff(x_range) * 0.012,
                size = 3, color = "#52514e", show.legend = FALSE) +
      { if (input$color_by == "regime" && nrow(switches) > 0)
          geom_point(data = switches, aes(x = year, y = episode_label),
                     inherit.aes = FALSE, shape = 21, size = 2.2,
                     fill = "#fcfcfb", color = "#0b0b0b", stroke = 0.6) } +
      scale_color_manual(values = active_colors(), name = active_legend_title(),
                          na.value = "#c3c2b7", drop = FALSE) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.05), add = c(left_pad, 0))) +
      labs(
        x = "Year", y = NULL,
        caption = if (input$color_by == "regime" && nrow(switches) > 0)
          "○  marks a year when the regime classification changed within an episode"
        else NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(color = "#e1e0d9"),
        axis.text.x = element_text(color = "#898781"),
        legend.position = "top",
        plot.caption = element_text(size = 9, color = "#52514e", hjust = 0,
                                     margin = margin(t = 8))
      )
  }, height = function() max(400, nrow(filtered_data()) * 16))

  output$stacked_plot <- renderPlot({
    d <- filtered_data()
    validate(need(nrow(d) > 0, "No episodes match the current filters."))

    label_col <- active_label_col()
    levels_now <- active_levels()

    episode_years <- d %>%
      select(episode_id, episode_start_year, episode_end_year, label = !!label_col) %>%
      rowwise() %>%
      mutate(year = list(seq(episode_start_year, episode_end_year))) %>%
      unnest(year) %>%
      ungroup()

    yr_range <- range(c(d$episode_start_year, d$episode_end_year))

    per_year <- episode_years %>%
      count(year, label, name = "n_episodes") %>%
      complete(year = seq(yr_range[1], yr_range[2]), label = levels_now,
               fill = list(n_episodes = 0)) %>%
      mutate(label = factor(label, levels = rev(levels_now)))

    ggplot(per_year, aes(x = year, y = n_episodes, fill = label)) +
      geom_area(position = "stack", color = "#fcfcfb", linewidth = 0.4) +
      scale_fill_manual(values = active_colors(), name = active_legend_title(),
                         drop = FALSE) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      labs(x = "Year", y = "Number of ongoing episodes") +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#e1e0d9"),
        legend.position = "top"
      )
  })

  episode_table_data <- reactive({
    filtered_data() %>%
      mutate(
        index_start = round(index_start, 3),
        index_end = round(index_end, 3),
        total_decline = round(total_decline, 3),
        `Experience (get_stock)` = combine_label_value(experience_label_getstock, dem_stock_electoral),
        `Experience (zero-fill)` = combine_label_value(experience_label_zerofill, stock_zerofill)
      ) %>%
      select(
        Country = country_name,
        `Start year` = episode_start_year,
        `End year` = episode_end_year,
        `Duration (yrs)` = episode_length,
        `Index at start` = index_start,
        `Index at end` = index_end,
        `Total decline` = total_decline,
        `Regime at start` = regime_label,
        `Experience (get_stock)`,
        `Experience (zero-fill)`,
        episode_id  # kept for drill-down lookup, hidden from display below
      )
  })

  output$episode_table <- renderDT({
    d <- episode_table_data()
    datatable(
      d,
      filter = "top", rownames = FALSE, selection = "single",
      options = list(pageLength = 15, scrollX = TRUE,
                      columnDefs = list(list(visible = FALSE,
                                              targets = which(names(d) == "episode_id") - 1)))
    )
  })

  # Row selection is a POSITION, not an identity - if the sidebar filters
  # change the underlying data (e.g. going from one country back to many),
  # that position could otherwise silently point at a different episode.
  # Handled with a single reactiveVal, validated at read-time rather than
  # proactively cleared by a second observer: an earlier version used a
  # separate observeEvent(filtered_data(), ...) to reset the selection, but
  # that observer can fire in the same reactive flush as a genuine row
  # click (whenever filtered_data() happens to be invalidated for any
  # reason), racing against it and wiping out a just-made selection before
  # it was ever shown - the "appears then immediately disappears" bug.
  # Storing *what* was clicked and checking whether it's still part of the
  # current filtered data - only when something actually reads the value -
  # has no second observer to race against.
  selected_episode_id <- reactiveVal(NULL)

  observeEvent(input$episode_table_rows_selected, {
    sel <- input$episode_table_rows_selected
    d <- isolate(episode_table_data())
    if (is.null(sel) || length(sel) == 0 || sel > nrow(d)) {
      selected_episode_id(NULL)
      return()
    }
    selected_episode_id(d$episode_id[sel])

    ep <- isolate(episode_data %>% filter(episode_id == d$episode_id[sel]))
    showModal(modalDialog(
      title = paste0(ep$country_name, ": ", ep$episode_start_year, "-", ep$episode_end_year),
      plotOutput("drilldown_plot", height = "400px"),
      p(style = "margin-top: 10px; color: #52514e;", describe_episode_end(ep)),
      size = "l", easyClose = TRUE, footer = modalButton("Close")
    ))
  }, ignoreNULL = FALSE)

  # Why the episode's detection ended where it did - the three flags are
  # mutually exclusive in practice (see prepare_data.R's state machine: once
  # one end condition fires the episode closes, so only one can apply)
  describe_episode_end <- function(ep) {
    if (isTRUE(ep$ended_by_increase)) {
      paste0("Ended by a single-year increase of at least ",
             params$EPISODE_END_INCREASE, " the following year.")
    } else if (isTRUE(ep$ended_by_stagnation)) {
      paste0("Ended by stagnation: ", params$EPISODE_END_STAGNATION,
             " consecutive years where the annual change stayed between ",
             params$EPISODE_START_DECLINE, " and ", params$EPISODE_END_INCREASE,
             " (i.e. the decline stalled without reversing).")
    } else if (isTRUE(ep$ended_by_data_end)) {
      paste0("No defined end - the decline runs through the most recent year ",
             "of available data, so the episode may still be ongoing.")
    } else {
      NULL
    }
  }

  selected_episode <- reactive({
    ep_id <- selected_episode_id()
    if (is.null(ep_id)) return(NULL)
    # still part of the currently filtered data? if the filters narrowed it
    # out, treat it the same as no selection rather than showing stale info
    if (!(ep_id %in% episode_table_data()$episode_id)) return(NULL)
    episode_data %>% filter(episode_id == ep_id)
  })

  output$drilldown_plot <- renderPlot({
    ep <- selected_episode()
    validate(need(!is.null(ep), ""))

    pad <- 10
    traj <- country_trajectories %>%
      filter(country_id == ep$country_id,
             year >= ep$episode_start_year - pad,
             year <= ep$episode_end_year + pad)

    ggplot(traj, aes(x = year, y = v2x_polyarchy)) +
      annotate("rect", xmin = ep$episode_start_year, xmax = ep$episode_end_year,
               ymin = -Inf, ymax = Inf, fill = "#1c5cab", alpha = 0.12) +
      geom_line(color = "#52514e", linewidth = 0.6, na.rm = TRUE) +
      geom_point(color = "#1c5cab", size = 1.6, na.rm = TRUE) +
      geom_hline(yintercept = 0.5, linetype = "dashed", color = "#898781") +
      labs(
        title = paste0(ep$country_name, ": ", ep$episode_start_year, "-", ep$episode_end_year),
        subtitle = "Shaded region = selected episode. Dashed line = electoral-democracy threshold (0.5).",
        x = "Year", y = "Electoral Democracy Index (v2x_polyarchy)"
      ) +
      coord_cartesian(ylim = c(0, 1)) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 9, color = "#52514e")
      )
  })
}

shinyApp(ui, server)
