library(shiny)
library(bslib)
library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(ggrepel)
library(ggiraph)
library(scales)
library(countrycode)


# Load datasets
jn_df <- readr::read_csv("data/jn_country_stats_all.csv.gz") |>
  filter(issn_l != "0027-8424") |>
  mutate(country_code = countrycode(country_code, "iso3c", "country.name"))
  
esac_df <- readr::read_csv("data/esac_match.csv")

publisher_df <- inner_join(jn_df, esac_df, by = "issn_l")

# Define a named vector for labels
label_map <- c(
  hoad_articles = "HOAD Articles",
  wos_first_author_articles = "WOS First Author Articles",
  wos_corresponding_author_articles = "WOS Corresponding Author Articles",
  scp_first_author_articles = "SCP First Author Articles",
  scp_corresponding_author_articles = "SCP Corresponding Author Articles",
  hoad_oa_articles = "HOAD\nOA Articles",
  hoad_ta_oa_articles = "HOAD\nOA under TA",
  scp_oa_first_author_articles = "SCP First Author\nOA Articles",
  scp_oa_corresponding_author_articles = "SCP Corresponding Author\nOA Articles",
  wos_oa_first_author_articles = "WOS First Author\nOA Articles",
  wos_oa_corresponding_author_articles = "WOS Corresponding Author\nOA Articles",
  scp_ta_oa_first_author_articles = "SCP First Author\n OA under TA",
  scp_ta_oa_corresponding_author_articles = "SCP Corresponding Author\nOA under TA",
  wos_ta_oa_first_author_articles = "WOS First Author\nOA under TA",
  wos_ta_oa_corresponding_author_articles = "WOS Corresponding Author\nOA under TA",
  # Adding labels for OA proportions
  hoad_oa_prop = "HOAD\nOA Proportion (%)",
  wos_first_oa_prop = "WOS First Author\nOA Proportion (%)",
  wos_corresponding_oa_prop = "WOS Corresponding Author\nOA Proportion (%)",
  scp_first_oa_prop = "SCP First Author\nOA Proportion (%)",
  scp_corresponding_oa_prop = "SCP Corresponding Author\nOA Proportion (%)",
  hoad_oa_ta_prop = "HOAD\nOA under TA Proportion (%)",
  wos_first_ta_oa_prop = "WOS First Author\nOA under TA Proportion (%)",
  wos_corresponding_ta_oa_prop = "WOS Corresponding Author\nOA under TA Proportion (%)",
  scp_first_ta_oa_prop = "SCP First Author\nOA under TA Proportion (%)",
  scp_corresponding_ta_oa_prop = "SCP Corresponding Author\nOA under TA Proportion (%)"
)

# Define the function for generating scatter plots
country_scatter_ggplot <- function(.data, x, y, log_scale = FALSE, selected_countries = NULL) {
  .data <- .data |>
    mutate(highlight = ifelse(country_code %in% selected_countries, "selected", "normal"))
  
  p <- ggplot(.data, aes(x = .data[[x]], y = .data[[y]], fill = highlight)) +
    geom_point_interactive(aes(tooltip = paste("Country:", country_code,
                                               "<br>", label_map[x], ":",
                                               if (grepl("prop", x)) {
                                                 scales::percent(.data[[x]], accuracy = 0.01) 
                                               } else {
                                                 label_number(big.mark = ",")(.data[[x]])
                                               },
                                               "<br>", label_map[y], ":",
                                               if (grepl("prop", y)) {
                                                 scales::percent(.data[[y]], accuracy = 0.01) 
                                               } else {
                                                 label_number(big.mark = ",")(.data[[y]])
                                               })),
                           pch = 21, alpha = 0.8, size = 3) +
    scale_fill_manual(values = c("selected" = "#fc5185", "normal" = "#cbcbcba0")) + # Selected color
    geom_text_repel(
      data = .data |>
        filter(highlight == "selected"),  
      aes(label = country_code),
      box.padding = 0.3,
      family = "Atkinson Hyperlegible"
    ) +
    geom_abline() +
    theme_minimal(base_family = "Atkinson Hyperlegible") +
    theme(
      aspect.ratio = 1,
      panel.border = element_rect(color = "grey50", fill = NA),
      axis.title = element_text(size = 10),
      legend.position = "none"  
    ) +
    labs(
      x = label_map[x],  
      y = label_map[y]  
    )
  
  if (log_scale) {
    p <- p + scale_x_log10(labels = label_number(big.mark = ",")) + 
      scale_y_log10(labels = label_number(big.mark = ","))
  } else {
    # Check if the selected variable relates to proportions
    if (grepl("prop", x)) {
      p <- p + scale_x_continuous(labels = scales::percent) +
        scale_y_continuous(labels = scales::percent)
    } else {
      p <- p + scale_x_continuous(labels = label_number(big.mark = ",")) + 
        scale_y_continuous(labels = label_number(big.mark = ","))
    }
  }
  
  return(p)
}

# Define selection inputs

# Calculate total articles by publisher
publisher_counts <- publisher_df |>
  group_by(esac_publisher) |>
  summarise(total_articles = sum(hoad_articles, na.rm = TRUE)) |>
  arrange(desc(total_articles))  # Sort by total articles in descending order

# Create a sorted vector for selectizeInput
sorted_publishers <- c("All" = "All", setNames(publisher_counts$esac_publisher, publisher_counts$esac_publisher))


# Define UI for application
ui <- page_sidebar(
  theme = bs_theme(base_font = "Atkinson Hyperlegible") |>
    bs_add_rules(
      "@font-face {
           font-family: Atkinson Hyperlegible;
           src: url('fonts/AtkinsonHyperlegible-Regular.ttf');
           font-weight: 400;
           font-style: normal;
           font-display: swap;
        }"
    ),
  padding = 0,
  fillable = FALSE,
    title = "Replication Dashboard: Hybrid Open Access in Transformative Agreements",
    sidebar = sidebar(
    selectizeInput(
      inputId = "indicator",
      label = "Select indicator",
      choices = c(Articles = "articles", 
                  `OA Articles` = "oa_articles", 
                  `OA Articles under TA` = "ta_oa_articles", 
                  `OA Articles (%)` = "prop_oa_articles", 
                  `OA Articles under TA (%)` = "prop_ta_oa_articles")
    ),
    selectizeInput(
      inputId = "year",
      label = "Select earliest publication year",
      choices = c("2019-23" = "all", rev(sort(unique(jn_df$earliest_year))))
    ),
    selectizeInput(
      inputId = "publisher",
      label = "Select Publisher",
      choices = sorted_publishers,
      multiple = FALSE,  # Single selection
      selected = "All"   # Default selection set to "All"
    ),
    selectInput(
      inputId = "country",
      label = "Highlight Countries",
      choices = sort(unique(jn_df$country_code)),
      multiple = TRUE,
      selected = "Germany"  # Default selection
    ),
    numericInput(
      inputId = "min_articles",
      label = "Minimum Publication Volume (Total Articles)",
      value = 10000, 
      min = 1
    )
  ),
  card(
    card_header("Hybrid Open Access Trends by Bibliometric Database"),
    full_screen = TRUE,
    max_height = 600,
    card_body(uiOutput("log_scale_ui"),  # Conditional UI for log scale
              girafeOutput("my_plot"))),
      card(
    card_header("About"),
    class = "fs-6",
      markdown("This dashboard presents indicators of open access uptake in hybrid journals included in [transformative agreements](https://esac-initiative.org/about/transformative-agreements/). It replicates [methodologies](https://doi.org/10.1162/qss_a_00348) developed for the publicly available [Hybrid Open Access Dashboard (HOAD)](https://subugoe.github.io/hoaddash/) that are applied to the Web of Science and Scopus. As such, it is a useful way of assessing the suitability of open research information in comparison to established bibliometric data sources in line with the [Barcelona Declaration of Open Research Information](https://barcelona-declaration.org/preamble/). Web of Science and Scopus data provided by the [Kompetenznetzwerk Bibliometrie](https://bibliometrie.info/).")
      )
  )

server <- function(input, output, session) {
  
  # Conditional UI for log scale
  output$log_scale_ui <- renderUI({
    if (input$indicator %in% c("articles", "oa_articles", "ta_oa_articles")) {
      checkboxInput(
        inputId = "log_scale",
        label = "Use log scale",
        value = TRUE
      )
    }
  })
  
  # Create reactive value for log scale
  log_scale_value <- reactive({
    if (input$indicator %in% c("articles", "oa_articles", "ta_oa_articles")) {
      return(if (!is.null(input$log_scale)) input$log_scale else FALSE)
    } else {
      return(FALSE)
    }
  })
  
  output$my_plot <- renderGirafe({
    print("Starting renderPlot")  
    
    # Define the variables based on the selected indicator
    hoad_var <- switch(input$indicator,
                       articles = "hoad_articles",
                       oa_articles = "hoad_oa_articles",
                       ta_oa_articles = "hoad_ta_oa_articles",
                       prop_oa_articles = "hoad_oa_prop",
                       prop_ta_oa_articles = "hoad_oa_ta_prop")
    
    # Filter publisher_df based on selection
    filtered_publishers <- publisher_df |>
      filter(if (input$publisher == "All") {
        TRUE  # Include all rows if "All" is selected
      } else {
        esac_publisher == input$publisher  # Filter by selected publisher
      })
    
    # Set the minimum articles threshold
    min_articles_threshold <- input$min_articles
    
    plot_df <- filtered_publishers |>
      filter(if (input$year == "all") TRUE else earliest_year %in% input$year) |>
      select(-earliest_year) |>
      group_by(country_code) |>
      summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) |> 
      mutate(
        hoad_oa_prop = hoad_oa_articles / hoad_articles,
        hoad_oa_ta_prop = hoad_ta_oa_articles / hoad_articles,
        wos_first_oa_prop = wos_oa_first_author_articles / wos_first_author_articles,
        wos_first_ta_oa_prop = wos_ta_oa_first_author_articles / wos_first_author_articles,
        wos_corresponding_oa_prop = wos_oa_corresponding_author_articles / wos_corresponding_author_articles,
        wos_corresponding_ta_oa_prop = wos_ta_oa_corresponding_author_articles / wos_corresponding_author_articles,
        scp_first_oa_prop = scp_oa_first_author_articles / scp_first_author_articles,
        scp_first_ta_oa_prop = scp_ta_oa_first_author_articles / scp_first_author_articles,
        scp_corresponding_oa_prop = scp_oa_corresponding_author_articles / scp_corresponding_author_articles,
        scp_corresponding_ta_oa_prop = scp_ta_oa_corresponding_author_articles / scp_corresponding_author_articles
      ) |>
      filter(hoad_articles >= min_articles_threshold, !is.na(country_code))
    
    print(paste("Number of rows in plot_df:", nrow(plot_df)))
    
    # Set WOS and SCP variables based on selected indicator
    wos_first_var <- switch(input$indicator,
                            articles = "wos_first_author_articles",
                            oa_articles = "wos_oa_first_author_articles",
                            ta_oa_articles = "wos_ta_oa_first_author_articles",
                            prop_oa_articles = "wos_first_oa_prop",
                            prop_ta_oa_articles = "wos_first_ta_oa_prop")
    
    wos_cor_var <- switch(input$indicator,
                          articles = "wos_corresponding_author_articles",
                          oa_articles = "wos_oa_corresponding_author_articles",
                          ta_oa_articles = "wos_ta_oa_corresponding_author_articles",
                          prop_oa_articles = "wos_corresponding_oa_prop",
                          prop_ta_oa_articles = "wos_corresponding_ta_oa_prop")
    
    scp_first_var <- switch(input$indicator,
                            articles = "scp_first_author_articles",
                            oa_articles = "scp_oa_first_author_articles",
                            ta_oa_articles = "scp_ta_oa_first_author_articles",
                            prop_oa_articles = "scp_first_oa_prop",
                            prop_ta_oa_articles = "scp_first_ta_oa_prop")
    
    scp_cor_var <- switch(input$indicator,
                          articles = "scp_corresponding_author_articles",
                          oa_articles = "scp_oa_corresponding_author_articles",
                          ta_oa_articles = "scp_ta_oa_corresponding_author_articles",
                          prop_oa_articles = "scp_corresponding_oa_prop",
                          prop_ta_oa_articles = "scp_corresponding_ta_oa_prop")
    
    print("Creating first plot")  
    p1 <- country_scatter_ggplot(.data = plot_df, x = hoad_var, y = wos_first_var, log_scale = log_scale_value(), selected_countries = input$country)
    print("Creating second plot")  
    p2 <- country_scatter_ggplot(.data = plot_df, x = hoad_var, y = wos_cor_var, log_scale = log_scale_value(), selected_countries = input$country)
    print("Creating third plot")  
    p3 <- country_scatter_ggplot(.data = plot_df, x = hoad_var, y = scp_first_var, log_scale = log_scale_value(), selected_countries = input$country)
    print("Creating fourth plot")  
    p4 <- country_scatter_ggplot(.data = plot_df, x = hoad_var, y = scp_cor_var, log_scale = log_scale_value(), selected_countries = input$country)
    
    print("Combining plots")  
    my_scatter_plot <- (p1 + p2)  / (p3 + p4) +
      theme(legend.position = "none") 
    
    girafe(ggobj = my_scatter_plot)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

## france -> year long delay: https://www.science.org/content/article/elsevier-deal-france-disappoints-open-access-advocates
