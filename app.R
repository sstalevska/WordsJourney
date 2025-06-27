
library(shiny)
library(dplyr)
library(stringr)
library(shinycssloaders)
library(visNetwork)
library(igraph)
library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(readr)
library(plotly)

# Load processed data 
dict <- readRDS("dict.rds")
etymology <- readRDS("etymology.rds")
top_languages <- readRDS("top_languages.rds")
top_roots <- readRDS("top_roots.rds")
words_relations <- readRDS("relations.rds")
semantic_displacement <- readRDS("semantic_displacement.rds")



strong_reltype_weights <- c(
  "inherited from" = 2.0
)

normalize_reltype <- function(rt) {
  rt <- gsub("_", " ", rt)
  rt <- trimws(rt)
  return(rt)
}

# prettify relation type strings 
pretty_reltype <- function(reltype) {
  reltype <- gsub("_", " ", reltype)
  if (reltype == "borrowed from") return("borrowed words from")
  if (reltype == "inherited from") return("inherited words from")
  paste0(reltype, " words from")
}

valid_words <- sort(unique(semantic_displacement$word))
valid_words <- valid_words[grepl("^[a-zA-Z'-]+$", valid_words) & nchar(valid_words) > 1]

# Create a smaller subset: e.g., top 500 English terms only
small_etymology <- etymology %>%
  filter(lang == "English") %>%
  slice_head(n = 500) %>%
  # Also include rows where term or related_term is in those 500 words to keep tree intact
  { 
    english_terms <- .$term
    etymology %>% filter(term %in% english_terms | related_term %in% english_terms)
  }


ui <- fluidPage(
  titlePanel("🌐 Journey of Words and Languages"),
  tags$style(HTML("
    .hoverlayer .hoverlabel {
      max-width: 300px ;
      white-space: normal ;
      text-align: left ;
      pointer-events: auto ;
    }
    .vis-network {
    font-family: 'Fira Sans', sans-serif;
    transition: all 0.4s ease-in-out;
  }
  ")),
  tabsetPanel(
    id = "main_tabs",
    tabPanel("Language Relatedness Network",
             sidebarLayout(
               sidebarPanel(
                 helpText("Network connectivity of languages by inheritance "),
                 helpText("• Node size = total weighted connectivity of language."),
                 helpText("• Edge width = weighted number of shared inherited words"),
                 helpText("• Node color = cluster community detected by graph clustering algorithm.")
               ),
               mainPanel(
                 withSpinner(visNetworkOutput("language_network", height = "900px"))
               )
             )),
    tabPanel("English Words Inheritance",
             sidebarLayout(
               sidebarPanel(
                 selectInput("source_lang", "Choose a Source Language:", choices = NULL)
               ),
               mainPanel(
                 h4(textOutput("inherited_title")),
                 withSpinner(tableOutput("borrowed_words"))
               )
             )),
    tabPanel("Word Relations Translator",
             sidebarLayout(
               sidebarPanel(
                 selectInput("origin_lang", "Select Origin Language:", choices = unique(unlist(strsplit(words_relations$Languages, " --> "))), selected = NULL),
                 selectInput("target_lang", "Select Target Language:", choices = unique(unlist(strsplit(words_relations$Languages, " --> "))), selected = NULL),
                 textInput("filter_word", "Type a word to filter:", "")
               ),
               mainPanel(
                 h4("Related Word Pairs"),
                 withSpinner(tableOutput("filtered_pairs"))
               )
             )),
    tabPanel("Languages and Their Influences",
             sidebarLayout(
               sidebarPanel(
                 selectInput("focus_lang", "Choose a Language to Analyze Influence:", choices = top_languages),
                 uiOutput("relation_type_selector")  
               ),
               mainPanel(
                 h4("Overall Shared Words with Other Languages"),
                 withSpinner(plotlyOutput("influence_plot")),  
                 hr(),
                 uiOutput("relations_summary_ui")
               )
             )),
    tabPanel("Semantic Displacement Over Time",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("selected_words", "Select Words:", choices = valid_words, selected = "broadcast", multiple = TRUE),
                 helpText("This tab shows the semantic displacement over time for words compared to a baseline."),
                 helpText("Plot Explanation:
                          This plot shows how much the meaning of a selected word has changed over time,
                          from the earliest year available to the latest.
                          The y-axis represents how different the word’s meaning is compared to its original meaning
                          — with 0 meaning no change and 1 meaning the maximum change observed for that word.
                          A steady upward line means gradual change; sharp rises mean sudden shifts in meaning. 
                          Big  shifts during certain decades — possible new slang uses, changes in common contexts, or different cultural meanings."),
                 sliderInput("year_range", "Select Year Range:",
                             min = min(semantic_displacement$decade, na.rm = TRUE),
                             max = max(semantic_displacement$decade, na.rm = TRUE),
                             value = c(min(semantic_displacement$decade, na.rm = TRUE),
                                       max(semantic_displacement$decade, na.rm = TRUE)),
                             step = 10,
                             sep = "")
                 
               ),
               mainPanel(
                 h4(textOutput("displacement_title")),
                 p("Note: Semantic displacement (Meaning Shift) in terms of baseline meaning 0."),
                 withSpinner(plotlyOutput("displacement_plot"))
               )
             )),
    tabPanel("Semantic Displacement Ranked",
             sidebarLayout(
               sidebarPanel(
                 selectInput("change_filter", "Filter by change level:",
                             choices = c("Most stable" = "stable", "Most changed" = "changed", "All" = "all"),
                             selected = "all")
               ),
               mainPanel(
                 withSpinner(tableOutput("ranked_words_table"))
               )
             )),
    
    tabPanel("Top 10 Semantic Shifts",
             fluidRow(
               column(
                 width = 3,
                 wellPanel(
                   helpText("Top 10 Semantic Shifts - 
This chart shows the words that have experienced the most sudden changes in meaning over time."),
                   sliderInput("top10_year_range", "Select Year Range:",
                               min = min(semantic_displacement$decade, na.rm = TRUE),
                               max = max(semantic_displacement$decade, na.rm = TRUE),
                               value = c(min(semantic_displacement$decade, na.rm = TRUE),
                                         max(semantic_displacement$decade, na.rm = TRUE)),
                               step = 10,
                               sep = "")
                 )
               ),
               column(
                 width = 9,
                 div(style = "overflow-x: auto; padding-right: 40px;",
                     withSpinner(plotlyOutput("top10_shift_plot", height = "600px"))
                 )
               )
             )
    ),
    tabPanel("Language Influence Spread",
             sidebarLayout(
               sidebarPanel(
                 uiOutput("relation_type_choice"),
                 selectInput("source_language", "Select Source Language:", choices = top_languages)
               ),
               mainPanel(
                 h4(textOutput("influence_spread_title")),
                 withSpinner(plotlyOutput("influence_spread_plot")),
                 helpText("Hover over bars to see the number of words shared via the selected relationship.")
               )
             )
    ),
    tabPanel("Etymology Tree",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("etym_word", "Enter an English Word:", 
                                choices = sort(unique(small_etymology$term[small_etymology$lang == "English"])), 
                                selected = "light", multiple = FALSE),
                 sliderInput("max_depth", "Max Tree Depth:", min = 2, max = 10, value = 5),
                 helpText("Explore how words trace back through history."),
                 helpText("Hover over nodes and edges to see details."),
                 helpText("Select a word.")
               ),
               mainPanel(
                 h4("Word Etymology Tree"),
                 withSpinner(visNetworkOutput("etymology_tree", height = "700px"))
               )
             )
    )
    
    
    
  )
)


server <- function(input, output, session) {
  all_languages <- sort(unique(c(etymology$lang, etymology$related_lang)))
  
  output$language_network <- renderVisNetwork({
    ety <- etymology %>%
      mutate(reltype_norm = normalize_reltype(reltype)) %>%
      filter(lang %in% top_languages, related_lang %in% top_languages) %>%
      filter(!is.na(term)) %>%
      filter(reltype_norm %in% names(strong_reltype_weights)) %>%
      distinct(term, lang, related_lang, reltype_norm)
    
    ety$weight <- strong_reltype_weights[ety$reltype_norm]
    
    # Aggregate weighted shared words between language pairs (undirected)
    edges_summary <- ety %>%
      mutate(lang1 = pmin(lang, related_lang),
             lang2 = pmax(lang, related_lang)) %>%
      group_by(lang1, lang2) %>%
      summarise(
        weighted_score = sum(weight),
        shared_words = n_distinct(term),
        .groups = "drop"
      ) %>%
      filter(lang1 != lang2) 
    
    g <- graph_from_data_frame(edges_summary, directed = FALSE, vertices = data.frame(name = top_languages))
    
    if (ecount(g) == 0) {
      # Empty graph fallback
      nodes <- data.frame(id = top_languages, label = top_languages, color = "gray", value = 10)
      edges <- data.frame(from = character(0), to = character(0))
      
      visNetwork(nodes, edges) %>%
        visNodes(shape = "dot") %>%
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
    } else {
      clusters <- cluster_louvain(g, weights = E(g)$weighted_score)
      membership <- membership(clusters)
      node_degree <- strength(g, weights = E(g)$weighted_score)
      nodes <- data.frame(
        id = V(g)$name,
        cluster = membership,
        value = node_degree + 5,
        label = V(g)$name,
        stringsAsFactors = FALSE
      )
      
      n_clusters <- length(unique(membership))
      cluster_colors <- RColorBrewer::brewer.pal(min(n_clusters, 8), "Set2")
      if (n_clusters > 8) {
        cluster_colors <- colorRampPalette(cluster_colors)(n_clusters)
      }
      nodes$color <- cluster_colors[nodes$cluster]
      
      edges <- data.frame(
        from = edges_summary$lang1,
        to = edges_summary$lang2,
        width = scales::rescale(edges_summary$weighted_score, to = c(1, 15)),
        title = paste0(edges_summary$shared_words, " inherited words; ", round(edges_summary$weighted_score, 2)),
        stringsAsFactors = FALSE
      )
      
      visNetwork(nodes, edges) %>%
        visNodes(shape = "dot", scaling = list(min = 10, max = 40)) %>%
        visEdges(smooth = FALSE, color = list(color = "#97C2FC", highlight = "orange")) %>%
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
        visPhysics(stabilization = TRUE, solver = "forceAtlas2Based") %>%
        visLayout(improvedLayout = TRUE)
    }
  })
  
  # English Words Inheritance tab
  inherited_langs <- reactive({
    etymology %>%
      filter(reltype == "inherited_from", lang == "English") %>%
      distinct(related_lang) %>%
      pull(related_lang) %>%
      intersect(top_languages)
  })
  
  observe({
    updateSelectInput(session, "source_lang", choices = inherited_langs())
  })
  
  output$inherited_title <- renderText({
    req(input$source_lang)
    paste("English Words Inherited from", input$source_lang)
  })
  
  output$borrowed_words <- renderTable({
    req(input$source_lang)
    etymology %>%
      filter(reltype == "inherited_from", lang == "English", related_lang == input$source_lang) %>%
      distinct(term) %>%
      inner_join(dict, by = c("term" = "word")) %>%
      group_by(term) %>%
      slice(1) %>%  # take only the first definition
      ungroup() %>%
      select(Word = term, Definition = definition)
    
  })
  
  # Word Relations Translator tab
  output$filtered_pairs <- renderTable({
    req(input$origin_lang, input$target_lang)
    
    lang_pair_str <- paste(input$origin_lang, "-->", input$target_lang)
    
    filtered <- words_relations %>%
      filter(Languages == lang_pair_str)
    
    if (nchar(input$filter_word) > 0) {
      word_filter <- tolower(input$filter_word)
      filtered <- filtered %>%
        filter(str_detect(tolower(Start), fixed(word_filter)) | str_detect(tolower(End), fixed(word_filter)))
    }
    
    filtered %>% select(Start, End, Languages)
  })
  
  # Languages and Their Influences tab
  related_languages_by_relation <- reactive({
    req(input$focus_lang)
    
    ety <- etymology %>%
      filter(!is.na(lang), !is.na(related_lang)) %>%
      filter(lang == input$focus_lang | related_lang == input$focus_lang) %>%
      distinct(term, lang, related_lang, reltype)
    
    ety <- ety %>%
      mutate(other_lang = ifelse(lang == input$focus_lang, related_lang, lang)) %>%
      filter(other_lang != input$focus_lang) %>%
      filter(other_lang %in% top_languages)
    
    counts <- ety %>%
      group_by(reltype, other_lang) %>%
      summarise(shared_words = n_distinct(term), .groups = "drop")
    
    counts
  })
  
  #checkbox group for selecting relation types
  output$relation_type_selector <- renderUI({
    df <- related_languages_by_relation()
    req(nrow(df) > 0)
    reltypes <- unique(df$reltype)
    
    tagList(
      fluidRow(
        column(6, actionButton("select_all_reltypes", "Select All")),
        column(6, actionButton("deselect_all_reltypes", "Select None"))
      ),
      checkboxGroupInput(
        "selected_reltypes", 
        "Filter Relation Types:", 
        choices = setNames(reltypes, sapply(reltypes, pretty_reltype)), 
        selected = reltypes
      )
      
    )
  })
  
  observeEvent(input$select_all_reltypes, {
    df <- related_languages_by_relation()
    reltypes <- unique(df$reltype)
    updateCheckboxGroupInput(session, "selected_reltypes", selected = reltypes)
  })
  
  observeEvent(input$deselect_all_reltypes, {
    updateCheckboxGroupInput(session, "selected_reltypes", selected = character(0))
  })
  
  
  
  # Interactive influence bar chart
  output$influence_plot <- renderPlotly({
    req(input$focus_lang)
    counts <- related_languages_by_relation()
    
    # Filter by selected relation types
    if (!is.null(input$selected_reltypes) && length(input$selected_reltypes) > 0) {
      counts <- counts %>% filter(reltype %in% input$selected_reltypes)
    }
    
    # Sort within filtered
    counts <- counts %>%
      group_by(other_lang) %>%
      summarise(total = sum(shared_words)) %>%
      arrange(desc(total)) %>%
      select(other_lang) %>%
      inner_join(counts, by = "other_lang")
    
    p <- ggplot(counts, aes(x = reorder(other_lang, shared_words), y = shared_words, fill = reltype,
                            text = paste("Relation Type:", reltype, "<br>",
                                         "Language:", other_lang, "<br>",
                                         "Shared Words:", shared_words))) +
      geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
      coord_flip() +
      labs(title = paste("Shared Words by Relation Type with", input$focus_lang),
           x = "Related Language", y = "Number of Shared Words") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(title = list(text = "<b>Relation Type</b>")))
  })
  
  output$relations_summary_ui <- renderUI({
    df <- related_languages_by_relation()
    req(nrow(df) > 0)
    
    reltypes <- unique(df$reltype)
    
    summaries <- lapply(reltypes, function(rel) {
      sub_df <- df %>% filter(reltype == rel) %>%
        select(Language = other_lang, Shared_Words = shared_words)
      
      
      tagList(
        h4(paste(input$focus_lang, "has", pretty_reltype(rel), ":")),
        tableOutput(paste0("summary_", gsub("[^a-zA-Z0-9]", "", rel))),
        br()
      )
    })
    
    do.call(tagList, summaries)
  })
  
  observe({
    df <- related_languages_by_relation()
    reltypes <- unique(df$reltype)
    
    for (rel in reltypes) {
      local({
        rel_local <- rel
        tbl_id <- paste0("summary_", gsub("[^a-zA-Z0-9]", "", rel_local))
        sub_df <- df %>% filter(reltype == rel_local) %>%
          select(Language = other_lang, `Shared Words` = shared_words)
        
        output[[tbl_id]] <- renderTable({
          sub_df
        }, colnames = FALSE, striped = TRUE, bordered = TRUE, spacing = "s")
      })
    }
  })
  # Semantic displacement plot
  output$displacement_title <- renderText({
    req(input$selected_words)
    paste("Semantic Displacement of:", paste(input$selected_words, collapse = ", "))
  })
  
  
  
  output$displacement_plot <- renderPlotly({
    req(input$selected_words)
    df <- semantic_displacement %>%
      filter(word %in% input$selected_words,
             decade >= input$year_range[1],
             decade <= input$year_range[2])
    
    if (nrow(df) == 0) {
      return(plotly_empty(type = "scatter", title = "No data available for selected words."))
    }
    
    p <- ggplot(df, aes(x = decade, y = displacement, color = word, group = word)) +
      geom_line(size = 1.5) +
      geom_point(size = 2) +
      labs(x = "Year", y = "Semantic Displacement",
           title = "Semantic Displacement over Time") +
      theme_minimal()
    
    ggplotly(p, tooltip = c("x", "y", "color"))
  })
  
  
  
  
  
  # Ranked list with filtering
  output$ranked_words_table <- renderTable({
    filtered_data <- semantic_displacement %>%
      group_by(word) %>%
      filter(n() >= 3) %>%
      ungroup() %>%
      filter(!str_detect(word, "^[0-9]+$"))  # remove words that are only digits
    
    df_summary <- filtered_data %>%
      group_by(word) %>%
      summarise(var_displacement = var(displacement, na.rm = TRUE)) %>%
      arrange(desc(var_displacement))
    
    filter_type <- input$change_filter
    if (filter_type == "stable") {
      df_summary <- df_summary %>% arrange(var_displacement) %>% head(50)
    } else if (filter_type == "changed") {
      df_summary <- df_summary %>% head(50)
    }
    
    df_summary %>%
      rename(`Word` = word, `Variance of Semantic Displacement` = var_displacement)
  })
  
  
  # Top 10 most shifted bar chart
  output$top10_shift_plot <- renderPlotly({
    req(input$top10_year_range)
    
    filtered_data <- semantic_displacement %>%
      filter(decade >= input$top10_year_range[1],
             decade <= input$top10_year_range[2]) %>%
      group_by(word) %>%
      filter(n() >= 3) %>%
      ungroup()
    
    top10 <- filtered_data %>%
      group_by(word) %>%
      summarise(var_displacement = var(displacement, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(var_displacement)) %>%
      head(10)
    
    top10_with_def <- top10 %>%
      left_join(dict, by = c("word")) %>%
      mutate(tooltip = paste0(
        "<b>", word, "</b><br>Variance: ", round(var_displacement, 4),
        ifelse(!is.na(definition),
               paste0("<br><i>", gsub("\n", "<br>", str_wrap(definition, width = 60)), "</i>"),
               "")
      ))
    
    
    p <- ggplot(top10_with_def, aes(x = reorder(word, var_displacement),
                                    y = var_displacement, text = tooltip)
    ) +
      geom_bar(stat = "identity", fill = "purple") +
      labs(title = "Top 10 Words with Highest Variance in Semantic Displacement",
           x = "Word",
           y = "Variance of Semantic Displacement") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        margin = list(l = 100, r = 100),
        hoverlabel = list(
          bgcolor = "white",
          font = list(size = 14),
          align = "left",
          namelength = -1,
          width = 120   
        )
        
      ) %>%
      htmlwidgets::onRender("
      function(el, x) {
        var tooltips = document.querySelectorAll('.hoverlayer .hoverlabel');
        tooltips.forEach(function(tt) {
          tt.style.maxWidth = '50px';       // narrower tooltip width
          tt.style.whiteSpace = 'normal';    // allow text wrap
          tt.style.textAlign = 'left';       // left align text
          tt.style.pointerEvents = 'auto';   // allow mouse interaction if needed
          tt.style.wordWrap = 'break-word';  // break long words if needed
        });
      }
    ")
  })
  
  
  
  # Language influence spread
  output$relation_type_choice <- renderUI({
    available_reltypes <- etymology %>%
      filter(lang %in% top_languages) %>%
      pull(reltype) %>%
      unique() %>%
      sort()
    
    selectInput("selected_relation_type", "Select Relation Type:",
                choices = setNames(available_reltypes, sapply(available_reltypes, pretty_reltype)),
                selected = "inherited_from")
  })
  output$influence_spread_title <- renderText({
    req(input$source_language, input$selected_relation_type)
    paste("Languages influenced by", input$source_language, "via", input$selected_relation_type)
  })
  
  output$influence_spread_plot <- renderPlotly({
    req(input$source_language, input$selected_relation_type)
    
    df <- etymology %>%
      filter(reltype == input$selected_relation_type,
             related_lang == input$source_language,
             lang != input$source_language) %>%
      group_by(lang) %>%
      summarise(shared_words = n_distinct(term), .groups = "drop") %>%
      arrange(desc(shared_words)) %>%
      head(20)
    
    
    if (nrow(df) == 0) {
      return(plotly_empty(type = "bar", title = "No data found for this relation."))
    }
    
    p <- ggplot(df, aes(x = reorder(lang, shared_words), y = shared_words,
                        text = paste0("Language: ", lang, "<br>Shared Words: ", shared_words))) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(x = "Influenced Language", y = "Number of Shared Words",
           title = paste("Languages influenced by", input$source_language, "via", input$selected_relation_type)) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  
  
  # nisho ich
  
  # Reactive subset of etymology data filtered to English & small set for performance
  filtered_etymology <- reactive({
    small_etymology
  })
  
  # Function to build graph nodes and edges for a given word up to max_depth, with levels for hierarchy
  build_etymology_graph <- function(start_word, max_depth) {
    nodes <- data.frame()
    edges <- data.frame()
    
    visited <- character()
    queue <- data.frame(term = start_word, depth = 0, stringsAsFactors = FALSE)
    
    while (nrow(queue) > 0) {
      current <- queue[1, ]
      queue <- queue[-1, , drop=FALSE]
      term <- current$term
      depth <- current$depth
      
      if (term %in% visited) next
      visited <- c(visited, term)
      
      ety_rows <- filtered_etymology() %>% filter(term == !!term)
      
      if (nrow(ety_rows) == 0) {
        if (!(term %in% nodes$id)) {
          nodes <- rbind(
            nodes,
            data.frame(
              id = term,
              label = term,
              title = paste0("<b>", term, "</b>"),
              group = "term",
              level = depth,
              stringsAsFactors = FALSE
            )
          )
        }
        next
      }
      
      if (!(term %in% nodes$id)) {
        first_lang <- ety_rows$lang[1]
        node_title <- paste0("<b>Term: </b>", term, "<br><b>Language: </b>", first_lang)
        nodes <- rbind(
          nodes,
          data.frame(
            id = term,
            label = term,
            title = node_title,
            group = first_lang,
            level = depth,
            stringsAsFactors = FALSE
          )
        )
      }
      
      for (i in seq_len(nrow(ety_rows))) {
        related <- ety_rows$related_term[i]
        rel_lang <- ety_rows$related_lang[i]
        rel_type <- ety_rows$reltype[i]
        
        if (!(related %in% nodes$id)) {
          node_title <- paste0("<b>Term: </b>", related, "<br><b>Language: </b>", rel_lang)
          nodes <- rbind(
            nodes,
            data.frame(
              id = related,
              label = related,
              title = node_title,
              group = rel_lang,
              level = depth + 1,
              stringsAsFactors = FALSE
            )
          )
        }
        
        edge_title <- paste0("<b>Relation: </b>", pretty_reltype(rel_type), "<br><b>Related Language: </b>", rel_lang)
        edges <- rbind(
          edges,
          data.frame(
            from = term,
            to = related,
            title = edge_title,
            stringsAsFactors = FALSE
          )
        )
        
        if (depth + 1 < max_depth && !(related %in% visited)) {
          queue <- rbind(queue, data.frame(term = related, depth = depth + 1, stringsAsFactors = FALSE))
        }
      }
    }
    
    list(nodes = nodes, edges = edges)
  }
  
  # Reactive graph data for the selected word and max depth
  etymology_graph_data <- reactive({
    req(input$etym_word)
    build_etymology_graph(input$etym_word, input$max_depth)
  })
  
  # Render the visNetwork graph with hierarchical layout
  output$etymology_tree <- renderVisNetwork({
    graph_data <- etymology_graph_data()
    visNetwork(graph_data$nodes, graph_data$edges) %>%
      visNodes(shape = "dot", size = 20, font = list(size = 16)) %>%
      visEdges(arrows = "to", smooth = TRUE) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visInteraction(hover = TRUE) %>%
      visPhysics(stabilization = TRUE) %>%
      visHierarchicalLayout(direction = "UD", sortMethod = "directed")
  })
  
  # Reactive value to track clicked node
  clicked_node <- reactiveVal(NULL)
  
  observeEvent(input$etymology_tree_selected, {
    clicked_node(input$etymology_tree_selected)
  })
  
  # Show detailed etymology entries for clicked node below the graph
  output$clicked_node_details <- renderTable({
    req(clicked_node())
    filtered_etymology() %>%
      filter(term == clicked_node()) %>%
      select(term, lang, related_term, related_lang, reltype, description = desc) %>%
      distinct()
  })
  
  
}

# Run app
shinyApp(ui = ui, server = server)
