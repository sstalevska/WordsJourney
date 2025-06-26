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

ui <- fluidPage(
  titlePanel("🌐 Journey of Words and Languages"),
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
                 selectInput("focus_lang", "Choose a Language to Analyze Influence:", choices = top_languages)
               ),
               mainPanel(
                 h4("Overall Shared Words with Other Languages"),
                 withSpinner(plotOutput("influence_plot")),
                 hr(),
                 uiOutput("relations_summary_ui")
               )
             )),
    tabPanel("Semantic Displacement Over Time",
             sidebarLayout(
               sidebarPanel(
                 selectInput("selected_word", "Select a Word:", choices = valid_words, selected = "gay"),
                 helpText("This tab shows the semantic displacement over time for words compared to a baseline."),
                 helpText("Plot Explanation:
                          This plot shows how much the meaning of a selected word has changed over time,
                          from the earliest year available to the latest.
                          The y-axis represents how different the word’s meaning is compared to its original meaning
                          — with 0 meaning no change and 1 meaning the maximum change observed for that word.
                          A steady upward line means gradual change; sharp rises mean sudden shifts in meaning. 
                          Big  shifts during certain decades — possible new slang uses, changes in common contexts, or different cultural meanings."),
               ),
               mainPanel(
                 h4(textOutput("displacement_title")),
                 p("Note: Semantic displacement (Meaning Shift) in terms of baseline meaning 0."),
                 withSpinner(plotOutput("displacement_plot"))
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
             sidebarLayout(
               sidebarPanel(
                 helpText("Top 10 Semantic Shifts - 
This chart shows the words that have experienced the biggest changes in meaning over time.
Each bar represents how much a word’s meaning has varied,
with taller bars indicating greater shifts. These words may have acquired new senses,
                          fallen out of use, or changed in cultural significance.")
               ),  
               mainPanel(
                 withSpinner(plotOutput("top10_shift_plot"))
               )
             ))
  )
)


server <- function(input, output, session) {
  
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
  
  output$influence_plot <- renderPlot({
    req(input$focus_lang)
    counts <- related_languages_by_relation()
    
    ggplot(counts, aes(x = reorder(other_lang, shared_words), y = shared_words, fill = reltype)) +
      geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
      coord_flip() +
      labs(title = paste("Shared Words by Relation Type with", input$focus_lang),
           x = "Related Language", y = "Number of Shared Words") +
      theme_minimal()
  })
  output$relations_summary_ui <- renderUI({
    df <- related_languages_by_relation()
    req(nrow(df) > 0)
    
    reltypes <- unique(df$reltype)
    
    summaries <- lapply(reltypes, function(rel) {
      sub_df <- df %>% filter(reltype == rel) %>%
        select(Language = other_lang, `Shared Words` = shared_words)
      
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
    req(input$selected_word)
    paste("Semantic Displacement of:", input$selected_word)
  })
  
  output$displacement_plot <- renderPlot({
    req(input$selected_word)
    df <- semantic_displacement %>%
      filter(word == input$selected_word)
    
    if (nrow(df) == 0) {
      plot.new()
      text(0.5, 0.5, "No data available for this word.", cex = 1.5)
      return()
    }
    
    ggplot(df, aes(x = decade, y = displacement)) +
      geom_line(color = "pink", size = 1.5) +
      geom_point(color = "pink", size = 2) +
      labs(x = "Year", y = "Semantic Displacement", 
           title = paste("Semantic Displacement over Time for", input$selected_word)) +
      theme_minimal()
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
  output$top10_shift_plot <- renderPlot({
    filtered_data <- semantic_displacement %>%
      group_by(word) %>%
      filter(n() >= 3) %>%  # filter out words with less than 3 records
      ungroup()
    
    top10 <- filtered_data %>%
      group_by(word) %>%
      summarise(var_displacement = var(displacement, na.rm = TRUE)) %>%
      arrange(desc(var_displacement)) %>%
      head(10)
    
    ggplot(top10, aes(x = reorder(word, var_displacement), y = var_displacement)) +
      geom_bar(stat = "identity", fill = "purple") +
      coord_flip() +
      labs(title = "Top 10 Words with Highest Variance in Semantic Displacement",
           x = "Word",
           y = "Variance of Semantic Displacement") +
      theme_minimal()
  })
  
}

# Run app
shinyApp(ui = ui, server = server)