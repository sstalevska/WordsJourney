library(shiny)
library(dplyr)
library(stringr)
library(shinycssloaders)  # for loading spinners
library(visNetwork)        # for interactive network
library(ggplot2)           # for plotting in last tab
library(tidyr)             # for data manipulation

# Load processed data
dict <- readRDS("dict.rds")
etymology <- readRDS("etymology.rds")
top_languages <- readRDS("top_languages.rds")
top_roots <- readRDS("top_roots.rds")
words_relations <- readRDS("relations.rds")  # your words_relations dataset

# Extract unique languages from the Languages column in words_relations
get_unique_langs <- function(rel_col) {
  langs <- unique(unlist(strsplit(rel_col, " --> ")))
  return(langs)
}

all_langs <- unique(unlist(strsplit(words_relations$Languages, " --> ")))
all_langs <- sort(all_langs)

# Helper: prettify relation type strings 
pretty_reltype <- function(reltype) {
  reltype <- gsub("_", " ", reltype)
  if (reltype == "borrowed from") return("borrowed words from")
  if (reltype == "inherited from") return("inherited words from")
  paste0(reltype, " words from")
}

# UI
ui <- fluidPage(
  titlePanel("🌐 The Global Journey of Words"),
  
  tabsetPanel(
    id = "main_tabs",  # id added to keep track of active tab
    
    # First tab: Language Relatedness Network
    tabPanel("Language Relatedness Network",
             sidebarLayout(
               sidebarPanel(
                 helpText("Interactive network showing shared related words among top 20 languages."),
                 br(),
                 helpText(
                   strong("Network Legend:"),
                   br(),
                   "• Line thickness represents the number of shared words between two languages. Hover to see exact number.",
                   br(),
                   "• Node size indicates the total number of shared words the language has with all others (larger nodes mean greater overall connectivity)."
                 )
               ),
               mainPanel(
                 withSpinner(visNetworkOutput("language_network", height = "900px", width = "100%"))
               )
             )),
    
    # Second tab: English Words Inheritance
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
    
    # Third tab: Word Relations Translator
    tabPanel("Word Relations Translator",
             sidebarLayout(
               sidebarPanel(
                 helpText("This is a vocabulary words translator. Select origin language and target language. Then type a word to see related word pairs."),
                 
                 selectInput("origin_lang", "Select Origin Language:", choices = all_langs, selected = all_langs[1]),
                 selectInput("target_lang", "Select Target Language:", choices = all_langs, selected = all_langs[2]),
                 
                 textInput("filter_word", "Type a word to filter:", "")
               ),
               mainPanel(
                 h4("Related Word Pairs"),
                 withSpinner(tableOutput("filtered_pairs"))
               )
             )),
    
    
    # Fourth tab: Languages and Their Influences
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
             ))
  )
)

# Server
server <- function(input, output, session) {
  
  # First tab (English Words Inheritance)
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
  
  # Second tab - Word Relations Translator
  output$filtered_pairs <- renderTable({
    req(input$origin_lang, input$target_lang)
    
    # Filter by selected language pair (Languages column)
    lang_pair_str <- paste(input$origin_lang, "-->", input$target_lang)
    
    filtered <- words_relations %>%
      filter(Languages == lang_pair_str)
    
    # If filter word is provided, filter Start or End columns (case insensitive)
    if (nchar(input$filter_word) > 0) {
      word_filter <- tolower(input$filter_word)
      filtered <- filtered %>%
        filter(str_detect(tolower(Start), fixed(word_filter)) | str_detect(tolower(End), fixed(word_filter)))
    }
    
    filtered %>% select(Start, End, Languages)
  })
  
  # Third tab (now first in UI) - Language Relatedness Network
  output$language_network <- renderVisNetwork({
    # Filter etymology to top languages only (both lang and related_lang)
    ety <- etymology %>%
      filter(lang %in% top_languages, related_lang %in% top_languages) %>%
      distinct(term, lang, related_lang)
    
    # Count shared words between pairs of languages (undirected)
    # Order lang and related_lang alphabetically to treat edges as undirected
    edges_df <- ety %>%
      mutate(lang1 = pmin(lang, related_lang),
             lang2 = pmax(lang, related_lang)) %>%
      group_by(lang1, lang2) %>%
      summarise(shared_words = n_distinct(term), .groups = "drop") %>%
      filter(lang1 != lang2)
    
    # Create nodes df with total shared words (weighted degree)
    node_sizes <- edges_df %>%
      pivot_longer(cols = c(lang1, lang2), names_to = "end", values_to = "lang") %>%
      group_by(lang) %>%
      summarise(total_shared = sum(shared_words)) %>%
      ungroup()
    
    nodes <- data.frame(id = top_languages) %>%
      left_join(node_sizes, by = c("id" = "lang")) %>%
      mutate(total_shared = ifelse(is.na(total_shared), 0, total_shared),
             label = id,
             value = total_shared + 5) # +5 to avoid zero size
    
    edges <- data.frame(from = edges_df$lang1, to = edges_df$lang2,
                        width = scales::rescale(edges_df$shared_words, to = c(1, 10)),
                        title = paste0(edges_df$shared_words, " shared words"))
    
    visNetwork(nodes, edges) %>%
      visNodes(shape = "dot", scaling = list(min = 10, max = 40)) %>%
      visEdges(smooth = FALSE, color = list(color = "#97C2FC", highlight = "pink")) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visPhysics(
        stabilization = TRUE,
        solver = "forceAtlas2Based",
        forceAtlas2Based = list(
          gravitationalConstant = -50,
          centralGravity = 0.01,
          springLength = 150,
          springConstant = 0.05,
          damping = 0.4
        )
      ) %>%
      visLayout(improvedLayout = TRUE)
  })
  
  # Fourth tab - Languages and Their Influences
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
      summarise(shared_words = n_distinct(term), .groups = "drop") %>%
      arrange(reltype, desc(shared_words))
    
    counts
  })
  
  output$influence_plot <- renderPlot({
    req(input$focus_lang)
    
    ety <- etymology %>%
      filter(!is.na(lang), !is.na(related_lang)) %>%
      filter(lang == input$focus_lang | related_lang == input$focus_lang) %>%
      distinct(term, lang, related_lang)
    
    ety <- ety %>%
      mutate(other_lang = ifelse(lang == input$focus_lang, related_lang, lang)) %>%
      filter(other_lang != input$focus_lang) %>%
      filter(other_lang %in% top_languages)
    
    counts <- ety %>%
      group_by(other_lang) %>%
      summarise(shared_words = n_distinct(term), .groups = "drop") %>%
      slice_max(order_by = shared_words, n = 20)
    
    ggplot(counts, aes(x = reorder(other_lang, shared_words), y = shared_words)) +
      geom_col(fill = "purple") +
      coord_flip() +
      labs(x = "Language", y = "Number of Shared Words", 
           title = paste("Shared Words with", input$focus_lang)) +
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
  
}

# Run app
shinyApp(ui = ui, server = server)
