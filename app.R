library(tidyverse)
library(shiny)
library(shinyjs)
library(pool)
library(yaml)

# Connection info is stored in dbconfig.yml (not in public repo) for security
dbconfig <- yaml::read_yaml("dbconfig.yml")
pool <- dbPool(
  drv = RMySQL::MySQL(),
  dbname = dbconfig$dbname,
  host = dbconfig$host,
  username = dbconfig$username,
  password = dbconfig$password
)
onStop(function() {
  poolClose(pool)
})

#
# Define the various judging groups
#
# - target_judges is the number of judges we seek in each group
# - min_per_judge is the number of judgements needed for a judge to count toward the number of judges
# studies <- read_csv("study-prompts.csv") %>% 
#   mutate(
#     target_judges = 25,
#     min_per_judge = 10
#   )
studies <- pool %>% 
  tbl("studies") %>% 
  filter(str_detect(study, "experts_|phd_|other_")) %>% 
  collect()

#
# Check on judging progress
#
all_existing_judgements <- pool %>% 
  tbl("judgements") %>% 
  collect() %>% 
  semi_join(studies, by = "study")

study_progress <- all_existing_judgements %>% 
  group_by(study, judge_id) %>% 
  tally() %>% 
  left_join(studies, by = "study") %>% 
  filter(n >= min_per_judge) %>% 
  group_by(study) %>% 
  summarise(
    num_judges = n_distinct(judge_id),
    num_judgements = sum(n)
  )

study_status <- studies %>% 
  left_join(study_progress, by = "study") %>% 
  mutate(across(starts_with("num_"), ~replace_na(.x, 0)))

assign_to_study <- function(study_group) {
  # allocate to one of the studies, weighted by current progress
  study_status %>%
    filter(str_detect(study, study_group)) %>% 
    # mutate(num_judges = c(1, 25, 25, 25, 25)) %>% #simulate unbalanced completion
    # identify the number of judges needed by each study to meet its target
    mutate(judge_slots = target_judges - num_judges) %>%
    # make sure there's always a small chance of being allocated to a study that has met it target
    # (so once all targets are met, each study has an equal chance of being picked)
    mutate(judge_slots = pmax(judge_slots, 0.1)) %>%
    sample_n(1, weight = judge_slots) %>%
    #pull(study_id)
    unlist()
}

# Check that the assignment is working the way it should
# simulate_assignment <- function(study_group) {
#   assigned_to <- assign_to_study(study_group)[["study"]]
#   study_status <<- study_status %>% 
#     mutate(num_judges = case_when(study == assigned_to ~ num_judges + 1, TRUE ~ num_judges))
#   return(assigned_to)
# }
# assignments_test <- tibble(iter = c(1:1)) %>%
#   mutate(study = map_chr(iter, ~ simulate_assignment("phd")))
# study_status %>% 
#   filter(str_detect(study, "phd")) %>% 
#   select(study, num_judges)

proofs <- read_yaml("proofs.yml") %>%
  purrr::map(as_tibble_row) %>%
  enframe(name = NULL) %>%
  unnest(cols = c("value")) %>% 
  rename_with(~ str_replace(., "-", "_")) %>%
  rename(markdown = html) %>% 
  mutate(html = purrr::map(markdown, ~ markdown::markdownToHTML(
    text = .,
    fragment.only = TRUE
  ))) 




ui <- fluidPage(
  useShinyjs(),
  withMathJax(),
  
  tags$head(
    # Custom CSS
    tags$style(HTML("
      /* body {padding-top: 50px}  for the boostrap nav */
      /*ul.nav-pills {margin-top: 5px}*/
      @media (max-width: 768px) { .navbar-nav {float: left; margin: 5px; } }
      /*.navbar-nav {float: left; margin: 5px; }*/
      .navbar-text {float:left; margin-left:15px; }
      .navbar-right {float:right; margin-right:15px; }
      #demographics .shiny-input-container { width: auto; clear: both; }
      #demographics .shiny-options-group { display: block; float: left; }
      #demographics .control-label { float: left; width: 12em; text-align: right; margin-right: 1em; }
      div.item_panel { padding: 1em 2em; border: 1px solid #ccc; box-shadow: 3px 4px 15px 0px #0000002b; overflow: auto;}
      div.item_content {margin-top: 1em; }
      .comparison-image { width: 100%; }
    "))
  ),
  
  # Navbar
  #tags$div(class = "navbar navbar-default navbar-fixed-top",
  tags$div(class = "navbar navbar-default", style = "margin: -2px -15px",
           #tags$p(class = "navbar-text", id = "tab0", "Comparing proofs"),
           tags$p(class = "navbar-text", id = "tab0", actionLink("help", label = "Comparing proofs")),
           tags$ul(class = "nav navbar-nav nav-pills",
                   tags$li(role = "presentation", class = "disabled", id = "tab1",
                           tags$a(href = "#", "Step 1")),
                   tags$li(role = "presentation", class = "disabled", id = "tab2",
                           tags$a(href = "#", "Step 2")),
                   tags$li(role = "presentation", class = "disabled", id = "tab3",
                           tags$a(href = "#", "Step 3"))
           ),
           # tags$ul(class = "nav navbar-nav navbar-right",
           #         tags$li(role = "presentation", id = "help", tags$a(href = "#", icon("question-circle"))))
           # tags$ul(class = "nav navbar-nav navbar-right",
           #         tags$li(role = "presentation", id = "help", actionLink("help", label = icon("question-circle"))))
  ),
  # Version of the navbar done with pills
  # fluidRow(
  #   column(12, 
  #          tags$ul(class = "nav nav-pills",
  #                  tags$li(role = "presentation", class = "disabled", id = "tab0",
  #                          tags$a(href = "", tags$strong("Comparing proofs"))),
  #                  tags$li(role = "presentation", class = "disabled", id = "tab1",
  #                          tags$a(href = "#", "Step 1")),
  #                  tags$li(role = "presentation", class = "disabled", id = "tab2",
  #                          tags$a(href = "#", "Step 2")),
  #                  tags$li(role = "presentation", class = "disabled", id = "tab3",
  #                          tags$a(href = "#", "Step 3"))
  #           )
  #   )
  # ),
  tags$div(class = "clearfix"),
  
  # Placeholder for page content - the server will update this as needed
  uiOutput("pageContent")
)



server <- function(input, output, session) {
  
  # These will be global variables within each session
  assigned_study <- NULL
  session_info <- NULL
  judge_id <- NULL
  judge_group <- NULL
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['g']])) {
      judge_group <<- query[['g']]
    } else {
      judge_group <<- "None"
    }
  })
  
  all_proofs_html = proofs %>%
    mutate(html_out = purrr::map(html, ~ markdown::markdownToHTML(
      text = .,
      fragment.only = TRUE
    ))) %>% 
    mutate(html_out = paste0("<h2>Proof ", item_num, "</h2>", html_out))
    
  #
  # Page 0 - consent form
  #
  output$pageContent <- renderUI({
    tagList(
      #p(paste("Judge group:", judge_group)),
      includeMarkdown("step0-participant-info.md"),
      #proofs %>% select(item_num) %>% display_item(),
      #paste(all_proofs_html$html_out, collapse = "") %>% HTML() %>% withMathJax(),
      fluidRow(
        column(4, offset = 4, actionButton("consentButton", "I consent", class = "btn-success btn-lg btn-block", icon = icon("check")))
      )
    )
  })
  
  #
  # Page 1 - demographic survey
  #
  observeEvent(input$consentButton, {

    # Set up the UI
    
    ## update the nav
    shinyjs::addClass(id = "tab1", class = "active")
    shinyjs::removeClass(id = "tab1", class = "disabled")
    
    ## update the page content
    output$pageContent <- renderUI({
      tagList(
        h3("About you"),
        p("For this survey we are seeking input from research-active mathematicians."),
        div(id = "demographics",
          radioButtons("demo_level", "Your current role",
                       selected = character(0), # none selected initially https://stackoverflow.com/q/39535980
                       c("Research student" = "phd",
                         "Post-doctoral researcher" = "postdoc",
                         "Faculty (lecturer/professor)" = "faculty",
                         "Other" = "other")),
          radioButtons("demo_field", "Your research area",
                       selected = character(0),
                       c("Applied mathematics" = "applied",
                         "Pure mathematics" = "pure",
                         "Statistics" = "stats"))
        ),
        div("", class = "clearfix"),
        #actionButton("step1submit", "Submit", class = "btn-success")
        fluidRow(
          column(4, offset = 4,
                 actionButton("step1submit", "Submit answers",
                              class = "btn-success btn-lg btn-block",
                              icon = icon("check")) %>% disabled()
                 )
        )
      )
    })
    
    # Check mandatory fields are filled before allowing submit
    # https://deanattali.com/2015/06/14/mimicking-google-form-shiny/#define-mandatory
    fieldsMandatory <- c("demo_level", "demo_field")
    observe({
      mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      
      shinyjs::toggleState(id = "step1submit", condition = mandatoryFilled)
    })
  })
  
  
  #
  # Page 2 - definition prompt
  #
  observeEvent(input$step1submit, {
    
    # Now they have consented, assign them to a condition matching their selected demographic
    study_group <- case_when(
      input$demo_level %in% c("postdoc", "faculty") ~ "experts_",
      input$demo_level == "phd" ~ "phd_",
      TRUE ~ "other_"
    )
    assigned_study <<- assign_to_study(study_group)
    
    # Create session_info and synch with the judges table in the database
    ## 1. Write session info to the database
    session_info <<- tibble(
      shiny_info = session$token,
      shiny_timestamp = as.character(Sys.time()),
      study_id = assigned_study[["study"]],
      judge_group = judge_group,
      demo_level = input$demo_level,
      demo_field = input$demo_field
    )
    dbWriteTable(pool,
                 "judges",
                 session_info,
                 row.names = FALSE,
                 append = TRUE)
    
    ## 2. Update session_info to include the autoincremented judge_id produced by the database
    session_info <<- pool %>% tbl("judges") %>%
      filter(shiny_info == !!session_info$shiny_info) %>%
      arrange(-judge_id) %>%
      collect() %>%
      slice(1)
    
    ## 3. Pick out the judge_id for ease of reference later on
    judge_id <<- session_info$judge_id
    print(judge_id)
    
    # update the nav
    shinyjs::addClass(id = "tab1", class = "disabled")
    shinyjs::removeClass(id = "tab1", class = "active")
    shinyjs::addClass(id = "tab2", class = "active")
    shinyjs::removeClass(id = "tab2", class = "disabled")
    
    # update the page content
    output$pageContent <- renderUI({
      tagList(
        h3("Judging proofs"),
        p(assigned_study[["definition_prompt"]]),
        textAreaInput("definition", label = NULL, width = "50em", height = "10em"),
        #actionButton("step2submit", "Submit", class = "btn-success")
        fluidRow(
          column(4, offset = 4, actionButton("step2submit", "Submit", class = "btn-success btn-lg btn-block", icon = icon("check")))
        )
      )
    })
  })
  
  #
  # Page 3 - judging instructions
  #
  observeEvent(input$step2submit, {
    # save the values input on Page 2
    conn <- poolCheckout(pool)
    query <- glue::glue_sql("UPDATE `judges` SET
                          `definition` = {input$definition}
                          WHERE `judge_id` = {session_info$judge_id}
                          ", .con = conn)
    #print(query)
    dbExecute(conn, sqlInterpolate(DBI::ANSI(), query))
    poolReturn(conn)
    
    # update the nav
    shinyjs::addClass(id = "tab2", class = "disabled")
    shinyjs::removeClass(id = "tab2", class = "active")
    shinyjs::addClass(id = "tab3", class = "active")
    shinyjs::removeClass(id = "tab3", class = "disabled")
    
    raw_md <- read_file("step3-judging-instructions.md")
    
    # update the page content
    output$pageContent <- renderUI({
      tagList(
        h3("Instructions"),
        markdown::markdownToHTML(text = raw_md, fragment.only = TRUE) %>%
          str_replace("\\[JUDGING PROMPT\\]", assigned_study[["judging_prompt"]]) %>% HTML() %>% withMathJax(),
        #actionButton("step3submit", "Start judging!", class = "btn-success")
        fluidRow(
          column(4, offset = 4, actionButton("step3submit", "Start judging!", class = "btn-success btn-lg btn-block", icon = icon("check")))
        )
      )
    })
  })
  
  #
  # Judging
  #
  
  # Set up pairs for judging - this is done at the start of the session
  # Produce pairs by sliding along a randomly shuffled list of the items,
  # i.e. moving from one pair to the next, one of the items stays on
  pairs <- sample_n(tibble(left = proofs$item_num), 100, replace = TRUE) %>% 
    # Avoid showing AA, or AB followed by BA
    filter(left != lead(left)) %>% 
    filter(left != lead(left)) %>% 
    filter(left != lead(left, n = 2)) %>% 
    filter(left != lead(left, n = 2)) %>% 
    mutate(right = lead(left)) %>% 
    drop_na() %>%
    mutate(pair_num = row_number(), .before = 1) %>% 
    mutate(comparisons = 0)
  
  first_pair = pairs %>% head(1)
  pair <- reactiveValues(
    pair_num = first_pair$pair_num,
    left = first_pair$left,
    right = first_pair$right
  )
  
  observeEvent(input$step3submit, {
    #proofs <- pool %>% tbl("proofs") %>% collect() %>% mutate(proof = as.character(proof))

    # update the page content
    output$pageContent <- renderUI({
      tagList(
        h3(assigned_study[["judging_prompt"]]),
        htmlOutput("judging_progress"),
        fluidRow(
          column(6, htmlOutput("item_left")),
          column(6, htmlOutput("item_right"))
        )
      )
    })
    
    pair$start_time = Sys.time()
    print("Judging intialised")
    #print(pair)
  })
  
  display_item <- function(item_id) {
    the_item <- proofs %>% filter(item_num == item_id)
    if(str_length(the_item$html %>% as.character()) > 0) {
      return(the_item$html %>% as.character() %>% HTML() %>% withMathJax())
    } else {
      return(img(src = the_item$img_src, class = "comparison-image"))
    }
  }
  render_item_panel <- function(button_id, item_id) {
    tagList(
      div(class = "item_panel",
          fluidRow(
            actionButton(button_id, "Choose this one", class = "btn-block btn-primary")
          ),
          div(class = "item_content", display_item(item_id))
      )
    )
  }
  
  output$item_left <- renderUI({
    render_item_panel("chooseLeft", pair$left)
  })
  output$item_right <- renderUI({
    render_item_panel("chooseRight", pair$right)
  })
  
  output$judging_progress <- renderPrint({
    pc <- round((pair$pair_num -1) / 20 * 100)
    pc <- min(pc, 100)
    # https://getbootstrap.com/docs/3.4/components/#progress
    div(
      class = "progress",
      div(
        class = ifelse(pc < 100, "progress-bar", "progress-bar progress-bar-success"),
        role = "progressbar",
        `aria-valuenow` = pc,
        `aria-valuemin` = 0,
        `aria-valuemax` = 100,
        style = str_glue("min-width: 1em; width: {pc}%;"),
        pair$pair_num - 1
      )
    )
  })
  
  next_pair = function(old_pair_num) {
    pair_to_return = ifelse(old_pair_num < nrow(pairs), old_pair_num + 1, 1)
    pairs %>% 
      filter(pair_num == pair_to_return) %>%
      head(1)
  }
  update_pair <- function() {
    new_pair <- next_pair(pair$pair_num)
    #print(paste("update pair to ", new_pair))
    pair$pair_num <- new_pair$pair_num
    pair$left <- new_pair$left
    pair$right <- new_pair$right
    pair$start_time <- Sys.time()
  }
  
  record_judgement <- function(pair, winner = "left", loser = "right") {
    #print(pair)
    start_time <- pair$start_time
    current_time <- Sys.time()
    #print(start_time)
    #print(current_time)
    time_taken = as.integer((current_time - start_time) * 1000)
    
    winning_item = ifelse(winner == "left", pair$left, pair$right)
    losing_item = ifelse(loser == "left", pair$left, pair$right)
    
    dbWriteTable(
      pool,
      "judgements",
      tibble(
        study = session_info$study_id,
        judge_id = session_info$judge_id,
        left = pair$left,
        right = pair$right,
        won = winning_item,
        lost = losing_item,
        time_taken = time_taken
      ),
      row.names = FALSE,
      append = TRUE
    )
  }
  observeEvent(input$chooseLeft, {
    record_judgement(pair, winner = "left", loser = "right")
    update_pair()
  })
  observeEvent(input$chooseRight, {
    record_judgement(pair, winner = "right", loser = "left")
    update_pair()
  })
  
  # Give a message when they reach the required number of comparisons
  observe({
    if (pair$pair_num != 21) return()
    showModal(modalDialog(
      title = "Thank you!",
      p("You have now completed the 20 comparisons needed for the survey."),
      p("You can continue making further comparisons if you wish, and these will continue to be recorded."),
      p("When you are ready to stop, please simply close this browser window."),
      p("Thank you for taking part."),
      easyClose = TRUE
    ))
  })
  observeEvent(input$help, {
    showModal(modalDialog(
      title = "About this site",
      p("This study is being run by Prof Chris Sangwin and Dr George Kinnear,
        from the School of Mathematics at the University of Edinburgh."),
      p(HTML("If you have any questions about the study, please contact Chris:
        <a href=\"mailto:C.J.Sangwin@ed.ac.uk\">C.J.Sangwin@ed.ac.uk</a>.")),
      easyClose = TRUE
    ))
  })
  
}

shinyApp(ui, server)