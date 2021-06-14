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
studies <- pool %>% 
  tbl("studies") %>% 
  filter(str_detect(study, "experts_|phd_|other_")) %>% 
  collect()

#
# Check on judging progress
#
all_existing_judgements <- pool %>% 
  tbl("judgements") %>% 
  select(-contains("_comment")) %>% 
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
      div#pageContent { margin-bottom: 2em; }
      #demographics .shiny-input-container { width: auto; clear: both; }
      #demographics .shiny-options-group { display: block; float: left; }
      #demographics .control-label { float: left; width: 12em; text-align: right; margin-right: 1em; }
      div.item_panel { padding: 1em 2em; border: 1px solid #ccc; box-shadow: 3px 4px 15px 0px #0000002b; overflow: auto;}
      div.item_content {margin-top: 1em; }
      #chooseLeft_comment-label, #chooseRight_comment-label { color: #999; font-weight: normal; font-style: italic; margin-top: 1em; }
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
                       c("PhD student" = "phd",
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
  
  make_pairs <- function(num_pairs = 20) {
    
    # 1. Gather data on which judgements have been made already in this study group

    # all comparisons from this study
    judgement_data <- pool %>% 
      tbl("judgements") %>% 
      filter(study == !!session_info$study_id) %>% 
      select(-contains("_comment")) %>% 
      collect() %>% 
      mutate(across(c("left", "right", "won", "lost"), as.integer))

    # count the number of comparisons for each pair
    pairs_judged <- judgement_data %>%
      rowwise() %>%
      mutate(pair = paste(sort(c(won, lost)), collapse = '_')) %>% 
      select(pair, judge_id) %>% 
      group_by(pair) %>% 
      tally() %>% 
      arrange(-n) %>% 
      separate(pair, c("s1", "s2"), "_") %>% 
      mutate(across(c("s1", "s2"), as.integer))
    
    # systematically list all pairs, and add the counts for each
    all_pairs_status <-
      crossing(proofs %>% select(s1 = item_num),
               proofs %>% select(s2 = item_num)) %>%
      filter(s1 < s2) %>%
      left_join(pairs_judged, by = c("s1", "s2")) %>%
      mutate(n = replace_na(n, 0))
    
    # count the number of times each individual script has been judged
    scripts_judged <- proofs %>% select(script = item_num) %>% 
      left_join(
        judgement_data %>%
        pivot_longer(cols = c(won, lost),
                     names_to = "position",
                     values_to = "script") %>%
        group_by(script) %>%
        tally(),
        by = "script"
      ) %>%
      mutate(n = replace_na(n, 0))
    judgements_per_script <- scripts_judged %>% deframe()
    
    # 2. Start off with the least judged script so far
    script_seq <- list()
    # choose the first script to be one of the least judged so far
    script_seq[[1]] <-
      scripts_judged %>%
      filter(n == min(n)) %>%
      sample_n(size = 1) %>%
      select(script) %>% deframe()
    
    # 3. Find successive pairings
    for (i in c(2:(num_pairs*2 + 1))) {
      player <- script_seq[[i - 1]]
      # choose one of the pairs featuring this script_id that have the least judgements so far
      pair <- all_pairs_status %>%
        filter(s1 == player | s2 == player) %>%
        filter(n == min(n)) %>%
        # extract the opponent
        mutate(script = if_else(s1 == player, s2, s1)) %>% 
        # attach information about how frequently the opponents have been judged
        left_join(
          judgements_per_script %>%
            enframe(value = "comparisons") %>%
            mutate_all(as.integer) %>%
            mutate(weight = max(comparisons) + 1 - comparisons),
          by = c("script" = "name")
        ) %>%
        # sample the opponent at random, but weighted by the number of comparisons,
        # so we're more likely to pair with an opponent that has not been paired with this script so far
        sample_n(size = 1, weight = weight)
      opponent <- pair$script
      script_seq[[i]] <- opponent
      # record this pair on the tally, incrementing the count by 100 so this judge does not see it for a long time!
      pairs_judged <- pairs_judged %>%
        mutate(n = case_when(s1 == player & s2 == opponent ~ n + 100L,
                             s2 == player & s1 == opponent ~ n + 100L,
                             TRUE ~ n))
      # also record it as a judgement on the per_script tally
      judgements_per_script[[opponent]] <- judgements_per_script[[opponent]] + 1
    }
    
    # 4. Return the pairings as a dataframe
    return(
      script_seq %>% enframe() %>%
        # set up the left and right scripts
        mutate(left = as.integer(value)) %>%
        mutate(right = lead(left)) %>%
        select(left, right) %>%
        # we discard every 2nd pair, otherwise we have "right-hand script stays on"
        filter(row_number() %% 2 == 1) %>% 
        # return only the desired number of pairings (chop of the last one that has right = NA)
        slice_head(n = num_pairs) %>%
        mutate(pair_num = row_number(), .before = 1)
    )
  }
  next_pair = function(old_pair_num) {
    print("next_pair")
    print(old_pair_num)
    print(nrow(pairs))
    # move on to the next pair
    pair_to_return = old_pair_num + 1
    
    # if we've reached the end, add 10 more pairs to the list
    if(pair_to_return > nrow(pairs)) {
      pairs <<- pairs %>% bind_rows(make_pairs(num_pairs = 10) %>% mutate(pair_num = pair_num + old_pair_num))
      pair$pairs_available <- nrow(pairs)
      print(pairs)
    }
    pairs %>% 
      filter(pair_num == pair_to_return) %>%
      head(1)
  }
  
  # initialise empty data structures, to be used when judging begins
  pair <- reactiveValues(
    pair_num = 0,
    pairs_available = 0,
    left = 1000,
    right = 1001
  )
  pairs <- tibble()
  
  observeEvent(input$step3submit, {
    
    pairs <<- make_pairs(num_pairs = 20)
    print(pairs)
    pair$pairs_available <- nrow(pairs)
  
    first_pair = pairs %>% head(1)
    pair$pair_num <- first_pair$pair_num
    pair$left <- first_pair$left
    pair$right <- first_pair$right

    print(pair)
    print("OK")

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
    print("Judging initialised")
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
          div(class = "item_content", display_item(item_id)),
          fluidRow(
            textAreaInput(paste0(button_id, "_comment"), label = "Comments (optional)", width = "100%", height = "4em")
          )
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
  
  update_pair <- function() {
    new_pair <- next_pair(pair$pair_num)
    print(new_pair)
    pair$pair_num <- new_pair$pair_num
    pair$left <- new_pair$left
    pair$right <- new_pair$right
    pair$start_time <- Sys.time()
  }
  
  record_judgement <- function(pair, winner = "left", loser = "right") {
    print(pair)
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
        time_taken = time_taken,
        left_comment = input$chooseLeft_comment,
        right_comment = input$chooseRight_comment
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
      p("If you would like to receive an update about the results of the study, please complete this separate form:"),
      p(a("https://edinburgh.onlinesurveys.ac.uk/expert-opinions-about-proofs",
          href="https://edinburgh.onlinesurveys.ac.uk/expert-opinions-about-proofs"), style = "text-align: center;"),
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
