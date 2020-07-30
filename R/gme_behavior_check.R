

#' @export
gme_behavior_check <- function() {
  library(shiny)
  library(miniUI)
  library(shinyjs)


 # Harcoded stuff ----
  disposition_text_lookup <- function(num){
    if(num <= 5) return('Passive')
    if(num > 5 & num < 11) return('Moderate')
    if(num >= 1 & num < 16) return('Active')
    return('Aggresive')
  }
  action_table2 <- function(num){
    if(num <= 5) return('Talks, exposition')
    if(num %in% c(7,8)) return('Performs an ambiguous action')
    if(num %in% c(9, 10)) return('Acts out of pc interest')
    if(num == 11) return('Gives something')
    if(num == 12) return('Seeks to end the encounter')
    if(num == 13) return('Changes the theme')
    if(num == 14) return('Changes descriptor')
    if(num %in% c(15,16,17)) return('Acts out of self interest')
    if(num == 18) return('Takes something')
    return('Causes harm')
  }

  # UI ----
  ui <- miniPage(
    shinyjs::useShinyjs(),
    gadgetTitleBar("MV 2 Behavior Check"),
    miniContentPanel(
      # Descriptors ----
      splitLayout(textInput(inputId = 'identity',
                    label = 'Identity',
                    value = "Unknown"),
               checkboxInput(inputId = 'identity_box',
                             label = 'Active?'),
               radioButtons(inputId = 'identity_positive',
                            label = 'Type?',
                            choices = c('Positive', 'Negative')),
               cellWidths = c('50%', '25%', '25%')),
      splitLayout(textInput(inputId = 'personality',
                                label = 'Personality',
                                value = "Unknown"),
                  checkboxInput(inputId = 'personality_box',
                                label = 'Active?'),
                  radioButtons(inputId = 'personality_positive',
                               label = 'Type?',
                               choices = c('Positive', 'Negative')),
                  cellWidths = c('50%', '25%', '25%')),
      splitLayout(textInput(inputId = 'Activity',
                                label = 'Activity',
                                value = "Unknown"),
                  checkboxInput(inputId = 'activity_box',
                                label = 'Active?'),
                  radioButtons(inputId = 'activity_positive',
                               label = 'Type?',
                               choices = c('Positive', 'Negative')),
                  cellWidths = c('50%', '25%', '25%')),
      # Disposition and theme ----
      splitLayout(
        textInput(inputId = 'theme', label = 'Theme'),
        textOutput(outputId = 'disposition_text'),
        cellWidths = c('75%', '25%')),
      # Buttons ----
      splitLayout(
        actionButton(inputId = 'roll_action',
                     label = 'Roll Action'),
        actionButton('increase_disposition',
                     '+2 Disposition'),
        actionButton('decrease_disposition',
                     '-2 Disposition'),
        actionButton(inputId = 'roll_disposition',
                     label = 'Roll Disposition')
      ),
      # Action area ----
      textOutput(outputId = 'action')
    )
  )

  # Server ----
  server <- function(input, output, session) {

    values <- reactiveValues(disposition = NA,
                             action1 = NA)
    # Enable / disable trait modifiers ----
    observeEvent(input$identity_box, {
      if(!input$identity_box){
        shinyjs::disable('identity_positive')
      }else{
        shinyjs::enable('identity_positive')
      }
    })
    observeEvent(input$activity_box, {
      if(!input$activity_box){
        shinyjs::disable('activity_positive')
      }else{
        shinyjs::enable('activity_positive')
      }
    })
    observeEvent(input$personality_box, {
      if(!input$personality_box){
        shinyjs::disable('personality_positive')
      }else{
        shinyjs::enable('personality_positive')
      }
    })

    # Disposition ----
     output$disposition_text <- renderText({
       if(is.na(values$disposition)){
         return('Please roll disposition')
       }else{
         paste(values$disposition,
               disposition_text_lookup(values$disposition),
               sep = ' - ')
       }
     })

    observeEvent(input$roll_disposition, {
      mod <- 0
      if(input$identity_box){
        if(input$identity_positive == 'Positive'){
          mod <- mod + 2
        }else{
          mod <- mod - 2
        }
      }
      if(input$personality_box){
        if(input$personality_positive == 'Positive'){
          mod <- mod + 2
        }else{
          mod <- mod - 2
        }
      }
      if(input$activity_box){
        if(input$activity_positive == 'Positive'){
          mod <- mod + 2
        }else{
          mod <- mod - 2
        }
      }
      values$disposition <- sum(sample(1:10, 2, T)) + mod
    })

    observeEvent(input$increase_disposition,{
      values$disposition <- values$disposition +2
    })

    observeEvent(input$decrease_disposition,{
      values$disposition <- values$disposition -2
    })

    # Roll Action ----
    observeEvent(input$roll_action, {
      values$action1 <- sample(c(rep('Theme Action', times = 3),
                        rep('NPC Continues', times = 2),
                        'NPC Continues +2',
                        'NPC Continues -2',
                        'NPC Action',
                        'NPC Action -4',
                        'NPC Action +4'),
                        size = 1)
    })

    output$action <- renderText({
      if(is.na(values$action1)){
        return(NULL)
      }else{
        final_action <- values$action1
        if(grepl('NPC Action', final_action)){
          if(grepl('+4', final_action)) mod <- 4
          if(grepl('+-4', final_action)){
            mod <- -4
          } else{
            mod <- 0
          }
          if(disposition_text_lookup(values$disposition) == 'Passive') mod <- mod -2
          if(disposition_text_lookup(values$disposition) == 'Active') mod <- mod +2
          if(disposition_text_lookup(values$disposition) == 'Aggressive') mod <- mod +4
          final_roll <- sum(sample(1:10, 2, T)) + mod
          final_action <- paste0(final_action,
                                 ' (',
                                 action_table2(final_roll),
                                 ')')
        }
        return(final_action)
      }
    })

    # Gadget stuff ----

    # Handle the Done button being pressed.
    observeEvent(input$done, {
      stopApp(returnValue = NULL)
    })
  }

  #runGadget(ui, server, viewer = dialogViewer('Behavior check'))
  runGadget(ui, server)
}