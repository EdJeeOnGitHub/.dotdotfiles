library(shiny)
source("arm-proportions.R")
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  # App title ----
  titlePanel("MDE Proportions"),
  withMathJax(),
    tags$div(HTML("<script type='text/x-mathjax-config'>
        MathJax.Hub.Config({
            TeX: {
  extensions: ['AMSmath.js', 'AMSsymbols.js', 'bm.js']
},
        tex2jax: {inlineMath: [['$','$']]}
        });
        </script>
        ")),


  # Sidebar layout with input and output definitions ----
  sidebarLayout(


    # Main panel for displaying outputs ----
    mainPanel(
        h1("Calculating Optimal Treatment Arm Proportions"),
p("
       Let's start with the familiar MDE formula: 
       $$
        MDE_\\kappa = \\sqrt{1 + \\rho ( m - 1)}(t_{1 - \\kappa} + t_{\\alpha/2}) \\frac{\\sigma}{\\sqrt{N}} \\left(\\frac{1}{p_c}  + \\frac{1}{p_t} \\right)^\\frac{1}{2}
       $$
       
       Where the first term is the ICC, $\\rho$, correction with average cluster size $m$, the second term is the $t$-stats corresponding to power and size respectively, and the final terms are a function 
       of outcome variance and treatment proportion. Since we're dealing with binary data we know the variance will just be $\\mu(1 - \\mu) = 0.3^2$ given a 
       baseline takeup of 10%."),
       p("
       We have treatment arms: 
       "),
tags$ol(
    tags$li("Respondents are told by enumerators at the end of the survey that a nurse is coming to offer them the COVID-19 vaccine – the nurse visits the household shortly after the end of the survey to offer the vaccine. (Nurse Announced)"), 
    tags$li(
         "Respondents are not told that the nurse is coming  – the nurse visits the household shortly after the end of the survey to offer the vaccine. (Nurse Unannounced)"

    ), 
    tags$li(

         "Respondents are not told that the nurse is coming – the nurse visits the household shortly after the end of the survey to offer a small gift and the vaccine. (Nurse Unannounced + Gift)"
    ),
    tags$li(

        "Enumerators inform the respondent of their local vaccination opportunities. (No Nurse Visit)"
    )
),
 p("and four hypotheses:"),

tags$ol(
    tags$li(
"$T1 = T2$ Do respondents try to avoid the nurse when informed ahead of time? Avoidance behavior will provide evidence that hypothesized increases in vaccine uptake from nurse door-to-door vaccine delivery operates through social influence channels."
        ), 
    tags$li(
        " $T2 = T3$ The role of reciprocity through gift exchange - REMOVED"

    ), 
    tags$li(
        " $T1/T2 = T4$ The role of social influence from the nurse"

    ),
    tags$li(
"$T3 = T4$ The role of social influence from the nurse and reciprocity - REMOVED"
    )
),

    p("Intuitively, we probably want more units in treatment 2 as it appears in 
    more hypotheses than any other - 3 versus 2. However, it's not quite as simple 
    as just doubling the units in arm 2. This is because the MDE formula isn't a 
    linear function of $p_c, p_t$ but instead concave. Furthermore, we probably 
    don't 'value' each hypothesis equally - we might care more about a lower 
    MDE for $H4$ as opposed to $H1$ say. Fortunately, this a problem we 
    know how to solve well:

    \\begin{align}
    argmin_{p_1, p_2, p_3, p_4} &u(MDE(\\boldsymbol{p}; X)_1, MDE(\\boldsymbol{p}; X)_2, MDE( \\boldsymbol{p}; X)_3, MDE(\\boldsymbol{p}; X)_4) \\\\\\
    s.t. \\ &0 < p_i < 1, \\forall i \\\\\\
    &\\sum^4_{i=1} p_i = 1
    \\end{align} 
    
        "),
    p("
    If we knew $u(\\cdot)$ we could solve this optimisation problem. One way to 
    learn $u(\\cdot)$ would be to offer two pairs of MDE vectors and let the 
    decision maker choose - then we could estimate a discrete choice model using 
    multinomial logit/some non-parametric model. Alternatively, I just assume 
    utility is either some weighted mean of MDEs or Cobb-Douglas (effectively a 
    geometric mean). This gives two utility functions:


    \\begin{align}
    U_{lin} &= \\gamma_1 MDE_1 + \\gamma_2 MDE_2 + \\gamma_3 MDE_3 + \\gamma_4 MDE_4   \\\\
    U_{log} &= MDE_1^{\\gamma_1} MDE_2^{\\gamma_2} MDE_3^{\\gamma_3}  MDE_4^{\\gamma_4}
    \\end{align} 

    With equal weights, the first effectively treats MDEs as perfect substitutes 
    across hypotheses whereas the second 'overweights' higher MDEs - the marginal 
    value of decreasing a high MDE is greater than an already low MDE. It's 
    worth noting that both these functional forms are homogeneous of degree 1 
    since I enforce $\\sum^4_{i=1} \\gamma_i = 1$ - 
    anything we do that scales all the MDEs equally, such as the intra-cluster 
    correlation, won't have any effect on treatment proportions.



    "),
    p("The advantage of setting up the problem this way is that we're always 'on 
    our budget constraint' - we always choose treatment proportions that equate 
    $MB = MC$ and so we aren't missing any assignments that could increase 
    power without losing power somewhere else."),
    ),


    fluidRow(
        column(4, 
        
        h1("Power Options"),
      numericInput(inputId = "N_total", 
                 label = "Total Sample Size", 
                 value = 2250,
                 min = 100,
                 max = NA,
                 step = 200),
        selectInput(inputId = "utility", 
                    label = "Utility Function", 
                    choices = c("linear", "log"), 
                    selected = "linear"),

        selectInput(inputId = "multiple", 
                    label = "Multiple Hypothesis Adjustments", 
                    choices = c("None", "Bonferroni"), 
                    selected = "None"),
      numericInput(inputId = "m", 
                 label = "Average Cluster Size", 
                 value = 8,
                 step = 1),
        sliderInput(inputId = "rho", 
                     label = "ICC", 
                     min = 0, max = 1, value = 0),
      # Input: Slider for the number of bins ----
      h2("Hypothesis Weights"),
      sliderInput(inputId = "weight_1",
                  label = "Weight on H1",
                  min = 1,
                  max = 10,
                  value = 5),
    #   sliderInput(inputId = "weight_2",
    #               label = "Weight on H2",
    #               min = 1,
    #               max = 10,
    #               value = 5),
      sliderInput(inputId = "weight_3",
                  label = "Weight on H3",
                  min = 1,
                  max = 10,
                  value = 5),
    #   sliderInput(inputId = "weight_4",
    #               label = "Weight on H4",
    #               min = 1,
    #               max = 10,
    #               value = 5)
        
        
        
        )
    )


      # Output: Histogram ----
    ),
    fluidRow(
        column(8,
        align = "center",
        status = "primary",

        tableOutput(
            outputId = "mde_df"
        ),

    ),
    fluidRow(
        column(8,

        p("I think the intuition that $H1$ has such a higher MDE is like so: 
        Because $T2$ 'triple dips', we get value from it across 3 hypotheses, but 
        $T1$ is only ever used either against or with $T2$ so we really don't want to 
        put much sample here. This is because we've already put a lot of sample on 
        $T2$ and so the marginal value of sample in $T1$ is much lower than in 
        $T3/4$. We can see this really clearly if we start from even weights and 
        move $H1$'s weight from 5 to 10. The $H4$ MDE increases from 4.89 to 5.24, 
        on the other hand, say we overweight $H2$ which doesn't contain $T1$ - 
        $H4$'s MDE only increases to 4.91")
    )
  )
    ))

# Define server logic required to draw a histogram ----
server <- function(input, output) {

   dataInput = reactive({
        wv = c(input$weight_1,
               input$weight_2,
               input$weight_3,
               input$weight_4
        )
        wv = wv/sum(wv)


        if (input$multiple == "None") {
            alpha = 0.025
        }
        if (input$multiple == "Bonferroni") {
            alpha = 0.025/4
        }
        if (input$utility == "linear") {
            prop = find_prop_linear(
                wv, 
                N_total = input$N_total,
                alpha = alpha,
                k = 0.8,
                rho = input$rho,
                m = input$m
                )
        }
        if (input$utility == "log") {
            prop = find_prop_log(
                wv, 
                N_total = input$N_total,
                alpha = alpha,
                k = 0.8,
                rho = input$rho,
                m = input$m
                )
        }
        df = as_tibble(prop)
        df$wv = wv
        df = df %>%
        mutate("Hypothesis/Treat Arm" = c("1", "2", "3", "4")) %>%
            select(`Hypothesis/Treat Arm`, wv, mde_vector, p_vector) %>%
            mutate(wv = paste0(round(wv*100, digits = 0), "%")) %>%
            mutate(mde_vector = paste0(round(mde_vector*100, digits = 2), "%")) %>%
            mutate(p_vector = paste0(round(p_vector*100, digits = 0), "%")) %>%
            rename("Treatment Arm Proportion" = p_vector, "Hypothesis MDE" = mde_vector, "Hypothesis Implied Weight" = wv)  
        return(df)
   })

    output$mde_df = renderTable({
        dataInput()
    }, align = "cccc")





}

shinyApp(ui = ui, server = server)
