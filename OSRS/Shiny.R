source("Shop Sell.R")


ui = fluidPage(
  tabsetPanel(
    tabPanel("ShopSell", fluid = T,
      fluidRow(
        sidebarLayout(
          sidebarPanel(
            textInput("ShopS1", "Sell price",value="0"),
            textInput("ShopS2", "Sell procent start",value="100"),
            textInput("ShopS3", "Sell procent change",value="0"),
            textInput("ShopS4", "Amount sold",value="0"),
            textInput("ShopS5", "Maximun GP Lost Per Hop",value="0")
          ),
          mainPanel(
            plotOutput("ShopPlot"),
            textOutput("ShopHop")
          )
        )
      ),
      br(),
    ),
    tabPanel("Giants Foundry", fluid = T,
      fluidRow(
        sidebarLayout(
          sidebarPanel(
            selectInput("GFMetal1", "First Metal",choices=c("Bronze","Iron","Steel","Mithril","Adamant","Rune")),
            textInput("GFPrice1", "Price per Amount",value="0"),
            selectInput("GFMetal2", "Second Metal",choices=c("Bronze","Iron","Steel","Mithril","Adamant","Rune")),
            textInput("GFPrice2", "Price per Amount",value="0"),
            textInput("GFNXP", "XP Needed",value="0")
          ),
          mainPanel(
            plotOutput("GFXP"),
            plotOutput("GFGP")
          )
        )
      ),
      br(),
    )
  )
)

server = function(input, output, session) {
  Vals = reactiveValues(Change = F, PriceTotal = 0, Amount = 0)
  observe({ ShopSell(input,output) })
  observe({
    T1 = match(input$GFMetal1,c("Bronze","Iron","Steel","Mithril","Adamant","Rune"))
    T2 = match(input$GFMetal2,c("Bronze","Iron","Steel","Mithril","Adamant","Rune"))
    P1 = as.numeric(input$GFPrice1);  if(is.na(P1)){ P1 = 0 };  P2 = as.numeric(input$GFPrice2);  if(is.na(P2)){ P2 = 0 }
    Qs = Ds = XPs = GPs = numeric(0)
    for(i in 0:28){
      A1 = 28 - (T1!=T2)*i;  A2 = (T1!=T2)*i;  V1 = 10 * T1 * A1 / 28;  V2 = 10 * T2 * A2 / 28
      Q = (floor((floor(10 * V1) + floor(10 * V2) + floor(V1 * V2)) / 10) + 59)
      XP = 30 * (floor(Q^2 / 73) + floor(1.5 * Q) + 1);  P = 2 * XP
      D = 3 * (Q>=69) + (Q>=79) + (Q>=119) + (Q>=149) + (Q>=179)
      Qs[1+i] = Q;  Ds[1+i] = D;  XPs[1+i] = XP;  GPs[1+i] = (P - (A1 * P1 + A2 * P2)) / XP;
    }
    NXP = as.numeric(input$GFNXP);  if(is.na(NXP)){ NXP = 0 }
    if(NXP <= 0){
      output$GFXP = renderPlot({ plot(0:28,XPs,xlab = "Metal", ylab = "XP",type="l");  text(0:28,XPs,Ds) })
      output$GFGP = renderPlot({ plot(0:28,GPs,xlab = "Metal", ylab = "GP/XP",type="l");  text(0:28,GPs,Ds) })
    }
    else{
      NXPs = ceiling(NXP/XPs)
      output$GFXP = renderPlot({ plot(0:28,NXPs,xlab = "Metal", ylab = "XP",type="l");  text(0:28,ceiling(NXP/XPs),Ds) })
      output$GFGP = renderPlot({ plot(0:28,NXPs*XPs*GPs,xlab = "Metal", ylab = "GP/XP",type="l");  text(0:28,NXPs*XPs*GPs,Ds) })
    }
  })
  
  session$onSessionEnded(function() { stopApp() })
}

shinyApp(ui, server)