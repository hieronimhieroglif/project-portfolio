library(shiny)
library(grid)
library(gridExtra)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(lattice)
library(graphics)
library(ggpubr)


ui <- shinyUI(fluidPage(
  
  mainPanel(
    tabsetPanel(
            tabPanel("Wykresy ogólne",
               #fluidRow
               br(),
               p("Poniższy wykres ukazuje częstotliwość występowania każdego z atrybutów każdej
                 ze zmiennych. Pozwala to lepiej poznać charakterystykę zbioru."),
               plotOutput("plot1"),
               br(),
               br(),
               p("Wykres drugi pokazuje modele regresji liniowej dla każdej ze zmiennych,
                 która jest skorelowana ze zmienną 'left', która jest przedmiotem rozważań tej analizy.
                 Zmienna left przyjmuje wartość 1 dla pracownika, który opuścił firmę."),
               plotOutput("plot2"),
               br(),
               p("Zmienne skorelowane dodatnio ze zmienną 'left': time_spend_company,
                 average_montly_hours, number_project. Z tychże wykresów można wywnioskować, iż
                 ilość lat przepracowanych w firmie, ilość godzin pracy w miesiącu i ilość projektów do
                 zrealizowania wpływają dodatnio na prawdopodobieństwo odejścia z pracy. Proszę jednak 
                 zwrócić uwagę na wyrazy początkowe. Przy latach pracy ryzyko wzrasta dopiero od 3,4 lat pracy,
                 przy godzinach pracy miesięcznie, od 200h a przy liczbie projektów od 3,8. Proszę również wziąć
                 pod uwagę duży średni błąd przy wyliczaniu 'left' na podstawie liczby projektóW."),
               br(),
               p("Zmienne skorelowane ujemnie ze zmienną 'left': satisfaction_level, work_accident, promotion_last_5years.
                 Z tych wykresów można wywnioskować, że spadek satysfakcji z pracy, prawdopodobieństwa wypadku i
                 awansu w ciągu najbliższych 5 lat powoduje wzrost prawdopodobienstwa odejścia z pracy. Wyniki analizy 
                 są intuicyjne co do satysfakcji i możliwości awansu. Te 2 atrybuty są pożądne przez pracownika.
                 Zaskakuje natomiast wynik korelacji ze zmienną work_accident. Z wykresu można wywnioskować, że
                 obniżenie prawdobodobieństwa wypadku pracy zwiększa prawdobodobieństwo odejścia.")
                              ),
            tabPanel("Wykresy szczegółowe",
               br(),
               p("wykres nr 1 ukazuje modele regresji z podziałęm na klasę zarobkóW, gdzie low to najniższe,
                 a high to najwyższe. Z wykresu dowiadujemy się, że przy niskim i średnim poziomie zarobków, 
                 wzrost liczby porjektów do wykonania zwiększa prawdopodobieństwo odejścia. Co ciekawe, w 
                 najwyższej kasie zarobkowej wzrost obowiązków przyczynia się do zmniejszenia ryzyka odejścia
                 ze stanowiska."),
               plotOutput("plot3"),
               br(),
               p("Poniżej widzimy 3 wykresy pokazujące wartości zmiennych dla pracowników, którzy odeszli z firmy
                 (left=1). Jak widać dominującą wartością satysfakcji z pracy dla tych którzy odeszli z pracy była 0,4.
                 Zagadkową jest to zmienna- ostatnia ocena pracy pracownika. Jest tu zarówno bardzo wiele ocen miernych
                 jak i tych najlepszych. Może to sugerować, że ocena wydajności pracy pracownika nie wpływa na jego chcęć
                 do odejścia."),
               plotOutput("plot4"),
               br(),
               p("Poniższy wykres jest wykresem gęstości satysfakcji z pracy z podziałęm na działy firmy. Z wykresu można
                 ocenić, że najbardziej usatysfakcjonowani z pracy są pracownicy kadry zarządzającej, a najmniej- IT i HR."),
               plotOutput("plot5"),
               br(),
               p("Ostatni wykres ukazuje częstość zwolnień w stosunku do ilości godzin pracy w miesiącu. Na wykresie widać,
                 że największy odsetek odchodzących z pracy występuje przy 150 i 260 godzinach pracy w miesiącu."),
               plotOutput("plot6")),
            
            tabPanel("Podsumowanie",
               h4("Podsumowanie tabeli HR"),
               verbatimTextOutput("summary"),
               br(),
               br(),
               h4("Pierwsze 10 wierszy"),
               tableOutput("view")
                     )
            )
            
    )
  )
)

server <- shinyServer(function(input, output) {
  hr<- read.csv("HR_comma_sep.csv")
  
  output$plot1 <- renderPlot({
      hr %>%
        keep(is.numeric) %>% 
        gather() %>% 
        ggplot(aes(value)) +
        facet_wrap(~ key, scales = "free") +
        geom_histogram()
  })
    output$plot2 <- renderPlot({
      lm1<- ggplot(hr, aes(x = left, y = time_spend_company)) + 
        geom_smooth(method = "lm", col = "red")
      
      lm2 <- ggplot(hr, aes(x = left, y = satisfaction_level)) + 
        geom_smooth(method = "glm", method.args = list(family = "binomial"), col = "red")
      
      lm3 <- ggplot(hr, aes(x = left, y = number_project)) + 
        geom_smooth(method = "lm", col = "red")
      
      lm4 <- ggplot(hr, aes(x = left, y = average_montly_hours)) + 
        geom_smooth(method = "lm", col = "red")
      
      lm5 <- ggplot(hr, aes(x = left, y = Work_accident)) + 
        geom_smooth(method = "lm", col = "red")
      
      lm6 <- ggplot(hr, aes(x = left, y = promotion_last_5years)) + 
        geom_smooth(method = "lm", col = "red")
      
      ggarrange(lm1, lm2, lm3, lm4, lm5, lm6, ncol=2, nrow=3)
    })
  
  output$plot3 <- renderPlot({
    xyplot(left ~ number_project | factor(salary), data=hr,
           layout=c(3, 1), aspect=2,
           panel=function(x, y) {
             panel.lmline(x, y)
             panel.xyplot(x, y)
           }, xlab='ilość projektów', ylab='odejścia', main='Wykres odejść z podziałem na klasę
           zarobków')
      })
  output$plot4 <- renderPlot({
    hr_hist <- hr %>% filter(left==1)
    par(mfrow=c(1,3))
    hist(hr_hist$satisfaction_level,col="#3090C7", main = "Poziom satysfakcji") 
    hist(hr_hist$last_evaluation,col="#3090C7", main = "Ostatnia ocena")
    hist(hr_hist$average_montly_hours,col="#3090C7", main = "ilość godzin pracy w miesiącu")
    
  })
  output$plot5 <- renderPlot({
    p2 <- ggplot(hr, aes(x=satisfaction_level, colour=sales)) + geom_density()
    p2
  })
  output$plot6 <- renderPlot({
    densityplot(hr$left ~ hr$average_montly_hours, ylab = "częstość zwolnień")
    
  })
  output$summary <- renderPrint({
    summary(hr)
  })
  
  output$view <- renderTable({
    head(hr, n = 10)
  })
})


shinyApp(ui = ui, server = server)