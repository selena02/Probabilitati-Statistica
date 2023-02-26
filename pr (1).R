#install.packages("shiny")
#install.packages("shinyjs")
#install.packages("shinydashboard")
library(shiny)
library(shinyjs)
library(shinydashboard)

# Define UI ----
ui <- fluidPage(
  titlePanel("Repartitii discrete si continue"),
  
  sidebarLayout(
    sidebarPanel(
      useShinyjs(),
      radioButtons("var",  h3("Repartitia"), #afisam variantele de repartitii
                   choices = list("Beta" = 1, 
                                  "Binomiala" = 2,
                                  "Cauchy" = 3,
                                  "Chy-Squared" = 4, 
                                  "Exponentiala" = 5,
                                  "Fisher" = 6,
                                  "Gamma" = 7, 
                                  "Hipergeometrica" = 8,
                                  "Log-Normala" = 9,
                                  "Logistica" = 10, 
                                  "Normala" = 11,
                                  "Poisson" = 12,
                                  "Student" = 13, 
                                  "Uniforma" = 14,
                                  "Weibull" = 15),
                   selected = 1),
      helpText(h3("Parametrii a si b")), #parametrii pentru hasurare
      numericInput( "a", 
                    h4("a"), 
                    value = 0),
      numericInput( "b", 
                    h4("b"), 
                    value = 0)
      
    ),
    
    mainPanel(#pentru fiecare tip de repartitie avem nevoie de parametrii diferiti
      box(id= "Beta",  width = '800px',
          numericInput( "alpha1", 
                        h3("alpha"),
                        value = 0),
          numericInput( "beta1", 
                        h3("beta"),
                        value = 0
          )
      ),
      box(id= "binomiala",  width = '800px',
          numericInput( "n", 
                        h3("n"), 
                        value = 0),
          numericInput( "p", 
                        h3("p"), 
                        value = 0)
      ),
      box(id= "cauchy",  width = '800px',
          numericInput( "t", 
                        h3("t"), 
                        value = 0),
          numericInput( "s", 
                        h3("s"), 
                        value = 0)
      ),
      box(id= "Chy-Squared",  width = '800px',
          numericInput( "k", 
                        h3("k"), 
                        value = 0)
      ),
      box(id= "exponentiala",  width = '800px',
          numericInput( "lambda", 
                        h3("Lambda"), 
                        value = 0)
      ),
      box(id= "fisher",  width = '800px',
          numericInput( "d1", 
                        h3("d1"), 
                        value = 0),
          numericInput( "d2", 
                        h3("d2"), 
                        value = 0)
      ),
      box(id= "gamma",  width = '800px',
          numericInput( "alpha", 
                        h3("alpha"), 
                        value = 0),
          numericInput( "beta", 
                        h3("beta"),
                        value = 0)
      ),
      box(id= "hipergeometrica",  width = '800px',
          numericInput( "m1", 
                        h3("Numarul de bile albe(m)"), 
                        value = 0),
          numericInput( "n1", 
                        h3("Numarul de bile negre(n)"), 
                        value = 0),
          numericInput( "k1", 
                        h3("Numarul de bile extrase(k)"), 
                        value = 0)
      ),
      box(id= "log-normala",  width = '800px',
          numericInput( "miu", 
                        h3("miu"), 
                        value = 0),
          numericInput( "teta", 
                        h3("teta"),
                        value = 0)
      ),
      box(id= "logistica",  width = '800px',
          numericInput( "t1", 
                        h3("t"), 
                        value = 0),
          numericInput( "s1", 
                        h3("s"), 
                        value = 1)
      ),
      box(id= "normala",  width = '800px',
          numericInput( "miu1", 
                        h3("miu"), 
                        value = 0),
          numericInput( "teta1", 
                        h3("teta"),
                        value = 0)
      ),
      box(id= "poisson",  width = '800px',
          numericInput( "lambda1", 
                        h3("lambda"), 
                        value = 0)
      ),
      box(id= "student",  width = '800px',
          numericInput( "d", 
                        h3("d"),
                        value = 0)
      ),
      box(id= "uniforma",  width = '800px',
          numericInput( "minU", 
                        h3("Minim"), 
                        value = 0),
          numericInput( "maxU", 
                        h3("Maxim"), 
                        value = 0)
      ),
      box(id= "weibull",  width = '800px',
          numericInput( "k2", 
                        h3("k"), 
                        value = 0)
      ),
      
      plotOutput("functii"), 
      fluidRow(
        column(6,#hasurarea
               plotOutput("hasurare")
        )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  #alege tipul de repartitie marcat de utilizator in functie de input$var
  observeEvent(input$var, {
    if(input$var == 1)
    { shinyjs::show(id = "Beta")#varianta aleasa
      shinyjs::hide(id = "binomiala")
      shinyjs::hide(id = "cauchy")
      shinyjs::hide(id = "Chy-Squared")
      shinyjs::hide(id = "exponentiala")
      shinyjs::hide(id = "fisher")
      shinyjs::hide(id = "gamma")
      shinyjs::hide(id = "hipergeometrica")
      shinyjs::hide(id = "log-normala")
      shinyjs::hide(id = "logistica")
      shinyjs::hide(id = "normala")
      shinyjs::hide(id = "poisson")
      shinyjs::hide(id = "student")
      shinyjs::hide(id = "uniforma")
      shinyjs::hide(id = "weibull")
      output$functii <- renderPlot({par(mfrow = c(1, 2))
        #functia de de masa/densitate
        curve(dbeta(x, shape1 = input$alpha1, shape2 = input$beta1), 0, 1, ylim = c(0, 2.5),
              ylab = "f(x)",col = "red", lwd = 2, main = "Functia de densitate")
        })
      
      #hasurarea probabilitatilor in functie de a si b pe functia de repartitie
      output$hasurare <- renderPlot({curve(pbeta(x, shape1 = input$alpha1, shape2 = input$beta1),0, 1, ylim = c(0, 1), type = "l", main = "Functia de repartitie",
                                       ylab = "F(x)", lwd = 2, col = "red")
        
        x <- seq(0,input$a, by=0.01)
        x1 <- c(x, seq(input$a,0, by=-0.01))
        y <- c(c(pbeta(x, shape1 = input$alpha1, shape2 = input$beta1)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "purple")
        
        x <- seq(input$b, 1, by=0.01)
        x1 <- c(x, seq(1, input$b, by=-0.01)) 
        y <- c(c(pbeta(x,shape1 = input$alpha1, shape2 = input$beta1)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "orange")
        
        x<-seq(input$a, input$b, by=0.01)
        x1 <- c(x, seq(input$b, input$a, by=-0.01))
        y <- c(c(pbeta(x, shape1 = input$alpha1, shape2 = input$beta1)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "blue")
      })
    }
    if(input$var == 2)
    { shinyjs::hide(id = "Beta")
      shinyjs::show(id = "binomiala")
      shinyjs::hide(id = "cauchy")
      shinyjs::hide(id = "Chy-Squared")
      shinyjs::hide(id = "exponentiala")
      shinyjs::hide(id = "fisher")
      shinyjs::hide(id = "gamma")
      shinyjs::hide(id = "hipergeometrica")
      shinyjs::hide(id = "log-normala")
      shinyjs::hide(id = "logistica")
      shinyjs::hide(id = "normala")
      shinyjs::hide(id = "poisson")
      shinyjs::hide(id = "student")
      shinyjs::hide(id = "uniforma")
      shinyjs::hide(id = "weibull")
      output$functii <- renderPlot({par(mfrow = c(1, 2))
        plot(seq(0,40,1),dbinom(seq(0,40,1),size = input$n, prob = input$p),type="l", xlab = "x", ylab = "F(x)", main = "Functia de masa",
             lwd = 2, col = "red")
       })
      output$hasurare <-renderPlot({plot(seq(0,40,1),pbinom(seq(0,40,1), size = input$n, prob = input$p),type="l", xlab = "x", ylab = "F(x)",
                                     main = "Functia de repartitie", lwd = 2, col = "red")
        x <- seq(0,input$a, by=0.2)
        x1 <- c(x, seq(input$a,0, by=-0.2))
        y <- c(c(pbinom(x, size = input$n, prob = input$p)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "purple")
        
        x <- seq(input$b, input$n, by=0.2)
        x1 <- c(x, seq(input$n, input$b, by=-0.2) ) 
        y <- c(c(pbinom(x, size = input$n, prob = input$p)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "orange")
        
        x<-seq(input$a, input$b, by=0.2)
        x1 <- c(x, seq(input$b, input$a, by=-0.2))
        y <- c(c(pbinom(x, size = input$n, prob = input$p)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "blue")
      })
    }
    
    if(input$var == 3)
    {shinyjs::hide(id = "Beta")
      shinyjs::hide(id = "binomiala")
      shinyjs::show(id = "cauchy")
      shinyjs::hide(id = "Chy-Squared")
      shinyjs::hide(id = "exponentiala")
      shinyjs::hide(id = "fisher")
      shinyjs::hide(id = "gamma")
      shinyjs::hide(id = "hipergeometrica")
      shinyjs::hide(id = "log-normala")
      shinyjs::hide(id = "logistica")
      shinyjs::hide(id = "normala")
      shinyjs::hide(id = "poisson")
      shinyjs::hide(id = "student")
      shinyjs::hide(id = "uniforma")
      shinyjs::hide(id = "weibull")
      output$functii <- renderPlot({par(mfrow = c(1, 2))
        curve(dcauchy(x, location  = input$t, scale = input$s), input$t-2, input$t+2, ylim = c(0, dcauchy(input$t, location  = input$t, scale = input$s)+0.2),
              ylab = "f(x)",col = "red", lwd = 2, main = "Functia de densitate")
      })
      output$hasurare <- renderPlot({curve(pcauchy(x, location  = input$t, scale = input$s),input$t-2, input$t+2, ylim = c(0, 1), type = "l", main = "Functie de repartitie",
                                       ylab = "F(x)", lwd = 2, col = "red")
        
        x <- seq(input$t-2,input$a, by=0.2)
        x1 <- c(x, seq(input$a,input$t-2, by=-0.2))
        y <- c(c(pcauchy(x, location  = input$t, scale = input$s)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "purple")
        
        x <- seq(input$b, input$t+2, by=0.2)
        x1 <- c(x, seq(input$t+2, input$b, by=-0.2)) 
        y <- c(c(pcauchy(x, location  = input$t, scale = input$s)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "orange")
        
        x<-seq(input$a, input$b, by=0.2)
        x1 <- c(x, seq(input$b, input$a, by=-0.2))
        y <- c(c(pcauchy(x, location  = input$t, scale = input$s)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "blue")
      })
    }
    
    if(input$var == 4)
    {shinyjs::hide(id = "Beta")
      shinyjs::hide(id = "binomiala")
      shinyjs::hide(id = "cauchy")
      shinyjs::show(id = "Chy-Squared")
      shinyjs::hide(id = "exponentiala")
      shinyjs::hide(id = "fisher")
      shinyjs::hide(id = "gamma")
      shinyjs::hide(id = "hipergeometrica")
      shinyjs::hide(id = "log-normala")
      shinyjs::hide(id = "logistica")
      shinyjs::hide(id = "normala")
      shinyjs::hide(id = "poisson")
      shinyjs::hide(id = "student")
      shinyjs::hide(id = "uniforma")
      shinyjs::hide(id = "weibull")
      output$functii <- renderPlot({par(mfrow = c(1, 2))
        curve(dchisq(x, df  = input$k, ncp = 0), 0, input$k + 10, ylim = c(0, 0.6),
              ylab = "f(x)",col = "red", lwd = 2, main = "Functia de densitate")
        })
      
      output$hasurare <- renderPlot({curve(pchisq(x, df  = input$k, ncp = 0),0, input$k + 10, ylim = c(0, 1), type = "l", main = "Functie de repartitie",
                                       ylab = "F(x)", lwd = 2, col = "red")
        
        x <- seq(0,input$a, by=0.05)
        x1 <- c(x, seq(input$a,0, by=-0.05))
        y <- c(c(pchisq(x, df  = input$k, ncp = 0)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "purple")
        
        x <- seq(input$b, input$k+10, by=0.05)
        x1 <- c(x, seq(input$k+10, input$b, by=-0.05)) 
        y <- c(c(pchisq(x, df  = input$k, ncp = 0)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "orange")
        
        x<-seq(input$a, input$b, by=0.05)
        x1 <- c(x, seq(input$b, input$a, by=-0.05))
        y <- c(c(pchisq(x, df  = input$k, ncp = 0)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "blue")
      })
    }
    
    if(input$var == 5)
    {shinyjs::hide(id = "Beta")
      shinyjs::hide(id = "binomiala")
      shinyjs::hide(id = "cauchy")
      shinyjs::hide(id = "Chy-Squared")
      shinyjs::show(id = "exponentiala")
      shinyjs::hide(id = "fisher")
      shinyjs::hide(id = "gamma")
      shinyjs::hide(id = "hipergeometrica")
      shinyjs::hide(id = "log-normala")
      shinyjs::hide(id = "logistica")
      shinyjs::hide(id = "normala")
      shinyjs::hide(id = "poisson")
      shinyjs::hide(id = "student")
      shinyjs::hide(id = "uniforma")
      shinyjs::hide(id = "weibull")
      output$functii <- renderPlot({par(mfrow = c(1, 2))
        curve(dexp(x, rate  = input$lambda), 0, 5, ylim = c(0, 1.5),
              ylab = "f(x)",col = "red", lwd = 2, main = "Functia de densitate")
      })
      output$hasurare <- renderPlot({curve(pexp(x, rate  = input$lambda),0, 5, ylim = c(0, 1), type = "l", main = "Functie de repartitie",
                                       ylab = "F(x)", lwd = 2, col = "red")
        
        x <- seq(0,input$a, by=0.05)
        x1 <- c(x, seq(input$a,0, by=-0.05))
        y <- c(c(pexp(x, rate  = input$lambda)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "purple")
        
        x <- seq(input$b, 5, by=0.05)
        x1 <- c(x, seq(5, input$b, by=-0.05)) 
        y <- c(c(pexp(x, rate  = input$lambda)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "orange")
        
        x<-seq(input$a, input$b, by=0.05)
        x1 <- c(x, seq(input$b, input$a, by=-0.05))
        y <- c(c(pexp(x, rate  = input$lambda)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "blue")
      })
    }
    
    if(input$var == 6)
    {shinyjs::hide(id = "Beta")
      shinyjs::hide(id = "binomiala")
      shinyjs::hide(id = "cauchy")
      shinyjs::hide(id = "Chy-Squared")
      shinyjs::hide(id = "exponentiala")
      shinyjs::show(id = "fisher")
      shinyjs::hide(id = "gamma")
      shinyjs::hide(id = "hipergeometrica")
      shinyjs::hide(id = "log-normala")
      shinyjs::hide(id = "logistica")
      shinyjs::hide(id = "normala")
      shinyjs::hide(id = "poisson")
      shinyjs::hide(id = "student")
      shinyjs::hide(id = "uniforma")
      shinyjs::hide(id = "weibull")
      output$functii <- renderPlot({par(mfrow = c(1, 2))
        curve(df(x, df1 = input$d1, df2 = input$d2), 0, 5, ylim = c(0, 2.5),
              ylab = "f(x)",col = "red", lwd = 2, main = "Functia de densitate")
        })
      
      output$hasurare <- renderPlot({curve(pf(x, df1 = input$d1, df2 = input$d2),0, 5, ylim = c(0, 1), type = "l", main = "Functie de repartitie",
                                       ylab = "F(x)", lwd = 2, col = "red")
        
        x <- seq(0,input$a, by=0.05)
        x1 <- c(x, seq(input$a,0, by=-0.05))
        y <- c(c(pf(x, df1 = input$d1, df2 = input$d2)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "purple")
        
        x <- seq(input$b, 5, by=0.05)
        x1 <- c(x, seq(5, input$b, by=-0.05)) 
        y <- c(c(pf(x, df1 = input$d1, df2 = input$d2)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "orange")
        
        x<-seq(input$a, input$b, by=0.05)
        x1 <- c(x, seq(input$b, input$a, by=-0.05))
        y <- c(c(pf(x, df1 = input$d1, df2 = input$d2)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "blue")
      })
    }
    
    if(input$var == 7)
    {shinyjs::hide(id = "Beta")
      shinyjs::hide(id = "binomiala")
      shinyjs::hide(id = "cauchy")
      shinyjs::hide(id = "Chy-Squared")
      shinyjs::hide(id = "exponentiala")
      shinyjs::hide(id = "fisher")
      shinyjs::show(id = "gamma")
      shinyjs::hide(id = "hipergeometrica")
      shinyjs::hide(id = "log-normala")
      shinyjs::hide(id = "logistica")
      shinyjs::hide(id = "normala")
      shinyjs::hide(id = "poisson")
      shinyjs::hide(id = "student")
      shinyjs::hide(id = "uniforma")
      shinyjs::hide(id = "weibull")
      output$functii <- renderPlot({par(mfrow = c(1, 2))
        curve(dgamma(x, shape = input$alpha, rate = input$beta, scale = 1/input$beta), 0, 20, ylim = c(0, 0.5),
              ylab = "f(x)",col = "red", lwd = 2, main = "Functia de densitate")
       })
      
      output$hasurare <- renderPlot({curve(pgamma(x, shape = input$alpha, rate = input$beta, scale = 1/input$beta),0, 20, ylim = c(0, 1), type = "l", main = "Functie de repartitie",
                                       ylab = "F(x)", lwd = 2, col = "red")
        
        x <- seq(0,input$a, by=0.05)
        x1 <- c(x, seq(input$a,0, by=-0.05))
        y <- c(c(pgamma(x, shape = input$alpha, rate = input$beta, scale = 1/input$beta)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "purple")
        
        x <- seq(input$b, 20, by=0.05)
        x1 <- c(x, seq(20, input$b, by=-0.05)) 
        y <- c(c(pgamma(x, shape = input$alpha, rate = input$beta, scale = 1/input$beta)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "orange")
        
        x<-seq(input$a, input$b, by=0.05)
        x1 <- c(x, seq(input$b, input$a, by=-0.05))
        y <- c(c(pgamma(x, shape = input$alpha, rate = input$beta, scale = 1/input$beta)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "blue")
      })
    }
    
    if(input$var == 8)
    { shinyjs::hide(id = "Beta")
      shinyjs::hide(id = "binomiala")
      shinyjs::hide(id = "cauchy")
      shinyjs::hide(id = "Chy-Squared")
      shinyjs::hide(id = "exponentiala")
      shinyjs::hide(id = "fisher")
      shinyjs::hide(id = "gamma")
      shinyjs::show(id = "hipergeometrica")
      shinyjs::hide(id = "log-normala")
      shinyjs::hide(id = "logistica")
      shinyjs::hide(id = "normala")
      shinyjs::hide(id = "poisson")
      shinyjs::hide(id = "student")
      shinyjs::hide(id = "uniforma")
      shinyjs::hide(id = "weibull")
      output$functii <- renderPlot({par(mfrow = c(1, 2))
        plot(seq(0,60,1),dhyper(seq(0,60,1), m = input$m1, n = input$n1, k = input$k1),type="l", xlab = "x", ylab = "F(x)", main = "Functia de masa",
             lwd = 2, col = "red")
        })
      output$hasurare <- renderPlot({plot(seq(0,60,1),phyper(seq(0,60,1), m = input$m1, n = input$n1, k = input$k1),type="l", xlab = "x", ylab = "F(x)", main = "Functie de repartitie",
                                      lwd = 2, col = "red")
        
        x <- seq(0,input$a, by=0.001)
        x1 <- c(x, seq(input$a,0, by=-0.001))
        y <- c(c(phyper(x, m = input$m1, n = input$n1, k = input$k1)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "purple")
        
        x <- seq(input$b, 60, by=0.001)
        x1 <- c(x, seq(60, input$b, by=-0.001)) 
        y <- c(c(phyper(x, m = input$m1, n = input$n1, k = input$k1)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "orange")
        
        x<-seq(input$a, input$b, by=0.001)
        x1 <- c(x, seq(input$b, input$a, by=-0.001))
        y <- c(c(phyper(x, m = input$m1, n = input$n1, k = input$k1)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "blue")
      })
    }
    if(input$var == 9)
    { shinyjs::hide(id = "Beta")
      shinyjs::hide(id = "binomiala")
      shinyjs::hide(id = "cauchy")
      shinyjs::hide(id = "Chy-Squared")
      shinyjs::hide(id = "exponentiala")
      shinyjs::hide(id = "fisher")
      shinyjs::hide(id = "gamma")
      shinyjs::hide(id = "hipergeometrica")
      shinyjs::show(id = "log-normala")
      shinyjs::hide(id = "logistica")
      shinyjs::hide(id = "normala")
      shinyjs::hide(id = "poisson")
      shinyjs::hide(id = "student")
      shinyjs::hide(id = "uniforma")
      shinyjs::hide(id = "weibull")
      output$functii <- renderPlot({par(mfrow = c(1, 2))
        curve(dlnorm(x, meanlog = input$miu, sdlog = input$teta), 0, 2.5, ylim = c(0, 2),
              ylab = "f(x)",col = "red", lwd = 2, main = "Functia de densitate")
       })
      output$hasurare <- renderPlot({curve(plnorm(x, meanlog = input$miu, sdlog = input$teta),0, 2.5, ylim = c(0, 1.5), type = "l", main = "Functie de repartitie",
                                       ylab = "F(x)", lwd = 2, col = "red")
        
        x <- seq(0,input$a, by=0.001)
        x1 <- c(x, seq(input$a,0, by=-0.001))
        y <- c(c(plnorm(x, meanlog = input$miu, sdlog = input$teta)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "purple")
        
        x <- seq(input$b, 2.5, by=0.001)
        x1 <- c(x, seq(2.5, input$b, by=-0.001)) 
        y <- c(c(plnorm(x, meanlog = input$miu, sdlog = input$teta)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "orange")
        
        x<-seq(input$a, input$b, by=0.001)
        x1 <- c(x, seq(input$b, input$a, by=-0.001))
        y <- c(c(plnorm(x, meanlog = input$miu, sdlog = input$teta)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "blue")
      })
    }
    if(input$var == 10)
    { shinyjs::hide(id = "Beta")
      shinyjs::hide(id = "binomiala")
      shinyjs::hide(id = "cauchy")
      shinyjs::hide(id = "Chy-Squared")
      shinyjs::hide(id = "exponentiala")
      shinyjs::hide(id = "fisher")
      shinyjs::hide(id = "gamma")
      shinyjs::hide(id = "hipergeometrica")
      shinyjs::hide(id = "log-normala")
      shinyjs::show(id = "logistica")
      shinyjs::hide(id = "normala")
      shinyjs::hide(id = "poisson")
      shinyjs::hide(id = "student")
      shinyjs::hide(id = "uniforma")
      shinyjs::hide(id = "weibull")
      output$functii <- renderPlot({par(mfrow = c(1, 2))
        curve(dlogis(x, location  = input$t1, scale = input$s1), -5, 20, ylim = c(0, 0.3),
              ylab = "f(x)",col = "red", lwd = 2, main = "Functia de densitate")
        })
      
      output$hasurare <- renderPlot({curve(plogis(x, location  = input$t1, scale = input$s1),-5, 20, ylim = c(0, 1), type = "l", main = "Functie de repartitie",
                                       ylab = "F(x)", lwd = 2, col = "red")
        
        x <- seq(-5,input$a, by=0.2)
        x1 <- c(x, seq(input$a,-5, by=-0.2))
        y <- c(c(plogis(x, location  = input$t1, scale = input$s1)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "purple")
        
        x <- seq(input$b, 20, by=0.2)
        x1 <- c(x, seq(20, input$b, by=-0.2)) 
        y <- c(c(plogis(x, location  = input$t1, scale = input$s1)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "orange")
        
        x<-seq(input$a, input$b, by=0.2)
        x1 <- c(x, seq(input$b, input$a, by=-0.2))
        y <- c(c(plogis(x, location  = input$t1, scale = input$s1)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "blue")
      })
    }
    if(input$var == 11)
    { shinyjs::hide(id = "Beta")
      shinyjs::hide(id = "binomiala")
      shinyjs::hide(id = "cauchy")
      shinyjs::hide(id = "Chy-Squared")
      shinyjs::hide(id = "exponentiala")
      shinyjs::hide(id = "fisher")
      shinyjs::hide(id = "gamma")
      shinyjs::hide(id = "hipergeometrica")
      shinyjs::hide(id = "log-normala")
      shinyjs::hide(id = "logistica")
      shinyjs::show(id = "normala")
      shinyjs::hide(id = "poisson")
      shinyjs::hide(id = "student")
      shinyjs::hide(id = "uniforma")
      shinyjs::hide(id = "weibull")
      output$functii <- renderPlot({par(mfrow = c(1, 2))
        curve(dnorm(x, mean = input$miu1, sd = sqrt(input$teta1)), -5, 5, ylim = c(0, 1),
              ylab = "f(x)",col = "red", lwd = 2, main = "Functia de densitate")
        })
      output$hasurare <- renderPlot({curve(pnorm(x, mean = input$miu1, sd = sqrt(input$teta1)),-5, 5, ylim = c(0, 1), type = "l", main = "Functie de repartitie",
                                       ylab = "F(x)", lwd = 2, col = "red")
        
        x <- seq(-5,input$a, by=0.001)
        x1 <- c(x, seq(input$a,-5, by=-0.001))
        y <- c(c(pnorm(x, mean = input$miu1, sd = sqrt(input$teta1))),seq(0,0, length = length(x)))
        polygon(x1, y, col = "purple")
        
        x <- seq(input$b, 5, by=0.001)
        x1 <- c(x, seq(5, input$b, by=-0.001)) 
        y <- c(c(pnorm(x, mean = input$miu1, sd = sqrt(input$teta1))),seq(0,0, length = length(x)))
        polygon(x1, y, col = "orange")
        
        x<-seq(input$a, input$b, by=0.001)
        x1 <- c(x, seq(input$b, input$a, by=-0.001))
        y <- c(c(pnorm(x, mean = input$miu1, sd = sqrt(input$teta1))),seq(0,0, length = length(x)))
        polygon(x1, y, col = "blue")
      })
    }
    if(input$var == 12)
    { shinyjs::hide(id = "Beta")
      shinyjs::hide(id = "binomiala")
      shinyjs::hide(id = "cauchy")
      shinyjs::hide(id = "Chy-Squared")
      shinyjs::hide(id = "exponentiala")
      shinyjs::hide(id = "fisher")
      shinyjs::hide(id = "gamma")
      shinyjs::hide(id = "hipergeometrica")
      shinyjs::hide(id = "log-normala")
      shinyjs::hide(id = "logistica")
      shinyjs::hide(id = "normala")
      shinyjs::show(id = "poisson")
      shinyjs::hide(id = "student")
      shinyjs::hide(id = "uniforma")
      shinyjs::hide(id = "weibull")
      output$functii <- renderPlot({par(mfrow = c(1, 2))
        plot(seq(0,20,1),dpois(seq(0,20,1), lambda = input$lambda1),type="l", xlab = "x", ylab = "f(x)",main = "Functia de masa",
             lwd = 2, col = "red")
        })
      output$hasurare <- renderPlot({plot(seq(0,20,1),ppois(seq(0,20,1), lambda = input$lambda1),type="l", xlab = "x", ylab = "F(x)", main = "Functie de repartitie",
                                      lwd = 2, col = "red")
        
        x <- seq(0,input$a, by=0.001)
        x1 <- c(x, seq(input$a,0, by=-0.001))
        y <- c(c(ppois(x, lambda = input$lambda1)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "purple")
        
        x <- seq(input$b, 20, by=0.001)
        x1 <- c(x, seq(20, input$b, by=-0.001)) 
        y <- c(c(ppois(x,lambda = input$lambda1)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "orange")
        
        x<-seq(input$a, input$b, by=0.001)
        x1 <- c(x, seq(input$b, input$a, by=-0.001))
        y <- c(c(ppois(x, lambda = input$lambda1)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "blue")
      })
    }
    if(input$var == 13)
    { shinyjs::hide(id = "Beta")
      shinyjs::hide(id = "binomiala")
      shinyjs::hide(id = "cauchy")
      shinyjs::hide(id = "Chy-Squared")
      shinyjs::hide(id = "exponentiala")
      shinyjs::hide(id = "fisher")
      shinyjs::hide(id = "gamma")
      shinyjs::hide(id = "hipergeometrica")
      shinyjs::hide(id = "log-normala")
      shinyjs::hide(id = "logistica")
      shinyjs::hide(id = "normala")
      shinyjs::hide(id = "poisson")
      shinyjs::show(id = "student")
      shinyjs::hide(id = "uniforma")
      shinyjs::hide(id = "weibull")
      output$functii <- renderPlot({par(mfrow = c(1, 2))
        curve(dt(x, df = input$d), -4, 4, ylim = c(0, 0.4),
              ylab = "f(x)",col = "red", lwd = 2, main = "Functia de densitate")
        })
      
      output$hasurare <- renderPlot({curve(pt(x, df = input$d),-4, 4, ylim = c(0, 1), type = "l", main = "Functie de repartitie",
                                       ylab = "F(x)", lwd = 2, col = "red")
        
        x <- seq(-4,input$a, by=0.1)
        x1 <- c(x, seq(input$a,-4, by=-0.1))
        y <- c(c(pt(x, df = input$d)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "purple")
        
        x <- seq(input$b, 4, by=0.1)
        x1 <- c(x, seq(4, input$b, by=-0.1)) 
        y <- c(c(pt(x, df = input$d)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "orange")
        
        x<-seq(input$a, input$b, by=0.1)
        x1 <- c(x, seq(input$b, input$a, by=-0.1))
        y <- c(c(pt(x, df = input$d)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "blue")
      })
    }
    if(input$var == 14)
    {shinyjs::hide(id = "Beta")
      shinyjs::hide(id = "binomiala")
      shinyjs::hide(id = "cauchy")
      shinyjs::hide(id = "Chy-Squared")
      shinyjs::hide(id = "exponentiala")
      shinyjs::hide(id = "fisher")
      shinyjs::hide(id = "gamma")
      shinyjs::hide(id = "hipergeometrica")
      shinyjs::hide(id = "log-normala")
      shinyjs::hide(id = "logistica")
      shinyjs::hide(id = "normala")
      shinyjs::hide(id = "poisson")
      shinyjs::hide(id = "student")
      shinyjs::show(id = "uniforma")
      shinyjs::hide(id = "weibull")
      output$functii <- renderPlot({par(mfrow = c(1, 2))
        curve(dunif(x, min = input$minU, max = input$maxU), input$minU-2, input$maxU+2, ylim = c(0, 1),
              ylab = "f(x)",col = "red", lwd = 2, main = "Functia de densitate")
      })
        output$hasurare <- renderPlot({curve(punif(x, min = input$minU, max = input$maxU),input$minU-2, input$maxU+2, ylim = c(0, 1), type = "l", main = "Functie de repartitie",
                                         ylab = "F(x)", lwd = 2, col = "red")
          
          polygon(c(input$minU,input$a,input$a), c(0, 0, punif(input$a, min = input$minU, max = input$maxU)), col = "purple")
          polygon(c(input$b,input$maxU,input$maxU,input$b), c(0, 0,1, punif(input$b, min = input$minU, max = input$maxU)), col = "orange")
          polygon(c(input$a,input$b,input$b,input$a), c(0, 0,punif(input$b, min = input$minU, max = input$maxU), punif(input$a, min = input$minU, max = input$maxU)), col = "blue")
        })

    }
    if(input$var == 15)
    { shinyjs::hide(id = "Beta")
      shinyjs::hide(id = "binomiala")
      shinyjs::hide(id = "cauchy")
      shinyjs::hide(id = "Chy-Squared")
      shinyjs::hide(id = "exponentiala")
      shinyjs::hide(id = "fisher")
      shinyjs::hide(id = "gamma")
      shinyjs::hide(id = "hipergeometrica")
      shinyjs::hide(id = "log-normala")
      shinyjs::hide(id = "logistica")
      shinyjs::hide(id = "normala")
      shinyjs::hide(id = "poisson")
      shinyjs::hide(id = "student")
      shinyjs::hide(id = "uniforma")
      shinyjs::show(id = "weibull")
      output$functii <- renderPlot({par(mfrow = c(1, 2))
        curve(dweibull(x, shape = input$k2, scale = 1), 0, 2.5, ylim = c(0, 2.5),
              ylab = "f(x)",col = "red", lwd = 2, main = "Functia de densitate")
       })
      
      output$hasurare <- renderPlot({curve(pweibull(x, shape = input$k2, scale = 1),0, 2.5, ylim = c(0, 1), type = "l", main = "Functie de repartitie",
                                       ylab = "F(x)", lwd = 2, col = "red")
        
        x <- seq(0,input$a, by=0.01)
        x1 <- c(x, seq(input$a,0, by=-0.01))
        y <- c(c(pweibull(x, shape = input$k2, scale = 1)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "purple")
        
        x <- seq(input$b, 2.5, by=0.01)
        x1 <- c(x, seq(2.5, input$b, by=-0.01)) 
        y <- c(c(pweibull(x,shape = input$k2, scale = 1)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "orange")
        
        x<-seq(input$a, input$b, by=0.01)
        x1 <- c(x, seq(input$b, input$a, by=-0.01))
        y <- c(c(pweibull(x, shape = input$k2, scale = 1)),seq(0,0, length = length(x)))
        polygon(x1, y, col = "blue")
      })
    }
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)