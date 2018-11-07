library(shiny)
library(BCRA)
library(flexdashboard)

char_race <- c("White","African-American","Hispanic-American","Other",
               "White","Chinese-American","Japanese-American","Filipino-American",
               "Hawaiian-American","Other Pacific Islander","Other Asian")

deter_race <- function(x) {
    return(char_race[as.numeric(x)])
}


shinyServer(
    function(input, output) {
        pred_five <- reactive({as.numeric(round(absolute.risk(data.frame(ID=1,
                                                    T1=as.numeric(input$age),
                                                    T2=as.numeric(input$age)+5,
                                                    N_Biop=as.integer(input$biopsies),
                                                    HypPlas=ifelse(as.integer(input$biopsies) == 0 |
                                                                       as.integer(input$biopsies) == 99,
                                                                   99,as.integer(input$hyperplasia)),
                                                    AgeMen=as.numeric(input$menstruation),
                                                    Age1st=as.numeric(input$first_birth),
                                                    N_Rels=as.numeric(input$relatives),
                                                    Race =as.integer(input$race))),1))
        })
        
        pred_avg <- reactive({as.numeric(round(absolute.risk(data.frame(ID=1,
                                                                         T1=as.numeric(input$age),
                                                                         T2=as.numeric(input$age)+5,
                                                                         N_Biop=as.integer(input$biopsies),
                                                                         HypPlas=ifelse(as.integer(input$biopsies) == 0 |
                                                                                            as.integer(input$biopsies) == 99,
                                                                                        99,as.integer(input$hyperplasia)),
                                                                         AgeMen=as.numeric(input$menstruation),
                                                                         Age1st=as.numeric(input$first_birth),
                                                                         N_Rels=as.numeric(input$relatives),
                                                                         Race =as.integer(input$race))),1))
        })
        
        pred_lifetime <- reactive({as.numeric(round(absolute.risk(data.frame(ID=1,
                                                                             T1=as.numeric(input$age),
                                                                             T2=90,
                                                                             N_Biop=as.integer(input$biopsies),
                                                                             HypPlas=ifelse(as.integer(input$biopsies) == 0 |
                                                                                                as.integer(input$biopsies) == 99,
                                                                                            99,as.integer(input$hyperplasia)),
                                                                             AgeMen=as.numeric(input$menstruation),
                                                                             Age1st=as.numeric(input$first_birth),
                                                                             N_Rels=as.numeric(input$relatives),
                                                                             Race =as.integer(input$race))),1))
        })
        
        pred_avg_life <- reactive({as.numeric(round(absolute.risk(data.frame(ID=1,
                                                                             T1=as.numeric(input$age),
                                                                             T2=90,
                                                                             N_Biop=as.integer(input$biopsies),
                                                                             HypPlas=ifelse(as.integer(input$biopsies) == 0 |
                                                                                                as.integer(input$biopsies) == 99,
                                                                                            99,as.integer(input$hyperplasia)),
                                                                             AgeMen=as.numeric(input$menstruation),
                                                                             Age1st=as.numeric(input$first_birth),
                                                                             N_Rels=as.numeric(input$relatives),
                                                                             Race =as.integer(input$race))),1))
        })
        
        output$lifetime_pred <- eventReactive(input$do,{
            round(absolute.risk(data.frame(ID=1,
                                           T1=as.numeric(input$age),
                                           T2=90,
                                           N_Biop=as.integer(input$biopsies),
                                           HypPlas=ifelse(as.integer(input$biopsies) == 0 |
                                                              as.integer(input$biopsies) == 99,
                                                          99,as.integer(input$hyperplasia)),
                                           AgeMen=as.numeric(input$menstruation),
                                           Age1st=as.numeric(input$first_birth),
                                           N_Rels=as.numeric(input$relatives),
                                           Race =as.integer(input$race))),1)
        })
        output$`5_year_pred` <- eventReactive(input$do,{
            round(absolute.risk(data.frame(ID=1,
                                           T1=as.numeric(input$age),
                                           T2=as.numeric(input$age)+5,
                                           N_Biop=as.integer(input$biopsies),
                                           HypPlas=ifelse(as.integer(input$biopsies) == 0 |
                                                              as.integer(input$biopsies) == 99,
                                                          99,as.integer(input$hyperplasia)),
                                           AgeMen=as.numeric(input$menstruation),
                                           Age1st=as.numeric(input$first_birth),
                                           N_Rels=as.numeric(input$relatives),
                                           Race =as.integer(input$race))),1)
        })
        output$avg_five <- eventReactive(input$do,{
            round(absolute.risk(data.frame(ID=1,
                                           T1=as.numeric(input$age),
                                           T2=as.numeric(input$age)+5,
                                           N_Biop=as.integer(input$biopsies),
                                           HypPlas=ifelse(as.integer(input$biopsies) == 0 |
                                                              as.integer(input$biopsies) == 99,
                                                          99,as.integer(input$hyperplasia)),
                                           AgeMen=as.numeric(input$menstruation),
                                           Age1st=as.numeric(input$first_birth),
                                           N_Rels=as.numeric(input$relatives),
                                           Race =as.integer(input$race))),1)
        })
        output$avg_life <- eventReactive(input$do,{
            round(absolute.risk(data.frame(ID=1,
                                           T1=as.numeric(input$age),
                                           T2=90,
                                           N_Biop=as.integer(input$biopsies),
                                           HypPlas=ifelse(as.integer(input$biopsies) == 0 |
                                                              as.integer(input$biopsies) == 99,
                                                          99,as.integer(input$hyperplasia)),
                                           AgeMen=as.numeric(input$menstruation),
                                           Age1st=as.numeric(input$first_birth),
                                           N_Rels=as.numeric(input$relatives),
                                           Race =as.integer(input$race))),1)
        })
        
        
        observeEvent( eventExpr = input$do , handlerExpr = {
            output$plt1 <- flexdashboard::renderGauge({
                gauge(pred_five(),
                      min = 0,
                      max = 17/10,
                      symbol = '%',
                      label = paste("5-Year Risk"),
                      gaugeSectors(success = c(0,14/10),
                                   warning = c(1.4, 1.69),
                                   danger = c(17/10,100),
                                   colors = c("success","warning","danger")
                ))
            })
        })
        
        text_race <- reactive({deter_race(input$race)
            })
        
        observeEvent(eventExpr = input$do, handlerExpr = { 
            output$five_yr_text <- renderText({ 
            paste0("根据提供的信息，这名妇女患侵袭性疾病的风险估计在接下来的5年里，乳腺癌是 ", pred_five(), "% 的风险比例，", " 该国家中人口中平均 ", input$age,  " 岁的 " ,text_race(), " 女性占 ", pred_avg(), "%.",
                   " 女性在该国家或者区域的统计结果说明在接下来的5年里不患乳腺癌风险是 ", 100-pred_five(), "%.")
            })
        })
        
        observeEvent(eventExpr = input$do, handlerExpr = { 
            output$lifetime_text <- renderText({ 
                paste0("根据提供的信息，这名妇女患侵袭性疾病的风险估计
                   乳腺癌在她的一生中(到90岁)是 ",pred_lifetime() ,"% 的风险比例 ", " 该国家中人口中平均 ", input$age,  " 岁的 " ,text_race(), " 女性占 ", pred_avg_life(), "%.",
                   " 女性在该国家或者区域的统计结果说明在接下来生命中不患乳腺癌风险是 ", 100-pred_lifetime(), "%.")
            })
        })
        
        observeEvent(eventExpr = input$do, handlerExpr = { 
            output$five_yr_title <- renderText({ 
                "5年患癌风险"
            })
        })
        
        observeEvent(eventExpr = input$do, handlerExpr = { 
            output$lifetime_title <- renderText({ 
                "终身患癌"
            })
        })
        
        observeEvent(eventExpr = input$do, handlerExpr = { 
            output$advice_title <- renderText({ 
                "建议"
            })
        })
        
        observeEvent(eventExpr = input$do, handlerExpr = { 
            output$advice_text1 <- renderText({ 
				"有患乳腺癌风险增加的患者，即计算出的5年风险>1.7%，且年龄至少为35岁，是化疗预防的候选对象(如他莫昔芬或雷洛昔芬)。"

            })
        })
        
        observeEvent(eventExpr = input$do, handlerExpr = { 
            output$advice_text2 <- renderText({ 
                "腺癌风险升高(>1.7%)的患者应转诊给乳腺外科医生，讨论可能的风险降低干预措施。"
                
            })
        })
        
        observeEvent(eventExpr = input$do, handlerExpr = { 
            output$short_five <- renderText({ 
                paste0("<font size=\"4\" color=\"#ff8c00\"><b>", ">>>  ","</b></font>", "<font size=\"4\"><b>Your 5-Year Risk: ", pred_five(), "%</b></font>")
            })
        })
        
        observeEvent(eventExpr = input$do, handlerExpr = { 
            output$short_life <- renderText({ 
                paste0("<font size=\"4\" color=\"#ff8c00\"><b>", ">>>  ","</b></font>", "<font size=\"4\"><b>Your Lifetime Risk: ", pred_lifetime(), "%</b></font>")
            })
        })
        
        output$image <- renderImage({
            list(src = "www/question-mark-icon.png",
                 contentType = 'image/png',
                 width = 25,
                 height = 25,
                 style = "border-radius: 50%;cursor:hand;cursor:pointer")
        }, deleteFile = FALSE)
        
        observeEvent(input$image_click, {
            showModal(modalDialog(
                title = "帮助",
                HTML("<span style=color:#ff8c00;>问题 1:</span> 当前年龄 <br>
                     说明:患乳腺癌的风险随着年龄的增长而增加。<br><br>
                     <span style=color:#ff8c00;>问题 2:</span> 初潮年龄 <br>
                     说明:在很年轻的时候就开始月经的女性患乳腺癌的风险有轻微的增加，这可能与她们长期接触雌激素有关。 <br><br>
                     <span style=color:#ff8c00;>问题 3:</span> 初次生孩子年龄<br>
                     解释:风险取决于许多因素，包括首次生孩子的年龄和乳腺癌家族史。这两个因素的关系有助于确定风险。 <br><br>
                     <span style=color:#ff8c00;>问题 4:</span> 有多少直系亲属患有乳腺癌? <br>
                     解释:有一个或多个直系亲属(母亲、姐妹、女儿)患有乳腺癌会增加女性患乳腺癌的几率。<br><br>
                     <span style=color:#ff8c00;>问题 5:</span> 乳房活检的数量?<br>
                     <span style=color:#ff8c00;>问题 5.1:</span> 活检显示增生吗?<br>
                     说明:接受过乳房活检的妇女患乳腺癌的风险增加，特别是如果活检标本显示非典型增生。有过乳房活检史的女性风险更高，因为无论乳房发生什么变化都会促使活检。乳房活检本身不会导致癌症。 <br><br>
                     <span style=color:#ff8c00;>问题 6:</span> 种族 <br>
                     解释:最初的乳腺癌风险评估是基于白人女性的数据。但种族/民族会影响乳腺癌风险的计算。多年来，随着获得更多数据，研究人员更新了模型，以更准确地估计风险。"),
                size = "l",
                easyClose = TRUE,
                footer = modalButton("结束")
            ))
        })
        
    }
)