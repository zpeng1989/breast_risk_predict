library(shiny)
library(flexdashboard)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme('superhero'),
    headerPanel('腺癌风险评估'),
    fluidRow(
    sidebarPanel(
        fluidRow(column(10, h2('查表')),
                 column(2, tags$br(), 
                        imageOutput('image', height = 25, width = 25,
                                    click = clickOpts(id = 'image_click')),
                        tags$br(), tags$br())),
        
        numericInput('age','当前年龄', 35, min=35, max=85, step = 1),
        
       selectInput('menstruation','初潮年龄',
                    choices = list("小于12岁" = 11,
                                   "12 - 13 岁" = 12,
                                   "大于 13 岁" = 14,
                                   "未知" = 99), selected = 99),
        
        selectInput('first_birth','第一次生育',
                    choices = list("无生育" = 98,
                                   "小于 20 岁"= 19,
                                   "20 - 24 岁" = 20,
                                   "25 - 29 岁" = 25,
                                   "30 岁"= 30,
                                   "未知" = 99), selected = 99),
        
        selectInput('relatives','患有乳腺癌的一级亲属人数',
                    choices = list("未知" = 99,
                                   "0 个亲属" = 0,
                                   "1 个亲属" = 1,
                                   "2 个或者更多亲属" = 2), selected = 99),
        
        selectInput('biopsies', '乳房活检次数',
                    choices = list("未知" = 99,
                                   "没有活检" = 0,
                                   "1 次活检" = 1,
                                   "2 次或者更多次活检" = 2), selected = 99),
        
        conditionalPanel(
            condition = 'input.biopsies == 1 || input.biopsies == 2',
            selectInput("hyperplasia", "活检显示增生吗",
                        choices = list("没有" = 0,
                                       "有" = 1,
                                       "未知" = 99), selected = 99)),
        
        selectInput('race','种族划分',
                    choices = list("白种人" = 1,
                                   "非洲裔美国人（指美国黑人）" = 2,
                                   "拉丁裔美国人" = 3,
                                   "其他(美洲土著和未知种族)" = 4,
                                   "华裔美国人" = 6,
                                   "日裔美国人" = 7,
                                   "菲律宾裔美国人" = 8,
                                   "夏威夷美国人" = 9,
                                   "其他太平洋附近人" = 10,
                                   "亚洲人" = 11), selected = 1),
        
        actionButton('do','计算风险', class = 'btn-primary')
        
    ),
    
    mainPanel(
        h3('你的结果'),
        tags$hr(),
        span(textOutput('five_yr_title'), style='font-size: 20px'),
        textOutput('five_yr_text'),
        tags$br(),
        span(textOutput('lifetime_title'), style='font-size: 20px'),
        textOutput('lifetime_text'),
        tags$hr(),
        fluidRow(
            column(7
                   , fluidRow(
                       column(12, span(textOutput('advice_title'), style='font-size: 20px'),
                              tags$hr(width = '50%', align = 'left'))
                   )
                   , fluidRow(
                       column(12, textOutput('advice_text1'), tags$br(),
                              textOutput('advice_text2'))
                   )
            )
            , column(5, tags$br(), htmlOutput('short_five'), htmlOutput('short_life'), tags$br(), gaugeOutput('plt1'))
        )
    )),
    
    fluidRow(
        tags$hr(),
        tags$div(class = 'footer',('我好厉害 - 哈哈 - '), style = 'bottom:0',
                 tags$a(
					 ref = '',
                     target='_blank',
                     'Zpeng'),
                 align = 'center')
        
    )
    
)

)