library(maps)
library(mapproj)
    
shinyServer(
  function(input, output) {
    output$myplot <- renderPlot({
        load <- function()
        {
            # d = read.csv('./data/2014.csv', header=T)
            d = read.csv('./data/2015-1-to-5.csv', header=T)
        }

        clean <- function(d, start.offset, end.offset, activities)
        {
            print(activities)

            chars = as.character(d$Activity.type)
            chars = revalue(chars, c('Class Work'='Classwork',
                                   'Errands '='Errand',
                                   'Personal care '='Personal Care',
                                   'RA'='RA work',
                                   'Self improvement '='Self-Impr'))
            d$Activity.type = chars

            # get date
            setyear = 2014
            d$From <- as.Date(paste(setyear, d$From), "%Y %b %d %I:%M %p")
            d$From_Day <- format(d$From, format="%Y-%m-%d")
            
            d$To   <- as.Date(paste(setyear, d$To),   "$Y %b %d %I:%M %p")
            d$To_Day <- format(d$To, format="%Y-%m-%d")

            d <- ddply(d, .(From_Day, Activity.type), 
                       function(x){ return(data.frame(Duration=sum(x$Duration)))})
            
            timeorigin = '1970-1-1'

            d$dayoffset = as.numeric(as.Date(d$From_Day) - as.Date(timeorigin))

            d = subset(d, dayoffset > start.offset & dayoffset < end.offset)
            d = subset(d, Activity.type %in% activities)


            return(d)
        }

        func <- function(d)
        {
            p = ggplot(d, aes(x=From_Day, y=Duration, fill=Activity.type))+
                geom_bar( position='stack', stat="identity" ) +
                theme_bw() +
                theme(axis.text.x=element_text(angle=90, vjust=0.5)) +
                ylab('Duration (Hr)')
            print(p)
        }

        do_main <- function()
        {
            d = load()
            d = clean(  d,
                        start.offset = input$dayrange[1],
                        end.offset = input$dayrange[2],
                        activities = input$checkGroup 
                      )
            func(d)
        }
        do_main()
    })

    output$day.sel <- renderUI({
        load <- function()
        {
            d = read.csv('./data/2014.csv', header=T)
        }
        d = load()

        setyear = 2014
        print(head(d$From))
        d$From <- as.Date(paste(setyear, as.character(d$From)), "%Y %b %d %I:%M %p")
        d$To   <- as.Date(paste(setyear, as.character(d$To)),   "%Y %b %d %I:%M %p")

        firstday = min(d$From)
        firstdayoff = as.numeric(firstday - as.Date('1970-1-1'))
        lastday = max(d$To)

        rangesize = as.numeric(lastday - firstday)
        cat('rangesize', firstday, lastday, rangesize)

        sliderInput("dayrange", 
            label = "Day Range:",
            min = firstdayoff, 
            max = firstdayoff + rangesize, 
            value = c(firstdayoff, firstdayoff + rangesize))
    })

    output$act.sel <- renderUI({
        load <- function()
        {
            d = read.csv('./data/2014.csv', header=T)
        }
        d = load()
        acts = unique(as.character(d$Activity.type))

        checkboxGroupInput("checkGroup", 
            label = h3("Checkbox group"), 
            #choices = list("Choice 1" = 1, 
               #"Choice 2" = 2, "Choice 3" = 3),
            choices = acts,
            selected = 'RA work' 
            )
    })


  }
)



