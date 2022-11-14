library(tidyverse)
library(rvest)
library(reactablefmtr)
library(reactable)
library(htmltools)
library(htmlwidgets)
library(sysfonts)
library(showtext)
library(webshot2)
library(dplyr)

#wikipedia data
url_monuments<-'https://en.wikipedia.org/wiki/Cycling_monument'

#scrape data with rvest
raw<-url_monuments%>%
  rvest::read_html()%>%
  rvest::html_elements(".wikitable")%>%
  .[1]%>%
  html_table()%>%
  .[[1]]%>%
  rename(MSR=2,ToF=3, PR=4, LBL=5, Lomb=6)

df<-raw%>%
  mutate(MSR = na_if(MSR, "Not contested"),
         ToF = na_if(ToF, "Not contested"),
         PR = na_if(PR, "Not contested"),
         LBL = na_if(LBL, "Not contested"),
         Lomb = na_if(Lomb, "Not contested")
         ) %>%
  slice(., 1:(n() - 1)) %>% #remove last uninformative row
  mutate(MSR = gsub("[/0-9()]", "", MSR),
         ToF = gsub("[/0-9()]", "", ToF),
         PR = gsub("[/0-9()]", "", PR),
         LBL = gsub("[/0-9()]", "", LBL),
         Lomb = gsub("[/0-9()]", "", Lomb)
         ) %>%
 # substr(rider,1,nchar(rider)-2) %>%
  filter(
    Year>2002)

#df$name <- str_sub(df$name, end = -2)



MSR<- df %>%
  count(MSR) %>%
  rename(rider=1,MSR=2)
ToF<- df %>%
  count(ToF) %>%
  rename(rider=1,ToF=2)
PR<- df %>%
  count(PR) %>%
  rename(rider=1,PR=2)
LBL<- df %>%
  count(LBL) %>%
  rename(rider=1,LBL=2)
Lomb<- df %>%
  count(Lomb) %>%
  rename(rider=1,Lomb=2)

count_monuments <- merge(x=MSR,y=ToF,
             by="rider", all.x=TRUE, all.y=TRUE)
count_monuments <- merge(x=count_monuments,y=PR,
                         by="rider", all.x=TRUE, all.y=TRUE)
count_monuments <- merge(x=count_monuments,y=LBL,
                         by="rider", all.x=TRUE, all.y=TRUE)
count_monuments <- merge(x=count_monuments,y=Lomb,
                         by="rider", all.x=TRUE, all.y=TRUE)

df_monuments <- count_monuments %>%
  replace(is.na(.), 0) %>%
  mutate(monuments = rowSums(across(where(is.numeric)))) %>%
  arrange(desc(monuments))%>%
  filter(monuments>2)%>%
  mutate(
    rank = dense_rank(desc(monuments))) %>%
    select(rank, rider, MSR, ToF, PR, LBL, Lomb, monuments)

df_monuments$rider = substr(df_monuments$rider, 1, nchar(df_monuments$rider)-5)

#add in region manually
df_monuments$region<-c("SUI","BEL","BEL","ES","ITA","ES","ITA","SLO","ITA")
df_monuments$region<-paste0("https://github.com/catamphetamine/country-flag-icons/tree/master/flags/1x1/",df_monuments$region,".svg")

#create custom color palette for scale fill
pal_scale<-c("#F4FFFD","#E9DAEC","#A270E5","#7814ff","#43009A")



#main body of reactable - note, I downloaded the Chivo font locally from Google Fonts first!
table<-reactable(df_monuments%>%select(rank, rider, region, MSR, ToF, PR, LBL, Lomb, monuments),
                 theme = reactableTheme(
                   style=list(fontFamily="Chivo"),
                   borderColor="#DADADA"
                 ),
                 defaultPageSize = 11,
                 defaultColDef = colDef(vAlign="center",
                                        align="center",
                                        headerVAlign="center",
                                        style = color_scales(df_monuments, span = 3:7, colors=pal_scale),
                                        headerStyle = list(fontFamily="Chivo"),
                                        width=90
                 ),
                 columnGroups = list(
                   colGroup(name="", columns=c("rider","region","monuments"), headerStyle = list(fontFamily="Chivo"), align="left"),
                   colGroup(name="Monument", columns=c("MSR", "ToF", "PR", "LBL", "Lomb"), headerStyle = list(fontFamily="Roboto"))
                 ),
                 columns = list(
                   rank = colDef(show=FALSE),
                   rider = colDef(name= "Rider",
                                   align="left", width=250,
                                   cell=function(value){
                                     image <- img(src = paste0("https://github.com/paublancoarnau/cycling_visualisations/tree/main/images_cyclists/",str_replace_all(tolower(value)," ","_"),".png"), style = "height: 33px;", alt = value)
                                     tagList(
                                       div(style = "display: inline-block;vertical-align:middle;width:50px", image),
                                       div(style="display: inline-block;vertical-align:middle;",
                                           div(style = "vertical-align:middle;", value))
                                     )}
                   ),
                   region = colDef(name="Country",
                                   align="left",
                                   cell=function(value, index){
                                     image <- img(src = value, style = "width:60px;height:20px;", alt = value)
                                     rider <- df_monuments$rider[index]
                                     if(rider %in% c("Tom Boonen")){
                                       tagList(div(style = "display:inline-block;vertical-align:middle;width:80px", image,"*")
                                               # div(style = "display:inline-block;", "*")
                                       )
                                     }
                                     else{
                                       tagList(div(style = "display:inline-block;vertical-align:middle;width:50px", image))
                                     }
                                   },
                                   width=120),
                   MSR = colDef(name="Milan San Remo"),
                   ToF = colDef(name="Tour of Flanders"),
                   PR = colDef(name="Paris -Roubaix"),
                   LBL = colDef(name="LBL"),
                   Lomb = colDef(name="Giro Lombardia",width=101),
                   monuments = colDef(name="Total Monuments",
                                   width=180,
                                   class = "border-left",
                                   align="left",
                                   cell = data_bars(df_monuments,
                                                    fill_color="#7814ff",
                                                    text_position = "outside-end",
                                                    bar_height = 10,
                                                    text_size = 10,
                                                    min_value=0,
                                                      max_value =10,
                                                    background = "transparent"))
                 )
)


#add title, subtitle, footnote and source
#note, I downloaded fonts locally - Chivo & Font Awesome Branded Icons
table_final<-table%>%
  #title & subtitle
  htmlwidgets::prependContent(
    tagList(
      tags$img(src = "https://pngimg.com/uploads/tennis/tennis_PNG10416.png", style = "width:50px;height:34px;display:inline-block;vertical-align:middle;"),
      #tags$h1("trophy  ",style="font-family:'Font Awesome 6 Free';margin-bottom:0;display:inline-block;vertical-align:middle;padding-right:10px;"),
      tags$div("5 Monuments Winners Legends", style="font-size:32px;font-weight:bold;font-family:Chivo;margin-bottom:0;display:inline-block;vertical-align:middle;"),
      tags$h3("Top Women's Tennis Players by Singles Championship Titles", style="font-family:Chivo;margin-bottom:0;margin-top:0;font-weight:400;color:#8C8C8C;padding-left:10px;")
    )
  )%>%
  #footnote and source
  htmlwidgets::appendContent(
    tags$div("* Retired cyclists.", style="font-family:Roboto;color:black;font-size:9pt;border-bottom-style:solid;border-top-style:solid;width:910px;padding-bottom:8px;padding-top:8px;border-color:#DADADA;"),
    tags$div(
      tags$div("Data: Wikipedia as of November 2022 | Graphic: ", style="display:inline-block;vertical-align:middle;"),
      tags$div("twitter", style="font-family:'Font Awesome 6 Brands';display:inline-block;vertical-align:middle;"),
      tags$div("paublancou", style="display:inline-block;vertical-align:middle;"),
      tags$div("github", style="font-family:'Font Awesome 6 Brands';display:inline-block;vertical-align:middle;"),
      tags$div("paublancoarnau", style="display:inline-block;vertical-align:middle;"),
      style="font-family:Chivo;color:#8C8C8C;font-size:10pt;width:910px;padding-top:8px;display:inline-block;vertical-align:middle;")
  )

#preview table
table_final


#option to save via webshot (you can also crop manually in browser)
html <- "monuments.html"
#save html file
#saveWidget(table_final, html)
#save as png static image
#webshot(html, "monuments.png", zoom=3, vwidth = 980)
