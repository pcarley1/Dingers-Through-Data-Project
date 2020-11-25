#Dingers Through Data Project

#LOADING PACKAGES

install.packages("ggrepel")
install.packages("overlap")
        library(tidyverse)
        library(sqldf)
        library(reshape2)
        library(readxl)
        library(ggrepel)
        library(overlap)
        setwd("C:/Users/peter.carley/Desktop/Other/Dingers Through Data")
        
        load("BATTING_DATA.Rda")
        load("PLAYER_DATA.Rda")

#Importing MLB Batting Data Set 
        BATTING_DATA<-
          read_csv("batting.csv")
        
        PLAYER_DATA<-
          read_csv("player.csv")
        
        MITCHELL_REPORT <- 
          read_excel("Mitchell Report Player List.xlsx")%>%
          mutate(`In Mitchell Report`="True")

#Joining Data Together 
        
        PLAYER_NAMES<-PLAYER_DATA %>%
          select(player_id, name_first, name_last) %>%
          mutate(FULL_NAME=paste(name_first,name_last,sep=" ")) %>%
          left_join(MITCHELL_REPORT, by=c("FULL_NAME"="Full Name"))
        
        JOINED_DATA<-
          BATTING_DATA%>%
          left_join(PLAYER_NAMES, by="player_id")
        

#Tidying MLB Batting Dataset
    
        TIDY_BATTING_DATA<-
        JOINED_DATA %>%
        filter(ab>100 & year>1940)%>%
        arrange(player_id,year)%>%
        group_by(player_id)%>%
        mutate(HR_PER_GAME= hr/g,RBI_PER_GAME=rbi/g, 
               PRIOR_YEAR_HR_PER_GAME=lag(HR_PER_GAME), 
               JUMP= HR_PER_GAME-PRIOR_YEAR_HR_PER_GAME,
               BATTING_AVG=h/ab)
        
        
        
    
#Exploratory Data Analysis
            
        #Exploring "Slugging" Variables
           HIST_HR_PER_GAME<-
             qplot(TIDY_BATTING_DATA$HR_PER_GAME, 
                  geom="histogram",
                  main="HR/Game Distribution",
                  xlab="HR Per Game",
                  ylab="Count of Players",
                  fill=I("blue"),
                  col=I("red"),
                  alpha=I(.2))
       
           HIST_RBI_PER_GAME<-
             qplot(TIDY_BATTING_DATA$RBI_PER_GAME, 
                   geom="histogram",
                   main="RBI/Game Distribution",
                   xlab="RBI Per Game",
                   ylab="Count of Players",
                   fill=I("blue"),
                   col=I("red"),
                   alpha=I(.2))
           HIST_JUMP_PER_GAME<-
             qplot(TIDY_BATTING_DATA$JUMP, 
                   geom="histogram",
                   main="Jump HR/Game Distribution",
                   xlab="YOY Increase in HR Per Game",
                   ylab="Count of Players",
                   fill=I("blue"),
                   col=I("red"),
                   alpha=I(.2))
           
           HIST_BATTING_AVERAGE<-
             qplot(TIDY_BATTING_DATA$BATTING_AVG, 
                   geom="histogram",
                   main="Batting Average Distribution",
                   xlab="Batting Average",
                   ylab="Count of Players",
                   fill=I("blue"),
                   col=I("red"),
                   alpha=I(.2))
           
      #Examining Changes Over Time
          #Mean, Max, Median, IQR Analysis
              RBI_PER_GAME_BY_YEAR<-
                TIDY_BATTING_DATA%>%
                ungroup(player_id)%>%
                arrange(year,player_id)%>%
                group_by(year)%>%
                summarize(mean_rbi_game=mean(RBI_PER_GAME), 
                          median_rbi_game=median(RBI_PER_GAME)) %>%
                gather(key="Figure",value="RBI_GAME", mean_rbi_game:median_rbi_game)
              
              HR_PER_GAME_BY_YEAR<-
                TIDY_BATTING_DATA%>%
                ungroup(player_id)%>%
                arrange(year,player_id)%>%
                group_by(year)%>%
                summarize(`Mean HR/Game`=mean(HR_PER_GAME),
                          `Median HR/Game`=median(HR_PER_GAME))%>%
                gather(key="Figure", value="HR_GAME", `Mean HR/Game`:`Median HR/Game`)

              BATTING_AVG_BY_YEAR<-
                TIDY_BATTING_DATA%>%
                ungroup(player_id)%>%
                arrange(year,player_id)%>%
                group_by(year)%>%
                summarize(`Mean Batting Avg`=mean(BATTING_AVG),
                          `Median Batting Avg`=median(BATTING_AVG))%>%
                gather(key="Figure", value="Batting Avg", `Mean Batting Avg`:`Median Batting Avg`)
      
    #Creating Visualizations For Changes Over Time                  
            #RBI_PER_GAME Over Time
                ggplot(data=RBI_PER_GAME_BY_YEAR,aes(year,RBI_GAME, col=Figure))+geom_point()+geom_smooth()+
                xlab("Year")+ ylab("RBI Per Game")+
                guides(shape = guide_legend(override.aes = list(size = 3)))+
                theme(
                  legend.direction = "vertical",
                  legend.title = element_text(color = "blue", size = 7),
                  legend.text = element_text(color = "blue", size=7))
           #HR_PER_GAME Over Time
                ggplot(data=HR_PER_GAME_BY_YEAR,aes(year,HR_GAME, col=Figure))+geom_point()+geom_smooth()+
                  xlab("Year")+ ylab("HR Per Game")+
                  guides(shape = guide_legend(override.aes = list(size = 3)))+
                  theme(
                    legend.direction = "vertical",
                    legend.title = element_text(color = "blue", size = 7),
                    legend.text = element_text(color = "blue", size=7))
    
            #Batting Average Over Time
                ggplot(data=BATTING_AVG_BY_YEAR,aes(year,`Batting Avg`, col=Figure))+geom_point()+geom_smooth()+
                  xlab("Year")+ ylab("Batting Average")+
                  guides(shape = guide_legend(override.aes = list(size = 3)))+
                  theme(
                    legend.direction = "vertical",
                    legend.title = element_text(color = "blue", size = 7),
                    legend.text = element_text(color = "blue", size=7))
                
    #Examining "Jumps" in HR Per Game
          JUMPS<-ggplot(data=TIDY_BATTING_DATA, mapping=aes(x=year,y=JUMP, color=`In Mitchell Report`))+ 
                    geom_point()+
                    labs(x="Year",y="YOY Jump in HR Per Game",title="YOY Jump in HR/Game By Year")
    
          JUMP_OUTLIERS<-mean(TIDY_BATTING_DATA$JUMP, na.rm=TRUE)+1.5*IQR(TIDY_BATTING_DATA$JUMP,na.rm=TRUE)
          
          HIGH_JUMPS<-
            TIDY_BATTING_DATA%>%
            filter(JUMP>JUMP_OUTLIERS)
           
          HIGH_JUMPS_IN_STEROID_ERA<-
            HIGH_JUMPS%>%
            filter(year>1989,year<2000)%>%
            distinct(FULL_NAME, `In Mitchell Report`)
            library(gridExtra)
            grid.table(HIGH_JUMPS_IN_STEROID_ERA)
            write_csv(HIGH_JUMPS_IN_STEROID_ERA,"HIGH_JUMPS_IN_STEROID_ERA.csv")
            
          HIGH_JUMPS_SCATTER<-
              ggplot(data = HIGH_JUMPS, aes(x = year, y = JUMP, color=`In Mitchell Report`)) +
                geom_point()+xlim(1980,2010)+
                geom_text(aes(label=ifelse(`In Mitchell Report`=="True" | JUMP>.18, FULL_NAME, '')), 
                          position=position_dodge(width = 0.5), 
                          vjust=-0.40, hjust=1, size=3, check_overlap=TRUE)+
                ggtitle("Jump in Home Runs Per Game By Year")+
                xlab("Year")+
                ylab("Jump in HR/Game")
            
          
        
    #Examining HR Per Game
          

          HR_PER_GAME_OUTLIERS<-
            mean(TIDY_BATTING_DATA$HR_PER_GAME,na.rm=TRUE)+1.5*IQR(TIDY_BATTING_DATA$HR_PER_GAME, na.rm=TRUE)
                        
              
          BIG_HR_HITTERS<-
            TIDY_BATTING_DATA%>%
            filter(HR_PER_GAME>HR_PER_GAME_OUTLIERS)
          
          BIG_HR_HITTERS_BY_YEAR<-
            BIG_HR_HITTERS%>%
            ungroup(player_id)%>%
            count(year)

          #Scatterplot
          
          ggplot(data=BIG_HR_HITTERS_BY_YEAR, mapping=aes(year,n, color="Red"))+
            geom_point()+
            labs(x="Year",y="Number of Players with Over .2 HR/Game", title="Big Home Run Hitters By Year")+
            theme_bw(base_size=16)+ theme(legend.position="none")
            
 
    #Comparing Distributions of HR/Game in 90's vs. Before
          
          BATTING_DATA_BY_ERA<-
            TIDY_BATTING_DATA%>%
            mutate(time_frame=ifelse(year>=1990 & year<=2005, "Steroid Era (1990-2005)","Pre/Post Era"))

          
            BATTING_DATA_BY_DECADE<-
              TIDY_BATTING_DATA%>%
              mutate(decade=
                            ifelse(year>=1940 & year<1950,"1940s",
                            ifelse(year>=1950 & year<1960,"1950s",
                            ifelse(year>=1960 & year<1970,"1960s",
                            ifelse(year>=1970 & year<1980,"1970s",
                            ifelse(year>=1980 & year<1990,"1980s",
                            ifelse(year>=1990 & year<2000,"1990s",
                            ifelse(year>=2000 & year<2010,"2000s",
                            ifelse(year>=2010 & year<2020,"2010s",
                                   ""))))))))) %>%
              filter(decade %in% c("1980s","1990s","2000s","2010s"))

    #Making Density Curves Based on the Two Eras
         
          ggplot(BATTING_DATA_BY_ERA,aes(x=HR_PER_GAME, fill=time_frame)) + geom_density(alpha=0.25)+
            labs(x="Home Runs/Game", y="Percentage of Players", title="Home Runs Per Game by Era")
          
          ggplot(BATTING_DATA_BY_ERA,aes(x=RBI_PER_GAME, fill=time_frame)) + geom_density(alpha=0.25)+
            labs(x="RBI/Game", y="Percentage of Players", title="RBI Per Game by Era")
          
          ggplot(BATTING_DATA_BY_ERA,aes(x=BATTING_AVG, fill=time_frame)) + geom_density(alpha=0.25)+
            labs(x="Batting Average", y="Percentage of Players", title="Batting Average by Era")
          
          ggplot(BATTING_DATA_BY_ERA,aes(x=JUMP, fill=time_frame)) + geom_density(alpha=0.25, na.rm=TRUE)+
            labs(x="Jump in YOY HRs Per Game", y="Percentage of Players", title="Jump in HR/Game Distribution by Era")
          
          
          ggplot(BATTING_DATA_BY_DECADE, aes(x=HR_PER_GAME, fill=decade))+geom_density(alpha=.35)
          
          ggplot(BATTING_DATA_BY_DECADE, aes(x=HR_PER_GAME, fill=decade)) + geom_histogram(alpha=0.25)
      
          
    #List of Players with Consistently Over .2 HRs/Game
          BIG_HR_HITTERS_SCATTER<-
            ggplot(BIG_HR_HITTERS, aes(year, HR_PER_GAME, color=`In Mitchell Report`))+geom_point()+
            geom_text(aes(label=ifelse(HR_PER_GAME>.3, FULL_NAME, '')), 
                      position=position_dodge(width = 0.5), 
                      vjust=-0.40, hjust=1, size=3, check_overlap=TRUE)+
            labs(title="Outlier Home Runs Per Game By Year", x="Year",y="Home Runs Per Game")+
            theme_bw(base_size=12)+ theme(legend.position="none")
        
          count_of_big_hr_hitters_in_2000<-
           BIG_HR_HITTERS%>%
           filter(year==2000)%>%
           select(FULL_NAME,`In Mitchell Report`)%>%
           mutate(`In Mitchell Report`=ifelse(is.na(`In Mitchell Report`),"False",`In Mitchell Report`))
          
          write_csv(count_of_big_hr_hitters_in_2000, "Count of Big HR Hitters in 2000.csv")
        
         try_this<-
         count_of_big_hr_hitters_in_2000%>% 
           ungroup(player_id)
         
         ggplot(try_this, aes(x="", y=`In Mitchell Report`, fill=`In Mitchell Report`)) +
           geom_bar(stat="identity", width=1) +
           coord_polar("y", start=0)+
           labs(title= "Players Who Averaged Over .2 HRs/Game in 2000")
          
          
    #Pulling Data for Players with High Jumps in HR/Game, and Outlier-Level HR/Game
        
        JUMPER_IDS<-unique(HIGH_JUMPS$player_id)
        HOMER_IDS<-unique(BIG_HR_HITTERS$player_id)
        
        JUMPERS_AND_SLUGGERS<-
          TIDY_BATTING_DATA%>%
          filter(HR_PER_GAME>.21 & JUMP>.08)
        
        JUMPERS_AND_SLUGGERS_IN_STEROID_ERA<-
          JUMPERS_AND_SLUGGERS%>%
          filter(year>=1990 & year<=2005)%>%
          ungroup(player_id)%>%
          select(FULL_NAME,year,JUMP,HR_PER_GAME,hr,`In Mitchell Report`)
         
        write_csv(JUMPERS_AND_SLUGGERS_IN_STEROID_ERA,"Jumpers and Sluggers in Steroid Era.csv")
        
        png("trying_this_pic.png", height=5000, width=3000)
        p<-tableGrob(JUMPERS_AND_SLUGGERS_IN_STEROID_ERA)
        grid.arrange(p)
        dev.off()
        

        JUMPERS_AND_SLUGGERS_BY_YEAR<-
          JUMPERS_AND_SLUGGERS%>%
          ungroup(player_id)%>%
          count(year)
        
        JUMPERS_AND_SLUGGERS_BY_YEAR_CHART<-
          ggplot(data=JUMPERS_AND_SLUGGERS_BY_YEAR, mapping=aes(x=year,y=n))+
          geom_point()+
          ggtitle("Outliers in YOY Jumps and HR/Game")+
          ylab("Number of Players")
        


  
        