#Q1. Distribution of Player age in Each League.[How to show mean in the graph]
#Q2. How rich a league is(Graph of that)
#Q3. Country based no of players.
#Q4. Player Specific Statistics Comparison
#Q5. Selecting a player and position-wise his stat[Using faceting but
# how can I achieve different colours and give particular position a particular place in graph]
#Q5. How fit a player is[Relative to Body Mass Index]
#Q6. Divide Data-set into player types(like, Goalkeeper, Midfielder, Forward , Winger)
#Q7. Select a club and Ovr vs Potential comparison of all players
#Q8. Best players in each position(without repetition) and their top 6 best stats
#Q9. Age vs wage [League wise]
#Q10. Top Young sensations  
#Q11. Position vs Strong Foot



library(dplyr)



library(tidyverse)
library(plotly)
library(maps)
library(ggcorrplot)
library(infer)

f21 <- read.csv("players_21.csv")

f21 <- read.csv("players_21.csv", na.strings = c("", "NA"))
f21[,2]
f21 <- f21[,-2]
f21 <- f21[,c(-22,-23)]
f21 <- f21[,-23]
f21 <- f21[,-25]
f21 <- f21[,-41]
levels(factor(f21$club_name))
length(factor(f21$club_name))
f21 %>% group_by(club_name== "Barcelona")  
levels(factor(f21$overall))
levels(factor(f21$potential))

head(strsplit(f21$player_positions, split = ","))
levels(factor(strsplit(f21$player_positions, split = ",")))
head(f21[,c(41:45)])
f21[,c(41:45)] %>% colnames() %>% strsplit(.,split = "_")








f21 <- f21 %>% select(-gk_diving,-gk_handling,-gk_kicking,-gk_reflexes,-gk_speed,-gk_positioning)
f21 <- f21 %>% select(-defending_marking)
f21 <- f21 %>% select(-sofifa_id)

f21 <- f21 %>% rename(Crossing=attacking_crossing        , Finishing=attacking_finishing ,      
                HeadingAccuracy=attacking_heading_accuracy, ShortPassing=attacking_short_passing  ,  Volleys=attacking_volleys  ,       
                Dribbling=skill_dribbling         ,   Curve=skill_curve     ,           FkAccuracy=skill_fk_accuracy    ,     
                LongPassing=skill_long_passing    ,     Ball_Control=skill_ball_control     ,    Acceleration=movement_acceleration   ,  
                SprintSpeed=movement_sprint_speed   ,   Agility=movement_agility    ,       Reactions=movement_reactions    ,    
                Balance=movement_balance    ,       ShotPower=power_shot_power    ,       Jumping=power_jumping   ,          
                Stamina=power_stamina   ,           Strength=power_strength   ,          LongShot=power_long_shots    ,      
                Aggression=mentality_aggression   ,    Interception=mentality_interceptions   , Positioning=mentality_positioning   ,  
                Vision=mentality_vision   ,        Penalty=mentality_penalties    ,    Composure=mentality_composure    ,   
                StandingTackle=defending_standing_tackle  ,    SlidingTackle=defending_sliding_tackle   ,   GK_Diving=goalkeeping_diving    ,    
                GK_Handling=goalkeeping_handling   ,    GK_Kicking=goalkeeping_kicking    ,    GK_Positioning=goalkeeping_positioning   ,
                GK_Reflexes=goalkeeping_reflexes)

#nationality wise some selection
France <- subset(f21,f21$nationality=="France")
India <- subset(f21,f21$nationality=="India")
Argentina <- subset(f21,f21$nationality=="Argentina")
Portugal <- subset(f21,f21$nationality=="Portugal")
Brazil <- subset(f21,f21$nationality=="Brazil")
Spain <- subset(f21,f21$nationality=="Spain")
Germany <- subset(f21,f21$nationality=="Germany")




#club wise some selection

Barcelona <- subset(f21,f21$club_name=="FC Barcelona")
BMunich <- subset(f21,f21$club_name=="FC Bayern München")
Juventus <- subset(f21,f21$club_name=="Juventus")







#League wise some selection

La_Liga <- subset(f21,f21$league_name=="Spain Primera Division")
Serie_A <- subset(f21,f21$league_name=="Italian Serie A")
Bundesliga <- subset(f21,f21$league_name=="German 1. Bundesliga")
Ligue_1 <- subset(f21,f21$league_name=="French Ligue 1")
EPL <- subset(f21,f21$league_name=="English Premier League")




#Small Data Preparation

df <- subset(f21, f21$league_name=="Spain Primera Division"|f21$league_name=="Italian Serie A"
             |f21$league_name=="German 1. Bundesliga"|f21$league_name=="French Ligue 1"
             |f21$league_name=="English Premier League"|f21$league_name=="Holland Eredivisie")

#Distribution and the Average Age of The Players in each League

summ <- df %>% 
  group_by(league_name) %>% 
  summarise(Age = mean(age))



options(repr.plot.width = 12, repr.plot.height = 8)

ggplot()+
  geom_histogram(df, mapping = aes(age, fill = league_name))+
  geom_vline(summ, mapping = aes(xintercept = Age), color = "red", size = 1.5)+
  geom_text(summ, mapping = aes(x = Age+3, y = 65, label = round(Age,digits = 2)))+
  facet_wrap(league_name~.)+
  theme_minimal()+
  theme(legend.position = "bottom")+
  labs(y = "Frequency", title = "Distribution & The Average Age of The Players in each League", 
       caption = "@EA Sports - FIFA 21")







#Interactive World Map & Number of Player



options(repr.plot.width = 12, repr.plot.height = 8)

world_map <- map_data("world")

numofplayers <- world_map %>% 
  mutate(region = as.character(region)) %>% 
  left_join((f21 %>% mutate(nationality = as.character(nationality),
                           nationality = if_else(nationality %in% "England", 
                                                 "UK", nationality)) %>%
               count(nationality, name = "Number of Player") %>%
               rename(region = nationality) %>%
               mutate(region = as.character(region))), by = "region")


ggplot(numofplayers, aes(long, lat, group = group))+
  geom_polygon(aes(fill = factor(`Number of Player`) ), color = "grey", show.legend = F)+
  scale_fill_viridis_d(option = "D")+
  theme_void()+
  labs(fill = "Number of Player",
       title = "Players enlisted from countries")




#Players from Barcelona



options(repr.plot.width = 12, repr.plot.height = 8)

Barcelona %>% 
  select(short_name, overall, potential) %>% 
  arrange(-overall) %>% 
  head(15) %>% 
  gather(variable, Exp, -short_name) %>% 
  ggplot(aes(short_name, Exp, fill = variable))+
  geom_col(position = "dodge")+
  geom_text(aes(label = Exp),position = position_dodge(width = 0.9), vjust = -0.5)+
  scale_fill_manual(values = c("#004D98", "#A50044"))+
  theme_minimal()+
  theme(legend.position = "bottom")+
  labs(fill = NULL, x = NULL, title = "Barcelona")+
  theme(axis.text.x = element_text(face="bold",angle = 90, vjust = 0.5, hjust=1))





# Brazil Player

options(repr.plot.width = 12, repr.plot.height = 8)

Brazil %>% 
  select(short_name, overall, potential) %>% 
  arrange(-overall) %>% 
  head(15) %>% 
  gather(variable, Exp, -short_name) %>% 
  ggplot(aes(short_name, Exp, fill = variable))+
  geom_col(position = "dodge")+
  geom_text(aes(label = Exp),position = position_dodge(width = 0.9), vjust = -0.5)+
  scale_fill_manual(values = c("#009c3b", "#ffdf00"))+
  theme_minimal()+
  theme(legend.position = "bottom")+
  labs(fill = NULL, x = NULL, title = "Brazil")+
  theme(axis.text.x = element_text(face="bold",angle = 90, vjust = 0.5, hjust=1))






#Messi And Ronaldo


MR <- rbind(f21[1,],f21[2,])

player<-   MR %>% mutate(Name= paste0(short_name,",", club_name))%>% select(Name,pace:Composure)%>% 
  gather(Skill,Exp,pace:'Composure',-Name)

options(repr.plot.width = 15,repr.plot.height = 8)


ggplot(player,aes(Skill,Exp,fill=Name))+ geom_col(position = "fill")+
  coord_flip()+ scale_fill_manual(values = c("#ff0000","#75AADB"))+theme_minimal()+
  geom_hline(yintercept = 0.5,color="yellow",size=0.5,linetype=2)+
  theme(legend.position = "top",axis.text.y = element_text(face = "bold"),axis.text.x = element_blank())+
  labs(title = "Ronaldo vs Messi")



#La liga native and foreign player 
L_NAT <- La_Liga %>% mutate(Nationality=as.character(nationality),
                   Nationality = if_else(nationality %in% "Spain","Native","Foreigner"))
                   
ggplot(L_NAT)+geom_bar(aes(x=Nationality,fill= Nationality),show.legend = F)+
  facet_wrap(club_name~.)+labs(title = "La Liga Native and Foreigner player")

######################
EPL_NAT <- EPL %>% mutate(Nationality=as.character(nationality),
                            Nationality = if_else(nationality %in% "England","Native","Foreigner"))

ggplot(EPL_NAT)+geom_bar(aes(x=Nationality,fill= Nationality),show.legend = F)+
  facet_wrap(club_name~.)+labs(title = "EPL Native and Foreigner player")

######################
Bund_NAT <- Bundesliga %>% mutate(Nationality=as.character(nationality),
                            Nationality = if_else(nationality %in% "Germany","Native","Foreigner"))

ggplot(Bund_NAT)+geom_bar(aes(x=Nationality,fill= Nationality),show.legend = F)+
  facet_wrap(club_name~.,nrow = 3)+labs(title = "Bundesliga Native and Foreigner player")





#######
#Distribution of players in the whole fifa data

f21$player_positionsb <- sub("\\,.*","",f21$player_positions)


options(repr.plot.width = 15,repr.plot.height = 8)


f21 %>% drop_na(player_positionsb)%>%
  ggplot()+geom_bar(aes(x=player_positionsb,fill=player_positionsb),show.legend = F)+
  labs(title = "Player position distribution in the World")


#Distribution in some top leagues

df$player_positionsb <- sub("\\,.*","",df$player_positions)


options(repr.plot.width = 15,repr.plot.height = 8)


df %>% drop_na(player_positionsb)%>%
  ggplot()+geom_bar(aes(y=reorder(player_positionsb,player_positionsb, function(x) tapply(x,x,length)),fill=player_positionsb),show.legend = F)+
  facet_wrap(league_name~.,strip.position = "top")+
  labs(title="League wise Player position distribution")+xlab("Count")+ylab("Positions")





#Positional players
#attacking = ST,CF
#midfielder= CAM,CDM,CM,LM,RM
#winger= LW, RW
#full back= LB,LWB,RB,RWB
#defender= CB
#GoalKeeper= GK



Pos_check<- function(x){
  if_else(x %in% c("CAM","CDM","CM","LM","RM"),"Midfielder",
          if_else(x %in% c("LB","LWB","RWB","RB"),"Full Back",
                  if_else(x %in% c("LW","RW"),"Winger",
                          if_else(x %in% "CB","Defender",
                                  if_else(x %in% c("ST","CF"),"Forward","Goal Keeper")))))
}



 
fpos <- f21 %>% mutate(Pos=as.character(player_positionsb),
                      Pos = Pos_check(Pos))

dfpos <- df %>% mutate(Pos=as.character(player_positionsb),
                        Pos = Pos_check(Pos))



subset(subset(fpos,Pos=="Forward"),overall>=85)[,1]

subset(fpos,Pos=="Forward") %>% arrange(desc(overall))%>%head(20)%>%
  ggplot(aes(x=overall,y=reorder(short_name,overall)))+geom_col(aes(fill=short_name),show.legend = F)+
  labs(x="Overall",y="Name",title = "Top 20 Forwards in the World")

subset(fpos,Pos=="Winger") %>% arrange(desc(overall))%>%head(20)%>%
  ggplot(aes(x=overall,y=reorder(short_name,overall)))+geom_col(aes(fill=short_name),show.legend = F)+
  labs(x="Overall",y="Name",title = "Top 20 Wingers in the World")

subset(fpos,Pos=="Defender") %>% arrange(desc(overall))%>%head(20)%>%
  ggplot(aes(x=overall,y=reorder(short_name,overall)))+geom_col(aes(fill=short_name),show.legend = F)+
  labs(x="Overall",y="Name",title = "Top 20 Defenders in the World")

subset(fpos,Pos=="Midfielder") %>% arrange(desc(overall))%>%head(20)%>%
  ggplot(aes(x=overall,y=reorder(short_name,overall)))+geom_col(aes(fill=short_name),show.legend = F)+
  labs(x="Overall",y="Name",title = "Top 20 Midfielders in the World")

subset(fpos,Pos=="Full Back") %>% arrange(desc(overall))%>%head(20)%>%
  ggplot(aes(x=overall,y=reorder(short_name,overall)))+geom_col(aes(fill=short_name),show.legend = F)+
  labs(x="Overall",y="Name",title = "Top 20 Full Backs in the World")

subset(fpos,Pos=="Goal Keeper") %>% arrange(desc(overall))%>%head(20)%>%
  ggplot(aes(x=overall,y=reorder(short_name,overall)))+geom_col(aes(fill=short_name),show.legend = F)+
  labs(x="Overall",y="Name",title = "Top 20 Goal Keepers in the World")








#Most powerful clubs

powerful<-  fpos %>%
   group_by(club_name) %>%
   summarise(mean=mean(overall)) %>%
   arrange(-mean) %>%
   head(20)


fpos %>%
  group_by(club_name,Pos) %>%
  summarise(mean=mean(overall)) %>%
  ungroup() %>% 
  filter(club_name %in% powerful$club_name) %>%
  ggplot(aes(reorder(club_name,mean),mean,fill= Pos))+
  geom_col(position = "fill")+
  geom_text(aes(label = round(mean,digits = 2)),position = position_fill(0.5),size=3.5)+
  coord_flip()+
  theme_minimal()+
  theme(legend.position = "top",axis.text.y = element_text(face = "bold"),axis.text.x = element_blank())+
  labs(x="",y="",title = "Top 20 powerful clubs with their position class")



##Wonder Kid

fpos %>% filter(age<20, potential>72) %>%
  arrange(-potential) %>%
  group_by(age)%>%
  do(head(.,10)) %>%
  ggplot(aes(reorder(paste0(paste(short_name,player_positionsb, sep = ", "),"(",club_name,
                            ")"),potential),potential,fill=as.factor(age)))+
  geom_col(show.legend = F)+
  coord_flip()+
  facet_wrap(age~.,scales = "free")+
  labs(x="",y="Potential",title = "Age-wise Wonder-Kids",subtitle = "Amongst some might become star")



######
subset(f21,team_jersey_number==10) %>% arrange(desc((overall+potential)/2)) %>%
  select(short_name,club_name,player_positionsb)%>% head(20)


###########
numcol <-cbind(f21$age,f21$height_cm,f21$weight_kg,f21$overall,f21$value_eur,f21$wage_eur,f21$release_clause_eur)
colnames(numcol) <- c("Age","Height","Weight","Ovr","Valuation","Wage","Release_Clause")
numcol<-as.data.frame(numcol)
cor(numcol)
ggcorrplot(cor(numcol))

ggplot(df,aes(overall,wage_eur))+
  geom_hex(bins=60)+
  facet_wrap(league_name~.,scales = "free")+
  scale_fill_viridis_c()+
  geom_smooth(method = "loess")+
  theme_minimal()

ggplot(df,aes(age,wage_eur))+
  geom_hex(bins=60)+
  facet_wrap(league_name~.,scales = "free")+
  scale_fill_viridis_c()+
  geom_smooth(method = "loess")+
  theme_minimal()

ggplot(df,aes(overall,value_eur))+
  geom_hex(bins=60)+
  facet_wrap(league_name~.,scales = "free")+
  scale_fill_viridis_c()+
  geom_smooth(method = "loess")+
  theme_minimal()

ggplot(df,aes(age,value_eur))+
  geom_hex(bins=60)+
  facet_wrap(league_name~.,scales = "free")+
  scale_fill_viridis_c()+
  geom_smooth(method = "loess")+
  theme_minimal()
##################


subset(dfpos,player_positionsb=="GK") %>% arrange(-value_eur) %>% head(1) %>% select(short_name)
subset(dfpos,player_positionsb=="CB") %>% arrange(-value_eur) %>% head(2) %>% select(short_name)
subset(dfpos,player_positionsb=="LB") %>% arrange(-value_eur) %>% head(1) %>% select(short_name)
subset(dfpos,player_positionsb=="RB") %>% arrange(-value_eur) %>% head(1) %>% select(short_name)
subset(dfpos,player_positionsb=="CM") %>% arrange(-value_eur) %>% head(2) %>% select(short_name)
subset(dfpos,player_positionsb=="LW") %>% arrange(-value_eur) %>% head(1) %>% select(short_name)
subset(dfpos,player_positionsb=="RW") %>% arrange(-value_eur) %>% head(1) %>% select(short_name)
subset(dfpos,player_positionsb=="ST") %>% arrange(-value_eur) %>% head(1) %>% select(short_name)


#####################3
d2 <- data.frame(   x=c(0, 0, 16.5, 100, 100,83.5), 
                    xend=c(16.5,16.5, 16.5, 83.5,83.5,83.5),
                    y=rep(c(13.68, 61.32, 13.68),2), 
                    yend=rep(c(13.68,61.32,61.32),2))



pp <- data.frame(   x=c(0,16.5,16.5,25,25,50,50,75,75,87.5),
                    y=c(37.5,13.68,61.32,0,75,18.75,56.25,0,75,37.5),
                    name=c("J. Oblak"," V. van Dijk","A. Laporte",
                           "T. Alexander-Arnold","A. Robertson",
                           "T. Kroos","F. de Jong",
                           "M. Salah","Neymar Jr",
                           "K. Mbappé"))


pp


p<- ggplot(dfpos)+
  xlim(0,100)+ylim(0,75)+
  geom_vline(xintercept = c(0,50,100), color="white") + 
  geom_segment(data = d2,aes(x=x, xend=xend, y=y,yend=yend), color="white") +
  geom_point(aes(x=50,y=75/2), size=2, color="white") +
  geom_point(data=pp,aes(x=x,y=y), size=7, color="orange")+
  geom_text(data=pp,aes(x=x,y=y,label = name),size=5)+
  theme(panel.background = element_rect(fill = "darkgreen"),
        panel.grid = element_line(colour = "darkgreen"))+
  labs(title = "Most Expensive team possible in Fifa 21",subtitle = "With most recent player valuation")+
  xlab("")+ylab("")


