# Basic descriptive statistics and graphs for Freedom of Information
# Briefing
# Stats available here: https://www.gov.uk/government/collections/government-foi-statistics 
# April to June 2021 file used

library(tidyverse)

setwd("C:/Users/Greg/Documents/Placement/foi_stats")
foi <- read.csv("foi-statistics-q2-2021-published-data.csv")

# Remove commas 

foi$Total.requests.received <- gsub(pattern = ",",x = foi$Total.requests.received,
                                    replacement = "")

# Transform into numbers 

foi$total_req_n <- as.numeric(foi$Total.requests.received)
foi$total_res_pc <- as.numeric(foi$Percentage.of.resolvable.requests.granted.in.full)

# Find the quarterly totals

foi_q<- foi[grep("Q", foi$Quarter),]

yearly_total <- foi_q %>% 
  group_by(Quarter) %>% 
  summarise(pc= mean(total_res_pc, na.rm = T))

# Now reorder the totals 

strsplit(yearly_total$Quarter, split = " ")

yearly_total$year <- str_split_fixed(yearly_total$Quarter, pattern = " ", 2)[,2]
yearly_total$quarter <- str_split_fixed(yearly_total$Quarter, pattern = " ", 2)[,1]
yearly_total <- yearly_total[,-1]
yearly_total <- arrange(yearly_total, year, quarter)
yearly_total$time <- paste(yearly_total$year, yearly_total$quarter)

# Make the graph 

ggplot() +
  geom_line(data = yearly_total, aes(x = time, y = pc, group = 1),col = "blue") +
  annotate(geom = "text", x = 1 + 4 * (0:11), y = 27, 
           label = unique(yearly_total$year), size = 3, angle = 45, vjust = 1) +
  coord_cartesian(ylim = c(30, 70), expand = FALSE, clip = "off") +
  xlab("Year")+
  ylab("Percentage")+
  ggtitle("Figure 1:\nProportion of Requests Granted in Full")+
  theme_bw() +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        axis.text.x = element_blank(),
        axis.title.x = element_text(vjust = -10),
        axis.title.y = element_text(vjust = 2),
        plot.title = element_text(color="black", size=10, face="bold"),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# Do an annual sum based on their annual measures (no Q)

foi_an<- foi[-grep("Q", foi$Quarter),]
summariser <- function(x){
  tapply(as.numeric(x), foi_an$Quarter, sum)
}


# select relevant columns 

relevant<- c( "S.22....Information.intended.for.future.publication", 
           "S.22A....Research.intended.for.future.publication", "S.23....Information.supplied.by..or.relating.to..bodies.dealing.with.security.matters", 
           "S.24....National.security", "S.26....Defence", "S.27....International.relations", 
           "S.28....Relations.within.the.United.Kingdom", "S.29....The.economy", 
           "S.30....Investigations.and.proceedings.conducted.by.public.authorities", 
           "S.31....Law.enforcement", "S.32....Court.records..etc", "S.33....Audit.functions", 
           "S.34....Parliamentary.privilege", "S.35....Formulation.of.Government.policy..etc", 
           "S.36....Prejudice.to.effective.conduct.of.public.affairs", "S.37....Communications.with.Her.Majesty..etc.and.honours", 
           "S.38....Health.and.Safety", "S.39....Environmental.information", 
           "S.40....Personal.information", "S.41....Information.provided.in.confidence", 
           "S.42....Legal.professional.privilege", "S.43....Commercial.interests", 
           "S.44....Prohibitions.on.disclosure", "X3.a....Exempt.personal.data", 
           "X4.a....Information.not.held", "X4.b....Manifestly.unreasonable", 
           "X4.c....Too.general", "X4.d....Work.in.progress...incomplete.data", 
           "X4.e....Internal.communications", "X5.a....Adverse.effect.on.international.relations..defence..national.security..public.safety", 
           "X5.b....Adverse.effect.on.course.of.justice.or.conduct.of.enquiries", 
           "X5.c....Adverse.effect.on.intellectual.property.rights", "X5.d....Impinges.on.confidentiality.of.a.public.authority.s.work", 
           "X5.e....Impinges.on.confidentiality.of.commercial.or.industrial.information", 
           "X5.f....Adverse.effect.on.interests.of.person.who.provided.the.information", 
           "X5.g....Adverse.effect.on.protection.of.environment.to.which.information.relates",
           "Fully.refused...vexatious", "Fully.refused...repeated", "Fully.refused...cost.limit", 
           "Fully.refused...exemption")

# loop summariser through relevant columns

rm(sum_df)
sum_df <- as.data.frame(relevant)
sum_df$summaries <- NA

for (i in sum_df$relevant){
  sum_df$summaries[sum_df$relevant == i] <- mean(summariser(foi_an[,i]), na.rm = T)
}
