setwd("~/Desktop/Vizathon")
library(readxl)
library(ggplot2)
library(stringr)
library(corrplot)
library(dplyr)
health <- read_excel('DataDownload.xls', sheet='HEALTH')
restaurants <- read_excel('DataDownload.xls', sheet='RESTAURANTS')
access <- read_excel('DataDownload.xls', sheet='ACCESS')
stores <- read_excel('DataDownload.xls', sheet='STORES')
assistance <- read_excel('DataDownload.xls', sheet='ASSISTANCE')
insecurity <- read_excel('DataDownload.xls', sheet='INSECURITY')
local <- read_excel('DataDownload.xls', sheet='LOCAL')
price_tax <- read_excel('DataDownload.xls', sheet='PRICES_TAXES')
socioec <- read_excel('DataDownload.xls', sheet='SOCIOECONOMIC')

colnames(health) <- str_to_lower(colnames(health))
obesity <- health %>% select(fips, pct_obese_adults09,pct_obese_adults10, pct_obese_adults13,
														 pct_obese_child08, pct_obese_child11, pch_obese_child_08_11, 
														 pct_hspa09)
health <- health[complete.cases(health),]
cor.matrix <- cor(health[,-c(1, 2, 3)], use="complete.obs")
corrplot(cor.matrix, method="color",addCoef.col = "black", number.cex=0.5)
ggplot(health, aes(recfacpth12, pct_obese_adults13))+geom_jitter()
wilcox.test(health$recfac12, health$recfac07, paired=T, alternative="two.sided", conf.int=T)
#no significant correlation between rec centers and obesity

colnames(restaurants) <- str_to_lower(colnames(restaurants))
fast_food <- restaurants %>% select(contains("ffr"), fips)
full_service <- restaurants %>% select(contains("fsr"), fips)
full.ff.ob <- full_join(obesity, fast_food, by="fips")
cor.ff.ob <- cor(full.ff.ob[,-1], use="complete.obs")
corrplot(cor.ff.ob, method="color",addCoef.col = "black", number.cex=0.5)
full.fs.ob <- full_join(obesity, full_service, by="fips")
cor.fs.ob <- cor(full.fs.ob[,-1], use="complete.obs")
corrplot(cor.ff.ob, method="color",addCoef.col = "black", number.cex=0.5)
#no significant correlation between ff/fs res and obesity

colnames(access) <- str_to_lower(colnames(access))
full.acc.ob <- full_join(obesity, access[,-c(2,3)], by="fips")
cor.acc.ob <- cor(full.acc.ob[,-1], use="complete.obs", method="spearman")
corrplot(cor.acc.ob, method="color",addCoef.col = "black", number.cex=0.5)
# pct of households, no car & low access to store (%), 2010 has moderately strong
# correlation with pct of obese adults (0.45)
ggplot(full.acc.ob, aes(pct_obese_adults10, pct_laccess_hhnv10))+geom_point()
#scatterplot doesn't show strong relationship, with the presence of outliers
#ggplot(full.acc.ob, aes(pct_hspa09, pct_laccess_hhnv10))+geom_jitter()
#ggplot(full.acc.ob, aes(pct_obese_adults10, pct_laccess_seniors10))+geom_jitter() # no real trend
ggplot(full.acc.ob, aes(pct_laccess_seniors10))+
	geom_histogram(aes(y=..density..), bins=25, fill="red", alpha = 0.5, color="black")+
	geom_density(color="blue")+
	geom_density(aes(x=pct_obese_adults10), color = "black")+
	theme_classic()+
	labs(x="Percentage", y="Density")

colnames(stores) <- str_to_lower(colnames(stores))
full.sto.ob <- full_join(obesity, stores[,-c(2,3)], by="fips")
cor.sto.ob <- cor(full.sto.ob[,-1], use="complete.obs", method="spearman")
corrplot(cor.sto.ob, method="color",addCoef.col = "black", number.cex=0.4, tl.cex=0.5)
#pct of SNAP authorized stores has strong positive correlation with obesity in adult 
# not so sure cause time frame disparity

colnames(assistance) <- str_to_lower(colnames(assistance))
assistance1 <- assistance %>% select(ends_with("09"), ends_with("10"), -starts_with("snap"),fips)
full.ast.ob <- full_join(obesity, assistance1, by="fips")
cor.ast.ob <- cor(full.ast.ob[,-1], use="complete.obs", method="spearman")
corrplot(cor.ast.ob, method="color",addCoef.col = "black", number.cex=0.5, tl.cex=0.5)
# in 2009, SNAP, national school lunch, WIC, school breakfast program correlate strongly with pct obese adult, 
# especially years after that? (2013)
# pct high school physically active generally negatively correlated with the food program, esp SNAP (-0.37) (moderately strong)

colnames(insecurity) <- str_to_lower(colnames(insecurity))
colnames(local) <- str_to_lower(colnames(local))
colnames(price_tax) <- str_to_lower(colnames(price_tax))
colnames(socioec) <- str_to_lower(colnames(socioec))

full.prt.ob <- full_join(obesity, price_tax[,-c(2,3)], by="fips")
cor.prt.ob <- cor(full.prt.ob[,-1], use="complete.obs", method="spearman")
corrplot(cor.prt.ob, method="color",addCoef.col = "black", number.cex=0.5, tl.cex=0.5)
# raise tax/price does not correlate with lower obesity rate, it even seems to suggest otherwise

full.lcl.ob <- full_join(obesity, local[,-c(2,3)], by="fips")
ggplot(full.lcl.ob, aes(x=factor(farm_to_school), y= pct_obese_adults09))+
	geom_boxplot()
aov(pct_obese_child08 ~ factor(farm_to_school), full.lcl.ob)
#the categorical var are not interesting, consider display it or not
full.lcl.ob <- full.lcl.ob %>% select(-ends_with("07"), -farm_to_school, -foodhub12)
cor.lcl.ob <- cor(full.lcl.ob[,-1], use="complete.obs", method="spearman")
corrplot(cor.lcl.ob, method="color",addCoef.col = "black", number.cex=0.4, tl.cex=0.4)
# farmer's market and vegetable stores moderately helps reduce obesity rate
# also stores have strong correlation with each other, maybe the opening of 1 store spark the opening of the others

full.ins.ob <- full_join(obesity, insecurity[,-c(2,3)], by="fips")
cor.ins.ob <- cor(full.ins.ob[,-1], use="complete.obs", method="spearman")
corrplot(cor.ins.ob, method="color",addCoef.col = "black", number.cex=0.5, tl.cex=0.5)
# why pct of food insecurity in the period of 2012 correlate positively (strong) with obesity in adult in 2013?

obesity$fips <- as.numeric(obesity$fips)
full.sec.ob <- full_join(obesity, socioec[,-c(2,3)], by="fips")
full.sec.ob[,c(17:23)] <- sapply(full.sec.ob[,c(17:23)], as.numeric)
cor.sec.ob <- cor(full.sec.ob[,-1], use="complete.obs", method="spearman")
corrplot(cor.sec.ob, method="color",addCoef.col = "black", number.cex=0.5, tl.cex=0.5)
# pct black pos corr with obesity, hisp and asia neg corr, median income neg corr with obesity (-0.47), but poverty pos corr with obesity

insecurity_cleaned <- insecurity %>% select(fips, state, county, 
																						food_insecurity10_12 = foodinsec_10_12, 
																						very_low_food_security10_12 = vlfoodsec_10_12,
																						child_food_insecurity03_11 = foodinsec_child_03_11 )
health <- read_excel('DataDownload.xls', sheet='HEALTH')
colnames(health) <- str_to_lower(colnames(health))
obesity <- health %>% select(fips, pct_obese_adults13,
														 pct_obese_child08, pct_obese_child11)
insecurity_cleaned <- insecurity_cleaned %>% left_join(obesity, by="fips")

library(GGally)
insecurity_cleaned %>%
	select(-fips, -state, -county) %>%
	ggcorr(method = c("complete.obs", "pearson"), nbreaks =10, label_size=2) 

insecurity_cleaned %>%
	select(-fips, -state, -county) %>%
	ggpairs()

write.csv(insecurity_cleaned, "insecurity.csv")

#save_file <- full_join(obesity, socioec, by="fips")
#save_file[,c(19:25)] <- sapply(save_file[,c(19:25)], as.numeric)
#write.csv(save_file, "socioeconomic.csv")
