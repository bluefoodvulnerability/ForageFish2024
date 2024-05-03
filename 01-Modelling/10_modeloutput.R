library(pacman)
p_load(tidyverse, readxl)

sheet1 <- read_rds('Results/Senario.model.results.rds') |> 
  group_by(taxa, Senario) |> 
  summarise(
    avgEstProd = mean(EstProd),
    q05Prod = quantile(EstProd, probs = .025, na.rm = T),
    q95Prod = quantile(EstProd, probs = .975, na.rm = T),
    recDiet = mean(diet_Rec),
    q05Diet_rec = quantile(diet_Rec, probs = .025, na.rm = T),
    q95Diet_rec = quantile(diet_Rec, probs = .975, na.rm = T),
    recImprov = mean(dietImprov),
    q05recImprov = quantile(dietImprov, probs = .025, na.rm = T),
    q95recImprov = quantile(dietImprov, probs = .975, na.rm = T),
    avgReal = mean(avgReal),
    aquaLoss.r = mean(aquaLoss.r),
    aquaLoss.r.05 = quantile(aquaLoss.r, probs = .025, na.rm = T),
    aquaLoss.r.95 = quantile(aquaLoss.r, probs = .0975, na.rm = T)
  ) |> 
  arrange(Senario) |> 
  rename(`物种` = taxa, 
         `场景` = Senario,
         `该场景下预估产量（均值）` = avgEstProd,
         `该场景下预估产量（2.5%分位数+模型结果）` = q05Prod,
         `该场景下预估产量（97.5%分位数+模型结果）` = q95Prod,
         `欲达到2022年产量的FF鱼饲料占比（均值）` = recDiet,
         `欲达到2022年产量的FF鱼饲料占比（2.5%分位数）` = q05Diet_rec,
         `欲达到2022年产量的FF鱼饲料占比（97.5.5%分位数）` = q95Diet_rec,
         `欲达到2022年产量的FF鱼饲料占比需提升（均值）` = recImprov,
         `欲达到2022年产量的FF鱼饲料占比需提升（2.5%分位数）` = q05recImprov,
         `欲达到2022年产量的FF鱼饲料占比需提升（97.5.5%分位数）` = q95recImprov,
         `2022年产量` = avgReal,
         `产量损失（%，均值）` = aquaLoss.r,
         `产量损失（%，2.5%分位数）` = aquaLoss.r.05,
         `产量损失（%，97.5%分位数）` = aquaLoss.r.95)

sheet2 <- read_rds('Results/FF.replacement.res.rds') |> 
  group_by(taxa, Scenario) |>
  summarise(`需替代鱼粉量（均值）` = mean(fMeal),
            `需替代鱼粉量（2.5%分位数）` = quantile(fMeal, 0.025),
            `需替代鱼粉量（97.5%分位数）` = quantile(fMeal, 0.975))


source('./Scripts/00-Support Functions/02_modelSupport.R')
scenarioModelRes_path <- './Results/Senario.model.results.rds'
# weights based on country production and taxa productivity
country_taxa_weight_path <- './Results/Country.Taxa.Weights.rds'

scenarioModelRes <- read_rds(scenarioModelRes_path)
country_taxa_weight <- read_rds(country_taxa_weight_path)

senarioLoss <- scenarioModelRes %>%
  mutate(prodLoss = avgReal - EstProd) %>%
  select(-dietOrigin,
         -diet_Rec,
         -dietImprov,
         -avgReal,
         -EstProd,
         -aquaLoss.r) %>% 
  rename(Taxa = taxa)


senario_loss <- senarioLoss %>% 
  full_join(country_taxa_weight, by = 'Taxa') %>% 
  mutate(country_taxa_loss = taxa_weight * prodLoss,
         country_taxa_loss = if_else(country_taxa_loss >= Year_2022,
                                     country_taxa_loss, 
                                     country_taxa_loss),
         country_taxa_loss_r = country_taxa_loss/Year_2022) 


# country level
sheet3 <- senario_loss %>%
  group_by(Senario, Country, Taxa) |> 
  group_split() |> 
  map(~mutate(.x, simu = 1:500)) |> 
  bind_rows() |> 
  group_by(Senario, Country, simu) %>%
  summarise(pod_loss = sum(country_taxa_loss),
            prod_2022 = sum(Year_2022)) |> 
  ungroup() %>%
  group_by(Senario, Country) %>%
  summarise(pod_loss = mean(pod_loss),
            prod_2022 = mean(prod_2022)) |> 
  mutate(loss_r = pod_loss / prod_2022,
         loss_r = if_else(loss_r > 1, 1, loss_r)) |> 
  rename(`场景` = Senario, `国家` = Country,
         `损失产量` = pod_loss,
         `2022年产量` = prod_2022, `损失产量占比` = loss_r)
  
sheet4 <- read_rds('Results/FF.replacement.res.rds') |> 
  group_by(Scenario) |>
  summarise(`需替代鱼粉量（均值）` = mean(fMeal),
            `需替代鱼粉量（2.5%分位数）` = quantile(fMeal, 0.025),
            `需替代鱼粉量（97.5%分位数）` = quantile(fMeal, 0.975))

sheet5 <- read_rds('Results/FishmealUsage.rds') |> 
  mutate(Year = as.numeric(Year)) %>%
  select(-starts_with('...')) %>%
  filter(Year == 2022) %>%
  mutate(Taxa = factor(Taxa,
                       levels = taxaLevel)) |> 
  select(-Year, -sdMO, - seMO) |> 
  rename(`鱼粉消耗量（平均）` = meanMO,
         `鱼粉消耗量（2.5%）` = q05MO,
         `鱼粉消耗量（97.5%）` = q95MO)

FILE <- 'Visuals/模型结果表.xlsx'
xlsx::write.xlsx2(x = as.data.frame(sheet1), file = FILE, row.names = F,
                 sheetName = '不同养殖物种各场景下产量')
xlsx::write.xlsx2(x = as.data.frame(sheet2), file = FILE, row.names = F,
                 sheetName = '各物种需替代鱼粉量', append = T)
xlsx::write.xlsx2(x = as.data.frame(sheet3), file = FILE, row.names = F,
                 sheetName = '按国家损失总量和百分比', append = T)
xlsx::write.xlsx2(x = as.data.frame(sheet4), file = FILE, row.names = F,
                 sheetName = '各场景需要替代的鱼粉总量', append = T)
xlsx::write.xlsx2(x = as.data.frame(sheet5), file = FILE, row.names = F,
                 sheetName = '2022年各养殖物种估算的鱼粉使用量', append = T)

FF.species.catch <- read_csv('Data/ForageFishLandingClean.csv') |> 
  filter(Year == 2022) |> 
  group_by(Species) |> 
  summarise(catch = sum(catch)) |> 
  arrange(desc(catch)) 

sum(top_n(FF.species.catch, 10)$catch) *1e-6
(sum(top_n(FF.species.catch, 10)$catch) / sum(FF.species.catch$catch)) *100

FF.SAU <- read_csv('Data/FF.enduse.SAU.csv')
FF.SAU |> 
  filter(year ==2019, end_use_type == 'Fishmeal and fish oil') |> 
  View()

stock_status <- read_rds('Results/FF.cmsy.stock.status.rds')
length(unique(stock_status$stock))
length(unique(stock_status$spe))

stock_status |> 
  group_by(status) |> 
  summarise(n())

stock_status |> 
  filter(FAO_area == 'Atlantic, Southeast') |> 
  group_by(status) |> 
  summarise(stock.n = n()) |> 
  ungroup() |> 
  mutate(stock_r = (stock.n/sum(stock.n))*100)

FF.landing <- read_csv('Data/ForageFishLandingClean.csv')
stock_in_analysis <- unique(stock_status$stock)
poor.stock <- stock_status |> filter(status %in% c('Over-exploited', 'Collapsed'))
poor.stock <- unique(poor.stock$stock)


FF.landing.decade <- sum((FF.landing |> 
  filter(Year >= 2012))$catch)
FF.landing.decade.top10 <- sum((FF.landing |> 
                                  filter(Year >= 2012,
                                         stock %in% stock_in_analysis))$catch)
(FF.landing.decade.top10/FF.landing.decade)*100

FF.landing.decade.poor <- sum((FF.landing |> 
                                  filter(Year == 2022,
                                         stock %in% stock_in_analysis))$catch)
(FF.landing.decade.poor/FF.landing.decade) *100

sum((read_rds('Results/FishmealUsage.rds') |> 
  mutate(Year = as.numeric(Year)) |> 
  select(-starts_with('...')) |> 
  filter(Year == 2022))$meanMO)*1e-6

sum((read_rds('Results/FishmealUsage.rds') |> 
       mutate(Year = as.numeric(Year)) |> 
       select(-starts_with('...')) |> 
       filter(Year == 2022))$meanMO*5.2)*1e-6

sum((read_rds('Results/FishmealUsage.rds') |> 
  mutate(Year = as.numeric(Year)) |> 
  select(-starts_with('...')) |> 
  filter(Year == 2022))$meanMO)*1e-6


read_rds('Results/FishmealUsage.rds') |> 
  mutate(Year = as.numeric(Year)) %>%
  select(-starts_with('...')) %>%
  filter(Year == 2022) %>%
  mutate(Taxa = factor(Taxa,
                       levels = taxaLevel)) |> 
  select(-Year, -sdMO, - seMO)

ffLanding |> 
  group_by(Species) |> 
  summarise(HistLanding = round(sum(catch, na.rm =F)*1e-6, 2)) |> 
  arrange(desc(HistLanding)) |> 
  top_n(10) |> View() 
  write_csv('Results/TableS1.csv')
  
