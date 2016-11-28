library(plyr)
money_data <- read.csv("...\\MoneyLover.csv",sep=";",dec=".",as.is=T,header=T)
cost_list <- list()
money_data$Datum <- as.Date(money_data$Datum,"%d.%m.%Y")
money_data$Datum <- as.POSIXlt(money_data$Datum)

month <- sort(unique(money_data$Datum$mo))
year <- sort(unique(money_data$Datum$year))
name_df <- paste(month+1,year+1900,sep="_")

for (i in 1:length(year)){
  for (j in 1:length(month)){
    cost_list[name_df[j]] <- list(subset(money_data,money_data$Datum$mo == month[j] & money_data$Datum$year == year[i]))
  }
}

# prijmy <- cost_list[[k]]
income_list <- list()
expenses_list <- list()
export_data_group <- data.frame()
export_data_nogroup <- data.frame()

bills <- c("Voda","Plyn","Elektřina","Pronájmy","Telefon","Televize","Internet")
car <- c("Parkovací poplatky","Benzín","Údržba")
shopping <- c("Oblečení","Elektronika","Příslušentsví domácnosti","Obuv","Noviny a časopisy","Drogerie")
fun <- c("Filmy","Hry","Gembl","Přednášky","Divadlo","Lístky na sport")
gifts <- c("Manželství","Charita","Pohřeb","Dárky & dary")
family <- c("Děti & miminka","Vylepšení domácnosti","Domácí služby","Domácí zvířata")
health <- c("Sporty","Lékárna","Osobní péče","Doktor")
education <- c("Knihy","Vzdělání")
transport <- c("Taxi","MHD","Autobus")

for (k in 1:length(cost_list)){
  month_money <- cost_list[[k]]
  income <- sum(month_money$Částka[which(month_money$Částka > 0)])
  expenses<- sum(month_money$Částka[which(month_money$Částka < 0)])
  bilance <- income + expenses
  category <- aggregate(month_money$Částka, list(Category = month_money$Kategorie), sum)
  income_cat <- subset(category, category$x > 0)
  expenses_cat <- subset(category, category$x < 0)
  
  expenses_list["Účty"] <- list(subset(expenses_cat, expenses_cat$Category %in% bills == T))
  expenses_list["Auto"] <- list(subset(expenses_cat, expenses_cat$Category %in% car == T))
  expenses_list["Nakupování"] <- list(subset(expenses_cat, expenses_cat$Category %in% shopping == T))
  expenses_list["Zábava"] <- list(subset(expenses_cat, expenses_cat$Category %in% fun == T))
  expenses_list["Dárky"] <- list(subset(expenses_cat, expenses_cat$Category %in% gifts == T))
  expenses_list["Rodina"] <- list(subset(expenses_cat, expenses_cat$Category %in% family == T))
  expenses_list["Zdraví"] <- list(subset(expenses_cat, expenses_cat$Category %in% health == T))
  expenses_list["Vzdělání"] <- list(subset(expenses_cat, expenses_cat$Category %in% education == T))
  expenses_list["Doprava"] <- list(subset(expenses_cat, expenses_cat$Category %in% transport == T))
  expenses_list["Ostatní"] <- list(expenses_cat[-which(expenses_cat$Category %in% expenses_list$Účty$Category |
                                     expenses_cat$Category %in% expenses_list$Auto$Category |
                                     expenses_cat$Category %in% expenses_list$Nakupování$Category |
                                     expenses_cat$Category %in% expenses_list$Zábava$Category |
                                     expenses_cat$Category %in% expenses_list$Dárky$Category |
                                     expenses_cat$Category %in% expenses_list$Rodina$Category |
                                     expenses_cat$Category %in% expenses_list$Zdraví$Category |
                                     expenses_cat$Category %in% expenses_list$Vzdělání$Category |
                                     expenses_cat$Category %in% expenses_list$Doprava$Category),])
  
  group_expense <- data.frame(names(expenses_list[1:9]),c(sum(expenses_list[[1]]$x),sum(expenses_list[[2]]$x),sum(expenses_list[[3]]$x),
                                   sum(expenses_list[[4]]$x),sum(expenses_list[[5]]$x),sum(expenses_list[[6]]$x),
                                   sum(expenses_list[[7]]$x),sum(expenses_list[[8]]$x),sum(expenses_list[[9]]$x)))
  names(group_expense) <- c("Category","x")
  export_group <- rbind(group_expense,expenses_list[[10]])
  
  export_group1 <- t(export_group[,2:ncol(export_group)])
  colnames(export_group1) <- export_group[,1]
  export_data_group <- rbind.fill(export_data_group,as.data.frame(export_group1))
  
  export_nogroup1 <- t(expenses_cat[,2:ncol(expenses_cat)])
  colnames(export_nogroup1) <- expenses_cat[,1]  
  export_data_nogroup <- rbind.fill(export_data_nogroup,as.data.frame(export_nogroup1))
}
export_data_group <- cbind(names(cost_list),export_data_group)
colnames(export_data_group)[1] <- "Datum"
export_data_nogroup <- cbind(names(cost_list),export_data_nogroup)
colnames(export_data_nogroup)[1] <- "Datum"
export_data_group[is.na(export_data_group)] <- 0
export_data_nogroup[is.na(export_data_nogroup)] <- 0
write.table(export_data_group,file="expenses_grouped.csv",sep=";",row.names=F)
write.table(export_data_nogroup,file="expenses_nogroup.csv",sep=";",row.names=F)


income_list["Příjmy"] <- list(income_cat)


# Pie Chart with Percentages
slices <- abs(c(group_expense$Účty,group_expense$Auto,group_expense$Nakupovaní,
                group_expense$Zábava,group_expense$Dárky,group_expense$Rodina,
                group_expense$Zdraví,group_expense$Vzdělání,group_expense$Doprava,
            expenses_list$Ostatní$x))
lbls <- c(names(expenses_list)[1:9],expenses_list$Ostatní$Category) #names(expenses_list)[1:9], protože nechci tu poslední kategorii
pct <- round(slices/sum(slices)*100)
rest_cumul <- sum(slices[which(pct <=  1)]) # součet malych polozek (mensich nebo rovno 1% z mesicnich nakladu)
slices <- slices[-which(pct <=  1)]
slices <- c(slices,rest_cumul)
lbls <- lbls[-which(pct <=  1)]
lbls <- c(lbls,"Zbytek")
pct <- pct[-which(pct <=  1)]
pct <- c(pct,round(rest_cumul/sum(slices)*100))
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Měsíční náklady")