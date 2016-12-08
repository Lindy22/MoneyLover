library(plyr)

money_data <- read.csv("..\\MoneyLover.csv",sep=";",dec=".",as.is=T,header=T)
cost_list <- list()
money_data$Datum <- as.Date(money_data$Datum,"%d.%m.%Y")
money_data$Datum <- as.POSIXlt(money_data$Datum)

month <- sort(unique(money_data$Datum$mo))
year <- sort(unique(money_data$Datum$year))
name_df <- paste(year+1900,sprintf("%02d",month+1),sep="_")

for (i in 1:length(year)){
  for (j in 1:length(month)){
    cost_list[name_df[j]] <- list(subset(money_data,money_data$Datum$mo == month[j] & money_data$Datum$year == year[i]))
  }
}

# prijmy <- cost_list[[k]]
income_list <- list()
expenses_list <- list()
overall_data <- data.frame()
income_export_data_nogroup <- data.frame()
expense_export_data_group <- data.frame()
expense_export_data_nogroup <- data.frame()
overall_export_data <- data.frame()

bills <- c("Voda","Plyn","Elektřina","Pronájmy","Telefon","Televize","Internet")
car <- c("Parkovací poplatky","Benzín","Údržba")
shopping <- c("Oblečení","Elektronika","Příslušentsví domácnosti","Obuv","Noviny a časopisy","Drogerie")
fun <- c("Filmy","Hry","Gembl","Přednášky","Divadlo","Lístky na sport")
travelling <- c("Letenky","Ubytování","Zájezd","Jízdenky")
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
  
#   income_list["Příjmy"] <- list(income_cat) --not neccessary because there are no groupped categories yet
  
  income_export_nogroup1 <- t(income_cat[,2:ncol(income_cat)])
  colnames(income_export_nogroup1) <- income_cat[,1]  
  income_export_data_nogroup <- rbind.fill(income_export_data_nogroup,as.data.frame(income_export_nogroup1))
  
  overall_data1 <- data.frame(cbind(income,expenses,bilance))
  colnames(overall_data1) <- c("Příjmy","Výdaje","Bilance")
  overall_data <-rbind.fill(overall_data,overall_data1)
  
  
  expenses_list["Účty"] <- list(subset(expenses_cat, expenses_cat$Category %in% bills == T))
  expenses_list["Auto"] <- list(subset(expenses_cat, expenses_cat$Category %in% car == T))
  expenses_list["Nakupování"] <- list(subset(expenses_cat, expenses_cat$Category %in% shopping == T))
  expenses_list["Zábava"] <- list(subset(expenses_cat, expenses_cat$Category %in% fun == T))
  expenses_list["Cestování"] <- list(subset(expenses_cat, expenses_cat$Category %in% travelling == T))
  expenses_list["Dárky"] <- list(subset(expenses_cat, expenses_cat$Category %in% gifts == T))
  expenses_list["Rodina"] <- list(subset(expenses_cat, expenses_cat$Category %in% family == T))
  expenses_list["Zdraví"] <- list(subset(expenses_cat, expenses_cat$Category %in% health == T))
  expenses_list["Vzdělání"] <- list(subset(expenses_cat, expenses_cat$Category %in% education == T))
  expenses_list["Doprava"] <- list(subset(expenses_cat, expenses_cat$Category %in% transport == T))
  expenses_list["Ostatní"] <- list(expenses_cat[-which(expenses_cat$Category %in% expenses_list$Účty$Category |
                                     expenses_cat$Category %in% expenses_list$Auto$Category |
                                     expenses_cat$Category %in% expenses_list$Nakupování$Category |
                                     expenses_cat$Category %in% expenses_list$Zábava$Category |
                                     expenses_cat$Category %in% expenses_list$Cestování$Category |
                                     expenses_cat$Category %in% expenses_list$Dárky$Category |
                                     expenses_cat$Category %in% expenses_list$Rodina$Category |
                                     expenses_cat$Category %in% expenses_list$Zdraví$Category |
                                     expenses_cat$Category %in% expenses_list$Vzdělání$Category |
                                     expenses_cat$Category %in% expenses_list$Doprava$Category),])
  

  expense_list_len <- length(expenses_list)
  
  group_expense <- data.frame(names(expenses_list[1:expense_list_len-1]),c(sum(expenses_list[[1]]$x),sum(expenses_list[[2]]$x),
                                    sum(expenses_list[[3]]$x),sum(expenses_list[[4]]$x),sum(expenses_list[[5]]$x),
                                    sum(expenses_list[[6]]$x),sum(expenses_list[[7]]$x),sum(expenses_list[[8]]$x),
                                    sum(expenses_list[[9]]$x),sum(expenses_list[[10]]$x)))
  names(group_expense) <- c("Category","x")
  expense_export_group <- rbind(group_expense,expenses_list[[expense_list_len]])
  
  expense_export_group1 <- t(expense_export_group[,2:ncol(expense_export_group)])
  colnames(expense_export_group1) <- expense_export_group[,1]
  expense_export_data_group <- rbind.fill(expense_export_data_group,as.data.frame(expense_export_group1))
  
  expense_export_nogroup1 <- t(expenses_cat[,2:ncol(expenses_cat)])
  colnames(expense_export_nogroup1) <- expenses_cat[,1]  
  expense_export_data_nogroup <- rbind.fill(expense_export_data_nogroup,as.data.frame(expense_export_nogroup1))
  
  # Pie Chart with Percentages
  png(filename=paste("Rozdělení výdajů za období ", names(cost_list[k]),".png", sep=""),width = 960, height = 960, units = "px")
  slices <- abs(c(sum(expenses_list$Účty$x),sum(expenses_list$Auto$x),sum(expenses_list$Nakupovaní$x),
                  sum(expenses_list$Zábava$x),sum(expenses_list$Cestování$x),sum(expenses_list$Dárky$x),
                  sum(expenses_list$Rodina$x),sum(expenses_list$Zdraví$x),sum(expenses_list$Vzdělání$x),
                  sum(expenses_list$Doprava$x),expenses_list$Ostatní$x))
  lbls <- c(names(expenses_list)[1:expense_list_len-1],expenses_list$Ostatní$Category) #names(expenses_list)[1:9], protože nechci tu poslední kategorii
  pct <- round(slices/sum(slices)*100)
  if (any(pct <=1) == T) {
    rest_cumul <- sum(slices[which(pct <=  1)]) # součet malych polozek (mensich nebo rovno 1% z mesicnich nakladu)
    slices <- slices[-which(pct <=  1)]
    slices <- c(slices,rest_cumul)
    lbls <- lbls[-which(pct <=  1)]
    lbls <- c(lbls,"Zbytek")
    pct <- pct[-which(pct <=  1)]
    pct <- c(pct,round(rest_cumul/sum(slices)*100))
  }
  lbls <- paste(lbls, pct) # add percents to labels 
  lbls <- paste(lbls,"%",sep="") # ad % to labels 
  pie(slices,labels = lbls, col=rainbow(length(lbls)),
      main="Měsíční náklady")
  dev.off()  
  
  # Pie Chart with Percentages
  png(filename=paste("Rozdělení příjmů za období ", names(cost_list[k]),".png", sep=""),width = 960, height = 960, units = "px")
  slices <- income_cat$x
  lbls <- income_cat$Category #names(expenses_list)[1:9], protože nechci tu poslední kategorii
  pct <- round(slices/sum(slices)*100)
  if (any(pct <=1) == T) {
    rest_cumul <- sum(slices[which(pct <=  1)]) # součet malych polozek (mensich nebo rovno 1% z mesicnich nakladu)
    slices <- slices[-which(pct <=  1)]
    slices <- c(slices,rest_cumul)
    lbls <- lbls[-which(pct <=  1)]
    lbls <- c(lbls,"Zbytek")
    pct <- pct[-which(pct <=  1)]
    pct <- c(pct,round(rest_cumul/sum(slices)*100))
  }
  lbls <- paste(lbls, pct) # add percents to labels 
  lbls <- paste(lbls,"%",sep="") # ad % to labels 
  pie(slices,labels = lbls, col=rainbow(length(lbls)),
      main="Měsíční náklady")
  dev.off()  
}
# adding column with date
income_export_data_nogroup <- cbind(names(cost_list),income_export_data_nogroup)
colnames(income_export_data_nogroup)[1] <- "Datum"
expense_export_data_group <- cbind(names(cost_list),expense_export_data_group)
colnames(expense_export_data_group)[1] <- "Datum"
expense_export_data_nogroup <- cbind(names(cost_list),expense_export_data_nogroup)
colnames(expense_export_data_nogroup)[1] <- "Datum"

overall_export_data <- cbind(names(cost_list),overall_data)
colnames(overall_export_data)[1] <- "Datum"

# replacing NA with 0
income_export_data_nogroup[is.na(income_export_data_nogroup)] <- 0
expense_export_data_group[is.na(expense_export_data_group)] <- 0
expense_export_data_nogroup[is.na(expense_export_data_nogroup)] <- 0
# exporting .csv file with results
write.table(income_export_data_nogroup,file="income_nogrouped.csv",sep=";",row.names=F)
write.table(expense_export_data_group,file="expenses_grouped.csv",sep=";",row.names=F)
write.table(expense_export_data_nogroup,file="expenses_nogroup.csv",sep=";",row.names=F)
write.table(overall_export_data,file="overall_result.csv",sep=";",row.names=F)

png(filename=paste("Vývoj výdaje vs. příjmy", ".png", sep=""),width = 960, height = 960, units = "px")
plot(abs(overall_export_data$Příjmy),type="o", col="green",xaxt="n", ann=F, ylim=range(abs(overall_export_data[,-1])))
lines(abs(overall_export_data$Výdaje), type="o", pch=22, lty=2, col="red")
axis(1,at=1:length(overall_export_data$Datum),labels=overall_export_data$Datum)
text(abs(overall_export_data$Příjmy), labels=abs(overall_export_data$Příjmy), cex= 0.7, pos=1)
text(abs(overall_export_data$Výdaje), labels=abs(overall_export_data$Výdaje), cex= 0.7, pos=1)
title(main="Přehled nákladů", col.main="red", font.main=4)
title(xlab="Čas")
title(ylab="Koruny")
legend("topleft", names(overall_export_data[2:3]), cex=0.8, col=c("green","red"), 
       lty=1:2, lwd=2, bty="n")
dev.off()

png(filename=paste("Vývoj bilance", ".png", sep=""),width = 960, height = 960, units = "px")
plot(overall_export_data$Bilance,type="o", col="blue",xaxt="n", ann=F, ylim=range(overall_export_data$Bilance))
axis(1,at=1:length(overall_export_data$Datum),labels=overall_export_data$Datum)
text(overall_export_data$Bilance, labels=overall_export_data$Bilance, cex= 0.7, pos=3)
title(main="Měsíční bilance", col.main="red", font.main=4)
title(xlab="Čas")
title(ylab="Zisky / Ztráty")
dev.off()

export_expense <- expense_export_data_group[,-1]
for (l in seq(1,length(export_expense))){
  png(filename=paste("Vývoj nákladů - ", names(export_expense[l]), ".png", sep=""),width = 960, height = 960, units = "px")
  plot(abs(export_expense[[l]]),type="o", col="blue",xaxt="n", ann=F,ylim=range(abs(export_expense)))
  axis(1,at=1:length(expense_export_data_group$Datum),labels=expense_export_data_group$Datum)
  text(abs(export_expense[[l]]), labels=abs(export_expense[[l]]), cex= 0.7, pos=3)
  title(main=names(export_expense[l]), col.main="red", font.main=4)
  title(xlab="Čas")
  title(ylab="Výše nákladů")
  dev.off()
}

export_income <- income_export_data_nogroup[,-1]
for (l in seq(1,length(export_income))){
  png(filename=paste("Vývoj příjmů - ", names(export_income[l]), ".png", sep=""),width = 960, height = 960, units = "px")
  plot(abs(export_income[[l]]),type="o", col="blue",xaxt="n", ann=F,ylim=range(abs(export_income)))
  axis(1,at=1:length(expense_export_data_group$Datum),labels=expense_export_data_group$Datum)
  text(abs(export_income[[l]]), labels=abs(export_income[[l]]), cex= 0.7, pos=3)
  title(main=names(export_income[l]), col.main="red", font.main=4)
  title(xlab="Čas")
  title(ylab="Výše příjmů")
  dev.off()
}
