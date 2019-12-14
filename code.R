
# Some messy code to download 2019 UK election results data from
# https://democracyclub.org.uk/ using their v0.9 API
# and comparing first-past-the-post vs. proportional representation voting
#
# Paul Schneider
# University of Sheffield
# p.schneider@sheffield.ac.uk

library(jsonlite)
library(httr)
library(ggplot2)
library(dplyr)
library(ggforce)
library(scales)
library(ggrepel)


df = data.frame(stringsAsFactors = F,name = NA,party = NA,res = NA,is_winner = NA,lsoa = NA)
go = T
index = 0
get_path = "https://candidates.democracyclub.org.uk/api/v0.9/result_sets/?election_id=parl.2019-12-12" #&page=5
while(go){
  x = (GET(get_path))
  parsed <- jsonlite::fromJSON(content(x,"text",encoding = "UTF-8"), simplifyVector = FALSE)
  index = index + 1
  for(l in 1:length(parsed$results)){
    cat("\r",index,"  ",l)
    x = parsed$results[[l]]
    for(j in 1:length(x$candidate_results)){
      has_lsoa = !is.null(x$candidate_results[[j]]$membership$post$id)
      temp = data.frame(stringsAsFactors = F,
                        name = x$candidate_results[[j]]$membership$person$name,
                        party = x$candidate_results[[j]]$membership$on_behalf_of$name,
                        res = x$candidate_results[[j]]$num_ballots,
                        is_winner = x$candidate_results[[j]]$is_winner,
                        lsoa = ifelse(has_lsoa,x$candidate_results[[j]]$membership$post$id,NA))
      df = rbind(df,temp)  
    }
    
  }
  if(!is.null(parsed$'next')){
    get_path = parsed$'next'  
  } else {
    cat("\n\n DONE.")
    go = F
  }
}

df = df[-1,]
# corrections
df$lsoa[is.na(df$lsoa)] = "WMC:E14000650"
df$party[df$party=="Labour and Co-operative Party"] = "Labour Party"
df$party[df$party=="Speaker seeking re-election"] = "Labour Party"

head(df)

# function for plotting half-donuts
parlDiag <- function(Parties, shares,
                     minimum=1, 
                     cols = NULL, 
                     repr=c("absolute", "proportion"),
                     custom_title=NULL,pcolors=NULL) { 
  plables = Parties
  Parties = substr(Parties,1,6)
  Parties = c(Parties[shares >= minimum],"Others")
  shares = c(shares[shares>=minimum],sum(shares[shares<minimum]))
  Parties_fac = as.factor(1:length(Parties))
  if(is.null(pcolors)){pcolors = 1:length(Parties)}
  repr = match.arg(repr)
  # arc start/end in rads, last one reset bc rounding errors
  cc <- cumsum(c(-pi/2, (shares / sum(shares)) * pi))
  cc[length(cc)] <- pi/2
  # get angle of arc midpoints
  meanAngles <- colMeans(rbind(cc[2:length(cc)], cc[1:length(cc)-1]))
  # unit circle
  labelX <- sin(meanAngles)
  labelY <- cos(meanAngles)
  # prevent bounding box < y=0
  labelY <- ifelse(labelY < 0.015, 0.015, labelY)
  p <- ggplot() + theme_no_axes() + coord_fixed() +
    expand_limits(x = c(-1.3, 1.3), y = c(0, 1.3)) + 
    theme(panel.border = element_blank()) +
    theme(legend.position = "bottom") +
    geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.5, r = 1,
                     start = cc[1:length(shares)], 
                     end = c(cc[2:length(shares)], pi/2), fill = Parties_fac)) +
    geom_label_repel(aes(x = 0.9 * labelX, y = 0.9 * labelY,
                         label = 
                           switch(repr, "absolute" = paste(Parties, " \n",shares," (",round(shares/sum(shares),2)*100,"%)",sep=""), 
                                  "proportion"=paste(Parties, " \n",round((shares/100)*650,0)," (",  shares,"%)",sep="")
                                  
                           )),
                     nudge_x = 0.1,box.padding = 0.55,
                     arrow =arrow(length = unit(0.02, "npc"))  ) +
    ggtitle(custom_title) +
    scale_fill_manual(values=pcolors,labels=plables,name="") +
    guides(fill=guide_legend(nrow = length(Parties))) +
    theme(legend.text = element_text(size = 18))
  
  
  
  
  return(p)
}


# first past the post winner
# compute winners (provided winner var not complete)
df$my_winner = 0
for(lsoa in unique(df$lsoa)){
  temp = df[df$lsoa == lsoa,]
  winner_temp = which(temp$res == max ( temp$res)) 
  df$my_winner[df$lsoa == lsoa][winner_temp] = 1
}

fptp_res = aggregate(my_winner~party,df,sum)
fptp_res = fptp_res[fptp_res$my_winner>0,]
fptp_res = fptp_res[order(fptp_res$my_winner,decreasing = T),]
fptp_res

parlDiag(fptp_res$party, fptp_res$my_winner,custom_title = "First pass the post",minimum = 5,
         pcolors = c("blue","red","yellow","orange","darkred","darkgreen","lightgreen","gray"))

# proportional representation
prop_res = aggregate(res~party,df,sum)
prop_res$res = round(prop_res$res/sum(prop_res$res),3)*100
prop_res = prop_res[prop_res$res>0.1,]
prop_res = prop_res[order(prop_res$res,decreasing = T),]
prop_res

parlDiag(prop_res$party, prop_res$res,custom_title = "Proportional representation",repr = "proportion",minimum = 2,
         pcolors = c("blue","red","orange","yellow","darkred","darkgreen","gray"))


# 
