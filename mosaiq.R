library(ggmosaic)
library(stringr)

topn = function(d, top=25, otherlabel=NA) {
  ret = d
  ret[ret == ""] <-NA
  topnames = names(head(sort(table(ret),d=T),top))
  ret[!ret %in% topnames] <-NA
  if (!is.na(otherlabel)){
    ret[is.na(ret)] = otherlabel
  }
  factor(ret)
}

bucketize = function(x,top=25, breaks = NA ){
  if (all(is.na(x)))
    return(c(NA))

  if (is.numeric(x)){
    if (is.na(breaks)){
          breaks = hist(x,plot=F)$breaks
    }
    nbreaks = paste0(breaks[-length(breaks)], "-", breaks[-1]) 
    nbreaks = factor(nbreaks,levels=nbreaks)
    nbreaks[findInterval(x, breaks)]
  } else {
    topn(x,top) 
  }
}


mosaic_feature = function(dat, feature, target, log10_scaling=FALSE){
  x = bucketize(dat[[feature]], top = 25)
  d = as.data.frame(matrix(nrow=nrow(dat)))
  d[feature] = factor(x)
  d[target] = dat[[target]] 
  if (log10_scaling){
    d[target] = factor(round(log10(d[[target]]+1)))
  } 
  d[target] = bucketize(d[[target]], top=11)
  palette = "RdYlGn"
  if (!is.numeric(target)){
    palette= "Spectral"
  }
  if (all(is.na(d[target])) || all(is.na(d[feature]))){
    return() 
  }
  ggplot(data=d, aes_string(fill=target)) +
    geom_mosaic(aes_string(x=paste0("product(",feature, ")"))) +
    labs(title=paste(feature, "vs.", target)) +
    theme(axis.text.x = element_text(size=20,angle = 45, hjust = 1))
}

gen = function(dat, metric, limit, trans){
  dat[[metric]] = trans(dat[[metric]])
  for (i in 1:length(names(dat))){
    x = names(dat)[i]
    if (all(is.na(dat[[x]]))){
      next
    }
    png(paste0(name,'/', x,'.png'), 900,600)
    print(mosaic_feature(dat, x, metric))
    dev.off()
  }
}

seclk_trans = function(x){
  sapply(x, function(y){
    if (y <= 5) {
      y
    } else if (y > 100) {
      100
    } else if (y > 25) {
      25
    } else {
      10
    }
  })
}

soqry_trans = function(x){
  as.integer(log10(x + 1))
}

#seclk_dat = read.csv(paste0("~/Downloads/seclk.csv"),header=T,nrow=10000)
#gen(seclk_dat, "rank", limit = 100, seclk_trans)
#
#soqry_dat = read.csv(paste0("~/Downloads/soqry.csv"),header=T,nrow=10000)
#gen(soqry_dat, "runTime", limit=10000, soqry_trans)
