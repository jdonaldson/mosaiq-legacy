require(ggmosaic)
require(stringr)

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
filter_feature=function(x, top=11, breaks=NA){
  if (is.numeric(x)){ 
    # If numeric, calculate histogram breaks
    if (is.na(breaks)){ 
      breaks = top 
    } else {
      breaks = as.numeric(breaks)
      if (min(breaks) >= min(x)){
        breaks = c(min(x), breaks)
      }
      if (max(breaks) <= max(x)){
        breaks = c(breaks, max(x))
      }
    }
    
    hx = hist(x,plot=F, breaks=breaks)
    x = hx$breaks[findInterval(x, hx$breaks)]
  } else { 
    # Otherwise, capture only top n (25) labels
    x = topn(x,top)
  }
  x 
} 

mosaic_feature = function(dat, feature, target, target_levels){
  target_levels = unlist(strsplit(target_levels, ","))
  x = filter_feature(dat[[feature]])
  d = as.data.frame(matrix(nrow=nrow(dat)))
  d[feature] = factor(x)
  target_norms = filter_feature(dat[[target]], breaks=target_levels)
  d[target] = factor(target_norms)
  palette = "RdYlGn"
  if (!is.numeric(target)){
    palette= "Spectral" 
  }
  ggplot(d, aes_string(fill=target)) +  
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
    message(x)
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
