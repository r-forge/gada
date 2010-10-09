countAltered.i<-function(i, x, max.number.cnv, length.base, gen.info)
 {

  chr<-c(1:22,"X","Y")
  selec<-c(1:length(chr))[chr==i]

  xx<-x[[selec]]
  chr.sel<-gen.info[gen.info$chr==i & !is.na(gen.info$chr),]
   

  extract.probe<-function(j,x,a,seg,max.number.cnv)
   {
    xx<-x[[j]]
    if (NROW(xx)>0)
     {
      tt<-xx[,2]-xx[,1]
      cond<-tt>length.base[1] & tt<length.base[2]
      xx<-xx[c(1:nrow(xx))[cond],]

      altered<-xx[xx[,6]==seg, ]

      probes<-NULL

      if (nrow(altered)>0 & nrow(altered)<max.number.cnv)
      {
       for (i in 1:nrow(altered))
        {
         probes.i<-a[a$pos>=altered[i,1] & a$pos<=altered[i,2],1]
         probes<-c(probes,probes.i)
        }
      }
      else
       {
        probes<-NA
       }
     }
    else
     {
      probes<-NA
     } 

    probes

   }

   probes.gain<-lapply(1:length(xx),extract.probe,x=xx,a=chr.sel,seg=1,max.number.cnv=max.number.cnv)
   probes.lose<-lapply(1:length(xx),extract.probe,x=xx,a=chr.sel,seg=-1,max.number.cnv=max.number.cnv)

   if (any(!unlist(lapply(probes.gain,is.na))))
    {
     gains<-data.frame(table(unlist(probes.gain)))
     colnames(gains)[1]<-"probe"
     gains.all<-merge(gains,chr.sel)
    }
   else
    {
     gains.all<-NULL
    }

   if (any(!unlist(lapply(probes.gain,is.na)))) 
    {
     losses<-data.frame(table(unlist(probes.lose)))
     colnames(losses)[1]<-"probe"
     losses.all<-merge(losses,chr.sel)
    }
   else 
    {
     losses.all<-NULL
    }
     




   ans<-list(gains=gains.all, losses=losses.all)

   ans
 }
