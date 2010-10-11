getAlteredProbes<-function(x, chr, min.perc=0.10, max.number.cnv=100, length.base)
 {
  if(!inherits(x,"summaryParGADA"))
   stop("object must be of class 'summaryParGADA'")

  if (missing(length.base))
    length.base<-attr(x,"length.base")

  nSamples<-attr(x,"Samples")[2]

  Info<-attr(x,"Info")
  load(paste(Info,"/SBL/gen.info.Rdata",sep=""))

  ans<-countAltered.i(chr, x, max.number.cnv, length.base, gen.info)

  cc<-ceiling(nSamples*min.perc) 

  gains<-ans$gains[ans$gains$Freq>cc,]
  losses<-ans$losses[ans$losses$Freq>cc,]

  ans<-list(gains=gains, losses=losses)
  ans 
 }
