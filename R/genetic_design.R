#' @author Omar Benites
#' @title North carolina design type I and II
#' @param set The number of sets 
#' @param r replication. At least 2 replications is suitable
#' @param male females. 
#' @param female males
#' @param type number 1: the North Caroline I. Number 2: the North Caroline II
#' @description North Carolina design I (NCDI) involves the performance of one male which is crossed with more than one females, since the male is selected randomly. It's also known as \code{Polygamous matting design}. On the other hand,
#' North Carolina design II (NCDII) takes one female at random which is crossed with more than one male. It's also known as \code{Polyandrous matting design}.  
#' @examples    
#' res <- design_carolina(set= 3, r =2, male = c("m1","m2"), female = c("f1", "f2"), type = 1)       
#' @references Statistical and Biometrical Techniques in Plant Breeding. Jawar R. Sharma. New Age International Publishers 2008.   
#' @importFrom agricolae design.rcbd
#' @importFrom dplyr mutate
#' @export
#' 


design_carolina <- function(set = 2, r = 3, male, female, type = 1){

    seed <- 1234
    
    #match arguments. Decide between north carolina 1 or 2
    type <- type
  
    #Setting NC parameters
    set <- as.numeric(set) #set
    replication <- as.numeric(r) #replication
    male <- as.character(male) #male
    female <- as.character(female) #female
    
    #Length or size of the vector of males and females
    msize <- length(male)  #male size
    fsize <- length(female) #female size 
    
    # Type 1: North Carolina I
    if(type == 1){
      
        #creation of empty data frame  
        diseno1 <- data.frame(SET=NA, PLOT=NA , REP=NA, MALE=NA, FEMALE=NA)
        #initilization of first set (Set1)
        set1 <- data.frame(V1=rep(1:msize, times=fsize), V2= rep(1:fsize, each=msize))
        
        #!is.null: we write in this way due to replication <- as.numeric(r) trows FALSE. So to adapt in this context, we put !is.null()
        if(replication==1 || is.na(replication)){
          print("You have entered just 1 replication or NA/NULL values.")
          return()
    }
          
        if(is.null(male) || is.na(male)){
          print("At minimimun 1 males")
          return()
    }
        
        if(length(female)==1 || is.null(female) || is.na(female)){
          print("At minimimun 2 females")
          return()
     }
        
        if(replication>1 || !is.null(replication) || !is.na(replication)) {
        
          for(j in 1:replication)
          {
          
            nt <- length(male)*length(female)
            tr <- 1:nt 
            est <- cbind(1:nt, rep(male, length(female)), rep(female, each=length(male)))
            diseno <-  as.data.frame(matrix(NA, nrow= replication*nt, ncol=4), stringsAsFactors=F)
            #fdcrd <- design.crd(tr,  replication, number=startn)
            fdcrd <- design.rcbd(trt = tr, r =  replication, seed= seed)
            fdcrd <- fdcrd$book
            diseno[,1:2] <- fdcrd[,1:2]
            ord <- fdcrd[,3]
            
            for (i in 1:(nt* replication)){
              diseno[i,3] <- est[est[,1]==ord[i],2]
              diseno[i,4] <- est[est[,1]==ord[i],3]
            }
            diseno <- data.frame(SET=rep(j,nt* replication),diseno)
            #colnames(diseno) <- c("SET","PLOT", "REP", "MALE", "FEMALE")
            colnames(diseno) <- c("SET","PLOT", "REP", "MALE", "FEMALE")
            diseno1 <- rbind(diseno1, diseno)
            
          }
        
              #remove first row (empty values)
              diseno <- diseno1[-1,]
              #plot first and the other columns at the end.
              diseno <- diseno[, c("PLOT", "SET","REP", "MALE", "FEMALE")] 
              
              #names(diseno) <- c("PLOT", "SET","REP", "MALE", "FEMALE")
              MALE <- FEMALE <- NULL
              diseno <- dplyr::mutate(diseno, FAMILY = paste( MALE, "x", FEMALE, sep = ""))
              diseno <- diseno[, c("PLOT", "SET", "REP", "FAMILY", "MALE", "FEMALE")] 
              type <- "NCI"
        
        }
        
    }
    
    # Type 2: North Carolina II
    if(type == 2){
      
      #creation of empty data frame  
      diseno1 <- data.frame(SET=NA, PLOT=NA , REP=NA, FEMALE=NA, MALE=NA)
      #initilization of first set (Set2)
      
      set1 <- data.frame(V1= rep(1:fsize, each=msize), V2=rep(1:msize, times=fsize))
      
      #!is.null: we write in this way due to replication <- as.numeric(r) trows FALSE. So to adapt in this context, we put !is.null()
      if(replication==1 || is.na(replication)){
        print("You have entered just 1 replication or NA/NULL values.")
        return()
      }
      
      if(is.null(female) || is.na(female)){
        print("At minimimun 1 females")
        return()
      }
      
      if(length(male)==1 || is.null(male) || is.na(male)){
        print("At minimimun 2 males")
        return()
      }
      
      if(replication>1 || !is.null(replication) || !is.na(replication)) {
        
        for(j in 1:replication)
        {
          
          nt <- length(female)*length(male) 
          tr <- 1:nt 
          est <- cbind(1:nt, rep(female, length(male)), rep(male, each=length(female)))
          diseno <-  as.data.frame(matrix(NA, nrow= replication*nt, ncol=4), stringsAsFactors=F)
          #fdcrd <- design.crd(tr,  replication, number=startn)
          fdcrd <- design.rcbd(trt = tr, r =  replication, seed= seed)
          fdcrd <- fdcrd$book
          diseno[,1:2] <- fdcrd[,1:2]
          ord <- fdcrd[,3]
          
          for (i in 1:(nt* replication)){
            diseno[i,3] <- est[est[,1]==ord[i],2]
            diseno[i,4] <- est[est[,1]==ord[i],3]
          }
          diseno <- data.frame(SET=rep(j,nt* replication),diseno)
          
          colnames(diseno) <- c("SET","PLOT", "REP", "FEMALE", "MALE")
          diseno1 <- rbind(diseno1,diseno)
          
        }
        
        #remove first row (empty values)
        diseno <- diseno1[-1,]
        #plot first and the other columns at the end.
        diseno <- diseno[, c("PLOT", "SET","REP", "FEMALE", "MALE")] 
        
        MALE <- FEMALE <- NULL
        diseno <- dplyr::mutate(diseno, FAMILY = paste( FEMALE, "x", MALE, sep = ""))
        diseno <- diseno[, c("PLOT", "SET", "REP", "FAMILY", "FEMALE", "MALE")] 
        type <- "NCII"
        
      }
      
      
      
      
        
      
    }
    
    out <- list(type = type, book = diseno)
    
}


# Line x Tester -----------------------------------------------------------

#' @author Omar Benites
#' @title Line by Tester Design
#'# @param set The number of sets
#' @param r replication. At least 2 replications is suitable
#' @param lines character vector. Generally, the lines (test genotypes, test != testers) are used as female. In case of employing male stile lines as testers. naturally lines becom the pollen parents. 
#' @param testers character vector. Generally testers as male or pollen parents. Minimun 2 parents testers and maximum 8-10 parents testers are adequate. 
#' @param type numeric. 1: parents and full and half siblings. 2: just full and half siblings
#' @description Line by Tester design. 
#' @examples
#' res <- design_lxt(r =2, lines = c("f1","f2","f3","f4","f5"), testers = c("m1", "m2"), type=1 )
#' @references Statistical and Biometrical Techniques in Plant Breeding. Jawar R. Sharma. New Age International Publishers 2008.
#' @importFrom agricolae design.rcbd
#' @importFrom purrr pmap map_chr
#' @importFrom dplyr '%>%' mutate arrange_ select_
#' @importFrom data.table rbindlist
#' @importFrom tidyr separate
#' @export
#' 
design_lxt <- function(r = 3, lines, testers, type=1){

  ln <- as.character(lines)
  tt <- as.character(testers)
  r <- as.numeric(r)
  
  tp <- as.numeric(type)
  
  #!is.null: we write in this way due to replication <- as.numeric(r) trows FALSE. So to adapt in this context, we put !is.null()
  if(length(ln)==1 || is.na(ln)){
    print("Your lines values just have 1 replication or NA/NULL values.")
    return()
  }
  
  if(length(tt)<=1 || is.na(ln)){
    print("At least two testers are required")
    return()
  }
  
  
  rln <- r #replication for lines
  rtt <- r #replicatino for testers
  rlxt <- r #replication for lines x testers

  Var1 <- Var2 <- NULL
  #Crossing of lines x testers: full siblings and half siblings
  sib_lxt <- expand.grid(lines,testers) %>% dplyr::mutate(INSTN= paste(Var1, Var2, sep="x")) %>% dplyr::arrange_("Var1") %>% dplyr::select_("INSTN")
  sib_lxt <- map_chr(sib_lxt[[1]], as.character)
  
  #List of parameters
  list_lntt <-pmap(.l = list( #the list of parameters for each factor: lines, testers and lxt
                               trt = list(lines, testers, sib_lxt),
                                r  = list(rln, rtt, rlxt)
                              ),
                    .f =  design.rcbd)

  #list of books

    fb_lines <- list_lntt[[1]][["book"]]
    names(fb_lines) <- c("PLOT","REP","LINE")
    
    fb_testers <- list_lntt[[2]][["book"]]
    names(fb_testers) <- c("PLOT","REP","TESTER")

    fb_lxt <- list_lntt[[3]][["book"]]
    names(fb_lxt) <- c("PLOT","REP","LXT")

    if(type ==1){
        book_lntt <- list(
        fb_lines <- fb_lines, #lines design (book)
        fb_testers <-fb_testers, #testers design  (book)
        fb_lxt <- fb_lxt #crossing between lines and testers (book)
     )
    }
    
    if(type ==2){
      book_lntt <- list(
        fb_lxt <- fb_lxt #crossing between lines and testers (book)
      )
    }
  
    out_table <- rbindlist(book_lntt, use.names=TRUE, fill=TRUE)
    out_table <- out_table %>% dplyr::arrange_("REP")  
  
    if(type ==2){
      out_table <- separate(data = out_table, col = "LXT",  c("LINE", "TESTER"), sep = "x")
    }
  
  
  diseno <- out_table
  out <- list(book = diseno, type = type, design= "LXT")
  out
}


