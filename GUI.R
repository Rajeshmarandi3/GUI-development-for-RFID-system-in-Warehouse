#Loading libraries

library(Rglpk)
library(tcltk)


##############################
tt <- tktoplevel()
tkwm.title(tt,"gui")

############################################################
############ Creating textfield for parameters #############
############################################################

f_range1 <- tclVar("")
e_f_range <-tkentry(tt,width="20", textvariable=f_range1)
tkgrid(tklabel(tt,text="F_range:"), e_f_range,sticky="E")

lembda1 <- tclVar("")
e_lembda <-tkentry(tt,width="20", textvariable=lembda1)
tkgrid(tklabel(tt,text="lembda:"), e_lembda,sticky="E")

Gt <- tclVar("")
e_Gt <-tkentry(tt,width="20", textvariable=Gt)
tkgrid(tklabel(tt,text="G_t:"), e_Gt,sticky="E")

Pt <- tclVar("")
e_Pt <-tkentry(tt,width="20", textvariable=Pt)
tkgrid(tklabel(tt,text="P_t:"), e_Pt,sticky="E")

Gr <- tclVar("")
e_Gr <-tkentry(tt,width="20", textvariable=Gr)
tkgrid(tklabel(tt,text="G_r:"), e_Gr,sticky="E")

Pth <- tclVar("")
e_Pth <-tkentry(tt,width="20", textvariable=Pth)
tkgrid(tklabel(tt,text="Pth:"), e_Pth,sticky="E")

cbln <- tclVar("")
e_cbln <-tkentry(tt,width="20", textvariable=cbln)
tkgrid(tklabel(tt,text="cable_length:"), e_cbln,sticky="E")

nop <- tclVar("")
e_nop <-tkentry(tt,width="20", textvariable=nop)
tkgrid(tklabel(tt,text="No of ports:"), e_nop,sticky="E")

norw <- tclVar("")
e_norw <-tkentry(tt,width="20", textvariable=norw)
tkgrid(tklabel(tt,text="no_of_row:"), e_norw,sticky="E")

nocl <- tclVar("")
e_nocl <-tkentry(tt,width="20", textvariable=nocl)
tkgrid(tklabel(tt,text="no_of_column:"), e_nocl,sticky="E")

stl <- tclVar("")
e_stl <-tkentry(tt,width="20", textvariable=stl)
tkgrid(tklabel(tt,text="stack size length:"), e_stl,sticky="E")

stb <- tclVar("")
e_stb <-tkentry(tt,width="20", textvariable=stb)
tkgrid(tklabel(tt,text="stack size breadth:"), e_stb,sticky="E")

rspc <- tclVar("")
e_rspc <-tkentry(tt,width="20", textvariable=rspc)
tkgrid(tklabel(tt,text="row space:"), e_rspc,sticky="E")

clspc <- tclVar("")
e_clspc <-tkentry(tt,width="20", textvariable=clspc)
tkgrid(tklabel(tt,text="column space:"), e_clspc,sticky="E")

crdr <- tclVar("")
e_crdr <-tkentry(tt,width="20", textvariable=crdr)
tkgrid(tklabel(tt,text="cost of reader:"), e_crdr,sticky="E")

cant <- tclVar("")
e_cant <-tkentry(tt,width="20", textvariable=cant)
tkgrid(tklabel(tt,text="cost of antenna:"), e_cant,sticky="E")

ctag <- tclVar("")
e_ctag <-tkentry(tt,width="20", textvariable=ctag)
tkgrid(tklabel(tt,text="cost of tag:"), e_ctag,sticky="E")

##########################################################################################
#########                  Creating textfield for plot coordinates       #################
##########################################################################################

x1 <- tclVar("")
x1_range <-tkentry(tt,width="20", textvariable=x1)
xl1=tklabel(tt,text="x11,y11 :")
tkgrid(xl1,row=0,column=3,sticky="E")
tkgrid(x1_range,row=0,column=4)

x2 <- tclVar("")
x2_range <-tkentry(tt,width="20", textvariable=x2)
xl2=tklabel(tt,text="x12,y12 :")
tkgrid(xl2,row=0,column=5,sticky="E")
tkgrid(x2_range,row=0,column=6)
################33333333
x3 <- tclVar("")
x3_range <-tkentry(tt,width="20", textvariable=x3)
xl3=tklabel(tt,text="x21,y21 :")
tkgrid(xl3,row=1,column=3,sticky="E")
tkgrid(x3_range,row=1,column=4)

x4 <- tclVar("")
x4_range <-tkentry(tt,width="20", textvariable=x4)
xl4=tklabel(tt,text="x22,y22 :")
tkgrid(xl4,row=1,column=5,sticky="E")
tkgrid(x4_range,row=1,column=6)
###############################333
x5 <- tclVar("")
x5_range <-tkentry(tt,width="20", textvariable=x5)
xl5=tklabel(tt,text="x31,y31 :")
tkgrid(xl5,row=2,column=3,sticky="E")
tkgrid(x5_range,row=2,column=4)

x6 <- tclVar("")
x6_range <-tkentry(tt,width="20", textvariable=x6)
xl6=tklabel(tt,text="x32,y32 :")
tkgrid(xl6,row=2,column=5,sticky="E")
tkgrid(x6_range,row=2,column=6)
###############################33
x7 <- tclVar("")
x7_range <-tkentry(tt,width="20", textvariable=x7)
xl7=tklabel(tt,text="x41,y41 :")
tkgrid(xl7,row=3,column=3,sticky="E")
tkgrid(x7_range,row=3,column=4)

x8 <- tclVar("")
x8_range <-tkentry(tt,width="20", textvariable=x8)
xl8=tklabel(tt,text="x42,y42 :")
tkgrid(xl8,row=3,column=5,sticky="E")
tkgrid(x8_range,row=3,column=6)
####
x9 <- tclVar("")
x9_range <-tkentry(tt,width="20", textvariable=x9)
xl9=tklabel(tt,text="x51,y51 :")
tkgrid(xl9,row=4,column=3,sticky="E")
tkgrid(x9_range,row=4,column=4)

x10 <- tclVar("")
x10_range <-tkentry(tt,width="20", textvariable=x10)
xl10=tklabel(tt,text="x52,y52 :")
tkgrid(xl10,row=4,column=5,sticky="E")
tkgrid(x10_range,row=4,column=6)
###
x11 <- tclVar("")
x11_range <-tkentry(tt,width="20", textvariable=x11)
xl11=tklabel(tt,text="x61,y61 :")
tkgrid(xl11,row=5,column=3,sticky="E")
tkgrid(x11_range,row=5,column=4)

x12 <- tclVar("")
x12_range <-tkentry(tt,width="20", textvariable=x12)
xl12=tklabel(tt,text="x62,y62 :")
tkgrid(xl12,row=5,column=5,sticky="E")
tkgrid(x12_range,row=5,column=6)
###
x13 <- tclVar("")
x13_range <-tkentry(tt,width="20", textvariable=x13)
xl13=tklabel(tt,text="x71,y71 :")
tkgrid(xl13,row=6,column=3,sticky="E")
tkgrid(x13_range,row=6,column=4)

x14 <- tclVar("")
x14_range <-tkentry(tt,width="20", textvariable=x14)
xl14=tklabel(tt,text="x72,y72 :")
tkgrid(xl14,row=6,column=5,sticky="E")
tkgrid(x14_range,row=6,column=6)
###
x15 <- tclVar("")
x15_range <-tkentry(tt,width="20", textvariable=x15)
xl15=tklabel(tt,text="x81,y81 :")
tkgrid(xl15,row=7,column=3,sticky="E")
tkgrid(x15_range,row=7,column=4)

x16 <- tclVar("")
x16_range <-tkentry(tt,width="20", textvariable=x16)
xl16=tklabel(tt,text="x82,y82 :")
tkgrid(xl16,row=7,column=5,sticky="E")
tkgrid(x16_range,row=7,column=6)
###
x17 <- tclVar("")
x17_range <-tkentry(tt,width="20", textvariable=x17)
xl17=tklabel(tt,text="x91,y91 :")
tkgrid(xl17,row=8,column=3,sticky="E")
tkgrid(x17_range,row=8,column=4)

x18 <- tclVar("")
x18_range <-tkentry(tt,width="20", textvariable=x18)
xl18=tklabel(tt,text="x92,y92 :")
tkgrid(xl18,row=8,column=5,sticky="E")
tkgrid(x18_range,row=8,column=6)
###
x19 <- tclVar("")
x19_range <-tkentry(tt,width="20", textvariable=x19)
xl19=tklabel(tt,text="x101,y101 :")
tkgrid(xl19,row=9,column=3,sticky="E")
tkgrid(x19_range,row=9,column=4)

x20 <- tclVar("")
x20_range <-tkentry(tt,width="20", textvariable=x20)
xl20=tklabel(tt,text="x102,y102 :")
tkgrid(xl20,row=9,column=5,sticky="E")
tkgrid(x20_range,row=9,column=6)

###############################################################################
###############                  Back-end coding                ###############
###############################################################################

optimize=function(){
  f_range=as.numeric(tclvalue(f_range1))
  lembda=as.numeric(tclvalue(lembda1))
  G_t=as.numeric(tclvalue(Gt))
  P_t=as.numeric(tclvalue(Pt))
  G_r=as.numeric(tclvalue(Gr))
  P_th=as.numeric(tclvalue(Pth))
  cable_length=as.numeric(tclvalue(cbln))
  no_of_ports=as.numeric(tclvalue(nop))
  no_row=as.numeric(tclvalue(norw))
  no_col=as.numeric(tclvalue(nocl))
  stack_size_l=as.numeric(tclvalue(stl))
  stack_size_b=as.numeric(tclvalue(stb))
  row_space=as.numeric(tclvalue(rspc))
  col_space=as.numeric(tclvalue(clspc))
  cost_reader=as.numeric(tclvalue(crdr))
  cost_antenna=as.numeric(tclvalue(cant))
  cost_tag=as.numeric(tclvalue(ctag))
######################
  x1=tclvalue(x1);x1=unlist(strsplit(x1,","));x1=as.numeric(x1)
  x2=tclvalue(x2);x2=unlist(strsplit(x2,","));x2=as.numeric(x2)
  x3=tclvalue(x3);x3=unlist(strsplit(x3,","));x3=as.numeric(x3)
  x4=tclvalue(x4);x4=unlist(strsplit(x4,","));x4=as.numeric(x4)
  x5=tclvalue(x5);x5=unlist(strsplit(x5,","));x5=as.numeric(x5)
  x6=tclvalue(x6);x6=unlist(strsplit(x6,","));x6=as.numeric(x6)
  x7=tclvalue(x7);x7=unlist(strsplit(x7,","));x7=as.numeric(x7)
  x8=tclvalue(x8);x8=unlist(strsplit(x8,","));x8=as.numeric(x8)
  x9=tclvalue(x9);x9=unlist(strsplit(x9,","));x9=as.numeric(x9)
  x10=tclvalue(x10);x10=unlist(strsplit(x10,","));x10=as.numeric(x10)
  x11=tclvalue(x11);x11=unlist(strsplit(x11,","));x11=as.numeric(x11)
  x12=tclvalue(x12);x12=unlist(strsplit(x12,","));x12=as.numeric(x12)
  x13=tclvalue(x13);x13=unlist(strsplit(x13,","));x13=as.numeric(x13)
  x14=tclvalue(x14);x14=unlist(strsplit(x14,","));x14=as.numeric(x14)
  x15=tclvalue(x15);x15=unlist(strsplit(x15,","));x15=as.numeric(x15)
  x16=tclvalue(x16);x16=unlist(strsplit(x16,","));x16=as.numeric(x16)
  x17=tclvalue(x17);x17=unlist(strsplit(x17,","));x17=as.numeric(x17)
  x18=tclvalue(x18);x18=unlist(strsplit(x18,","));x18=as.numeric(x18)
  x19=tclvalue(x19);x19=unlist(strsplit(x19,","));x19=as.numeric(x19)
  x20=tclvalue(x20);x20=unlist(strsplit(x20,","));x20=as.numeric(x20)
 #############
  v1=c(x1,x2);v2=c(x3,x4);v3=c(x5,x6);v4=c(x7,x8);v5=c(x9,x10);v6=c(x11,x12);v7=c(x13,x14);v8=c(x15,x16);v9=c(x17,x18);v10=c(x19,x20)
  
  rectangle=array(dim=c(10,4))
  V=list(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10)
  for (r in 1:10){
    for (s in 1:4){
      rectangle[r,s]=V[[r]][s]
    }
  }
  print(rectangle)
#############
  
  Range=(lembda/(4*3.14))*sqrt(P_t*G_t*G_r/P_th)
  x_ij=array(dim=c((no_row+1)*(no_col+1),2))
  y_ij=array(dim=c((no_row+1)*(no_col+1),2))
  z_ij=array(dim=c((2*no_row+1)*(2*no_col+1),2))
  k=1
  for(i in 1:(no_row+1)){
    for(j in 1:(no_col+1)){
      x_ij[k,1]=(j-1)*(stack_size_l+row_space)
      x_ij[k,2]=(i-1)*(stack_size_b+col_space)
      k=k+1
    }
  }
  k=1
  for(i in 1:(no_row+1)){
    for(j in 1:(no_col+1)){
      y_ij[k,1]=(j-1)*(stack_size_l+row_space)
      y_ij[k,2]=(i-1)*(stack_size_b+col_space)
      k=k+1
    }
  }
  k=1
  for(i in 1:(2*no_row+1)){
    for(j in 1:(2*no_col+1)){
      z_ij[k,1]=(j-1)*(stack_size_l+row_space)/2
      z_ij[k,2]=(i-1)*(stack_size_b+col_space)/2
      k=k+1
    }
  }
  a_ij=array(dim=c((2*no_row+1)*(2*no_col+1),(no_row+1)*(no_col+1)))
  
  
  for(i in 1:((2*no_row+1)*(2*no_col+1))){
    q=0
    for (k in 1:10){
      if((rectangle[k,1]==rectangle[k,3]) && (rectangle[k,2]==rectangle[k,4])){
        break
      }
      else if(((z_ij[i,1]>=rectangle[k,1]) && (z_ij[i,1]<=rectangle[k,3])) && ((z_ij[i,2]>=rectangle[k,2]) && (z_ij[i,2]<=rectangle[k,4]))){
        q=1
        break
      }
    }
    
    for(j in 1:((no_row+1)*(no_col+1))){
      p=sqrt((x_ij[j,1]-z_ij[i,1])^2+(x_ij[j,2]-z_ij[i,2])^2)
      if(p<=Range||q==0){
        a_ij[i,j]=1
      }else {
        a_ij[i,j]=0
      }
    }
  }
  d_jk=array(dim=c((no_row+1)*(no_col+1),(no_row+1)*(no_col+1)))
  for(j in 1:((no_row+1)*(no_col+1))){
    for(k in 1:((no_row+1)*(no_col+1))){
      q=(sqrt((x_ij[j,1]-y_ij[k,1])^2)+sqrt((x_ij[j,2]-y_ij[k,2])^2))
      if(q<=cable_length){
        d_jk[j,k]=1
      } else{
        d_jk[j,k]=0
      }
    }
  }
  
  
  
  obj =matrix(nrow=1,ncol=((no_row+1)*(no_col+1)*2))
  for(i in 1:((no_row+1)*(no_col+1)*2)){
    if(i<=((no_row+1)*(no_col+1))){
      obj[1,i]=cost_antenna
    } else{
      obj[1,i]=cost_reader
    }
  }
  
  mat=matrix(nrow=((2*no_row+1)*(2*no_col+1)+(no_row+1)*(no_col+1)*2),ncol=((no_row+1)*(no_col+1)*2))
  
  for(i in 1:((2*no_row+1)*(2*no_col+1)+(no_row+1)*(no_col+1)*2)){
    for(j in 1:((no_row+1)*(no_col+1)*2)){
      
      if(i<=((2*no_row+1)*(2*no_col+1))){
        if(j<=((no_row+1)*(no_col+1))){
          mat[i,j]=a_ij[i,j]
        }else{
          mat[i,j]=0
        }
      }
      
      else if(i>((2*no_row+1)*(2*no_col+1))&&i<=((2*no_row+1)*(2*no_col+1)+(no_row+1)*(no_col+1))){
        if(j<=((no_row+1)*(no_col+1))){
          if(j==(i-((2*no_row+1)*(2*no_col+1)))){
            mat[i,j]=-1
          }
          else{
            mat[i,j]=0
          }
        }
        else{
          mat[i,j]=d_jk[(i-((2*no_row+1)*(2*no_col+1))),(j-(no_row+1)*(no_col+1))]
        }
      }
      
      else{
        if(j<=((no_row+1)*(no_col+1))){
          mat[i,j]=d_jk[j,(i-((2*no_row+1)*(2*no_col+1)+(no_row+1)*(no_col+1)))]
        }else{
          mat[i,j]=0
        }
        
      }
    }
  }
  
  dir=matrix(nrow=1,ncol=((2*no_row+1)*(2*no_col+1)+(no_row+1)*(no_col+1)*2))
  for(j in 1:((2*no_row+1)*(2*no_col+1)+(no_row+1)*(no_col+1)*2)){
    if(j<=((2*no_row+1)*(2*no_col+1))){
      dir[1,j]=">="
    }
    else if(j>((2*no_row+1)*(2*no_col+1))&&j<=(((2*no_row+1)*(2*no_col+1))+(no_row+1)*(no_col+1))){
      dir[1,j]=">="
    } 
    else{
      dir[1,j]="<="
    }
  }
  
  rhs=matrix(nrow=1,ncol=((2*no_row+1)*(2*no_col+1)+(no_row+1)*(no_col+1)*2))
  
  for(j in 1:((2*no_row+1)*(2*no_col+1)+(no_row+1)*(no_col+1)*2)){
    if(j<=((2*no_row+1)*(2*no_col+1))){
      rhs[1,j]=1
    }
    else if(j>((2*no_row+1)*(2*no_col+1))&&j<=(((2*no_row+1)*(2*no_col+1))+(no_row+1)*(no_col+1))){
      rhs[1,j]=0
    } 
    else{
      rhs[1,j]=no_of_ports
    }
  }
  
  types=matrix(nrow=1,ncol=((2*no_row+1)*(2*no_col+1)+(no_row+1)*(no_col+1)*2))
  
  for(j in 1:((2*no_row+1)*(2*no_col+1)+(no_row+1)*(no_col+1)*2)){
    types="B"
  }
  
  sol=Rglpk_solve_LP(obj, mat, dir, rhs, types = types, max = FALSE)
  
##########################################################################################
######################           Implementing codes for plotting      ####################
##########################################################################################
  
  #making scroll button
  scr1 <- tkscrollbar(tt,repeatinterval=5,command=function(...)tkyview(lsbDim1,...))
  scr2 <- tkscrollbar(tt,repeatinterval=5,command=function(...)tkxview(lsbDim1,...),orient="horizontal")
  
  #making a object listbox
  
  lsbDim1<-tklistbox(tt,width=30,height=8,selectmode="multiple",xscrollcommand=function(...)tkset( scr2, ... ),yscrollcommand=function(...)tkset(scr1,...),background="light blue",exportselection=FALSE)
  
  #Insering vales for printing
  
  text1=paste("optimum sol. is " ,sol$optimum )
  tkinsert(lsbDim1,"end",text1)
  
  tkinsert(lsbDim1,"end","FOR Reader")
  matreader=matrix(nrow=1,ncol=2)
  for(i in 1:(((no_row+1)*(no_col+1)*2))){
    if(sol$solution[i]==1&&i>((no_row+1)*(no_col+1))){
      txt12=paste("x= ",x_ij[(i-((no_row+1)*(no_col+1))),1],"and y= ",x_ij[(i-((no_row+1)*(no_col+1))),2],sep=" ") 
      tkinsert(lsbDim1,"end",txt12)
      matreader=rbind(matreader,cbind(x_ij[(i-((no_row+1)*(no_col+1))),1]*5,x_ij[(i-((no_row+1)*(no_col+1))),2]*5))
    }
  }
  matreader=na.omit(matreader)
  matantenna=matrix(nrow=1,ncol=2)
  tkinsert(lsbDim1,"end","FOR Antenna")
  
  for(i in 1:(((no_row+1)*(no_col+1)*2))){
    if(sol$solution[i]==1&&i<=((no_row+1)*(no_col+1))){
      
      txt11=paste("x= ",x_ij[i,1],"and y= ",x_ij[i,2],sep=" ") 
      tkinsert(lsbDim1,"end",txt11)
      matantenna=rbind(matantenna,cbind(x_ij[i,1]*5,x_ij[i,2]*5))
      
    }}
  matantenna=na.omit(matantenna)
  
  #Plot Layout fitting using grid
  
  tkgrid(lsbDim1,scr1,row=19,column=0)
  tkgrid(scr1,row=19,column=1,sticky="NES")
  #tkgrid.configure(scr1,rowspan=20)
  tkgrid(scr2,row=20,column=0,sticky="new")
  lbl.MainT<-tklabel(tt,text=" ")
  lbl.MainT1<-tklabel(tt,text=" ")
  
  tkgrid(lbl.MainT,row=21,column=0)
  tkgrid(lbl.MainT1,row=22,column=0)
  ####################################  
  
  # Plotting graph using R canvas
  
  plotit=tktoplevel()
  tkwm.title(plotit,"RFID plot")
  canvas <- tkcanvas(plotit,width=1300, height=700)
  tkpack(canvas, side="top", fill="x")
  item <- tkcreate ( canvas , "rectangle" , no_row*(stack_size_l+row_space)*5, 0 , 0 , no_col*(stack_size_b+col_space)*5 ,
                     width = 2 , outline = "black" ,
                     fill = "light yellow" )
  for(i in 1:10){
    tkcreate ( canvas , "rectangle" , rectangle[i,1]*5 ,rectangle[i,4]*5  , (rectangle[i,3])*5 , (rectangle[i,2])*5 ,
               width = 2 , outline = "dark blue" ,fill="light blue"
    )
  }
  points=matreader
  point.items <- apply(points, 1, function(row) {
    x <- row[1]
    y <- row[2]
    item <- tkcreate(canvas, "oval", x - 5, y - 5, x + 5, y + 5,
                     width=1, outline="black",
                     fill="red")
    tkaddtag(canvas, "point", "withtag", item)
    item
  })
  points=matantenna
  point.items <- apply(points, 1, function(row) {
    x <- row[1]
    y <- row[2]
    item <- tkcreate(canvas, "oval", x - 5, y - 5, x + 5, y + 5,
                     width=1, outline="black",
                     fill="green")
    tkaddtag(canvas, "point", "withtag", item)
    item
  })
  
   # Creating rectangle
  
  for (i in 1:130){
    tkcreate(canvas,"line",10 * i, 0, 10 * i, 800)}
  for(j in 1:130){
    tkcreate(canvas,"line",0, 10 * j, 1300, 10*j)
  }
  for (i in 1:130){
    tkcreate(canvas,"line",10 * i*10, 0, 10 * i*10, 800,width=2)}
  for(j in 1:130){
    tkcreate(canvas,"line",0, 10 * j*10, 1300, 10*j*10,width=2)
  }
  item <- tkcreate ( canvas , "rectangle" , 1140 , 0 , 1340 , 100 ,
                     width = 1 , outline = "black" ,
                     fill = "white" )
  
  tkcreate(canvas, "text", 1220, 20, text="\n\n\nScale:\nX axis:1 unit=20 div\ny axis:1 unit=20 div\n\     for reader\n    for antenna",
           fill="brown")
  
  
  item <- tkcreate(canvas, "oval", 1160 - 5, 70 - 5, 1160 + 5, 70 + 5,
                   width=1, outline="black",
                   fill="red")
  item <- tkcreate(canvas, "oval", 1160 - 5, 90 - 5, 1160 + 5, 90 + 5,
                   width=1, outline="black",
                   fill="green")
  
  
  
  
  
  
  
}

OK.but <-tkbutton(tt,text="    calculate   ",
                  command=optimize)
tkgrid(OK.but)
tkgrid.configure(OK.but)
tkfocus(tt)
tkwm.resizable(tt,TRUE,TRUE)
tkwm.maxsize(tt,1300,800)
tkwait.window(tt)
