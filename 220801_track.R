setwd("O:/Data/Nina/220728_track")

files<-list.files(pattern = "Cell.csv")

### colnames need improvemnt (: best from the past
colnames<-c(


"Parent_ID","Time_index","Center_x","Center_Y","Object_ID","Area","Circularity","Mean_Red","SD_Red","Sum_Red",



"Red_Drop_Parent_ID","Red_Drop_ID","Red_Drop_Count","Red_Drop_Area","Red_Drop_Regions","Red_Drop_Mean_Area","Red_Drop_Mean_red","Red_Drop_SD_red","Red_Drop_Sum_red")



### specify

t_index<-2

### specify columns with x & y bounds
posx<-3
posy<-4


### plot delimiter

xlow<-0
xhigh<-100
ylow<-0
yhigh<-100



### specify allowed movement here nm 
move<-2500

### file read in loop

for (k in 1:length(files))
	{
	data_raw<-read.csv(files[k], header = FALSE, skip=2, dec=",", sep=";")


### object loop = l, 


	init_objects<-subset(data_raw,data_raw[,t_index]==1)
	
	for (l in 1:length(init_objects[,1]))
		{
		result<-matrix(nrow=max(data_raw[,t_index]),ncol=length(init_objects[1,]))
		 
### time loop =m		
			
			result[1,]<-as.numeric(init_objects[l,])


			for (m in 2:max(data_raw[,t_index]))
			{
			
### find nearest value
				
				fol_objects<-subset(data_raw,data_raw[,t_index]==m)

			

				distmap<-matrix(nrow=1,ncol=length(fol_objects[,1]))
				for (n in 1:length(fol_objects[,1]))
					{
					sing_fol_objects<-fol_objects[n,]
					distmap[n] <- sqrt((result[m-1,posx] - sing_fol_objects[posx])^2+ (result[m-1,posy] - sing_fol_objects[posy])^2)			
					}

					

						mini<-which.min(distmap)
						if (distmap[mini]< move)
							{
							result[m,]<-as.numeric(fol_objects[mini,])
							}
						else
							{
							result[m,]<-as.numeric(result[m-1,])
							}

				
					
			
			}
					

			write.table(result,paste(substring(files[k],1,nchar(files[k])-4),"_",l,".txt",sep=""), col.names=colnames, row.names=FALSE, sep="\t")	
			


		}		
	

	}


