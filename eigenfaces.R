####################
# 
#
# Michael Regan
# mr3543@columbia.edu
# Stat 4240
# HW 2 Problem 2
# 
#
#####################


#(a)

#set working directory to directory containing CroppedYale repository

library("pixmap")

dir_list_1 = dir(path="CroppedYale/",all.files=FALSE)
dir_list_2 = dir(path="CroppedYale/",all.files=FALSE,recursive=TRUE)

len_dl1 = length(dir_list_1)
len_dl2 = length(dir_list_2)
view_list = c( 'P00A+000E+00','P00A+005E+10','P00A+005E-10','P00A+010E+00' )

faces_matrix = vector()

for (i in 1:len_dl1){
	this_face_row = vector()
	for(j in 1:length(view_list)){
		this_filename = sprintf("CroppedYale/%s/%s_%s.pgm",dir_list_1[i],dir_list_1[i],view_list[j])
		#print(this_filename)
		this_face = read.pnm(file=this_filename)
		this_face_matrix = getChannels(this_face)
		this_face_vector = as.vector(this_face_matrix)
		faces_matrix = rbind(faces_matrix, this_face_vector)
		}
		
	}
	
dim(faces_matrix)

#(b)

meanFace = apply(faces_matrix,2,mean)
meanMat = matrix(meanFace, nrow = 192,ncol=168)

mf = pixmapGrey(meanMat)
plot(mf)

#(c)

cFaceMat = faces_matrix
for (i in 1:dim(cFaceMat)[1]){
	cFaceMat[i,] = cFaceMat[i,] - meanFace
	}

pc = prcomp(cFaceMat)

ev = pc$sdev^2
propExp = vector(mode="integer", length= length(ev))
for (i in 1:length(propExp)){
	propExp[i] = sum(ev[1:i])/sum(ev)
}
	
plot(propExp,main="Proportion of variance explained vs # of Eigenvectors")

#(d)
k=1
eigenFaceMat = vector()
for(i in 1:3){
	faceRow = vector()
		for (j in 1:3){
			fMatrix = matrix(pc$rotation[,k],nrow = 192,ncol=168)
			faceRow = cbind(faceRow,fMatrix)
			k = k + 1
			}
		eigenFaceMat = rbind(eigenFaceMat,faceRow)
}

eigenFaces = pixmapGrey(eigenFaceMat)
plot(eigenFaces)
			
#(e)

#faceList = vector(mode="list",length = 25)

scores = cFaceMat%*%pc$rotation

mat_list = list()
mat_list[[1]] = meanMat

for (i in 1:24){
	mat_list[[i+1]] = mat_list[[i]] + matrix(scores[20,i]*t(pc$rotation[,i]),nrow=192,ncol=168)
	}
	

new_mat = vector()

k=1
for (i in 1:5){
	new_row = vector()
	for (j in 1:5){
		new_row = cbind(new_row,mat_list[[k]])
		k=k+1
		}
	new_mat = rbind(new_mat,new_row)
}

nm = pixmapGrey(new_mat)
plot(nm)

mat_list_by5 = list()
mat_list_by5[[1]] = meanMat
mat = matrix()


k=1
for (i in 1:24){
	mat = matrix(0,nrow=192,ncol=168)
	for(j in 1:5){
		mat = mat + matrix(scores[20,k]*t(pc$rotation[,k]),nrow=192,ncol=168)
		k = k + 1
		}
		mat_list_by5 [[i+1]] = mat_list_by5[[i]] + mat
}

new_mat_by5 = vector()
k=1
for (i in 1:5){
	new_row = vector()
	for (j in 1:5){
		new_row = cbind(new_row,mat_list_by5[[k]])
		k=k+1
		}
	new_mat_by5 = rbind(new_mat_by5,new_row)
}

nmb5 = pixmapGrey(new_mat_by5)
plot(nmb5)

#(f)

faces_matrix_2 = faces_matrix[-(1:4),]

meanFace2 = apply(faces_matrix_2,2,mean)

cFaceMat_2 = faces_matrix_2
for (i in 1:dim(cFaceMat_2)[1]){
	cFaceMat_2[i,] = cFaceMat_2[i,] - meanFace2
	}

pc2 = prcomp(cFaceMat_2)

targetFace = faces_matrix[4,]
tf_demean = targetFace - meanFace2

tf_scores = tf_demean%*%pc2$rotation

tf_new = meanFace2
for (i in 1:dim(pc2$rotation)[2]){
	tf_new = tf_new + tf_scores[,i]*t(pc2$rotation[,i])
	}

tf_new_mat = matrix(tf_new,nrow = 192,ncol=168)
tfn = pixmapGrey(tf_new_mat)
plot(tfn)


