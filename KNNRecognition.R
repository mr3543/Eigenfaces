####################
# 
#
# Michael Regan
# mr3543@columbia.edu
# Stat 4240 Sec 001
# HW 3 Problem 4
# 
#
#####################

library("pixmap")
library("class")

#(a)

#set working directory to the directory containing CroppedYale dataset

#load faces
dir_list_1 = dir(path="CroppedYale/",all.files=FALSE)
dir_list_2 = dir(path="CroppedYale/",all.files=FALSE,recursive=TRUE)

len_dl1 = length(dir_list_1)
len_dl2 = length(dir_list_2)
view_list = c( 'P00A+000E+00','P00A+005E+10','P00A+005E-10','P00A+010E+00' )

face_matrix_6a = vector()
subjNo = vector()
view = vector()

#create one large matrix with each face as a row
#add subject and view to data frame
for (i in 1:len_dl1){
  this_face_row = vector()
  for(j in 1:length(view_list)){
    subjNo = c(subjNo,i)
    view = c(view,view_list[j])
    
    this_filename = sprintf("CroppedYale/%s/%s_%s.pgm",dir_list_1[i],dir_list_1[i],view_list[j])
    #print(this_filename)
    this_face = read.pnm(file=this_filename)
    this_face_matrix = getChannels(this_face)
    this_face_vector = as.vector(this_face_matrix)
    face_matrix_6a= rbind(face_matrix_6a, this_face_vector)
  }
  
}


faceDF = data.frame(subjNo,view)

fm_6a_size = dim(face_matrix_6a)
# Use 4/5 of the data for training, 1/5 for testing
ntrain_6a = floor(fm_6a_size[1]*4/5)
ntest_6a = fm_6a_size[1]-ntrain_6a
set.seed(1)
ind_train_6a = sample(1:fm_6a_size[1],ntrain_6a)
ind_test_6a = c(1:fm_6a_size[1])[-ind_train_6a]

#print the first 5 training faces

for(i in 1:5){
  print(faceDF[ind_train_6a[i],])
  
}

#print the first 5 test faces

for(i in 1:5){
  print(faceDF[ind_test_6a[i],])
  
}

#(b)


trainingFaces = vector()
testFaces = vector()


#row bind all training indicies 
for(i in 1:length(ind_train_6a)){
  trainingFaces = rbind(trainingFaces,face_matrix_6a[ind_train_6a[i],])
}

#row bind all test indicies
for(i in 1:length(ind_test_6a)){
  testFaces = rbind(testFaces,face_matrix_6a[ind_test_6a[i],])
}

#calculate the mean face
meanFace = apply(trainingFaces,2,mean)

#center the training data
for (i in 1:nrow(trainingFaces)){
  trainingFaces[i,] = trainingFaces[i,] - meanFace
}


#center the test data
for (i in 1:nrow(testFaces)){
  testFaces[i,] = testFaces[i,] - meanFace
}

#get PCs
pc = prcomp(trainingFaces)

#get scores USE SCORES FOR TRAINING X
testProj = testFaces%*%pc$rotation[,c(1:25)]
trainingProj = trainingFaces%*%pc$rotation[,c(1:25)]

cl = vector()
for (i in 1:length(ind_train_6a)){
  cl = c(cl,faceDF[ind_train_6a[i],1])
}


faceknn = knn(trainingProj,testProj,cl,k=1)
actualLabels = vector()

for (i in 1:length(ind_test_6a)){
  actualLabels = c(actualLabels,faceDF[ind_test_6a[i],1])
}

faceknnv = as.vector(faceknn,mode="integer")

mislabeled = 0 
for (i in 1:length(actualLabels)){
  if (actualLabels[i] != faceknnv[i]){
    mislabeled = mislabeled + 1
    frame()
    actual = matrix(face_matrix_6a[ind_test_6a[i],],nrow=192,ncol=168)
    predicted  = matrix(face_matrix_6a[faceknnv[i]*4,],nrow=192,ncol=168)
    faceplot = cbind(actual,predicted)
    faceplot = pixmapGrey(faceplot)
    plot(faceplot)
    title(main = "actual face and predicted face, actual face on left")
    
  }
}

print(mislabeled)

#(c)


dir_list_1 = dir(path="CroppedYale/",all.files=FALSE)
dir_list_2 = dir(path="CroppedYale/",all.files=FALSE,recursive=TRUE)

len_dl1 = length(dir_list_1)
len_dl2 = length(dir_list_2)
view_list = c('P00A-035E+15','P00A-050E+00','P00A+035E+15','P00A+050E+00')

face_matrix_6c = vector()
subjNo = vector()
view = vector()


for (i in 1:len_dl1){
  this_face_row = vector()
  for(j in 1:length(view_list)){
    subjNo = c(subjNo,i)
    view = c(view,view_list[j])
    
    this_filename = sprintf("CroppedYale/%s/%s_%s.pgm",dir_list_1[i],dir_list_1[i],view_list[j])
    #print(this_filename)
    this_face = read.pnm(file=this_filename)
    this_face_matrix = getChannels(this_face)
    this_face_vector = as.vector(this_face_matrix)
    face_matrix_6c= rbind(face_matrix_6c, this_face_vector)
  }
  
}


faceDF = data.frame(subjNo,view)

fm_6c_size = dim(face_matrix_6c)
# Use 4/5 of the data for training, 1/5 for testing
ntrain_6c = floor(fm_6c_size[1]*4/5)
ntest_6c = fm_6c_size[1]-ntrain_6c
set.seed(2)
ind_train_6c = sample(1:fm_6c_size[1],ntrain_6c)
ind_test_6c = c(1:fm_6c_size[1])[-ind_train_6c]


trainingFaces = vector()
testFaces = vector()


#row bind all training indicies 
for(i in 1:length(ind_train_6c)){
  trainingFaces = rbind(trainingFaces,face_matrix_6c[ind_train_6c[i],])
}

#row bind all test indicies
for(i in 1:length(ind_test_6c)){
  testFaces = rbind(testFaces,face_matrix_6c[ind_test_6c[i],])
}

#calculate the mean face
meanFace = apply(trainingFaces,2,mean)

#center the training data
for (i in 1:nrow(trainingFaces)){
  trainingFaces[i,] = trainingFaces[i,] - meanFace
}


#center the test data
for (i in 1:nrow(testFaces)){
  testFaces[i,] = testFaces[i,] - meanFace
}

#get PCs
pc = prcomp(trainingFaces)

#get scores USE SCORES FOR TRAINING X
testProj = testFaces%*%pc$rotation[,c(1:25)]
trainingProj = trainingFaces%*%pc$rotation[,c(1:25)]

cl = vector()
for (i in 1:length(ind_train_6c)){
  cl = c(cl,faceDF[ind_train_6c[i],1])
}


faceknn = knn(trainingProj,testProj,cl,k=1)
actualLabels = vector()

for (i in 1:length(ind_test_6c)){
  actualLabels = c(actualLabels,faceDF[ind_test_6c[i],1])
}

faceknnv = as.vector(faceknn,mode="integer")

mislabeled = 0 
for (i in 1:length(actualLabels)){
  if (actualLabels[i] != faceknnv[i]){
    mislabeled = mislabeled + 1
    frame()
    actual = matrix(face_matrix_6c[ind_test_6c[i],],nrow=192,ncol=168)
    predicted  = matrix(face_matrix_6c[faceknnv[i]*4,],nrow=192,ncol=168)
    faceplot = cbind(actual,predicted)
    faceplot = pixmapGrey(faceplot)
    plot(faceplot)
    title(main = "actual face and predicted face, actual face on left")
    
  }
}

print(mislabeled)






