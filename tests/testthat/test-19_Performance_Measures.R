test_that("Krippendorff's Alpha",{
  #Values taken from
  #Krippendorff, K. (2019). Content Analysis: An Introduction to
  #Its Methodology (4th Ed.). SAGE

  r_1<-factor(levels=c("Buch","Brief","Telefon","Computer","Ordner"),
              c("Buch","Brief","Telefon","Telefon","Brief","Buch","Computer","Buch","Brief",NA,NA,NA))
  r_2<-factor(levels=c("Buch","Brief","Telefon","Computer","Ordner"),c("Buch","Brief","Telefon","Telefon","Brief","Brief","Computer","Buch","Brief","Ordner",NA,NA))
  r_3<-factor(levels=c("Buch","Brief","Telefon","Computer","Ordner"),c(NA,"Telefon","Telefon","Telefon","Brief","Telefon","Computer","Brief","Brief","Ordner","Buch","Telefon"))
  r_4<-factor(levels=c("Buch","Brief","Telefon","Computer","Ordner"),c("Buch","Brief","Telefon","Telefon","Brief","Computer","Computer","Buch","Brief","Ordner","Buch",NA))

  results<-kripp_alpha(rater_one = r_1,rater_two = r_2,additional_raters=list(r_3,r_4))

  expect_equal(results$alpha_nominal,0.743,tolerance = 1e-3)
  expect_equal(results$alpha_ordinal,0.815,tolerance = 1e-3)

  results<-NULL
  results<-kripp_alpha(rater_one = r_1,rater_two = r_1,additional_raters=NULL)
  expect_equal(results$alpha_nominal,1,tolerance = 1e-3)
  expect_equal(results$alpha_ordinal,1,tolerance = 1e-3)
})

test_that("Cohens Kappa",{
  #Example from Cohen (1960)
  freq_table<-matrix(
    data = c(88,14,18,10,40,10,2,6,12),
    ncol = 3,
    nrow = 3,
    byrow = TRUE
  )
  r_1=vector(length = 200)
  r_2=vector(length = 200)
  index=1
  for(i in 1:3){
    for(j in 1:3){
      for(n_1 in 1:freq_table[i,j]){
      r_1[index]=i
      r_2[index]=j
      index=index+1
      }
    }
  }
  r_1<-factor(r_1,levels = c(1,2,3))
  r_2<-factor(r_2,levels = c(1,2,3))
  results<-cohens_kappa(rater_one = r_1,rater_two = r_2)
  expect_equal(results$kappa_unweighted,0.492,tolerance = 1e-3)

  results<-NULL
  results<-cohens_kappa(rater_one = r_1,rater_two = r_1)
  expect_equal(results$kappa_unweighted,1,tolerance = 1e-3)
  expect_equal(results$kappa_linear,1,tolerance = 1e-3)
  expect_equal(results$kappa_squared,1,tolerance = 1e-3)

  #Example from Cohen (1969)
  freq_table<-matrix(
    data = c(.44,.07,.09,.05,.20,.05,.01,.03,.06),
    ncol = 3,
    nrow = 3,
    byrow = TRUE
  )*200
  r_1=vector(length = sum(freq_table))
  r_2=vector(length = sum(freq_table))
  index=1
  for(i in 1:3){
    for(j in 1:3){
      for(n_1 in 1:freq_table[i,j]){
        r_1[index]=i
        r_2[index]=j
        index=index+1
      }
    }
  }
  r_1<-factor(r_1,levels = c(1,2,3))
  r_2<-factor(r_2,levels = c(1,2,3))
  results<-cohens_kappa(rater_one = r_1,rater_two = r_2)
  expect_equal(results$kappa_unweighted,0.492,tolerance = 1e-3)

  results<-NULL
  results<-cohens_kappa(rater_one = r_1,rater_two = r_1)
  expect_equal(results$kappa_unweighted,1,tolerance = 1e-3)
  expect_equal(results$kappa_linear,1,tolerance = 1e-3)
  expect_equal(results$kappa_squared,1,tolerance = 1e-3)

})

test_that("Fleiss Kappa",{
  #Example taken form Fleiss (1971)
  freq_table<-matrix(
    data = c(0,0,0,6,0,
             0,3,0,0,3,
             0,1,4,0,1,
             0,0,0,0,6,
             0,3,0,3,0,
             2,0,4,0,0,
             0,0,4,0,2,
             2,0,3,1,0,
             2,0,0,4,0,
             0,0,0,0,6,
             1,0,0,5,0,
             1,1,0,4,0,
             0,3,3,0,0,
             1,0,0,5,0,
             0,2,0,3,1,
             0,0,5,0,1,
             3,0,0,1,2,
             5,1,0,0,0,
             0,2,0,4,0,
             1,0,2,0,3,
             0,0,0,0,6,
             0,1,0,5,0,
             0,2,0,1,3,
             2,0,0,4,0,
             1,0,0,4,1,
             0,5,0,1,0,
             4,0,0,0,2,
             0,2,0,4,0,
             1,0,5,0,0,
             0,0,0,0,6),
    ncol = 5,
    nrow = 30,
    byrow = TRUE
  )
raters=matrix(data = 0,nrow = 30,ncol = 6)

for(i in 1:nrow(freq_table)){
  index=0
  for (k in 1:5) {
    tmp_value=freq_table[i,k]
    if(tmp_value>0){
     for (j in (1+index):(tmp_value+index)) {
       raters[i,j]=k
     }
      index=index+tmp_value
    }
  }
}

  r_1<-factor(raters[,1],levels = c(1,2,3,4,5))
  r_2<-factor(raters[,2],levels = c(1,2,3,4,5))
  r_3<-factor(raters[,3],levels = c(1,2,3,4,5))
  r_4<-factor(raters[,4],levels = c(1,2,3,4,5))
  r_5<-factor(raters[,5],levels = c(1,2,3,4,5))
  r_6<-factor(raters[,6],levels = c(1,2,3,4,5))
  results<-fleiss_kappa(rater_one = r_1,
                        rater_two = r_2,
                        additional_raters=list(r_3,r_4,r_5,r_6))
  expect_equal(results,0.430,tolerance = 1e-3)

  results<-NULL
  results<-fleiss_kappa(rater_one = r_1,rater_two = r_1)
  expect_equal(results,1,tolerance = 1e-3)
})

test_that("Kendall's w",{
  #Example taken form Clark-Carter (2018) Quantitative Psychological Research
  freq_table<-matrix(
    data = c(1,2,3,4,5,
             2,1,4,3,5,
             3,2,1,5,4,
             1,3,2,4,5),
    ncol = 5,
    nrow = 4,
    byrow = TRUE
  )
  raters=matrix(data = 0,nrow = 4,ncol = 5)

  for(i in 1:nrow(freq_table)){
    index=0
    for (k in 1:5) {
      tmp_value=freq_table[i,k]
      if(tmp_value>0){
        for (j in (1+index):(tmp_value+index)) {
          raters[i,j]=k
        }
        index=index+tmp_value
      }
    }
  }

  r_1<-factor(raters[,1],levels = c(1,2,3,4,5))
  r_2<-factor(raters[,2],levels = c(1,2,3,4,5))
  r_3<-factor(raters[,3],levels = c(1,2,3,4,5))
  r_4<-factor(raters[,4],levels = c(1,2,3,4,5))
  r_5<-factor(raters[,5],levels = c(1,2,3,4,5))
  results<-kendalls_w(rater_one = r_1,
                        rater_two = r_2,
                        additional_raters=list(r_3,r_4,r_5))
  expect_equal(results$kendall_w,0.6875,tolerance = 1e-3)
  expect_equal(results$kendall_w_corrected,0.6794,tolerance = 1e-3)

  results<-NULL
  results<-kendalls_w(rater_one = r_1,rater_two = r_1)
  expect_equal(results$kendall_w,1,tolerance = 1e-3)
  expect_equal(results$kendall_w_corrected,1,tolerance = 1e-3)
})

