################################################################################
# Block 1 Testing - Grids
################################################################################

test_that("Grid analysis of NRC, CD, AS, AS.rel match proofs", {

  data("grid.patchy.associated")
  data("param.grid")
  temp<-ComSpat(data=grid.patchy.associated, params = param.grid[1:10,],
                dim_max = 64, type = "Grid")

  # NRC TEST
  expect_type(temp,"list")
  x<-apply(round(temp[["NRC"]],3), 2, function(x)max(x, na.rm = TRUE))
  y<-c(10,23,39,47,50,59,59,54,49,47)
  names(y)<-c("Step_1","Step_2","Step_3","Step_4","Step_5", "Step_6", "Step_7",
              "Step_8", "Step_9", "Step_10")
  expect_equal(x,y, tolerance = 0)

  # CD TEST
  x<-apply(round(temp[["CD"]],3), 2, function(x)max(x, na.rm = TRUE))
  y<-c(1.842,3.210,3.813,4.006,4.037,4.046,4.082,4.135,4.156,4.135)
  names(y)<-c("Step_1","Step_2","Step_3","Step_4","Step_5", "Step_6", "Step_7",
              "Step_8", "Step_9", "Step_10")
  expect_equal(x,y, tolerance = 0.001)

  # AS TEST
  x<-apply(round(temp[["AS"]],3), 2, function(x)max(x, na.rm = TRUE))
  y<-c(0.072,0.324,0.754,1.024,1.201,1.275,1.248,1.149,1.030,0.909)
  names(y)<-c("Step_1","Step_2","Step_3","Step_4","Step_5", "Step_6", "Step_7",
              "Step_8", "Step_9", "Step_10")
  expect_equal(x,y, tolerance = 0.001)

  # AS.rel TEST
  x<-apply(round(temp[["AS.rel"]],3), 2, function(x)max(x, na.rm = TRUE))
  y<-c(0.039,0.101,0.198,.256,0.297,0.315,0.306,0.278,0.248,0.220)
  names(y)<-c("Step_1","Step_2","Step_3","Step_4","Step_5", "Step_6", "Step_7",
              "Step_8", "Step_9", "Step_10")
  expect_equal(x,y, tolerance = 0.001)

})

################################################################################
# Block 2 Testing - Transect
################################################################################

test_that("Transect analyses of NRC, CD, AS, AS.rel match proofs", {

  data("tran.grass.t")
  data("param.tran")
  temp<-ComSpat(data=tran.grass.t, params = param.tran[1:10,], dim_max = 500,
                type="Transect")

  # NRC TEST
  expect_type(temp,"list")
  x<-apply(round(temp[["NRC"]],3), 2, function(x)max(x, na.rm = TRUE))
  y<-c(29,35,37,37,33,30,26,27,26,24)
  names(y)<-c("Step_1","Step_2","Step_3","Step_4","Step_5", "Step_6", "Step_7",
              "Step_8", "Step_9", "Step_10")
  expect_equal(x,y, tolerance = 0.001)

  # CD TEST
  x<-apply(round(temp[["CD"]],3), 2, function(x)max(x, na.rm = TRUE))
  y<-c(3.444, 3.916, 4.125, 4.249, 4.289, 4.327, 4.306, 4.282, 4.125, 3.690)
  names(y)<-c("Step_1","Step_2","Step_3","Step_4","Step_5", "Step_6", "Step_7",
              "Step_8", "Step_9", "Step_10")
  expect_equal(x,y, tolerance = 0.001)

  # AS TEST
  x<-apply(round(temp[["AS"]],3), 2, function(x)max(x, na.rm = TRUE))
  y<-c(0.072, 0.077, 0.096, 0.122, 0.131, 0.136, 0.146, 0.151, 0.178, 0.177)
  names(y)<-c("Step_1","Step_2","Step_3","Step_4","Step_5", "Step_6", "Step_7",
              "Step_8", "Step_9", "Step_10")
  expect_equal(x,y, tolerance = 0.001)

  # AS.rel TEST
  x<-apply(round(temp[["AS.rel"]],3), 2, function(x)max(x, na.rm = TRUE))
  y<-c(0.021, 0.020, 0.023, 0.029, 0.030, 0.031, 0.034, 0.035, 0.043, 0.048)
  names(y)<-c("Step_1","Step_2","Step_3","Step_4","Step_5", "Step_6", "Step_7",
              "Step_8", "Step_9", "Step_10")
  expect_equal(x,y, tolerance = 0.001)



})
