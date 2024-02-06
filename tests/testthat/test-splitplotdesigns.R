library(skpr)
test_that("Split-plot design categorical factors are retained", {
  cand <- expand.grid(Zoom = c("None", "Optical", "Digital"),

                      TargetType = c("Man Sized", "NATO Standard"),

                      Orientation = c("Side", "Front/Back"),

                      Movement = c("Stationary", "Moving"),

                      Illum = c("Day", "Night", "Low Light"))

  htc <- gen_design(candidateset = cand,

                    model = ~ Zoom,

                    trials = 10,

                    repeats = 10)

  DOE <- gen_design(candidateset = cand,

                     model = ~(Illum +Zoom + TargetType + Orientation + Movement ),

                     trials = 60,

                     splitplotdesign = htc,

                     repeats = 1)
  for(col in seq_len(ncol(DOE))) {
    expect_equal(class(DOE[,col]), "factor")
  }
  DOE2 <- gen_design(candidateset = cand,

                    model = ~(Illum + Zoom),

                    trials = 60,

                    splitplotdesign = htc,

                    repeats = 20)
  for(col in seq_len(ncol(DOE2))) {
    expect_equal(class(DOE2[,col]), "factor")
  }

  htc2 <- gen_design(candidateset = cand,

                    model = ~ Illum + Zoom + Illum:Zoom,

                    trials = 10,

                    repeats = 10)
  DOE3 <- gen_design(candidateset = cand,

                     model = ~(Illum +Zoom + Illum:Zoom + TargetType + Orientation + Movement ),

                     trials = 60,

                     splitplotdesign = htc2,

                     repeats = 20)
  for(col in seq_len(ncol(DOE3))) {
    expect_equal(class(DOE3[,col]), "factor")
  }
  vhtc <- gen_design(candidateset = cand,

                     model = ~ Illum,

                     trials = 10,

                     repeats = 10)
  htc3 <- gen_design(candidateset = cand,
                    model = ~ Illum + Zoom + Illum:Zoom,
                    trials = 20,
                    splitplotdesign = vhtc,
                    repeats = 10)
  splitsplitdesign <- gen_design(candidateset = cand,
                     model = ~(Illum +Zoom + Illum:Zoom + TargetType + Orientation + Movement ),
                     trials = 60,
                     splitplotdesign = htc3,
                     repeats = 20)
  for(col in seq_len(ncol(splitsplitdesign))) {
    expect_equal(class(splitsplitdesign[,col]), "factor")
  }
})
