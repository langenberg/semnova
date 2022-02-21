
data_syntax <- "
.eta1 =~
    c(.lambda_y__1_, .lambda_y__1_, .lambda_y__1_, .lambda_y__1_)*.Y1_W1_1_W2_1 + c(.lambda_y__2_, .lambda_y__2_, .lambda_y__2_, .lambda_y__2_)*.Y2_W1_1_W2_1 + c(.lambda_y__3_, .lambda_y__3_, .lambda_y__3_, .lambda_y__3_)*.Y3_W1_1_W2_1
.eta2 =~
    c(.lambda_y__1_, .lambda_y__1_, .lambda_y__1_, .lambda_y__1_)*.Y1_W1_2_W2_1 + c(.lambda_y__2_, .lambda_y__2_, .lambda_y__2_, .lambda_y__2_)*.Y2_W1_2_W2_1 + c(.lambda_y__3_, .lambda_y__3_, .lambda_y__3_, .lambda_y__3_)*.Y3_W1_2_W2_1
.eta3 =~
    c(.lambda_y__1_, .lambda_y__1_, .lambda_y__1_, .lambda_y__1_)*.Y1_W1_1_W2_2 + c(.lambda_y__2_, .lambda_y__2_, .lambda_y__2_, .lambda_y__2_)*.Y2_W1_1_W2_2 + c(.lambda_y__3_, .lambda_y__3_, .lambda_y__3_, .lambda_y__3_)*.Y3_W1_1_W2_2
.eta4 =~
    c(.lambda_y__1_, .lambda_y__1_, .lambda_y__1_, .lambda_y__1_)*.Y1_W1_2_W2_2 + c(.lambda_y__2_, .lambda_y__2_, .lambda_y__2_, .lambda_y__2_)*.Y2_W1_2_W2_2 + c(.lambda_y__3_, .lambda_y__3_, .lambda_y__3_, .lambda_y__3_)*.Y3_W1_2_W2_2
.eta5 =~
    c(.lambda_y__1_, .lambda_y__1_, .lambda_y__1_, .lambda_y__1_)*.Y1_W1_1_W2_3 + c(.lambda_y__2_, .lambda_y__2_, .lambda_y__2_, .lambda_y__2_)*.Y2_W1_1_W2_3 + c(.lambda_y__3_, .lambda_y__3_, .lambda_y__3_, .lambda_y__3_)*.Y3_W1_1_W2_3
.eta6 =~
    c(.lambda_y__1_, .lambda_y__1_, .lambda_y__1_, .lambda_y__1_)*.Y1_W1_2_W2_3 + c(.lambda_y__2_, .lambda_y__2_, .lambda_y__2_, .lambda_y__2_)*.Y2_W1_2_W2_3 + c(.lambda_y__3_, .lambda_y__3_, .lambda_y__3_, .lambda_y__3_)*.Y3_W1_2_W2_3
.pi1 =~
    c(0.408248290463863, 0.408248290463863, 0.408248290463863, 0.408248290463863)*.eta1 + c(0.408248290463863, 0.408248290463863, 0.408248290463863, 0.408248290463863)*.eta2 + c(0.408248290463863, 0.408248290463863, 0.408248290463863, 0.408248290463863)*.eta3 + c(0.408248290463863, 0.408248290463863, 0.408248290463863, 0.408248290463863)*.eta4 + c(0.408248290463863, 0.408248290463863, 0.408248290463863, 0.408248290463863)*.eta5 + c(0.408248290463863, 0.408248290463863, 0.408248290463863, 0.408248290463863)*.eta6
.pi2 =~
    c(-0.408248290463863, -0.408248290463863, -0.408248290463863, -0.408248290463863)*.eta1 + c(0.408248290463863, 0.408248290463863, 0.408248290463863, 0.408248290463863)*.eta2 + c(-0.408248290463863, -0.408248290463863, -0.408248290463863, -0.408248290463863)*.eta3 + c(0.408248290463863, 0.408248290463863, 0.408248290463863, 0.408248290463863)*.eta4 + c(-0.408248290463863, -0.408248290463863, -0.408248290463863, -0.408248290463863)*.eta5 + c(0.408248290463863, 0.408248290463863, 0.408248290463863, 0.408248290463863)*.eta6
.pi3 =~
    c(-0.5, -0.5, -0.5, -0.5)*.eta1 + c(-0.5, -0.5, -0.5, -0.5)*.eta2 + c(4.80740671595891e-17, 4.80740671595891e-17, 4.80740671595891e-17, 4.80740671595891e-17)*.eta3 + c(-1.44222201478767e-16, -1.44222201478767e-16, -1.44222201478767e-16, -1.44222201478767e-16)*.eta4 + c(0.5, 0.5, 0.5, 0.5)*.eta5 + c(0.5, 0.5, 0.5, 0.5)*.eta6
.pi4 =~
    c(0.288675134594813, 0.288675134594813, 0.288675134594813, 0.288675134594813)*.eta1 + c(0.288675134594813, 0.288675134594813, 0.288675134594813, 0.288675134594813)*.eta2 + c(-0.577350269189626, -0.577350269189626, -0.577350269189626, -0.577350269189626)*.eta3 + c(-0.577350269189626, -0.577350269189626, -0.577350269189626, -0.577350269189626)*.eta4 + c(0.288675134594813, 0.288675134594813, 0.288675134594813, 0.288675134594813)*.eta5 + c(0.288675134594813, 0.288675134594813, 0.288675134594813, 0.288675134594813)*.eta6
.pi5 =~
    c(0.5, 0.5, 0.5, 0.5)*.eta1 + c(-0.5, -0.5, -0.5, -0.5)*.eta2 + c(-1.44222201478767e-16, -1.44222201478767e-16, -1.44222201478767e-16, -1.44222201478767e-16)*.eta3 + c(-4.80740671595891e-17, -4.80740671595891e-17, -4.80740671595891e-17, -4.80740671595891e-17)*.eta4 + c(-0.5, -0.5, -0.5, -0.5)*.eta5 + c(0.5, 0.5, 0.5, 0.5)*.eta6
.pi6 =~
    c(-0.288675134594813, -0.288675134594813, -0.288675134594813, -0.288675134594813)*.eta1 + c(0.288675134594813, 0.288675134594813, 0.288675134594813, 0.288675134594813)*.eta2 + c(0.577350269189626, 0.577350269189626, 0.577350269189626, 0.577350269189626)*.eta3 + c(-0.577350269189626, -0.577350269189626, -0.577350269189626, -0.577350269189626)*.eta4 + c(-0.288675134594813, -0.288675134594813, -0.288675134594813, -0.288675134594813)*.eta5 + c(0.288675134594813, 0.288675134594813, 0.288675134594813, 0.288675134594813)*.eta6
covariate2 =~
    c(.lambda_cov_2_1_, .lambda_cov_2_1_, .lambda_cov_2_1_, .lambda_cov_2_1_)*covariate2_1 + c(.lambda_cov_2_2_, .lambda_cov_2_2_, .lambda_cov_2_2_, .lambda_cov_2_2_)*covariate2_2
.pi1 ~
    c(.beta_1_1_1, .beta_1_1_2, .beta_1_1_3, .beta_1_1_4)*covariate1 + c(.beta_1_2_1, .beta_1_2_2, .beta_1_2_3, .beta_1_2_4)*covariate2
.pi2 ~
    c(.beta_2_1_1, .beta_2_1_2, .beta_2_1_3, .beta_2_1_4)*covariate1 + c(.beta_2_2_1, .beta_2_2_2, .beta_2_2_3, .beta_2_2_4)*covariate2
.pi3 ~
    c(.beta_3_1_1, .beta_3_1_2, .beta_3_1_3, .beta_3_1_4)*covariate1 + c(.beta_3_2_1, .beta_3_2_2, .beta_3_2_3, .beta_3_2_4)*covariate2
.pi4 ~
    c(.beta_4_1_1, .beta_4_1_2, .beta_4_1_3, .beta_4_1_4)*covariate1 + c(.beta_4_2_1, .beta_4_2_2, .beta_4_2_3, .beta_4_2_4)*covariate2
.pi5 ~
    c(.beta_5_1_1, .beta_5_1_2, .beta_5_1_3, .beta_5_1_4)*covariate1 + c(.beta_5_2_1, .beta_5_2_2, .beta_5_2_3, .beta_5_2_4)*covariate2
.pi6 ~
    c(.beta_6_1_1, .beta_6_1_2, .beta_6_1_3, .beta_6_1_4)*covariate1 + c(.beta_6_2_1, .beta_6_2_2, .beta_6_2_3, .beta_6_2_4)*covariate2
.eta1 ~~
    c(0, 0, 0, 0)*.eta1 + c(0, 0, 0, 0)*.eta2 + c(0, 0, 0, 0)*.eta3 + c(0, 0, 0, 0)*.eta4 + c(0, 0, 0, 0)*.eta5 + c(0, 0, 0, 0)*.eta6
.eta2 ~~
    c(0, 0, 0, 0)*.eta2 + c(0, 0, 0, 0)*.eta3 + c(0, 0, 0, 0)*.eta4 + c(0, 0, 0, 0)*.eta5 + c(0, 0, 0, 0)*.eta6
.eta3 ~~
    c(0, 0, 0, 0)*.eta3 + c(0, 0, 0, 0)*.eta4 + c(0, 0, 0, 0)*.eta5 + c(0, 0, 0, 0)*.eta6
.eta4 ~~
    c(0, 0, 0, 0)*.eta4 + c(0, 0, 0, 0)*.eta5 + c(0, 0, 0, 0)*.eta6
.eta5 ~~
    c(0, 0, 0, 0)*.eta5 + c(0, 0, 0, 0)*.eta6
.eta6 ~~
    c(0, 0, 0, 0)*.eta6
.pi1 ~~
    c(.sigma_pi_1_1_1, .sigma_pi_1_1_2, .sigma_pi_1_1_3, .sigma_pi_1_1_4)*.pi1 + c(.sigma_pi_1_2_1, .sigma_pi_1_2_2, .sigma_pi_1_2_3, .sigma_pi_1_2_4)*.pi2 + c(.sigma_pi_1_3_1, .sigma_pi_1_3_2, .sigma_pi_1_3_3, .sigma_pi_1_3_4)*.pi3 + c(.sigma_pi_1_4_1, .sigma_pi_1_4_2, .sigma_pi_1_4_3, .sigma_pi_1_4_4)*.pi4 + c(.sigma_pi_1_5_1, .sigma_pi_1_5_2, .sigma_pi_1_5_3, .sigma_pi_1_5_4)*.pi5 + c(.sigma_pi_1_6_1, .sigma_pi_1_6_2, .sigma_pi_1_6_3, .sigma_pi_1_6_4)*.pi6
.pi2 ~~
    c(.sigma_pi_2_2_1, .sigma_pi_2_2_2, .sigma_pi_2_2_3, .sigma_pi_2_2_4)*.pi2 + c(.sigma_pi_2_3_1, .sigma_pi_2_3_2, .sigma_pi_2_3_3, .sigma_pi_2_3_4)*.pi3 + c(.sigma_pi_2_4_1, .sigma_pi_2_4_2, .sigma_pi_2_4_3, .sigma_pi_2_4_4)*.pi4 + c(.sigma_pi_2_5_1, .sigma_pi_2_5_2, .sigma_pi_2_5_3, .sigma_pi_2_5_4)*.pi5 + c(.sigma_pi_2_6_1, .sigma_pi_2_6_2, .sigma_pi_2_6_3, .sigma_pi_2_6_4)*.pi6
.pi3 ~~
    c(.sigma_pi_1__1, .sigma_pi_1__2, .sigma_pi_1__3, .sigma_pi_1__4)*.pi3 + c(0, 0, 0, 0)*.pi4 + c(.sigma_pi_3_5_1, .sigma_pi_3_5_2, .sigma_pi_3_5_3, .sigma_pi_3_5_4)*.pi5 + c(.sigma_pi_3_6_1, .sigma_pi_3_6_2, .sigma_pi_3_6_3, .sigma_pi_3_6_4)*.pi6
.pi4 ~~
    c(.sigma_pi_1__1, .sigma_pi_1__2, .sigma_pi_1__3, .sigma_pi_1__4)*.pi4 + c(.sigma_pi_4_5_1, .sigma_pi_4_5_2, .sigma_pi_4_5_3, .sigma_pi_4_5_4)*.pi5 + c(.sigma_pi_4_6_1, .sigma_pi_4_6_2, .sigma_pi_4_6_3, .sigma_pi_4_6_4)*.pi6
.pi5 ~~
    c(.sigma_pi_5_5_1, .sigma_pi_5_5_2, .sigma_pi_5_5_3, .sigma_pi_5_5_4)*.pi5 + c(.sigma_pi_5_6_1, .sigma_pi_5_6_2, .sigma_pi_5_6_3, .sigma_pi_5_6_4)*.pi6
.pi6 ~~
    c(.sigma_pi_6_6_1, .sigma_pi_6_6_2, .sigma_pi_6_6_3, .sigma_pi_6_6_4)*.pi6
.Y1_W1_1_W2_1 ~~
    c(.epsilon_y_1_1_1, .epsilon_y_1_1_2, .epsilon_y_1_1_3, .epsilon_y_1_1_4)*.Y1_W1_1_W2_1
.Y1_W1_1_W2_2 ~~
    c(.epsilon_y_3_1_1, .epsilon_y_3_1_2, .epsilon_y_3_1_3, .epsilon_y_3_1_4)*.Y1_W1_1_W2_2
.Y1_W1_1_W2_3 ~~
    c(.epsilon_y_5_1_1, .epsilon_y_5_1_2, .epsilon_y_5_1_3, .epsilon_y_5_1_4)*.Y1_W1_1_W2_3
.Y1_W1_2_W2_1 ~~
    c(.epsilon_y_2_1_1, .epsilon_y_2_1_2, .epsilon_y_2_1_3, .epsilon_y_2_1_4)*.Y1_W1_2_W2_1
.Y1_W1_2_W2_2 ~~
    c(.epsilon_y_4_1_1, .epsilon_y_4_1_2, .epsilon_y_4_1_3, .epsilon_y_4_1_4)*.Y1_W1_2_W2_2
.Y1_W1_2_W2_3 ~~
    c(.epsilon_y_6_1_1, .epsilon_y_6_1_2, .epsilon_y_6_1_3, .epsilon_y_6_1_4)*.Y1_W1_2_W2_3
.Y2_W1_1_W2_1 ~~
    c(.epsilon_y_1_2_1, .epsilon_y_1_2_2, .epsilon_y_1_2_3, .epsilon_y_1_2_4)*.Y2_W1_1_W2_1
.Y2_W1_1_W2_2 ~~
    c(.epsilon_y_3_2_1, .epsilon_y_3_2_2, .epsilon_y_3_2_3, .epsilon_y_3_2_4)*.Y2_W1_1_W2_2
.Y2_W1_1_W2_3 ~~
    c(.epsilon_y_5_2_1, .epsilon_y_5_2_2, .epsilon_y_5_2_3, .epsilon_y_5_2_4)*.Y2_W1_1_W2_3
.Y2_W1_2_W2_1 ~~
    c(.epsilon_y_2_2_1, .epsilon_y_2_2_2, .epsilon_y_2_2_3, .epsilon_y_2_2_4)*.Y2_W1_2_W2_1
.Y2_W1_2_W2_2 ~~
    c(.epsilon_y_4_2_1, .epsilon_y_4_2_2, .epsilon_y_4_2_3, .epsilon_y_4_2_4)*.Y2_W1_2_W2_2
.Y2_W1_2_W2_3 ~~
    c(.epsilon_y_6_2_1, .epsilon_y_6_2_2, .epsilon_y_6_2_3, .epsilon_y_6_2_4)*.Y2_W1_2_W2_3
.Y3_W1_1_W2_1 ~~
    c(.epsilon_y_1_3_1, .epsilon_y_1_3_2, .epsilon_y_1_3_3, .epsilon_y_1_3_4)*.Y3_W1_1_W2_1
.Y3_W1_1_W2_2 ~~
    c(.epsilon_y_3_3_1, .epsilon_y_3_3_2, .epsilon_y_3_3_3, .epsilon_y_3_3_4)*.Y3_W1_1_W2_2
.Y3_W1_1_W2_3 ~~
    c(.epsilon_y_5_3_1, .epsilon_y_5_3_2, .epsilon_y_5_3_3, .epsilon_y_5_3_4)*.Y3_W1_1_W2_3
.Y3_W1_2_W2_1 ~~
    c(.epsilon_y_2_3_1, .epsilon_y_2_3_2, .epsilon_y_2_3_3, .epsilon_y_2_3_4)*.Y3_W1_2_W2_1
.Y3_W1_2_W2_2 ~~
    c(.epsilon_y_4_3_1, .epsilon_y_4_3_2, .epsilon_y_4_3_3, .epsilon_y_4_3_4)*.Y3_W1_2_W2_2
.Y3_W1_2_W2_3 ~~
    c(.epsilon_y_6_3_1, .epsilon_y_6_3_2, .epsilon_y_6_3_3, .epsilon_y_6_3_4)*.Y3_W1_2_W2_3
covariate1 ~~
    c(.epsilon_cov_1__1, .epsilon_cov_1__2, .epsilon_cov_1__3, .epsilon_cov_1__4)*covariate1
covariate2 ~~
    c(.sigma_cov_2__1, .sigma_cov_2__2, .sigma_cov_2__3, .sigma_cov_2__4)*covariate2
covariate2_1 ~~
    c(.epsilon_cov_2_1_1, .epsilon_cov_2_1_2, .epsilon_cov_2_1_3, .epsilon_cov_2_1_4)*covariate2_1
covariate2_2 ~~
    c(.epsilon_cov_2_2_1, .epsilon_cov_2_2_2, .epsilon_cov_2_2_3, .epsilon_cov_2_2_4)*covariate2_2
.eta1 ~
    c(0, 0, 0, 0)*1
.eta2 ~
    c(0, 0, 0, 0)*1
.eta3 ~
    c(0, 0, 0, 0)*1
.eta4 ~
    c(0, 0, 0, 0)*1
.eta5 ~
    c(0, 0, 0, 0)*1
.eta6 ~
    c(0, 0, 0, 0)*1
.pi1 ~
    c(.alpha_pi_1_1, .alpha_pi_1_2, .alpha_pi_1_3, .alpha_pi_1_4)*1
.pi2 ~
    c(.alpha_pi_2_1, .alpha_pi_2_2, .alpha_pi_2_3, .alpha_pi_2_4)*1
.pi3 ~
    c(.alpha_pi_3_1, .alpha_pi_3_2, .alpha_pi_3_3, .alpha_pi_3_4)*1
.pi4 ~
    c(.alpha_pi_4_1, .alpha_pi_4_2, .alpha_pi_4_3, .alpha_pi_4_4)*1
.pi5 ~
    c(.alpha_pi_5_1, .alpha_pi_5_2, .alpha_pi_5_3, .alpha_pi_5_4)*1
.pi6 ~
    c(.alpha_pi_6_1, .alpha_pi_6_2, .alpha_pi_6_3, .alpha_pi_6_4)*1
.Y1_W1_1_W2_1 ~
    c(.nu_y__1_, .nu_y__1_, .nu_y__1_, .nu_y__1_)*1
.Y1_W1_1_W2_2 ~
    c(.nu_y__1_, .nu_y__1_, .nu_y__1_, .nu_y__1_)*1
.Y1_W1_1_W2_3 ~
    c(.nu_y__1_, .nu_y__1_, .nu_y__1_, .nu_y__1_)*1
.Y1_W1_2_W2_1 ~
    c(.nu_y__1_, .nu_y__1_, .nu_y__1_, .nu_y__1_)*1
.Y1_W1_2_W2_2 ~
    c(.nu_y__1_, .nu_y__1_, .nu_y__1_, .nu_y__1_)*1
.Y1_W1_2_W2_3 ~
    c(.nu_y__1_, .nu_y__1_, .nu_y__1_, .nu_y__1_)*1
.Y2_W1_1_W2_1 ~
    c(.nu_y__2_, .nu_y__2_, .nu_y__2_, .nu_y__2_)*1
.Y2_W1_1_W2_2 ~
    c(.nu_y__2_, .nu_y__2_, .nu_y__2_, .nu_y__2_)*1
.Y2_W1_1_W2_3 ~
    c(.nu_y__2_, .nu_y__2_, .nu_y__2_, .nu_y__2_)*1
.Y2_W1_2_W2_1 ~
    c(.nu_y__2_, .nu_y__2_, .nu_y__2_, .nu_y__2_)*1
.Y2_W1_2_W2_2 ~
    c(.nu_y__2_, .nu_y__2_, .nu_y__2_, .nu_y__2_)*1
.Y2_W1_2_W2_3 ~
    c(.nu_y__2_, .nu_y__2_, .nu_y__2_, .nu_y__2_)*1
.Y3_W1_1_W2_1 ~
    c(.nu_y__3_, .nu_y__3_, .nu_y__3_, .nu_y__3_)*1
.Y3_W1_1_W2_2 ~
    c(.nu_y__3_, .nu_y__3_, .nu_y__3_, .nu_y__3_)*1
.Y3_W1_1_W2_3 ~
    c(.nu_y__3_, .nu_y__3_, .nu_y__3_, .nu_y__3_)*1
.Y3_W1_2_W2_1 ~
    c(.nu_y__3_, .nu_y__3_, .nu_y__3_, .nu_y__3_)*1
.Y3_W1_2_W2_2 ~
    c(.nu_y__3_, .nu_y__3_, .nu_y__3_, .nu_y__3_)*1
.Y3_W1_2_W2_3 ~
    c(.nu_y__3_, .nu_y__3_, .nu_y__3_, .nu_y__3_)*1
covariate1 ~
    c(.nu_cov_1__1, .nu_cov_1__2, .nu_cov_1__3, .nu_cov_1__4)*1
covariate2 ~
    c(.alpha_cov_2__1, .alpha_cov_2__2, .alpha_cov_2__3, .alpha_cov_2__4)*1
covariate2_1 ~
    c(.nu_cov_2_1_, .nu_cov_2_1_, .nu_cov_2_1_, .nu_cov_2_1_)*1
covariate2_2 ~
    c(.nu_cov_2_2_, .nu_cov_2_2_, .nu_cov_2_2_, .nu_cov_2_2_)*1
"

power <- power_analysis_semnova(
  n_within = c(2, 3),
  n_between = c(2, 2),
  n_indicators = 3,
  n_manifest_covariates = 1,
  n_latent_covariates = 2,
  sphericity = list(2),
  normalize_contrasts = TRUE,
  data_syntax = data_syntax,
  sample_size = c(50, 50, 50, 50),
  replications = 500
)

# hyopthesis tests
summary(power)

# parameters
print(power)

# path diagram
plot(power)
