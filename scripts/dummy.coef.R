# START

# Title:  Some notes on using dummy.coef()
# Author: Sebastian Weinand
# Date:   2020-09-10

# Be careful when using dummy.coef() as it does not work in cases where we
# input a matrix from workspace plus variables from data into lm():

# example price data:
df <- data.frame("region" = rep(LETTERS[1:4], times = 3L),
                 "product" = as.factor(x = rep(1:3, each = 4L)),
                 "price" = c(5,5.05,4.95,5, 7,7.1,6.95,6.8, 9,8,11,10.2),
                 "rent_level" = rep(c(1,0.8,1.5,1.2), times = 3L),
                 stringsAsFactors = TRUE)

# ususal approach:
lm(log(price) ~ product + region - 1, data = df)

# create model.matrix:
XR <- model.matrix(~ region, data = df)

# identical estimates:
(my_reg <- lm(log(price) ~ product + XR[,-1] - 1, data = df))

# but dummy.coef() not able to extract coefficients properly:
dummy.coef(my_reg)

# new way for usage in cpd():

# set base region:
base <- "A" # NULL = "contr.sum", or some region if "contr.treatment"
if(base %in% levels(df$region) && !is.null(base)){
  df$region <- relevel(x = df$region, ref = base)
  }

# set contrasts:
if(is.null(base)){
  
  contrasts(x = df$region) <- contr.sum(levels(df$region))
  colnames(contrasts(x = df$region)) <- levels(df$region)[-nlevels(df$region)]
  
}else{
  
  contrasts(x = df$region) <- contr.treatment(levels(df$region))
  
}

# estimate CPD regression:
cpd_est_mod <- lm(formula = log(price) ~ product + region - 1,
                  data = df,
                  singular.ok = FALSE)

# extract and clean regional price level coefficients:
lnP <- coef(cpd_est_mod)
lnP <- lnP[names(lnP) %in% paste0("region", levels(df$region))]
names(lnP) <- gsub(pattern = "^region", 
                   replacement = "", 
                   x = names(lnP), 
                   ignore.case = FALSE)

# add price level of base region:
lnP <- c(ifelse(test = is.null(base), 
                yes = 0 - sum(lnP), 
                no = 0),
         lnP)
names(lnP)[1] <- setdiff(x = levels(df$region), y = names(lnP))

lnP <- lnP[match(x = levels(df$region), table = names(lnP))]
# -> use this also in cpd() [currently, it is only in cd(); see Github]

lnP

# END