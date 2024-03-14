# 🟥Prior Setting ##############################################################
# rm(list=ls())
path_save = "/Users/Ido/Library/CloudStorage/Dropbox/@DataAnalysis/DataAnalysis___Moral Injury Among Diagnostic Radiologists"


## 🟨Loading packages ==========================================================
install_packages = function(packages, load=TRUE) {
  # load : load the packages after installation?
  for(pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg)
    }
    
    if(load){
      library(pkg, character.only = TRUE)
    }
  }
}

visual = c("ggpubr", "ggplot2", "ggstatsplot", "ggsignif")
stat = c("fda")
data_handling = c("tidyverse", "dplyr", "clipr", "tidyr")
qmd = c("janitor", "knitr")
texts = c("stringr")

packages_to_install_and_load = c(visual, stat, data_handling, qmd, texts)
install_packages(packages_to_install_and_load)


install_github("regbook/regbook")
library(regbook)


## 🟨Loading my functions ======================================================
# Check my OS
os <- Sys.info()["sysname"]
if(os ==  "Darwin"){
  
  path_OS = "/Users/Ido/" # mac
  
}else if(os ==  "Window"){
  
  path_OS = "C:/Users/lleii/"  
  
}
path_Dropbox = paste0(path_OS, "Dropbox")
path_GitHub = list.files(path_Dropbox, pattern = "GitHub", full.names = T)
path_GitHub_Code = paste0(path_GitHub, "/GitHub___Code")
Rpkgs = c("ADNIprep", "StatsR", "refineR", "dimR")
Load = sapply(Rpkgs, function(y){
  list.files(path = path_GitHub_Code, pattern = y, full.names = T) %>%
    paste0(., "/", y,"/R") %>%
    list.files(., full.names = T) %>%
    purrr::walk(source)
})



# 🟥Data Preprocessing #######################################################
## 🟧Data Load ============================================================
path_data = "/Users/Ido/Library/CloudStorage/Dropbox/GitHub/PaperData/PaperData___Moral Injury Among Diagnostic Radiologists/Assessing moral injury amongst diagnostic radiologists (Responses) - Form Responses 1.csv"
data = read.csv(path_data)


## 🟧Check Variables & Modify elements =====================================================
### 🟨Var Names ------------------------------------------------------------
names(data)


### 🟨Remove "Timestamp" --------------------------------------------------
data$Timestamp = NULL

names(data) %>% length
names(data) %>% head


### 🟨Categorical (1) -------------------------------------------------------
#### 🤍Q1_Categorical_Concepts : With.which.of.the.following.concepts.are.you.familiar.-------------------------------
data$With.which.of.the.following.concepts.are.you.familiar. %>% table

#### 🤍Q2_Categorical_Survey : How.did.you.hear.about.this.survey. ---------------------
data$How.did.you.hear.about.this.survey. %>% table
data <- data %>%
  mutate(How.did.you.hear.about.this.survey. = case_when(
    grepl("Professional society", How.did.you.hear.about.this.survey.) ~ "Professional society-based communication",
    How.did.you.hear.about.this.survey. == "LinkedIn" ~ "LinkedIn",
    How.did.you.hear.about.this.survey. == "Twitter" ~ "Twitter",
    How.did.you.hear.about.this.survey. == "Facebook" ~ "Facebook",
    How.did.you.hear.about.this.survey. == "Instagram" ~ "Instagram",
    How.did.you.hear.about.this.survey. == "Email" ~ "Email",
    TRUE ~ "etc" # 이외의 모든 경우
  ))


#### 🤍Q3_Categorical_Position : I.am ----------------------------------------
data$I.am %>% table

#### 🤍Q4_Categorical_Years : How.many.years.have.you.been.in.practice.------------------
data$How.many.years.have.you.been.in.practice. %>% table

#### 🤍Q5_Written_Country : Which.country.do.you.practice.in.--------------------
data$Which.country.do.you.practice.in. %>% table
data <- data %>%
  mutate(Which.country.do.you.practice.in. = case_when(
    grepl("USA|US|Usa|us", Which.country.do.you.practice.in., ignore.case = TRUE) ~ "USA",
    grepl("United States", Which.country.do.you.practice.in.) ~ "USA",
    grepl("Uk|United Kingdom", Which.country.do.you.practice.in., ignore.case = TRUE) ~ "United Kingdom",
    grepl("Canada", Which.country.do.you.practice.in., ignore.case = TRUE) ~ "Canada",
    # 추가적인 국가 이름 변환 규칙을 여기에 추가할 수 있습니다.
    TRUE ~ as.character(Which.country.do.you.practice.in.) # 기타
  ))

#### 🤍Q6_Categorical_Sex : What.is.your.gender. ----------------------------------------
data$What.is.your.gender. %>% table

#### 🤍Q7_Categorical_Ethnicity : What.is.your.race.ethnicity.--------------------------------
data$What.is.your.race.ethnicity. %>% table

#### 🤍Q8_Categorical_Marriage : What.is.your.marital.status.-------------------------------
data$What.is.your.marital.status. %>% table

#### 🤍Q9_Categorical_Practice : What.best.describes.your.practice---------------------------
data$What.best.describes.your.practice. %>% table

#### 🤍Q10_Written_Ownership : What.best.describes.the.ownership.of.your.practice.------------------------
data$What.best.describes.the.ownership.of.your.practice. %>% table

#### 🤍Q11_Categorical_HomeWork : What.percentage.of.your.work.is.completed.at.home.------------------------------
data$What.percentage.of.your.work.is.completed.at.home. %>% table
variable_name <- "What.percentage.of.your.work.is.completed.at.home."
data[[variable_name]] <- ifelse(data[[variable_name]] == "", NA, data[[variable_name]])



#### 🤍Q12_Categorical_PracticeHospital : Do.you.practice.at.more.than.one.hospital.--------------------------
data$Do.you.practice.at.more.than.one.hospital. %>% table

#### 🤍Q13_Categorical_Leadership : Do.you.have.an.institutional.leadership.position...e.g..program.director..section.chief..department.chair.-------------------
data$Do.you.have.an.institutional.leadership.position...e.g..program.director..section.chief..department.chair. %>% table
variable_name <- "Do.you.have.an.institutional.leadership.position...e.g..program.director..section.chief..department.chair."
data[[variable_name]] <- ifelse(data[[variable_name]] == "", NA, data[[variable_name]])


#### 🤍Q14_Categorical_Practicing_Procedure ------------------------------------------------
data$X.Practicing.DRs.only..What.percentage.of.your.responsibilities.pertain.to.procedures.direct.patient.care. %>% table
# NA
data$X.Practicing.DRs.only..What.percentage.of.your.responsibilities.pertain.to.procedures.direct.patient.care.[data$X.Practicing.DRs.only..What.percentage.of.your.responsibilities.pertain.to.procedures.direct.patient.care. == ""] <- NA



#### 🤍Q15_Categorical_Practicing_Diagnostic ------------------------------------------------
data$X.Practicing.DRs.only..What.percentage.of.your.responsibilities.pertain.to.diagnostic.radiology. %>% table

#### 🤍Q16_Categorical_Practicing_Administrative ------------------------------------
data$X.Practicing.DRs.only..What.percentage.of.your.time.is.dedicated.to.administrative.non.clinical. %>% table

#### 🤍Q17_Categorical_Trainee_Level ----------------------------------------------
data$X.Trainees.only..What.is.your.level.of.training. %>% table
variable_name <- "X.Trainees.only..What.is.your.level.of.training."
which(data[[variable_name]]=="")
data[[variable_name]] <- ifelse(data[[variable_name]] == "", NA, data[[variable_name]])




### 🟨Scores ====================================================================
#### 🤍Q18_Score_QOL -------------------------------------------------------
data$Choose.any.number.between.0.and.100.that.describes.your.quality.of.life...100.....Perfect.quality.of.life..95.......Nearly.perfect.quality.of.life..90..85.......Very.good.quality.of.life..80..75..70.......Good.quality.of.life..65..60.............Moderately.good.quality.of.life.55..50..45..40.......Somewhat.bad.quality.of.life..35..30.............Bad.quality.of.life.25..20..15.......Very.bad.quality.of.life..10..5.........Extremely.bad.quality.of.life..0.........No.quality.of.life
#### Find elements which can never be transformed to Numeric
variable_name <- "Choose.any.number.between.0.and.100.that.describes.your.quality.of.life...100.....Perfect.quality.of.life..95.......Nearly.perfect.quality.of.life..90..85.......Very.good.quality.of.life..80..75..70.......Good.quality.of.life..65..60.............Moderately.good.quality.of.life.55..50..45..40.......Somewhat.bad.quality.of.life..35..30.............Bad.quality.of.life.25..20..15.......Very.bad.quality.of.life..10..5.........Extremely.bad.quality.of.life..0.........No.quality.of.life"
which_non_numeric <- which(!grepl("^\\d+$", data[[variable_name]]))
data[[variable_name]][which_non_numeric]
##### Change elements
data[[variable_name]][which_non_numeric[1]] = mean(c(55, 50, 45, 40))
data[[variable_name]][which_non_numeric[2]] = 70
#### Change class
data[[variable_name]] = as.numeric(data[[variable_name]])

#### 🤍Q19_Score_MI_Betrayed -----------------------------------------------------
data$I.feel.betrayed.by.other.health.professionals.whom.I.once.trusted.

#### 🤍Q20_Score_MI_Guilt ----------------------------------------------------------
data$I.feel.guilt.over.failing.to.save.someone.from.being.seriously.injured.or.dying.

#### 🤍Q21_Score_MI_Ashamed -----------------------------------------------------
data$I.feel.ashamed.about.what.I.ve.done.or.not.done.when.providing.care.to.my.patients.

#### 🤍Q22_Score_MI_Troubled ----------------------------------------------------
data$I.am.troubled.by.having.acted.in.ways.that.violated.my.own.morals.or.values.

#### 🤍Q23_Score_MI_Trustworthy --------------------------------------------------
data$Most.people.with.whom.I.work.as.a.health.professional.are.trustworthy.

#### 🤍Q24_Score_MI_Meaningful ----------------------------------------------------------
data$I.have.a.good.sense.of.what.makes.my.life.meaningful.as.a.health.professional.

#### 🤍Q25_Score_MI_Forgiven ----------------------------------------------------------
data$I.have.forgiven.myself.for.what.s.happened.to.me.or.to.others.whom.I.have.cared.for.

#### 🤍Q26_Score_MI_Failure ---------------------------------------------------------
data$All.in.all..I.am.inclined.to.feel.that.I.am.a.failure.in.my.work.as.a.health.professional.

#### 🤍Q27_Score_MI_Punishment -----------------------------------------------------
data$I.sometimes.feel.God.is.punishing.me.for.what.I.ve.done.or.not.done.while.caring.for.patients.

#### 🤍Q28_Score_MI_RegiousFaith ------------------------------------------------------
data$Compared.to.before.I.went.through.these.experiences..my.religious.spiritual.faith.was.strengthened.



### 🟨Categorical (2) ================================================================
#### 🤍Q29_Category_Distress ------------------------------------------------------------------
data$Do.the.feelings.you.indicated.above.cause.you.significant.distress.or.impair.your.ability.to.function.in.relationships..at.work..or.other.areas.of.life.important.to.you..In.other.words..if.you.indicated.any.problems.above..how.have.these.problems.made.it.challenging.for.you.to.do.work..take.care.of.things.at.home..or.get.along.with.other.people. %>% table

#### 🤍Q30_Written_Contributor ---------------------------------------------------------
data$As.you.understand.it..what.do.you.believe.is.the.greatest.contributor.to.moral.injury. %>% table

#### 🤍Q31_Written_Alleviate ---------------------------------------------------------
data$What.do.you.believe.would.limit.alleviate.moral.injury.most. %>% table



### 🟨GPT ======================================================================
#### 🤍Did I check all variables? --------------------------------------------------
names(data)[19:28]
# 변수 확인 목록을 제공해주신 데이터의 변수들과 비교해보았습니다. 확인 결과, 목록에 포함된 모든 변수가 데이터의 변수 목록에도 나타나고 있으며, 별도로 빠진 변수는 없는 것으로 보입니다. 
# 
# 제공해주신 코드에 따라, 각각의 변수에 대해 `table` 함수를 사용하여 데이터의 분포를 확인하고 계신 것 같습니다. `table` 함수는 주어진 범주형 데이터의 빈도수를 계산하는 데 사용됩니다.



#### 🤍Check the following ---------------------------------------------------------
# 다만, 확인해야 할 몇 가지 사항이 있습니다:
#   
#   1. **Q16_Practicing_Administrative 변수의 처리:** 
# 이 변수에 대한 `table` 함수가 사용되지 않았습니다. 
# 이것이 의도적인지 확인이 필요합니다. 
# 이 변수가 수치형이라면 `table` 대신 다른 요약 통계를 사용해야 할 수 있습니다.
# 
# 2. **수치형 변수의 처리:** 
# Q18부터 Q28까지의 변수들은 'Scores' 섹션에 속하는 것으로 보이며, 
# 이들은 수치형 데이터일 가능성이 높습니다. 
# 이러한 경우, `table` 대신에 `summary` 함수를 사용하여 
# 각 변수의 기초 통계량(평균, 중앙값, 최솟값, 최댓값 등)을 확인하는 것이 더 적절할 수 있습니다.
# 
# 3. **변수 이름의 명확성:** 변수 이름이 매우 길고, 
# 점이 많이 사용되어 가독성이 떨어집니다. 
# 가능하다면 더 간결하고 명확한 변수 이름을 사용하는 것이 
# 데이터 분석 과정에서의 이해도와 효율성을 높일 수 있습니다.
# 
# 이외에도 데이터 분석을 수행하면서 데이터의 구조, 누락된 값, 이상치 등을 확인하는 것이 중요합니다. 데이터를 충분히 이해하고 분석 계획을 세우는 것이 좋은 결과를 얻는 데 도움이 될 것입니다.








## 🟧Change Variables' names ###########################################################

### 🟨Old data (Shortened variable names) ================================================================

#### Shortened Old Names(Questions)
Shortened_Original_Name = c(
  "With.which.of.the.following.concepts.are.you.familiar.",
  "How.did.you.hear.about.this.survey.",
  "I.am",
  "How.many.years.have.you.been.in.practice.",
  "Which.country.do.you.practice.in.",
  "What.is.your.gender.",
  "What.is.your.race.ethnicity.",
  "What.is.your.marital.status.",
  "What.best.describes.your.practice.",
  "What.best.describes.the.ownership.of.your.practice.",
  "What.percentage.of.your.work.is.completed.at.home.",
  "Do.you.practice.at.more.than.one.hospital.",
  "Do.you.have.an.institutional.leadership.position...e.g..program.director..section.chief..department.chair.",
  "X.Practicing.DRs.only..What.percentage.of.your.responsibilities.pertain.to.procedures.direct.patient.care.",
  "X.Practicing.DRs.only..What.percentage.of.your.responsibilities.pertain.to.diagnostic.radiology.",
  "X.Practicing.DRs.only..What.percentage.of.your.time.is.dedicated.to.administrative.non.clinical.",
  
  "X.Trainees.only..What.is.your.level.of.training.",
  
  "Choose.any.number.between.0.and.100.that.describes.your.quality.of.life",
  
  "I.feel.betrayed.by.other.health.professionals.whom.I.once.trusted.",
  
  "I.feel.guilt.over.failing.to.save.someone.from.being.seriously.injured.or.dying.",
  
  "I.feel.ashamed.about.what.I.ve.done.or.not.done.when.providing.care.to.my.patients.",
  
  "I.am.troubled.by.having.acted.in.ways.that.violated.my.own.morals.or.values.",
  
  "Most.people.with.whom.I.work.as.a.health.professional.are.trustworthy.",
  
  "I.have.a.good.sense.of.what.makes.my.life.meaningful.as.a.health.professional.",
  
  "I.have.forgiven.myself.for.what.s.happened.to.me",
  
  "All.in.all..I.am.inclined.to.feel.that.I.am.a.failure",
  
  "I.sometimes.feel.God.is.punishing.me",
  
  "Compared.to.before.my.religious.spiritual.faith.was.strengthened.",
  
  "Do.the.feelings.you.indicated.above.cause.you.significant.distress.or.impair.your.ability",
  
  "As.you.understand.it..what.do.you.believe.is.the.greatest.contributor.to.moral.injury.",
  
  "What.do.you.believe.would.limit.alleviate.moral.injury.most.")

#### Backup
data_old = data

#### change names
names(data_old) = Shortened_Original_Name

### Save as RDS
saveRDS(object = data_old, file = paste0(path_save, "/Data___Old (Shortened Names).rds"))
saveRDS(object = data, file = paste0(path_save, "/Data___Old (Original Names).rds"))



### 🟨New data: Name Change =================================================================
library(dplyr)
data <- data %>%
  rename(
    Q1_Category_Concepts = With.which.of.the.following.concepts.are.you.familiar.,
    Q2_Category_Survey = How.did.you.hear.about.this.survey.,  # 규칙 적용 제외
    Q3_Category_Position = I.am,
    Q4_Category_Years = How.many.years.have.you.been.in.practice.,
    Q5_Written_Country = Which.country.do.you.practice.in.,  # 규칙 적용 제외
    Q6_Category_Sex = What.is.your.gender.,
    Q7_Category_Ethnicity = What.is.your.race.ethnicity.,
    Q8_Category_Marriage = What.is.your.marital.status.,
    Q9_Category_Practice = What.best.describes.your.practice.,
    Q10_Written_Ownership = What.best.describes.the.ownership.of.your.practice.,  # 규칙 적용 제외
    Q11_Category_HomeWork = What.percentage.of.your.work.is.completed.at.home.,
    Q12_Category_PracticeHospital = Do.you.practice.at.more.than.one.hospital.,
    Q13_Category_Leadership = Do.you.have.an.institutional.leadership.position...e.g..program.director..section.chief..department.chair.,
    Q14_Category_Practicing_Procedure = X.Practicing.DRs.only..What.percentage.of.your.responsibilities.pertain.to.procedures.direct.patient.care.,
    Q15_Category_Practicing_Diagnostic = X.Practicing.DRs.only..What.percentage.of.your.responsibilities.pertain.to.diagnostic.radiology.,
    Q16_Category_Practicing_Administrative = X.Practicing.DRs.only..What.percentage.of.your.time.is.dedicated.to.administrative.non.clinical.,
    Q17_Category_Trainee_Level = X.Trainees.only..What.is.your.level.of.training.,
    Q18_Score_QOL = Choose.any.number.between.0.and.100.that.describes.your.quality.of.life...100.....Perfect.quality.of.life..95.......Nearly.perfect.quality.of.life..90..85.......Very.good.quality.of.life..80..75..70.......Good.quality.of.life..65..60.............Moderately.good.quality.of.life.55..50..45..40.......Somewhat.bad.quality.of.life..35..30.............Bad.quality.of.life.25..20..15.......Very.bad.quality.of.life..10..5.........Extremely.bad.quality.of.life..0.........No.quality.of.life,  # 규칙 적용 제외
    Q19_Score_MI_Betrayed = I.feel.betrayed.by.other.health.professionals.whom.I.once.trusted.,  # 규칙 적용 제외
    Q20_Score_MI_Guilt = I.feel.guilt.over.failing.to.save.someone.from.being.seriously.injured.or.dying.,  # 규칙 적용 제외
    Q21_Score_MI_Ashamed = I.feel.ashamed.about.what.I.ve.done.or.not.done.when.providing.care.to.my.patients.,  # 규칙 적용 제외
    Q22_Score_MI_Troubled = I.am.troubled.by.having.acted.in.ways.that.violated.my.own.morals.or.values.,  # 규칙 적용 제외
    Q23_Score_MI_Trustworthy = Most.people.with.whom.I.work.as.a.health.professional.are.trustworthy.,  # 규칙 적용 제외
    Q24_Score_MI_Meaningful = I.have.a.good.sense.of.what.makes.my.life.meaningful.as.a.health.professional.,  # 규칙 적용 제외
    Q25_Score_MI_Forgiven = I.have.forgiven.myself.for.what.s.happened.to.me.or.to.others.whom.I.have.cared.for.,  # 규칙 적용 제외
    Q26_Score_MI_Failure = All.in.all..I.am.inclined.to.feel.that.I.am.a.failure.in.my.work.as.a.health.professional.,  # 규칙 적용 제외
    Q27_Score_MI_Punishment = I.sometimes.feel.God.is.punishing.me.for.what.I.ve.done.or.not.done.while.caring.for.patients.,  # 규칙 적용 제외
    Q28_Score_MI_RegiousFaith = Compared.to.before.I.went.through.these.experiences..my.religious.spiritual.faith.was.strengthened.,  # 규칙 적용 제외
    Q29_Category_Distress = Do.the.feelings.you.indicated.above.cause.you.significant.distress.or.impair.your.ability.to.function.in.relationships..at.work..or.other.areas.of.life.important.to.you..In.other.words..if.you.indicated.any.problems.above..how.have.these.problems.made.it.challenging.for.you.to.do.work..take.care.of.things.at.home..or.get.along.with.other.people.,
    Q30_Written_Contributor = As.you.understand.it..what.do.you.believe.is.the.greatest.contributor.to.moral.injury.,  # 규칙 적용 제외
    Q31_Written_Alleviate = What.do.you.believe.would.limit.alleviate.moral.injury.most.  # 규칙 적용 제외
  )




## 🟧🩵Save modified new data =============================================================
saveRDS(data, paste0(path_save, "/Data___1.Preprocessing.rds"))





# 🟥Survey Internal Consistency ############################################################
# 1. Consistency of items for Moral Injury.
# Subjects : Interventional Radiologists
# Number of questions regarding MI : 10
# Cronbach’s alpha : 0.71
# Cronbach’s alpha = 0.71 shows good reliability (𝛼 > 0.7) of the 10 items that represent MI.
# Note: Reliability for Quality of Life (QOL) is not validated since there is only one question for QOL.

# _MI_ 이름을 포함하는 변수들에 대해서 

## 🟧Load Data =================================================================
Data.df = list.files(path_save, full.names = T, pattern = "Data___1") %>% readRDS


## 🟧Results list =================================================================
results.list = list()

## 🟧Select MI Scores =================================================================
MI_Scores.df = Data.df %>% dplyr::select(contains("Score_MI"))




## 🟧Decide negative vs positive questions =================================================================
# Generate df
survey_questions.df <- data.frame(
  Category = c("Negative", "Negative", "Negative", "Negative", "Positive",
               "Positive", "Positive", "Negative", "Negative", "Positive"),
  Name = c("Q19_Score_MI_Betrayed", "Q20_Score_MI_Guilt", "Q21_Score_MI_Ashamed",
           "Q22_Score_MI_Troubled", "Q23_Score_MI_Trustworthy", "Q24_Score_MI_Meaningful",
           "Q25_Score_MI_Forgiven", "Q26_Score_MI_Failure", "Q27_Score_MI_Punishment",
           "Q28_Score_MI_RegiousFaith"),
  Question = c("I feel betrayed by other health professionals whom I once trusted.",
               "I feel guilt over failing to save someone from being seriously injured or dying.",
               "I feel ashamed about what I've done or not done when providing care to my patients.",
               "I am troubled by having acted in ways that violated my own morals or values.",
               "Most people with whom I work as a health professional are trustworthy.",
               "I have a good sense of what makes my life meaningful as a health professional.",
               "I have forgiven myself for what's happened to me or to others whom I have cared for.",
               "All in all, I am inclined to feel that I am a failure in my work as a health professional.",
               "I sometimes feel God is punishing me for what I've done or not done while caring for patients.",
               "Compared to before I went through these experiences, my religious/spiritual faith was strengthened."))


survey_questions.df %>% dplyr::filter(Category == "Negative") %>% dplyr::select(Question)

## 🟧Inverse coding =================================================================
# which positive cols
which_positive_cols = which(survey_questions.df$Category=="Positive")

# Inverse on positive items
MI_Scores_Inverse.df = Survey___InverseCoding(data.df = MI_Scores.df, 
                                              cols = survey_questions.df$Name[which_positive_cols], 
                                              max_score = max(MI_Scores.df), 
                                              min_score = min(MI_Scores.df))
# add "inverse" in names
names(MI_Scores_Inverse.df)[which_positive_cols] = paste0(names(MI_Scores_Inverse.df)[which_positive_cols], "___Inverse")
  
  


## 🟧Cronbach Alpha =================================================================
results.list$Cronbach = psych::alpha(MI_Scores_Inverse.df)

  


## 🟧McDonald's Omega ===========================================================
if(!require("psych")){
  install.packages("psych")  
}
if(!require("GPArotation")){
  install.packages("GPArotation")  
}
# 탐색적 요인 분석으로 적절한 요인 수 탐색 (Minimum Residuals method)
results.list$EFA_result <- fa.parallel(MI_Scores_Inverse.df, fm = "minres", fa = "fa")

# omega 함수에 적절한 nfactors 적용
results.list$Omega = omega(MI_Scores_Inverse.df, nfactors = results.list$EFA_result$nfact)




## 🟧Export ===========================================================================
saveRDS(results.list, paste0(path_save, "/Results___InternalConsistency.rds"))
saveRDS(list(Data = Data.df, MI_Scores_Inverse = MI_Scores_Inverse.df, survey_questions.df=survey_questions.df), paste0(path_save, "/Data___2.Consistency.rds"))







  # 🟥Descriptive Statistics ############################################################
## 🟧Data load =======================================================================
data_descriptive = readRDS(paste0(path_save, "/Data___2.Consistency.rds"))
data_descriptive = data_descriptive$Data


## 🟧Category =======================================================================
### 🟨Define a function --------------------------------------------------------------------
create_category_tables <- function(data) {
  result_list <- list()
  
  for (col_name in names(data)) {
    
    if (grepl("_Category_", col_name)) {
      column <- data[[col_name]]
      column[is.na(column)] = "NA"
      column[column==""] = "NA"
      freq <- table(column, useNA = "ifany")
      percent <- prop.table(freq) * 100
      
      result_df <- data.frame(
        Element = names(freq),
        Frequency = as.integer(freq),
        Percentage = round(percent, 4) %>% unname %>% as.vector
      )
      
      # freq 열을 기준으로 내림차순으로 정렬
      result_df <- result_df[order(result_df$Frequency, decreasing = TRUE), ]
      
      result_list[[col_name]] <- result_df
    }
  }
  
  return(result_list)
}




### 🟨Calculate results --------------------------------------------------------------------
Results_Category.list <- create_category_tables(data_descriptive)



### 🟨Plotting --------------------------------------------------------------------
# Define a function
plotting = function(frequency_data, path_save, filename){
  # 파이 차트 생성
  p <- ggplot(frequency_data, aes(x = factor(1), y = Percentage, fill = Element)) + 
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") + 
    scale_fill_brewer(palette = "Pastel1") +
    labs(title = "Percentage of Responses", y = "Percentage", x = NULL) +
    theme_void() +
    theme(legend.title = element_blank())
  
  # percentage 레이블 추가
  p <- p + geom_text(aes(label = paste0(round(Percentage, 2), "%")), 
                     position = position_stack(vjust = 0.5),
                     check_overlap = TRUE, size = 3)  # 텍스트 크기 조정
  
  
  
  ggsave(filename = paste0(path_save, "/[Piecharts] ", filename, ".png"), plot = p, bg = "white", width = 2000, height = 1000, units = "px")
  return(p)
}


# save plotting
Plot.list = lapply(1:length(Results_Category.list), function(i){
  plotting(frequency_data = Results_Category.list[[i]], path_save, filename = names(Results_Category.list)[i])
})



### 🟨Save results --------------------------------------------------------------------
Results = list(cartegory = Results_Category.list, plot = Plot.list)
saveRDS(Results, paste0(path_save, "/Results___DescriptiveStatistics___Categorical.rds"))





## 🟧Numeric =====================================================================



## 🟧Save Data =====================================================================
data_descriptive = readRDS(paste0(path_save, "/Data___2.Consistency.rds"))
saveRDS(data_descriptive, paste0(path_save, "/Data___3.Descriptive.rds"))




# 🟥Correlation ##########################################################################
## 🟧Results list =============================================================
Results.list = list()

## 🟧Load data =============================================================
data_corr = readRDS(paste0(path_save, "/Data___3.Descriptive.rds"))


## 🟧Setting variables =============================================================
# MI: New Object for MI scores
Score_MI = data_corr$MI_Scores_Inverse %>% rowSums()

# QOL
Score_QOL = data_corr$Data$Q18_Score_QOL


## 🟧Correlation between MI and QOL ======================================================
### 🟨Speaman ------------------------------------------------------------
result_spearman = cor.test(Score_MI, Score_QOL, method = "spearman")


### 🟨Pearson ------------------------------------------------------------
result_pearson = cor.test(Score_MI, Score_QOL, method = "pearson")



### 🟨combine ------------------------------------------------------------
methods = c("Spearman", "Pearson")
estimate = c(result_spearman$estimate, result_pearson$estimate)
pvals = c(result_spearman$p.value, result_pearson$p.value)
Combined = data.frame(Methods = methods, Estimate = estimate, Pvals = pvals)
rownames(Combined) = NULL
Results.list$Corr_Result_QOL = Combined


### 🟨Visualization ==============================================================
require(ggplot2)
require(ggpubr)
# plotting
df = data.frame(Score_MI, Score_QOL)
Results.list$Corr_Result_QOL_Plot <- ggplot___scatterplot(df, x = "Score_MI", y = "Score_QOL", method = "pearson")



## 🟧 Correlation 2 =================================================
# combined data
data_corr_2 = cbind(data_corr$MI_Scores_Inverse,  Score_QOL)

# correlation with MI subscores
Results.list$Corr_tests = ggplot___correlation(df = data_corr_2, y = "Score_QOL", method = "pearson", p.adj.method = "bonferroni")






## 🟧Export results ====================================================
# test result
saveRDS(Results.list, paste0(path_save, "/Results___Correlation.rds"))
# Data 
data_corr_2 = c(data_corr, list(df))
names(data_corr_2)[4] = "QOL_and_MI_only"
saveRDS(data_corr_2, paste0(path_save, "/Data___4.Corr.rds"))




# 🟥 Linear Regression Analysis ###############################################################
## 🟧 Total Results =================================================
Combined_Results.list = list()



## 🟧 Loading data ==========================================================================
data_reg = readRDS(list.files(path_save, pattern = "Data___4.Corr", full.names = T))
data_reg = cbind(Score_QOL = data_reg$QOL_and_MI_only$Score_QOL, data_reg$MI_Scores_Inverse)




## 🟧 Results List================================================================
Results.list = list()




## 🟧 Correlation ================================================================
# Plot
Results.list$Correlation_Plot = ggplot___correlation(data_reg, 
                                                     method = "pearson",
                                                     p.adj.method = "bonferroni")
# Corr test results
Results.list$Correlation_Test =  Test___Correlation(data_reg, 
                                                    y = "Score_QOL", 
                                                    alpha = 0.05,
                                                    method = "pearson", 
                                                    p.adj.method = "bonferroni")



## 🟧 Multicollinearity ================================================================
Results.list$Multicollinearity = Regression___LinearRegression(df = data_reg, y = "Score_QOL")



## 🟧 Linear Regression analysis 1 =================================================
df_new = data.frame(Score_MI = rowSums(data_reg[,-1]), Score_QOL = data_reg$Score_QOL)
Results.list$Fit_1 = Regression___LinearRegression(df = df_new, y = "Score_QOL")



## 🟧 Linear Regression analysis 2 =================================================
Results.list$Fit_2 = Regression___LinearRegression(df = data_reg, y = "Score_QOL")



## 🟧 Linear Regression analysis 3 =================================================
# significant variables
X = Results.list$Fit_2$Regression_Results$Coefficients$Signif_Variables
Results.list$Fit_3 = Regression___LinearRegression(data_reg, "Score_QOL", X)



## 🟧 Comparison plot of Adj R^2 ==============================================================
adj_R2 = c(Results.list$Fit_1$Regression_Results$Diagnosis$adj_R2,
           Results.list$Fit_2$Regression_Results$Diagnosis$adj_R2,
           Results.list$Fit_3$Regression_Results$Diagnosis$adj_R2)


# barplot
Results.list$Plot_Adj.R2.Vals = ggplot___barplot(adj_R2, title = "Adjusted R^2 Comparison", xlab = "Model", ylab = "R^2 values", adding.values = T)



## 🟧 Comparison plot of p-vals ==============================================================
ANOVA_pvals = c(Results.list$Fit_1$Regression_Results$Diagnosis$ANOVA_Total$p_val,
                Results.list$Fit_2$Regression_Results$Diagnosis$ANOVA_Total$p_val,
                Results.list$Fit_3$Regression_Results$Diagnosis$ANOVA_Total$p_val)

# Barplot
Results.list$Plot_ANOVA_pvals = ggplot___barplot(ANOVA_pvals, title = "P values of ANOVA", xlab = "Model", ylab = "p values", adding.values = T)


## 🟧 Export =================================================
saveRDS(Results.list, file = paste0(path_save, "/Results___Linear Regression.rds"))








# 🟥Mean difference test: MI  yes / no ###############################################################
## 🟧Data Load ============================================================================
Data.list = readRDS(list.files(path_save, pattern = "Data___4.Corr", full.names = T)) 
# combine data
Data_MeanDiff.df = cbind(Data.list$Data, Data.list$QOL_and_MI_only)


## 🟧New coloumn : MI group ================================================================
Data_MeanDiff_2.df = Data_MeanDiff.df %>% 
  dplyr::mutate(Category_MI_Group = ifelse(Score_MI >= 36, "Yes", "No"))




## 🟧Mean diff test ================================================================
ANOVA = Test___MeanDiff(Data = Data_MeanDiff_2.df,
                        Response_Vars = "Score_MI",
                        Group_Var = "Category_MI_Group",
                        Group_Var_Type = "Nominal",
                        alpha_ANOVA = 0.05, 
                        path_save = path_save)




## 🟧Compute Statistics ================================================================
Stats = Stats___byGroup(df = Data_MeanDiff_2.df, 
                        group_var = "Category_MI_Group",
                        numeric_var = "Score_MI",
                        na.rm = F)



## 🟧Export the results ================================================================
# Results
Results = list(ANOVA = ANOVA, Stats = Stats)
saveRDS(object = Results, file = paste0(path_save, "/Results___ANOVA___`MI_Group`.rds"))
# New Data
saveRDS(Data_MeanDiff_2.df, file = paste0(path_save, "/Data___5.[ANOVA] `MI_Group`.rds"))



# 🟥Mean difference test 2 : MI, QOL ###############################################################
## 🟧Data Load ====================================================================
Data.df = readRDS(list.files(path_save, pattern = "Data___5", full.names = T))



## 🟧New Group vars =============================================================================
### 🟨Overall ------------------------------------------------------------------
Data.df = Data.df %>%
  mutate(Category_Sex_Male_Female_Only = ifelse(Q6_Category_Sex %in% c("Female", "Male"), Q6_Category_Sex, NA)) %>% 
  mutate(Category_Sex_Male_and_Not_Male = ifelse(Q6_Category_Sex == "Male", "Male", "Not Male")) %>% 
  mutate(Category_Marriage_Only = ifelse(Q8_Category_Marriage == "Married", "Married", "Not Married")) %>% 
  mutate(Category_Marriage_LongTerm = ifelse(Q8_Category_Marriage %in% c("Married", "Long-term partner"), "Married/Long-term", "Other")) %>% 
  mutate(Category_Marriage_ANOVA = ifelse(Q8_Category_Marriage %in% c("Married", "Long-term partner", "Single"), Q8_Category_Marriage, NA))



### 🟨Ethinicity -----------------------------------------------------------------
# Extract 
ethnicity_data <- Data.df$Q7_Category_Ethnicity
# Ethnicity 변수를 새로운 그룹으로 묶음
ethnicity_group <- case_when(
  ethnicity_data %in% c("Asian") ~ "Asian",
  ethnicity_data %in% c("White") ~ "White",
  ethnicity_data %in% c("Black or African American") ~ "Black & Hispanic/Latin",
  ethnicity_data %in% c("Hispanic/Latinx") ~ "Black & Hispanic/Latin",
  ethnicity_data %in% c("Mixed race") ~ "Other",
  ethnicity_data %in% c("Other") ~ "Other",
  TRUE ~ "Prefer not to say"
)
# New var
Data.df <- Data.df %>%
  mutate(Category_Ethnicity = ethnicity_group)


### 🟨Year variable factorization --------------------------------------------------
Data.df$Q4_Category_Years = factor(Data.df$Q4_Category_Years, labels = c("Currently in training", 
                                                                         "<1 year",
                                                                         "1-10 years",
                                                                         "10-20 years",
                                                                         ">20 years"))



## 🟧Select Variables =============================================================================
### 🟨Response Vars -------------------------------------------------------
Response_Vars = c("Score_MI", "Score_QOL")



### 🟨Group Vars ------------------------------------------------------------------
# Common group vars for both response variables
Group_Vars_Common = c(# Q3 Position
                      "Q3_Category_Position", # Training / Practicing
                      # Q4 Practice Years
                      "Q4_Category_Years",
                      # Q6 Sex
                      "Category_Sex_Male_Female_Only",  # Male / Female
                      "Category_Sex_Male_and_Not_Male", # Male / others
                      # Q7 Ethnicity
                      "Category_Ethnicity",
                      # Q8 Marriage
                      "Category_Marriage_Only",
                      "Category_Marriage_LongTerm",
                      "Category_Marriage_ANOVA",
                      # Q9 practice
                      "Q9_Category_Practice",
                      # Q12 Practice Hospital
                      "Q12_Category_PracticeHospital")
# Group vars for MI
Group_Vars_for_MI = Group_Vars_Common
# Group vars for QOL
Group_Vars_for_QOL = c(Group_Vars_Common, "Category_MI_Group")



 





## 🟧Statistics ===========================================================================
# MI
Stats_MI = lapply(Group_Vars_for_MI, function(y){

  Stats___byGroup(df = Data.df, 
                  group_var = y,
                  numeric_var = "Score_MI",
                  na.rm = F)
  
}) %>% setNames(Group_Vars_for_MI)


# QOL
Stats_QOL = lapply(Group_Vars_for_QOL, function(y){
  
  Stats___byGroup(df = Data.df, 
                  group_var = y,
                  numeric_var = "Score_QOL",
                  na.rm = F)
  
}) %>% setNames(Group_Vars_for_QOL)





## 🟧Test : MI =============================================================================
Response_Vars = "Score_MI"
Group_Vars = Group_Vars_for_MI
Group_Var_Types = rep("Nominal", times = length(Group_Vars))
ANOVA_MI = lapply(seq_along(Group_Vars), function(k){
  
  Test___MeanDiff(Data = Data.df,
                  Response_Vars = Response_Vars,
                  Group_Var = Group_Vars[k],
                  Group_Var_Type = Group_Var_Types[k],
                  alpha_ANOVA = 0.05/length(Group_Vars),
                  path_save = path_save) 
  
}) %>% setNames(Group_Vars) %>% suppressWarnings()
 



## 🟧Test : QOL =============================================================================
Response_Vars = "Score_QOL"
Group_Vars = Group_Vars_for_QOL
Group_Var_Types = rep("Nominal", times = length(Group_Vars))

ANOVA_QOL = lapply(seq_along(Group_Vars), function(k){
  
  Test___MeanDiff(Data = Data.df,
                  Response_Vars = Response_Vars,
                  Group_Var = Group_Vars[k],
                  Group_Var_Type = Group_Var_Types[k],
                  alpha_ANOVA = 0.05/length(Group_Vars),
                  path_save = path_save) 
  
}) %>% setNames(Group_Vars) %>% suppressWarnings()




## 🟧Export =============================================================================
saveRDS(Data.df, paste0(path_save, "/Data___6.[ANOVA] `QOL & MI`.rds"))
Results = list(ANOVA_MI = ANOVA_MI, 
               ANOVA_QOL = ANOVA_QOL,
               Stats_MI = Stats_MI, 
               Stats_QOL = Stats_QOL)
saveRDS(Results, paste0(path_save, "/Results___6.[ANOVA] `QOL & MI`.rds"))










# 🟥Extra Mean difference test 3: the other categorical variables ####################################
# Data.df$Q9_Category_Practice %>% table
# Data.df$Q11_Category_HomeWork %>% table
# Data.df$Q17_Category_Trainee_Level %>% table
# Data.df$Q16_Category_Practicing_Administrative %>% table


# 다음은 p-val 조정을 안 했으니 참고만 하고, 연구에 필요할 것 같으면 알려줘라.
# Data$Q29_Category_Distress %>% table
# Data.df$Q7_Category_Ethnicity %>% table
# Data.df$Q1_Category_Concepts %>% table
# Q9_Category_Practice 변수에 대해서도 할 필요가 있는지?




# 🟥Wordcloud #############################################################################
## 🟧Data Load ======================================================
Data.df = readRDS(list.files(path_save, pattern = "Data___6", full.names = T))




## 🟧Chunk words ====================================================
Written = c("Q30_Written_Contributor", "Q31_Written_Alleviate")


# Q30
Data.df[,Written[1]]




## 🟧 Contributor  ================================================================================================
# As.you.understand.it..what.do.you.believe.is.the.greatest.contributor.to.moral.injury.
### 🟨 Total texts ================================================================================================
Contributor_texts = Data.df[, Written[1]]
which_NA = which(Contributor_texts %in% c("", "-"))
Contributor_texts_New = Contributor_texts[-which_NA]


Contributor_texts_New[1:30]
Contributor_texts_New[31:60]
grep("Over worked", Contributor_texts_New)




### 🟨 Grouping (overlapped) ================================================================================================






## 🟧 Alleviate  ===================================================================
#"What.do.you.believe.would.limit.alleviate.moral.injury.most."
Alleviate_texts = Data.df[, Written[2]]




# 행정적 부담 감소 및 의료 전문가 존중
administrative_reduction_and_respect <- c(
  "Streamline healthcare and cutout the unnecessary administrative bloat. Instill respect for healthcare professionals' unique challenges in management who have never cared for a patient or been a physician.",
  "More MD/DOs and fewer non physician admins making healthcare policy/decisions",
  "Getting rid of poor managers",
  "Better staffing, less pressure, better equity ",
  "Fire all administrators",
  "flattening the hierarchy",
  "Honesty about health funding issues and its effect",
  "Getting rid of incentives to use employer-based health insurance",
  "Eliminating insurance and significantly downsizing administration ",
  "Reduce Bureaucracy. Eliminate or better protect docs from frivolous lawsuits. So people can spend more time on what matters.",
  "Eliminate insurance companies"
)

# 업무 부담 감소 및 지원 증대
workload_reduction_and_support <- c(
  "Limit case load",
  "Less volume ",
  "Support groups",
  "Being heard",
  "Support",
  "More autonomy and patient centric care",
  "Better staffing ",
  "Support",
  "Early retirement ",
  "Advocacy, support, and control ",
  "Work life balance",
  "Less work. Standards for mid level providers (who over order and mistreat residents).",
  "Therapy",
  "Therapy or having a safe space to talk about it.",
  "More in person interactions to foster more personal relationships at work",
  "Having control over quality of imaging/study appropriateness.",
  "make physicians accountable for their actions",
  "Shifting culture towards safe vulnerability and honesty.",
  "People being an honest and decent human first, not falling to outside pressure",
  "Acceptance. Honesty conversations",
  "Going remote",
  "Less admins dictating how doctors practice",
  "Honesty, ethics, team spirit to get work done in interest of patients and not profits",
  "Better boundaries ",
  "Retirement",
  "Medical Law suits",
  "Allowing more physician leadership",
  "Everyone doing what is best for patients at all times",
  "Putting more emphasis on quality of care instead of quantity.",
  "Better standards and recognition for quality reporting rather than emphasizing quantity. it seems at my practice that radiologists whose reads are very fast but low quality (lacking detail or any information useful to clinician; frequent misses) are allowed to continue practicing and rewarded with raises for productivity, while radiologists who follow search patterns and take even a little extra time to be thorough (ask other rads for consults; look up references) are just derisively pegged as \"slow\" and lower-value....despite the fact that referrers prefer these reads.",
  "Limit the politics in admin and having so many in leadership that are non medical making decisions in departments that are directly hurting patients",
  "Working half days",
  "Truth",
  "Inc respect for MDs",
  "Physician leadership and respect",
  "Eliminating bad eggs, instead of promoting or moving them on to other jobs",
  "Telling everyone that we are good but not God. Less fear of getting sued",
  "Transparency",
  "More time off",
  "More time outside of work.",
  "Better management",
  "Time to communicate",
  "If leadership had the junior people’s backs more and actually stood up for them.",
  "Working in teams",
  "Supportive colleagues",
  "Transparency, fair work hours, fair compensation",
  "Awareness, support of and belief in each other, calling out bullying behaviors",
  "If people would learn to respond without judgement, validate someone’s concern/complaint/distress before making further comment",
  "Being able to say I'm sorry and I wish I did differently",
  "Rewarded for quality",
  "Leave medicine",
  "Not being overworked.",
  "Respect",
  "Emotional intelligence and shame resilience training as early in medical career as possible",
  "Balance and realistic expectations",
  "Time off",
  "Better screening of exams through MDs through the techs",
  "Autonomy to do what I know is best, the ability to advocate for my patients' needs without fear of retribution, the resources to address barriers to care without politics and unnecessary delay, fair reimbursement rates, treated with respect as the professional that I have worked so hard to become",
  "Respectful actions",
  "Those doctors having a moral compass",
  "Less workload",
  "Improved litigious environment decreasing burden on us as well as other physicians",
  "Revolution in the way we train physicians, gender equality",
  "Ability to influence practice activities, clinical and moral support of both leadership and colleagues, transparency among leadership that allows understanding of decisions, staff support that allows one to practice as a physician rather than assembly line worker",
  "Better communication from referring providers, accurate history",
  "I don’t think it can be limited/ alleviated. All too often it’s those who are most damaging and full of platitudes that are promoted, donned with awards and accolades. The system promotes moral Injury in its leadership structure.",
  "Be true to yourself",
  "Believing in a higher power/God",
  "Lightening load, giving physicians more autonomy",
  "Communication",
  "Strong leadership with integrity + diversity. Acceptance to residency & attending positions blinded to gender and ethnic status of applicants (truly merit based), programs that create pipelines for women and minorities to be successful in applying to and being accepted by medical school & Radiology programs",
  "A fair and transparent work culture.",
  "Having more time to do our work",
  "Time",
  "Enough time to do the job well.",
  "Greater focus on patient care"
)

# 전문가 자율성 및 리더십 강화
professional_autonomy_and_leadership <- c(
  "More autonomy ",
  "Complete Physician Autonomy",
  "More physician autonomy",
  "Physicians in control, less administrators",
  "Getting back to physician leadership",
  "Being able to manage the way I practice rather than being told how to do so",
  "Less admins dictating how doctors practice",
  "Autonomy to do what I know is best, the ability to advocate for my patients' needs without fear of retribution, the resources to address barriers to care without politics and unnecessary delay, fair reimbursement rates, treated with respect as the professional that I have worked so hard to become"
)

# 환자 중심의 관리 및 의사 결정 참여
patient_centered_care_and_decision_engagement <- c(
  "Patient and physician centered right sizing",
  "understanding an individual's perspective and allowing room for all options/opinions re: care",
  "Engagement in operational decisions",
  "Fix the system",
  "Not sure, maybe giving the practice of medicine back to physicians and other healthcare providers",
  "Financial independence so we can walk away from jobs that force us to act in ways that are against our core beliefs.",
  "Always doing what you think is the right thing by the patient. Standing up for yourself.",
  "Transparency and a safe space to discuss mistakes, to not feel like a lone wolf",
  "Limiting unnecessary scans, eliminating rvu minimums or rvu based compensation",
  "The freedom to choose the course if action you feel is best/right no matter what",
  "Administrators and healthcare leadership and ancillary staff listening to their physicians when they tell them what they need to provide high level patient care rather than focusing on the bottom line and RVUS. Respecting the fact that physicians have knowledge and expertise and without them there would be no health care system. That their training is worth something."
)

# 주제별 벡터를 리스트로 결합
text_suggestions <- list(
  administrative_reduction_and_respect = administrative_reduction_and_respect,
  workload_reduction_and_support = workload_reduction_and_support,
  professional_autonomy_and_leadership = professional_autonomy_and_leadership,
  patient_centered_care_and_decision_engagement = patient_centered_care_and_decision_engagement
)

Results.list[[2]] = text_suggestions







## 🟧Define a word cloud function ====================================================
# Written = c("Q10_Written_Ownership", "Q30_Written_Contributor", "Q31_Written_Alleviate")
New_Written_Variables = c()
for(kth_Written in Written){
  Text___Wordcloud(text = Data.df[, kth_Written] %>% unlist, path_save, filename = paste0("Wordcloud___", kth_Written))
}







## 🟧 Save results ===============================================================
saveRDS(Results.list, file = paste0(path_save, "/Results___Wordcloud.rds"))










