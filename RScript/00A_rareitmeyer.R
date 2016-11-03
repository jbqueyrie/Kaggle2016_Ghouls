###################
# 00A_rareitmeyer #
###################
# Score (11/03): 0.72023


# Simple solution from rareitmeyer
train$is_ghost <- ifelse(train$type=='Ghost', 1, 0)
train$is_ghoul <- ifelse(train$type=='Ghoul', 1, 0)
train$is_goblin <- ifelse(train$type=='Goblin', 1, 0)

rhs <- 'bone_length+rotting_flesh+hair_length+has_soul+color'

# make one-vs-rest models
ghost_model <- glm(as.formula(sprintf("is_ghost ~ %s", rhs)), data=train, family='binomial')
ghoul_model <- glm(as.formula(sprintf("is_ghoul ~ %s", rhs)), data=train, family='binomial')
goblin_model <- glm(as.formula(sprintf("is_goblin ~ %s", rhs)), data=train, family='binomial')

# work out scores for each model
guessed_scores <- cbind(
  predict(ghost_model, test, type='response'),
  predict(ghoul_model, test, type='response'),
  predict(goblin_model, test, type='response')
)

# predict answers based on model with highest likelyhood score
guessed_answers <- data.frame(
  id=test$id,
  type=c('Ghost', 'Ghoul', 'Goblin')[apply(guessed_scores, 1, which.max)]
)

# save answer
write.csv(guessed_answers, 'submission_20161031.csv', row.names=FALSE)