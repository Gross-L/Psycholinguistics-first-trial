#LOAD THE PACKAGES

library(tidyverse)
library(childesr)
library(tidytext)

# source: http://ling-blogs.bu.edu/lx754f19/childes/
# goal: find out how Nina omits subjects in different points in her linguistic development, and whether that's a good measure of language development

utt <- get_utterances(collection = "Eng-NA", target_child = "Nina",
                      corpus = "Suppes", role = "target_child")
tok <- get_tokens(token = "*", collection = "Eng-NA", target_child = "Nina",
                  corpus = "Suppes", role = "target_child")

#TO HAVE AN OVERVIEW:
#VIEW 

view(utt)
view(tok)

#COUNT THE AMOUNT OF UTTERANCES PER MONTH

utt_count<-utt%>%
  mutate(age=floor(target_child_age))%>%
  group_by(age)%>%
  count()

ggplot()+
  geom_smooth(utt_count, mapping=aes(x=age, y=n))


# 1. get measure of linguistic development

# mean length of utterance (MLU): ratio of morphemes over utterances
mlu <- utt %>% 
  group_by(transcript_id) %>% 
  summarize(mlu = sum(num_morphemes, na.rm = TRUE)/n(),
            age = mean(target_child_age))
# MLU is a measure of linguistic productivity in children.
# A higher MLU is taken to indicate a higher level of language proficiency.
# Mean length of utterance is a good marker of language impairment. 
# It can be used to benchmark language acquisition and is used to compare language intervention outcomes in children with autism.
# (wikipedia)

ggplot(mlu, aes(age, mlu)) +
  geom_point() +
  theme_light()

# 2. find omitted subjects in a subset of utterances

# source uses only most frequent word per transcript because analysis is manual; I use all verbs

# keep only declaratives and questions with verbs in it:
utt_sub <- utt %>% 
  filter(str_detect(part_of_speech, "^(v|.* v)"),
         type %in% c("declarative", "question"))

# find subset of utt_sub where verb appears with a subject (allowing modals or auxiliaries in between)
utt_sub <- utt_sub %>% 
  mutate(subject = str_detect(part_of_speech, "(pro:sub|n)( mod)?( aux)? v"))

# get fraction of utterances with proper subjects per transcript

subjects <- utt_sub %>% 
  group_by(transcript_id) %>% 
  summarize(subject = sum(subject)/n())

# 3. check correlation between two measures of language development:
# MLU and use of proper subjects

full_join(mlu, subjects) %>% 
  ggplot(aes(mlu, subject)) + 
  geom_point() +
  theme_light()

# 4. effect of wh-questions on using subjects

# repeat the same analysis as part 2-3 but filter for utterances that include a wh-question

wh_sub <- utt %>% 
  filter(str_detect(part_of_speech, "^(v|.* v)"),
         str_detect(part_of_speech, "pro:int"),
         str_detect(gloss, "wh"),
         type %in% c("declarative", "question"))

wh_sub <- wh_sub %>% 
  mutate(subject = str_detect(part_of_speech, "(pro:sub|n)( mod)?( aux)? v"))

wh_subjects <- wh_sub %>% 
  group_by(transcript_id) %>% 
  # filter(n() >= 5) %>% 
  summarize(wh_subject = sum(subject)/n())

# correlation between wh-question subject use rate and MLU

full_join(mlu, wh_subjects) %>% 
  ggplot(aes(mlu, wh_subject)) + 
  geom_point() +
  theme_light()

# compare subject use for all utterances and utterances with wh-questions

full_join(subjects, wh_subjects) %>% 
  ggplot(aes(subject, wh_subject)) +
  geom_point() +
  theme_light()

# much more variation in wh-questions, likely due to small samples per transcript
# if transcripts with <5 wh-utterances are excluded, there is a weak positive relationship 
# overall subject use rate is a better measure of linguistic development based on MLU



#FIND A COMMON MISTAKE AND VISUALIZE IT OVER TIME

#While browsing through the data set I realized that a common mistake was the lack of the verb "to be" between the subject and the verb "going to/gonna".
#An example would be I gonna play.

#Therefore I visualized it over time

#USAGE OF "TO BE" IN FRONT OF "GOING TO":

utt_going<-utt%>%
  mutate(age=floor(target_child_age))%>%
  filter(type=="declarative")%>%
  filter(str_detect(gloss, "going to|gonna"))%>%
  mutate(preverb=str_count(gloss, "\\'re|\\'s|\\'m|are|is|am|am not|is not|are not|\\'m not|\\'s not|\\'re not(?=[:blank:]going to)
                           |\\'re|\\'m|\\'s|are|is|am|am not|is not|are not|\\'m not|\\'s not|\\'re not(?=[:blank:]gonna)"))%>%
  filter(preverb<2)%>%
  full_join(utt_count, by="age")%>%
  mutate(pre_div_gloss_per_month=preverb/n)

#ABSOLUTE VALUES 

ggplot()+
  geom_smooth(data=utt_going,mapping = aes(x=age, y=preverb))

#VALUES OVER RATIO 

ggplot()+
  geom_smooth(data=utt_going,mapping = aes(x=age, y=pre_div_gloss_per_month))




#ENGLISH RULES


#Prepositions give more precise details over the action but require more words and thinking.
#faculty.washington.edu/cicero/370syntax.htm
#Therefore I analyzed the amount of prepositions used in utterances per age.

#PREPOSITION USAGE PER AGE
utt%>%
  mutate(age=floor(target_child_age))%>%
  mutate(prep=str_count(part_of_speech, "prep"))%>%
  ggplot()+
  geom_smooth(mapping=aes(x=age,  y= prep))

#PREPOSITION USAGE PER MLU

utt%>%
  full_join(mlu)%>%
  mutate(age=floor(target_child_age))%>%
  mutate(prep=str_count(part_of_speech, "prep"))%>%
  ggplot()+
  geom_smooth(mapping=aes(x=mlu,  y= prep))

#The curve looks very much like a correlation. 
#Let's check it: 

prep<-utt%>%
  full_join(mlu)%>%
  mutate(age=floor(target_child_age))%>%
  mutate(prep=str_count(part_of_speech, "prep"))

cor(prep$prep, prep$mlu)

#Conclusion: It is not a convincing correlation.




#QUESTIONS WITH ACADEMIC BACKGROUND


#The older the child gets, the longer the utterances are.
#Source:
#link: https://pubs.asha.org/doi/abs/10.1044/0161-1461%282003/027%29
#doi: https://doi.org/10.1044/0161-1461(2003/027)

# To compute this theory I thought what does more words in a string mean? One answer is more spaces between the words.
# Therefore I counted the amount of spaces per utterance and added one to get the amount of words.


#AMOUNT OF WORDS PER UTTERANCE OVER TIME
utt%>%
  mutate(age=floor(target_child_age))%>%
  mutate(spaces=str_count(gloss, " ")+1)%>%
  ggplot()+
  geom_smooth(mapping=aes(x=age, y=spaces))

#To check if my spaces idea works I can compare it to another graph using the variable "num_tokens" counting the number of tokens per utterance.

#AMOUNT OF WORDS PER UTTERANCE OVER TIME USING NUM_TOKENS
utt%>%
  mutate(age=floor(target_child_age))%>%
  ggplot()+
  geom_smooth(mapping=aes(x=age, y=num_tokens))


#It is the same graph!


#Apparently, in the first, "receptive" stage, the child says far fewer nouns than he or she understands and does not give any verbs although he or she understands many. 
#Then, there is a coordination of comprehension/production and this is the "productive" stage where the child says almost all the nouns he understands and uses his first verbs.


#Source: https://www.sciencedirect.com/science/article/pii/0010027776900044
#doi: https://doi.org/10.1016/0010-0277(76)90004-4

utt_v_n<-utt%>%
  mutate(age=floor(target_child_age))%>%
  mutate(nouns=str_count(part_of_speech, "n[:blank:]|[:blank:]n[:blank:]|n:|[:blank:]n|^n|n$"))%>%
  mutate(verbs=str_count(part_of_speech, "(?<!ad)v[:blank:]|[:blank:]v[:blank:]|(?<!ad)v:|[:blank:]v|^v|(?<!ad)v$"))


ggplot(data=utt_v_n)+
  geom_smooth(mapping = aes(x=age, y=nouns))

ggplot(data=utt_v_n)+
  geom_smooth(mapping = aes(x=age, y=verbs))

ggplot(data=utt_v_n)+
  geom_smooth(mapping = aes(x=age, y=nouns))+
  geom_smooth(mapping = aes(x=age, y=verbs))



#NOW LET'S TRY THOSE ON ANOTHER CHILD


# source: http://ling-blogs.bu.edu/lx754f19/childes/
# goal: find out how Nina omits subjects in different points in her linguistic development, and whether that's a good measure of language development

utt <- get_utterances(collection = "Eng-NA", target_child = "Adam",
                      corpus = "Brown", role = "target_child")
tok <- get_tokens(token = "*", collection = "Eng-NA", target_child = "Adam",
                  corpus = "Brown", role = "target_child")

#TO HAVE AN OVERVIEW:
#VIEW 

view(utt)
view(tok)

#COUNT THE AMOUNT OF UTTERANCES PER MONTH

utt_count<-utt%>%
  mutate(age=floor(target_child_age))%>%
  group_by(age)%>%
  count()

ggplot()+
  geom_smooth(utt_count, mapping=aes(x=age, y=n))


# 1. get measure of linguistic development

# mean length of utterance (MLU): ratio of morphemes over utterances
mlu <- utt %>% 
  group_by(transcript_id) %>% 
  summarize(mlu = sum(num_morphemes, na.rm = TRUE)/n(),
            age = mean(target_child_age))
# MLU is a measure of linguistic productivity in children.
# A higher MLU is taken to indicate a higher level of language proficiency.
# Mean length of utterance is a good marker of language impairment. 
# It can be used to benchmark language acquisition and is used to compare language intervention outcomes in children with autism.
# (wikipedia)

ggplot(mlu, aes(age, mlu)) +
  geom_point() +
  theme_light()

# 2. find omitted subjects in a subset of utterances

# source uses only most frequent word per transcript because analysis is manual; I use all verbs

# keep only declaratives and questions with verbs in it:
utt_sub <- utt %>% 
  filter(str_detect(part_of_speech, "^(v|.* v)"),
         type %in% c("declarative", "question"))

# find subset of utt_sub where verb appears with a subject (allowing modals or auxiliaries in between)
utt_sub <- utt_sub %>% 
  mutate(subject = str_detect(part_of_speech, "(pro:sub|n)( mod)?( aux)? v"))

# get fraction of utterances with proper subjects per transcript

subjects <- utt_sub %>% 
  group_by(transcript_id) %>% 
  summarize(subject = sum(subject)/n())

# 3. check correlation between two measures of language development:
# MLU and use of proper subjects

full_join(mlu, subjects) %>% 
  ggplot(aes(mlu, subject)) + 
  geom_point() +
  theme_light()

# 4. effect of wh-questions on using subjects

# repeat the same analysis as part 2-3 but filter for utterances that include a wh-question

wh_sub <- utt %>% 
  filter(str_detect(part_of_speech, "^(v|.* v)"),
         str_detect(part_of_speech, "pro:int"),
         str_detect(gloss, "wh"),
         type %in% c("declarative", "question"))

wh_sub <- wh_sub %>% 
  mutate(subject = str_detect(part_of_speech, "(pro:sub|n)( mod)?( aux)? v"))

wh_subjects <- wh_sub %>% 
  group_by(transcript_id) %>% 
  # filter(n() >= 5) %>% 
  summarize(wh_subject = sum(subject)/n())

# correlation between wh-question subject use rate and MLU

full_join(mlu, wh_subjects) %>% 
  ggplot(aes(mlu, wh_subject)) + 
  geom_point() +
  theme_light()

# compare subject use for all utterances and utterances with wh-questions

full_join(subjects, wh_subjects) %>% 
  ggplot(aes(subject, wh_subject)) +
  geom_point() +
  theme_light()

# much more variation in wh-questions, likely due to small samples per transcript
# if transcripts with <5 wh-utterances are excluded, there is a weak positive relationship 
# overall subject use rate is a better measure of linguistic development based on MLU



#FIND A COMMON MISTAKE AND VISUALIZE IT OVER TIME

#While browsing through the data set I realized that a common mistake was the lack of the verb "to be" between the subject and the verb "going to/gonna".
#An example would be I gonna play.

#Therefore I visualized it over time

#USAGE OF "TO BE" IN FRONT OF "GOING TO":

utt_going<-utt%>%
  mutate(age=floor(target_child_age))%>%
  filter(type=="declarative")%>%
  filter(str_detect(gloss, "going to|gonna"))%>%
  mutate(preverb=str_count(gloss, "\\'re|\\'s|\\'m|are|is|am|am not|is not|are not|\\'m not|\\'s not|\\'re not(?=[:blank:]going to)
                           |\\'re|\\'m|\\'s|are|is|am|am not|is not|are not|\\'m not|\\'s not|\\'re not(?=[:blank:]gonna)"))%>%
  filter(preverb<2)%>%
  full_join(utt_count, by="age")%>%
  mutate(pre_div_gloss_per_month=preverb/n)

#ABSOLUTE VALUES 

ggplot()+
  geom_smooth(data=utt_going,mapping = aes(x=age, y=preverb))

#VALUES OVER RATIO 

ggplot()+
  geom_smooth(data=utt_going,mapping = aes(x=age, y=pre_div_gloss_per_month))




#ENGLISH RULES


#Prepositions give more precise details over the action but require more words and thinking.
#faculty.washington.edu/cicero/370syntax.htm
#Therefore I analyzed the amount of prepositions used in utterances per age.

#PREPOSITION USAGE PER AGE
utt%>%
  mutate(age=floor(target_child_age))%>%
  mutate(prep=str_count(part_of_speech, "prep"))%>%
  ggplot()+
  geom_smooth(mapping=aes(x=age,  y= prep))

#PREPOSITION USAGE PER MLU

utt%>%
  full_join(mlu)%>%
  mutate(age=floor(target_child_age))%>%
  mutate(prep=str_count(part_of_speech, "prep"))%>%
  ggplot()+
  geom_smooth(mapping=aes(x=mlu,  y= prep))

#The curve looks very much like a correlation. 
#Let's check it: 

prep<-utt%>%
  full_join(mlu)%>%
  mutate(age=floor(target_child_age))%>%
  mutate(prep=str_count(part_of_speech, "prep"))

cor(prep$prep, prep$mlu)

#Conclusion: It is not a convincing correlation.




#QUESTIONS WITH ACADEMIC BACKGROUND


#The older the child gets, the longer the utterances are.
#Source:
#link: https://pubs.asha.org/doi/abs/10.1044/0161-1461%282003/027%29
#doi: https://doi.org/10.1044/0161-1461(2003/027)

# To compute this theory I thought what does more words in a string mean? One answer is more spaces between the words.
# Therefore I counted the amount of spaces per utterance and added one to get the amount of words.


#AMOUNT OF WORDS PER UTTERANCE OVER TIME
utt%>%
  mutate(age=floor(target_child_age))%>%
  mutate(spaces=str_count(gloss, " ")+1)%>%
  ggplot()+
  geom_smooth(mapping=aes(x=age, y=spaces))

#To check if my spaces idea works I can compare it to another graph using the variable "num_tokens" counting the number of tokens per utterance.

#AMOUNT OF WORDS PER UTTERANCE OVER TIME USING NUM_TOKENS
utt%>%
  mutate(age=floor(target_child_age))%>%
  ggplot()+
  geom_smooth(mapping=aes(x=age, y=num_tokens))


#It is the same graph!


#Apparently, in the first, "receptive" stage, the child says far fewer nouns than he or she understands and does not give any verbs although he or she understands many. 
#Then, there is a coordination of comprehension/production and this is the "productive" stage where the child says almost all the nouns he understands and uses his first verbs.


#Source: https://www.sciencedirect.com/science/article/pii/0010027776900044
#doi: https://doi.org/10.1016/0010-0277(76)90004-4

utt_v_n<-utt%>%
  mutate(age=floor(target_child_age))%>%
  mutate(nouns=str_count(part_of_speech, "n[:blank:]|[:blank:]n[:blank:]|n:|[:blank:]n|^n|n$"))%>%
  mutate(verbs=str_count(part_of_speech, "(?<!ad)v[:blank:]|[:blank:]v[:blank:]|(?<!ad)v:|[:blank:]v|^v|(?<!ad)v$"))


ggplot(data=utt_v_n)+
  geom_smooth(mapping = aes(x=age, y=nouns))

ggplot(data=utt_v_n)+
  geom_smooth(mapping = aes(x=age, y=verbs))

ggplot(data=utt_v_n)+
  geom_smooth(mapping = aes(x=age, y=nouns))+
  geom_smooth(mapping = aes(x=age, y=verbs))


