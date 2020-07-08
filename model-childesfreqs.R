# without loss of generality parameters (fixed):
n_learners = 100
max_age = 48 # months

wf = read.csv("data/childes_english_word_freq_cleaned.csv")
pos = read.csv("data/POSEnglish.csv", sep=';', stringsAsFactors = F)
pos2<-pos
pos2$PoS <- as.factor(pos2$PoS)
pos2$word <- as.factor(pos2$word)
#pos2$word <- as.factor(pos2$word)
wf = pos2 %>% # SUBTLWF = frequency per million words
  right_join(wf, by="word")
# indices of CDI items
cdi_idx = which(wf$on_cdi==1)
noun_idx = which(wf$PoS=="Noun")
other_idx = which(wf$PoS=="Other")
verb_idx = which(wf$PoS=="Verb")
adj_idx = which(wf$PoS=="Adjective")
vocab_size = nrow(wf) # 10190
waking_hours_per_day = 12 # eventually make normally-distributed (if we can find literature on sleeping time)
#cdi_list = wf%>%filter(on_cdi==1)%>%select(word)


acceleration_test <- function(dat) {
  d = dat$known_words %>% filter(month>11 & month<25) %>% group_by(month) %>% summarize(mean=mean(words)) 
  return( mean(diff(diff(d$mean))) )
}

#library(profvis)
#profvis({

make_long_df <- function(df, n_learners, max_age) {
  #df = cbind(rep(zeroval,n_learners), df) # value at 0 mos
  df = data.frame(df)
  names(df) = 1:max_age
  df$id = 1:nrow(df)
  df_l = gather(df, "month", "words", 1:max_age) 
  df_l$month = as.numeric(as.character(df_l$month))
  return(df_l)
}


# eventually: what happens when you have a larger vocab (60k) but are only testing 500 (CDI sim)
# test for acceleration across all param values
# consolidate learning rate and threshold ?
#  variability in # effective instances to learn


# function(vocab_size=10000, distro, input_rate, input_rate_sd, n_learners=100, threshold, 
#  max_age=48, mean_learning_rate, learning_rate_sd, threshold_sd, 
#  proc_facilitates, proc_speed_dev, proc_speed_dev_sd) {

# call this once whenever proc_speed parms change?
get_proc_speed <- function(print_plot=F) {
  a = rnorm(n_learners, mean=.56, sd=.1) # individual adult asymptotes for proc speed
  c = rnorm(n_learners, mean=.72, sd=.1)
  b = 1.04
  proc_speed = matrix(0, nrow=n_learners, ncol=max_age)
  #proc_speed = apply(get_proc_speed)
  for(t in 1:max_age) {
    proc_speed[,t] = a + b*exp(-c*log(t+1)) #/ vocab_size 
  }

  if(print_plot) {
    proc_speed_long = make_long_df(proc_speed, n_learners, max_age)
    gg <- ggplot(proc_speed_long, aes(x=month, y=words)) + 
      geom_line(aes(group=id), alpha=.1) +  
      labs(colour="Quantile") + geom_smooth() + 
      xlab("Age (months)") + ylab("Response Time (seconds)") 
    print(gg)
  }
  
  return(proc_speed)
}

#get_proc_speed(print_plot=T)


# need to add proc_speed term and proc_facilitates interaction term
simulate <- function(parms) {
  if(parms$distro=="uniform") {
    probs = rep(1/vocab_size, vocab_size)
  } else if(parms$distro=="zipf") {
    probs = wf$word_count / sum(wf$word_count) # based on CHILDES WF distro
  } else if(parms$distro=="logzipf") {
    probs = log(wf$word_count) / sum(log(wf$word_count))
  } else {
    print("error")
  }
  start_age = parms$start_age# age (months) at which words start accumulating 
  input_rate = rnorm(n_learners, mean=parms$input_rate, sd=parms$input_rate_sd) # per-child variability in input rate
  input_rate[which(input_rate<10)] = 10 # minimum 10 words/hour..
  #learning_rate = rnorm(n_learners, mean=mean_learning_rate, sd=learning_rate_sd) # individual learning rates
  #learning_rate[which(learning_rate<.1)] = 0.1 # truncate
  a = rnorm(n_learners, mean=parms$mean_learning_rate, sd=parms$learning_rate_sd) # individual adult asymptotes for proc speed
  c = rnorm(n_learners, mean=parms$proc_speed_dev, sd=parms$proc_speed_dev_sd) # individual rate of processing speed development
  # changing learning_rate to be adult processing speed asymptote
  a[which(a<.01)] = 0.01 # truncate
  c[which(c<.01)] = 0.01 # truncate
  # maybe beta-distributed?
  
  # fixed threshold for all words, or normal distribution over word difficulty (like McMurray 2007)
  #if(threshold_varies) threshold = rnorm(vocab_size, mean=threshold, sd=20)
  threshold = rnorm(vocab_size, mean=parms$threshold, sd=parms$threshold_sd)
  threshold[which(threshold<1)] = 1 # minimum 1 occurrence to learn
  # child x word
  cumulative_word_occs = matrix(0, nrow=n_learners, ncol=vocab_size) # number of times each word has appeared per learner
  colnames(cumulative_word_occs) = wf$word
  # child x age
  known_words = matrix(0, nrow=n_learners, ncol=max_age) # known words per individual (row) per month (col)
  known_cdi_words = matrix(0, nrow=n_learners, ncol=max_age) # just the CDI words
  known_other = matrix(0, nrow=n_learners, ncol=max_age) # known other words
  known_adj = matrix(0, nrow=n_learners, ncol=max_age) # known adjectives
  known_verb = matrix(0, nrow=n_learners, ncol=max_age) # known verbs
  known_noun = matrix(0, nrow=n_learners, ncol=max_age) # known nouns
  proc_speed = matrix(0, nrow=n_learners, ncol=max_age) # do we want per word instead of per learner?
  
  # word x age: proportion of children knowing each word per age
  prop_knowing_word = matrix(0, nrow=vocab_size, ncol=max_age)
  rownames(prop_knowing_word) = wf$word

  # processing speed - each of these parameters could have individual variability
  # Kail (1991): RT slopes follow an exponential, such that Y(i) = a + b*exp(−c*i), where Y=predicted var, i=age (mos? year?). 
  #a = 0.56 # eventual (adult) asymptote VARIABILITY HERE
  b = 1.04 # multiplier for the (infant) intercept - SES or preemie VARIABILITY HERE
  #c = proc_speed_dev #rate of development (0.72 in Frank, Lewis, & MacDonald, 2016)
  # age vs. # of vocab terms as leverage for learning rate
  # how to model clinically-important variables, e.g., they tell parents to follow-in (e.g., trains not animals for ASD)
  # can we get a true late-talker by 1) screwing around with input factors, and/or 2) screwing around with child-level variables

  # tokens per month for each child
  tokens_per_mo = round(input_rate*waking_hours_per_day*30.42) # tokens/hour * waking hours/day * days/month)
  for(t in 1:max_age) {
    # expected occurences of each word this month per subject (column)
    
    # children (rows) x words (cols)
    #mo_word_occs = tokens_per_mo %o% probs
    
    # words (rows) x children (cols)
    mo_word_occs = probs %o% tokens_per_mo
    
    #mo_word_occs = matrix(rep(mo_word_occs, n_learners), byrow=F, ncol=n_learners)
    
    #proc_speed[,t] = a + rowSums(b*exp(-c*log(cumulative_word_occs+1))) / vocab_size 
    #proc_speed[,t] = a + rowSums(b*exp(-c*log(cumulative_word_occs+1))) / vocab_size 
    proc_speed[,t] = a + b*exp(-c*log(t+1)) #/ vocab_size 
    
    # learning rate scales value of occurrences
    if(t >= start_age) {
      if(parms$proc_facilitates) { # further scale value of word occurrences by processing speed
        #cumulative_word_occs = cumulative_word_occs + (3-proc_speed[,t])*learning_rate*t(mo_word_occs) 
        cumulative_word_occs = cumulative_word_occs + (1/proc_speed[,t])*t(mo_word_occs) 
      } else {
        #cumulative_word_occs = cumulative_word_occs + learning_rate*t(mo_word_occs) # accumulate occurrences this month
        cumulative_word_occs = cumulative_word_occs + 1*t(mo_word_occs)
      }
    }
    # test thresholding
    #cumulative_word_occs_test = matrix(1:30, nrow=3)
    #threshold_test = rep(5,10)
    #rowSums(cumulative_word_occs_test > threshold_test)
    known_words[,t] = rowSums(cumulative_word_occs>threshold) # does this do colwise comparison??
    known_cdi_words[,t] = rowSums(cumulative_word_occs[,cdi_idx] > threshold[cdi_idx])
    known_verb[,t] = rowSums(cumulative_word_occs[,verb_idx] > threshold[verb_idx])
    known_noun[,t] = rowSums(cumulative_word_occs[,noun_idx] > threshold[noun_idx])
    known_other[,t] = rowSums(cumulative_word_occs[,other_idx] > threshold[other_idx])
    known_adj[,t] = rowSums(cumulative_word_occs[,adj_idx] > threshold[adj_idx])
    prop_knowing_word[,t] = colSums(cumulative_word_occs>threshold) / n_learners
  }
  
  # return known words and mean RT per individual per month 
  # reshape to long
  known_words_l = make_long_df(known_words, n_learners, max_age)
  known_cdi_words_l = make_long_df(known_cdi_words, n_learners, max_age)
  known_words_l$cdi_words = known_cdi_words_l$words
  #known_words_l$noncdi_words = with(known_cdi_words_l, words - cdi_words)
  proc_speed_l = make_long_df(proc_speed, n_learners, max_age)
  cdi_list = wf%>%filter(on_cdi==1)%>%select(word)
  
  # some data wrangling for part of speech
  known_other_l = make_long_df(known_other, n_learners, max_age)
  known_verb_l = make_long_df(known_verb, n_learners, max_age)
  known_adj_l = make_long_df(known_adj, n_learners, max_age)
  known_noun_l = make_long_df(known_noun, n_learners, max_age)
  # for plotting avgPoS
  known_other_s = aggregate(known_other_l$words, by=list(Category=known_other_l$month), FUN=sum) %>% mutate("PoS" = "Other", words = x/100)
  known_verb_s = aggregate(known_verb_l$words, by=list(Category=known_verb_l$month), FUN=sum) %>% mutate("PoS" = "Verb", words = x/100)
  known_adj_s = aggregate(known_adj_l$words, by=list(Category=known_adj_l$month), FUN=sum) %>% mutate("PoS" = "Adjective", words = x/100)
  known_noun_s = aggregate(known_noun_l$words, by=list(Category=known_noun_l$month), FUN=sum) %>% mutate("PoS" = "Noun", words = x/100)
  known_PoS = rbind(known_other_s,known_verb_s,known_adj_s,known_noun_s)
  # for plotting propPos
  known_words_pos = known_words_l %>% arrange(id) %>% mutate("PoS" = "All") %>% select(id, month, words, PoS)
  known_other_a = known_other_l %>% arrange(id) %>% mutate("PoS" = "Other")
  known_verb_a = known_verb_l %>% arrange(id) %>% mutate("PoS" = "Verb")
  known_adj_a = known_adj_l %>% arrange(id) %>% mutate("PoS" = "Adjective")
  known_noun_a = known_noun_l %>% arrange(id) %>% mutate("PoS" = "Noun")
  known_pos = rbind(known_words_pos, known_noun_a, known_adj_a, known_verb_a, known_other_a)
  pos_48 = known_pos %>% filter(month == 48)
  pos_48 = pos_48[rep(seq_len(nrow(pos_48)), each = 48),] %>% rename(twords = words) %>% select(twords)
  known_pos = cbind(known_pos, pos_48) 
  all1 = known_pos %>% filter(PoS == "All")%>% mutate("Proportion" = words/twords) %>% select(Proportion)
  all = rbind(all1,all1,all1,all1,all1)
  known_pos = cbind(known_pos, all)
  known_pos = known_pos %>% filter(PoS !="All")

  return(list(known_words = known_words_l,
              known_cdi_words = known_cdi_words_l,
              known_other = known_other_l,
              known_verb = known_verb_l,
              known_adj = known_adj_l,
              known_noun = known_noun_l,
              known_PoS = known_PoS,
              proc_speed = proc_speed_l, 
              prop_knowing_word = prop_knowing_word,
              known_pos = known_pos,
              cdi_list = cdi_list))
}


# vocab_size, distro, input_rate, n_learners, threshold, max_age, mean_learning_rate, threshold_sd, proc_facilitates, proc_speed_dev)
#sim = simulate(10000, "uniform", 1000, 100, 100, 48, 2, .5, 10, F, .72)
#sim %>% group_by(month) %>% summarise(mean=mean(words), sd=sd(words))

#}) # profiler

ex_parms = list(distro="zipf",
             input_rate = 1000,
             input_rate_sd = 100,
             threshold = 1000,
             threshold_sd = 100,
             mean_learning_rate = .5,
             learning_rate_sd = .1,
             proc_facilitates = T,
             proc_speed_dev = .72, 
             proc_speed_dev_sd = .1,
             start_age = 6 # age at which words start accumulating
  )

start_time = Sys.time()
ex = simulate(ex_parms) 
stop_time = Sys.time()

stop_time - start_time  

# 100 learners: (48 mos)
#  10k words = 1.4s
#  30k words = 3.5 s

# 500 learners:
#  10k words = 4.9 s
#  30k words = 23.9 s
