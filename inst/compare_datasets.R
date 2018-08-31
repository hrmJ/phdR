#Checking differences between datasets:

x <- rbind(fi,ru)
x  %>% filter(lang=="ru") %>% count(group, corpustype)  %>% rename(small.n=n)  %>% 
    left_join(
              d  %>% filter(lang=="ru") %>% count(group, corpustype)  %>% rename(big.n=n),
              by=c("group","corpustype")) %>% 
    mutate(dif=big.n-small.n) %>% 
    mutate(rel.dif=round(small.n/big.n*100,2)) %>% 
    #arrange(desc(dif)) -> differences
    arrange(rel.dif) -> differences
#GetD(rbind(fi,ru))


x  %>% filter(lang=="ru",group=="E6b")  %>% sample_n(9) %>% pull(sent)

d2 <- readRDS("~/phd_data/data/vaihe1df.rds")
d2 <- as_tibble(d2)
d2  %>% filter(lang=="ru",group=="E3b")  %>% sample_n(9) %>% pull(sent)
x  %>% filter(lang=="ru",group=="E3b")  %>% sample_n(9) %>% pull(sent)

d2  %>% filter(lang=="ru",group=="E6b")  %>% sample_n(9) %>% pull(sent)

x %>% filter(lang=="ru",corpustype=="araneum",group=="L9d") %>% sample_n(20)  %>% pull(sent)  
d2 %>% filter(lang=="ru",corpustype=="araneum",group=="L9d") %>% sample_n(20)  %>% pull(sent)  

x %>% filter(lang=="ru",corpustype=="araneum") %>% xtabs(~location,.) %>% prop.table(.)
d2 %>% filter(lang=="ru",corpustype=="araneum") %>% xtabs(~location,.) %>% prop.table(.)
