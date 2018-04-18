vp <- vp %>% 
  mutate(white = recode(race_baseline, "1=1; else=0"))

## Baptist

vp <- vp %>%
  mutate(sbc_baseline = recode(vp$religpew_baptist_baseline, "1=1; else=0")) %>% 
  mutate(sbc_baseline = white + sbc_baseline) %>% 
  mutate(sbc_baseline = recode(sbc_baseline, "2=1; else=0"))

vp <- vp %>%
  mutate(abc_baseline = recode(vp$religpew_baptist_baseline, "2=1; else=0")) %>% 
  mutate(abc_baseline = white + abc_baseline) %>% 
  mutate(abc_baseline = recode(abc_baseline, "2=1; else=0"))

vp <- vp %>%
  mutate(ibc_baseline = recode(vp$religpew_baptist_baseline, "5=1; else=0")) 

vp <- vp %>%
  mutate(bgc_baseline = recode(vp$religpew_baptist_baseline, "6=1; else=0")) 

vp <- vp %>%
  mutate(mbc_baseline = recode(vp$religpew_baptist_baseline, "7=1; else=0")) %>% 
  mutate(mbc_baseline = white + mbc_baseline) %>% 
  mutate(mbc_baseline = recode(mbc_baseline, "2=1; else=0"))

vp <- vp %>%
  mutate(cb_baseline = recode(vp$religpew_baptist_baseline, "8=1; else=0")) 

vp <- vp %>%
  mutate(fwb_baseline = recode(vp$religpew_baptist_baseline, "9=1; else=0")) 

vp <- vp %>%
  mutate(gabb_baseline = recode(vp$religpew_baptist_baseline, "10=1; else=0")) 

vp <- vp %>%
  mutate(obc_baseline = recode(vp$religpew_baptist_baseline, "90=1; else=0")) %>% 
  mutate(obc_baseline = white + obc_baseline) %>% 
  mutate(obc_baseline = recode(obc_baseline, "2=1; else=0"))

vp <- vp %>% 
  mutate(evanbap_baseline = sbc_baseline + abc_baseline + ibc_baseline + bgc_baseline + mbc_baseline + cb_baseline + fwb_baseline + gabb_baseline + obc_baseline)

## Methodist
vp <- vp %>%
  mutate(fmc_baseline = recode(vp$religpew_methodist_baseline, "2=1; else=0")) 

vp <- vp %>%
  mutate(omc_baseline = recode(vp$religpew_methodist_baseline, "90=1; else=0")) %>% 
  mutate(omc_baseline = white + omc_baseline) %>% 
  mutate(omc_baseline = recode(omc_baseline, "2=1; else=0"))

vp <- vp %>% 
  mutate(evanmeth_baseline = fmc_baseline + omc_baseline)

##Non-Denom

vp <- vp %>% 
  mutate(hiatt_baseline = recode(pew_churatd_baseline, "1:3=1; else=0")) %>% 
  mutate(nd_baseline = recode(religpew_nondenom_baseline, "1:90=1; else=0")) %>% 
  mutate(evannd_baseline = nd_baseline + hiatt_baseline) %>% 
  mutate(evannd_baseline =  recode(evannd_baseline, "2=1; else=0"))

## Lutheran 

vp <- vp %>% 
  mutate(mz_baseline = recode(religpew_lutheran_baseline, "2=1; else=0")) %>% 
  mutate(wi_baseline = recode(religpew_lutheran_baseline, "3=1; else=0")) %>% 
  mutate(evanluth_baseline = mz_baseline + wi_baseline)

## Presbyterian

vp <- vp %>% 
  mutate(pca_baseline = recode(religpew_presby_baseline, "2=1; else=0")) %>% 
  mutate(epc_baseline = recode(religpew_presby_baseline, "6=1; else=0")) %>% 
  mutate(evanpres_baseline = pca_baseline + epc_baseline)

## Pentecostal 

vp <- vp %>% 
  mutate(evanpent_baseline = recode(religpew_pentecost_baseline, "1:90 =1; else=0"))

## Episcopal 
## None

## Congregregational

vp <- vp %>% 
  mutate(evancong_baseline = recode(religpew_congreg_baseline, "2=1; else=0"))

## Holiness
vp <- vp %>% 
  mutate(evanholy_baseline = recode(religpew_holiness_baseline, "1:90 =1; else=0"))

## Advent
## None 

## Totaling Up

vp <- vp %>% 
  mutate(evangelical_baseline = evanbap_baseline + evanmeth_baseline + evannd_baseline + evanluth_baseline + evanpres_baseline + evanpent_baseline + evancong_baseline + evanholy_baseline) %>% 
  mutate(evangelical_baseline = recode(evangelical_baseline, "1:4=1; else=0"))