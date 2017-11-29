names(data) [1:1000]
names(data) [1001:2000]
names(data) [2001:3000]
names(data) [3001:4000]
names(data) [4001:5000]




data_sub <- 
  data %>% 
  select(id,
         leftright_self4,
         vote6_HolSarMacBay,
         vote8_HolJup,
         vote9,
         vote10,
         vote11,
         vote11bis,vote12,vote12bis,vote13)

data_sub %>% names()
