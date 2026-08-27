 library(readxl)

 dados_dir<-'~/GitHub/CEnPE/Piaui/2026/Agosto/Dados'
 
 
 
 df_4ª_SIMULA_2ª_SÉRIE <- read_excel(file.path(dados_dir,'df - 2ª SÉRIE.xlsx'))
 df_4ª_SIMULA_3ª_SÉRIE <- read_excel(file.path(dados_dir,'df - 3ª SÉRIE.xlsx'))
 
 
 saveRDS(df_4ª_SIMULA_2ª_SÉRIE,file.path(dados_dir,'df_4ª_SIMULA_2ª_SÉRIE.RDS'))
 saveRDS(df_4ª_SIMULA_3ª_SÉRIE,file.path(dados_dir,'df_4ª_SIMULA_3ª_SÉRIE.RDS'))
 
 