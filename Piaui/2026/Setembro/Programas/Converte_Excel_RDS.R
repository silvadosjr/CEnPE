 library(readxl)

 dados_dir<-'~/GitHub/CEnPE/Piaui/2026/Setembro/Dados'
 
 
 
 df_5ª_SIMULA_2ª_SÉRIE <- read_excel(file.path(dados_dir,'df - 2ª SÉRIE 5º SIMULA.xlsx'))
 
 
 saveRDS(df_5ª_SIMULA_2ª_SÉRIE,file.path(dados_dir,'df_5ª_SIMULA_2ª_SÉRIE.RDS'))

 
 