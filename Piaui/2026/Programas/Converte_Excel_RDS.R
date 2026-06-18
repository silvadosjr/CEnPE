 library(readxl)

 df_1ª_SIMULA_2ª_SÉRIE <- read_excel("Piaui/2026/Dados/df 1ª SIMULA 2ª SÉRIE.xlsx")
 df_1ª_SIMULA_3ª_SÉRIE <- read_excel("Piaui/2026/Dados/df 1ª SIMULA 3ª SÉRIE.xlsx")
 df_2ª_SIMULA_2ª_SÉRIE <- read_excel("Piaui/2026/Dados/df 2ª SIMULA 2ª SÉRIE.xlsx")
 df_2ª_SIMULA_3ª_SÉRIE <- read_excel("Piaui/2026/Dados/df 2ª SIMULA 3ª SÉRIE.xlsx")
 
 
 saveRDS(df_1ª_SIMULA_2ª_SÉRIE,file.path(dados_dir,'df_1ª_SIMULA_2ª_SÉRIE.RDS'))
 saveRDS(df_1ª_SIMULA_3ª_SÉRIE,file.path(dados_dir,'df_1ª_SIMULA_3ª_SÉRIE.RDS'))
 saveRDS(df_2ª_SIMULA_2ª_SÉRIE,file.path(dados_dir,'df_2ª_SIMULA_2ª_SÉRIE.RDS'))
 saveRDS(df_2ª_SIMULA_3ª_SÉRIE,file.path(dados_dir,'df_2ª_SIMULA_3ª_SÉRIE.RDS'))
 
 