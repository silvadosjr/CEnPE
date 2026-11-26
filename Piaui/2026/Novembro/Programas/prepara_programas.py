from pathlib import Path

root = Path(__file__).resolve().parents[1]
for profile in ['LP', 'MT']:
    for prefix in ['TCM', 'Fit_3PL_Fix']:
        name = f'{prefix}_2EM_{profile}_S5.R'
        text = (root.parent / 'Setembro/Programas' / name).read_text(encoding='utf-8-sig')
        for old,new in [('Setembro','Novembro'),('Simulado_V','Simulado_VI'),('_S5','_S6'),('5ª','6ª')]:
            text = text.replace(old,new)
        text = text.replace("setwd('~/GitHub/CEnPE')", "setwd('C:/Users/Usuário/OneDrive/Documentos/GitHub/CEnPE')")
        text = text.replace('novos_pacotes <- pacotes[!(pacotes %in% installed.packages()[, "Package"])]', 'novos_pacotes <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly=TRUE)]')
        text = text.replace(f"sheet = '{profile}_2EM_S6'", "sheet = 'Sheet1'")
        text = text.replace('v_item_conhecido <- !is.na(InfoItens$Conhecido)', '''names(InfoItens)[toupper(names(InfoItens)) == 'CONHECIDO'] <- 'Conhecido'
stopifnot(identical(as.integer(InfoItens$Item), as.integer(substring(auxnomesitens,5))))
v_item_conhecido <- !is.na(InfoItens$Conhecido) & nzchar(trimws(InfoItens$Conhecido))''')
        text = text.replace("InfoItens <- InfoItens %>% filter(Conhecido=='Sim')", "InfoItens <- InfoItens[v_item_conhecido, ]")
        if profile == 'MT':
            text = text.replace('# 2.4) Extrai gabarito:', "# Exclusão do item 25 da TCM e da TRI por solicitação em 26/11/2026.\nbase <- base %>% dplyr::select(-dplyr::all_of(c('rpa_025', 'rp_025')))\n\n# 2.4) Extrai gabarito:")
            text = text.replace("names(InfoItens)[toupper(names(InfoItens))", "InfoItens <- InfoItens[InfoItens$Item != 25, ]\nnames(InfoItens)[toupper(names(InfoItens))")
        if prefix == 'Fit_3PL_Fix':
            if profile == 'MT':
                text = text.replace('InfoItens <- InfoItens[v_item_conhecido, ]', '# Item 6 liberado para estimação por solicitação em 26/11/2026.\nv_item_conhecido <- v_item_conhecido & InfoItens$Item != 6\nInfoItens <- InfoItens[v_item_conhecido, ]')
            text = 'set.seed(20261124)\n' + text
            text += '''
# Registro da estimação para auditoria e reprodutibilidade.
stopifnot(isTRUE(resultML3P@OptimInfo$converged))
saveRDS(resultML3P, file.path(result_dir, 'modelo_3PL.rds'))
capture.output(resultML3P@OptimInfo, file=file.path(result_dir,'convergencia.txt'))
capture.output(sessionInfo(), file=file.path(result_dir,'sessionInfo.txt'))
'''
        (root/'Programas'/name.replace('_S5','_S6')).write_text(text,encoding='utf-8')
