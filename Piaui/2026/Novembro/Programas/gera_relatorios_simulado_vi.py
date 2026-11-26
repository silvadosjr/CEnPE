"""Relatórios S6 com o padrão dos simulados anteriores e dados de novembro."""
from pathlib import Path
import importlib.util
import json
import hashlib
from zipfile import ZipFile

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location('s4', ROOT.parent/'Agosto/Programas/gera_relatorios_simulado_iv.py')
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)
source = module.source
for old,new in [('Simulado IV','Simulado VI'),('SIMULADO IV','SIMULADO VI'),('Simulado_IV','Simulado_VI'),('_S4','_S6'),('pasta Agosto','pasta Novembro')]:
    source = source.replace(old,new)
source = source.replace('ROOT.parent / "Julho/Relatorios/2EM_LP/Relatorio_Tecnico_2EM_LP_S3_TCM_TRI.docx"', 'ROOT.parent / f"Setembro/Relatorios/{profile}/Relatorio_Tecnico_{profile}_S5_TCM_TRI.docx"')
source = source.replace('run.text.replace("SIMULADO III", "SIMULADO VI").replace("2EM-LP", profile.replace("_", "-"))','run.text.replace("SIMULADO V", "SIMULADO VI")')
source = source.replace('date.today()', '(date(2026, 11, 26) if profile == "2EM_MT" else date(2026, 11, 24))')
source = source.replace('    doc.add_heading("1. Síntese executiva", level=1)', '    if profile == "2EM_MT":\n        exclusion_text = "25 (TCM e TRI)"\n    doc.add_heading("1. Síntese executiva", level=1)')
source = source.replace('"Exclusão TRI"', '"Exclusão TCM/TRI"')
source = source.replace('    doc.add_heading("Parâmetros dos itens", level=2)', '    doc.add_heading("Parâmetros dos itens", level=2).paragraph_format.page_break_before = (profile == "2EM_MT")')
# Representar valores negativos na mesma escala das barras positivas.
source = source.replace('        if shade:\n', '        vmin = min(0.0, min(values) * 1.5)\n        def yvalue(v):\n            return bottom - (float(v)-vmin)/(vmax-vmin)*(bottom-top)\n        zero = yvalue(0)\n        if shade:\n',1)
source = source.replace('bottom - (hi / vmax) * (bottom - top)', 'yvalue(hi)').replace('bottom - (lo / vmax) * (bottom - top)', 'yvalue(lo)')
source = source.replace('value = vmax * tick / 4\n            yy = bottom - (value / vmax) * (bottom - top)', 'value = vmin + (vmax-vmin) * tick / 4\n            yy = yvalue(value)',1)
source = source.replace('bottom - (reference / vmax) * (bottom - top)', 'yvalue(reference)')
source = source.replace('yy = bottom - (max(value, 0.0) / vmax) * (bottom - top)', 'yy = yvalue(value)')
source = source.replace('draw.rectangle((cx - bw / 2, yy, cx + bw / 2, bottom), fill=fill)', 'draw.rectangle((cx - bw / 2, yy, cx + bw / 2, zero), fill=fill)',1)
source = source.replace('draw.rectangle((cx - bw / 2, bottom, cx + bw / 2, bottom + 12), fill=fill)', 'draw.rectangle((cx - bw / 2, zero, cx + bw / 2, yy), fill=fill)')
source = source.replace('"Informações gerais.odt, com as ressalvas técnicas registradas pela equipe."', '"Dados e gabaritos do 6º simulado; auditoria de entrada em Relatorios/qa/auditoria_entrada.json. Não foi fornecido documento de informações gerais."')
source = source.replace('MedidasTCM_Item.xlsx e Informações gerais.odt.', 'MedidasTCM_Item.xlsx e auditoria dos dados e gabaritos do Simulado VI.')
source = source.replace('"IC 95% da média"', '"IC 95% média populacional"')
source = source.replace('O intervalo de confiança é reproduzido do arquivo de resultados; não representa o erro individual de proficiência.', 'O intervalo de confiança refere-se à média populacional estimada em ResumoThetaIC, cujo estimador difere da média individual EAP em ResumoTheta. Não representa erro individual de proficiência; o DP EAP também difere do desvio-padrão populacional estimado.')
source = source.replace('    doc.add_heading("3. Resultados da TCM", level=1)', '''    doc.add_paragraph("Os gabaritos conferem com a base. Incluíram-se apenas avaliados (fl_avaliado = 1), sem matrícula duplicada. " + ("Excluiu-se o item 25 da TCM e da TRI; mantiveram-se 25 itens e a numeração original. O item 6 foi liberado para estimação de a, b e c, com dez âncoras. " if profile == "2EM_MT" else "Mantiveram-se os 26 itens e as chaves fornecidas. ") + "As âncoras usam aSAEB, bSAEB e c; diferenças de arredondamento estão na auditoria de entrada.")
    doc.add_heading("3. Resultados da TCM", level=1)''')
source = source.replace('    doc.add_heading("Parâmetros dos itens", level=2).paragraph_format.page_break_before = (profile == "2EM_MT")', '''    doc.add_heading("Parâmetros dos itens", level=2).paragraph_format.page_break_before = (profile == "2EM_MT")
    if profile == "2EM_MT":
        item6 = items.loc[items.Item == 6].iloc[0]
        doc.add_paragraph(f"O item 6 foi mantido na prova e teve os três parâmetros liberados para estimação: a = {fmt_num(item6.a, 3)}, b na escala 250 e 50 = {fmt_num(item6.bSAEB, 1)} e c = {fmt_num(item6.c, 3)}. Seus valores do gabarito deixaram de ser fixados. Permanecem como âncoras os itens 7, 8, 13, 14, 15, 16, 17, 18, 19 e 20. A mudança requer acompanhamento da estabilidade da ligação entre aplicações.")''')
n = {'__file__':__file__, '__name__':'s6_components'}
exec(compile(source,__file__,'exec'),n)
n['PROFILES'] = ['2EM_LP','2EM_MT']
n['PROFILE_NOTES'] = {
 '2EM_LP': {
  'lead':'Leitura principal. O Simulado VI de Língua Portuguesa da 2ª série manteve os 26 itens nas análises TCM e TRI, com seis itens de ancoragem. Nenhum item apresentou discriminação ou correlação ponto-bisserial abaixo de 0,20. Os itens 19, 25 e 26 apresentam as menores proporções de acertos e requerem acompanhamento.',
  'diagnostic_items':[], 'status':{},
  'narrative':'As proporções de acertos variaram de 24,6% no item 19 a 85,4% no item 1. Os itens 19, 25 e 26 tiveram respectivamente 24,6%, 29,3% e 24,8% de acertos. Suas correlações ponto-bisseriais foram 0,213, 0,248 e 0,221, acima do limiar diagnóstico de 0,20. Nenhuma exclusão ou alteração de chave foi aplicada. A ausência de indicadores abaixo do limiar não substitui a análise TRI e a revisão pedagógica.',
  'recommendations':['Examinar os itens 19, 25 e 26 quanto à dificuldade, clareza e adequação ao conteúdo trabalhado.','Examinar os parâmetros TRI e as curvas dos itens antes de reutilizá-los.','Planejar intervenções a partir dos conteúdos e da distribuição de proficiência, sem atribuir níveis pedagógicos a faixas não validadas.']},
 '2EM_MT': {
  'lead':'Leitura principal. O Simulado VI de Matemática da 2ª série foi reprocessado sem o item 25 na TCM e na TRI, por solicitação em 26/11/2026. As análises utilizam 25 itens, com dez itens de ancoragem e 28.727 avaliados. O item 6 foi liberado para estimação de a, b e c em 26/11/2026; as proficiências foram reestimadas com dez âncoras. A TCM permanece calculada sobre os mesmos 25 itens.',
  'diagnostic_items':[], 'status':{},
  'narrative':'O item 25 foi retirado antes da correção e da estimação, preservando-se os números 1 a 24 e 26 para os itens válidos. A exclusão não atribui ponto ao item e não altera as chaves dos demais itens. Os indicadores TCM, incluindo discriminação por grupos e correlações, foram recalculados com o escore de 25 itens. Nenhum item válido apresentou discriminação ou correlação ponto-bisserial abaixo de 0,20. As proporções de acertos variaram de 26,5% no item 7 a 51,3% no item 9. O item excluído não integra os gráficos, parâmetros nem apêndices desta versão. O diagnóstico anterior indicava possível conflito de gabarito; sua resolução pedagógica permanece necessária antes de qualquer reutilização. A versão com o item 25 foi preservada na pasta Historico/2026-11-24_MT_com_item25.',
  'recommendations':['Usar o denominador de 25 itens para interpretar os escores e percentuais desta versão e manter documentada a exclusão do item 25 da TCM e da TRI.','Revisar o enunciado, a resolução e o gabarito do item 25 antes de qualquer reutilização. A exclusão desta análise não equivale à troca de sua chave.','Examinar os parâmetros e as curvas dos itens válidos e a cobertura de dificuldade, sobretudo entre os dez itens fixados. Documentar a liberação do item 6 e acompanhar seus parâmetros estimados e a estabilidade da ligação à escala.','Evitar comparar diretamente as médias das duas disciplinas como medidas equivalentes de desempenho.']}
}

def audit_results():
    np = n['np']
    audit = {}
    for profile in n['PROFILES']:
        d = n['read_profile'](profile)
        scores,theta,m,items,dist = [d[k] for k in ['scores','base_theta','measures','items','dist']]
        assert len(scores)==len(theta)==int(dist['N de alunos'].sum())==28727
        assert not scores.matricula.duplicated().any()
        assert scores.matricula.astype(str).tolist()==theta.matricula.astype(str).tolist()
        assert np.isclose(scores.iloc[:,1].mean(),m.Dificuldade.sum())
        assert np.isclose(theta.Theta.mean(),d['theta'].loc['media'],atol=.001)
        assert np.isclose(theta.Theta.std(),d['theta'].loc['dp'],atol=.001)
        expected = set(range(1,27)) - ({25} if profile == '2EM_MT' else set())
        assert set(m.Item)==set(items.Item)==expected
        assert scores.iloc[:,1].between(0,len(expected)).all()
        if profile == '2EM_MT':
            assert 'rpa_025' not in theta.columns
            prior_root = ROOT/'Historico/2026-11-26_MT_sem25_item6_fixo'
            prior_scores=n['pd'].read_excel(prior_root/'ResultadosTCM_2EM_MT/Simulado_VI/EscoresBrutos_Aluno.xlsx')
            assert scores.equals(prior_scores)
            assert items.loc[items.Item==6,'SAEB'].iloc[0] != 'Sim'
        assert np.allclose(items.bSAEB,250+50*items.b)
        info=n['pd'].read_excel(next((ROOT/'Dados').glob(f'gabarito*{profile[-2:].lower()}*')))
        known=info.iloc[:,-1].notna()
        if profile == '2EM_MT':
            known = known & (info.Item != 6)
        fixed=items[items.SAEB=='Sim']
        assert fixed.Item.tolist()==info.loc[known,'Item'].tolist()
        assert np.allclose(fixed.a,info.loc[known,'aSAEB']*50)
        assert np.allclose(fixed.bSAEB,info.loc[known,'bSAEB'])
        assert np.allclose(fixed.c,info.loc[known,'c'])
        audit[profile]={'N':len(scores),'media_escore':float(scores.iloc[:,1].mean()),'media_EAP':float(theta.Theta.mean()),'DP_EAP':float(theta.Theta.std()),'TCM':len(m),'TRI':len(items),'ancoras':fixed.Item.tolist(),'itens_TCM_sinalizados':m.loc[(m.Discriminacao<.2)|(m.CPBisserial<.2),'Item'].tolist(),'itens_excluidos':[25] if profile=='2EM_MT' else [],'itens_liberados':[6] if profile=='2EM_MT' else [],'data_processamento':'2026-11-26' if profile=='2EM_MT' else '2026-11-24'}
    (ROOT/'Relatorios/qa/auditoria_resultados.json').write_text(json.dumps(audit,ensure_ascii=False,indent=2),encoding='utf-8')
    return audit

if __name__=='__main__':
    print(json.dumps(audit_results(),ensure_ascii=False,indent=2))
    n['main']()
