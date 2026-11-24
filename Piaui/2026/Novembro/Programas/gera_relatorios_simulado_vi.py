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
source = source.replace('date.today()', 'date(2026, 11, 24)')
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
source = source.replace('    doc.add_heading("3. Resultados da TCM", level=1)', '''    doc.add_paragraph("Os gabaritos conferem com as colunas rp da base. Selecionaram-se registros com fl_avaliado = 1, sem matrícula duplicada por disciplina. Mantiveram-se os 26 itens e as chaves fornecidas. A ancoragem usa aSAEB, bSAEB e c; diferenças com as colunas a e b constam da auditoria de entrada.")
    doc.add_heading("3. Resultados da TCM", level=1)''')
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
  'lead':'Leitura principal. O Simulado VI de Matemática da 2ª série manteve os 26 itens nas análises TCM e TRI, com onze itens de ancoragem. O item 25 apresenta discriminação e correlação ponto-bisserial negativas para o gabarito B, enquanto E se associa positivamente ao desempenho. As estimativas devem ser interpretadas com essa ressalva até a revisão do item.',
  'diagnostic_items':[25], 'status':{25:'Mantido na TRI; possível conflito de gabarito a revisar'},
  'narrative':'O item 25 teve 19,4% de acertos pelo gabarito B, discriminação de −0,028 e ponto-bisserial de −0,069. A alternativa E reuniu 28,7% das escolhas, discriminação de 0,431 e ponto-bisserial de 0,442. O padrão constitui sinal relevante de possível conflito de gabarito, mas a confirmação exige enunciado e resolução. Os demais itens superaram 0,20 em discriminação e ponto-bisserial. Na TRI, o item 25 apresentou dificuldade extrema de 1.377,8 pontos e c = 0,194, próximo da proporção de acertos. Esse ajuste é compatível com uma curva praticamente plana no intervalo observado e não deve ser interpretado como dificuldade pedagógica válida. A convergência numérica do modelo não resolve a inadequação do item. A chave B e o item foram mantidos nesta execução para preservar a fonte recebida; não foi fornecida orientação de anulação ou exclusão.',
  'recommendations':['Revisar prioritariamente o enunciado, a resolução e o gabarito do item 25, com atenção à alternativa E.','Após decisão pedagógica documentada sobre o item 25, reprocessar TCM e TRI caso haja alteração de chave ou exclusão. Até essa decisão, considerar os resultados provisórios quanto ao tratamento desse item.','Examinar os parâmetros e as curvas dos itens e a cobertura de dificuldade, sobretudo entre os onze itens fixados.','Evitar comparar diretamente as médias das duas disciplinas como medidas equivalentes de desempenho.']}
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
        assert set(m.Item)==set(items.Item)==set(range(1,27))
        assert np.allclose(items.bSAEB,250+50*items.b)
        info=n['pd'].read_excel(next((ROOT/'Dados').glob(f'gabarito*{profile[-2:].lower()}*')))
        known=info.iloc[:,-1].notna()
        fixed=items[items.SAEB=='Sim']
        assert fixed.Item.tolist()==info.loc[known,'Item'].tolist()
        assert np.allclose(fixed.a,info.loc[known,'aSAEB']*50)
        assert np.allclose(fixed.bSAEB,info.loc[known,'bSAEB'])
        assert np.allclose(fixed.c,info.loc[known,'c'])
        audit[profile]={'N':len(scores),'media_escore':float(scores.iloc[:,1].mean()),'media_EAP':float(theta.Theta.mean()),'DP_EAP':float(theta.Theta.std()),'TCM':len(m),'TRI':len(items),'ancoras':fixed.Item.tolist(),'itens_TCM_sinalizados':m.loc[(m.Discriminacao<.2)|(m.CPBisserial<.2),'Item'].tolist()}
    (ROOT/'Relatorios/qa/auditoria_resultados.json').write_text(json.dumps(audit,ensure_ascii=False,indent=2),encoding='utf-8')
    return audit

if __name__=='__main__':
    print(json.dumps(audit_results(),ensure_ascii=False,indent=2))
    n['main']()
