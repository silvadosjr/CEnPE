"""Relatórios S5 derivados dos componentes e da apresentação dos simulados anteriores."""
from pathlib import Path
import importlib.util
import json
import hashlib
from zipfile import ZipFile

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location('s4', ROOT.parent / 'Agosto/Programas/gera_relatorios_simulado_iv.py')
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)
source = module.source
for old, new in [('Simulado IV','Simulado V'), ('SIMULADO IV','SIMULADO V'), ('Simulado_IV','Simulado_V'), ('_S4','_S5'), ('pasta Agosto','pasta Setembro'), ('Informações gerais.odt','Informações gerais..odt')]:
    source = source.replace(old,new)
source = source.replace('ROOT.parent / "Julho/Relatorios/2EM_LP/Relatorio_Tecnico_2EM_LP_S3_TCM_TRI.docx"', 'ROOT.parent / f"Agosto/Relatorios/{profile}/Relatorio_Tecnico_{profile}_S4_TCM_TRI.docx"')
source = source.replace('run.text.replace("SIMULADO III", "SIMULADO V").replace("2EM-LP", profile.replace("_", "-"))', 'run.text.replace("SIMULADO IV", "SIMULADO V")')
original = (ROOT.parent / 'Julho/Programas/gera_relatorios_simulado_iii.py').read_text(encoding='utf-8')
start = original.index('    density_chart = None\n')
end = original.index('    return tcm_chart, theta_chart, density_chart', start)
density = original[start:end].replace('"3EM_MT"','"2EM_MT"')
source = source.replace('    density_chart = None\n\n', density)
source = source.replace('    doc.add_heading("Parâmetros dos itens", level=2)', '''    if profile == "2EM_MT":
        info = density_with_difficulty(data)
        doc.add_heading("Distribuição das habilidades e bimodalidade", level=2)
        add_picture_with_alt(doc, density_chart, Inches(6.35), "Distribuição bimodal das habilidades EAP de 2EM MT no Simulado V, com dois picos e dificuldade dos itens na escala 250 e 50.")
        add_caption(doc, "Figura 3. Distribuição das habilidades EAP e dificuldade dos itens.")
        add_source(doc, "BaseRespTheta_2EM_MT_S5.xlsx e EstItens_2EM_MT_S5.xlsx; densidade gaussiana com largura de banda " + fmt_num(info['bandwidth'], 2) + " pontos.")
        peaks = info['peaks']
        doc.add_paragraph(f"A distribuição das habilidades apresenta comportamento bimodal evidente, com picos próximos de {fmt_num(peaks[0][0], 0)} e {fmt_num(peaks[1][0], 0)} pontos e vale em torno de {fmt_num(info['valley'], 0)} pontos. A média de {fmt_num(theta.loc['media'], 1)} pontos resume uma população heterogênea e não representa adequadamente, sozinha, as duas concentrações de estudantes. O grupo na faixa de 350 pontos ou mais corresponde a {fmt_num(info['pct_350'], 1)}% dos avaliados.")
        doc.add_paragraph("A bimodalidade recomenda examinar separadamente a composição das duas concentrações por escola, turma e padrões de respostas, sem atribuir uma causa ao padrão apenas pela figura. A forma da densidade depende da suavização e das estimativas EAP; os dois modos não constituem níveis pedagógicos oficiais.")
        doc.add_paragraph(f"Os itens tendem a ser difíceis para este grupo: {int((items.bSAEB > theta.loc['media']).sum())} dos {len(items)} parâmetros de dificuldade estão acima da média EAP. O item 13 foi liberado para estimação porque a dificuldade anteriormente fixada foi considerada baixa para o grupo; nos resultados atuais, apresenta a = {fmt_num(items.loc[items.Item == 13, 'a'].iloc[0], 3)}, b na escala 250 e 50 = {fmt_num(items.loc[items.Item == 13, 'bSAEB'].iloc[0], 1)} e c = {fmt_num(items.loc[items.Item == 13, 'c'].iloc[0], 3)}, com SAEB = Não. Essa decisão deve permanecer documentada ao avaliar a estabilidade da ligação entre aplicações.")

    doc.add_heading("Parâmetros dos itens", level=2)''')
source = source.replace('date.today()', 'date(2026, 10, 7)')
source = source.replace('"IC 95% da média"', '"IC 95% média populacional"')
source = source.replace('O intervalo de confiança é reproduzido do arquivo de resultados; não representa o erro individual de proficiência.', 'O intervalo de confiança refere-se à média populacional estimada no arquivo ResumoThetaIC, cujo estimador difere da média das habilidades individuais EAP em ResumoTheta. Não representa o erro individual de proficiência. O DP EAP também difere do desvio-padrão populacional estimado.')
n = {'__file__': __file__, '__name__':'s5_components'}
exec(compile(source, __file__, 'exec'), n)
n['PROFILES'] = ['2EM_LP','2EM_MT']
n['PROFILE_NOTES'] = {
 '2EM_LP': {
  'lead':'Leitura principal. O Simulado V de Língua Portuguesa da 2ª série manteve os 26 itens nas análises TCM e TRI. O item 24 tem possível conflito de gabarito registrado nas informações gerais, embora seus indicadores TCM superem 0,20. O item 14 apresenta discriminação e correlação ponto-bisserial abaixo de 0,20.',
  'diagnostic_items':[14,24], 'status':{14:'Mantido na TRI; discriminação e ponto-bisserial baixos',24:'Mantido na TRI; possível conflito de gabarito'},
  'narrative':'O item 14 teve 22,3% de acertos, discriminação 0,161 e ponto-bisserial 0,164. Seu gabarito B foi menos escolhido que A (28,9%), mas A teve ponto-bisserial negativo. No item 24, o gabarito A recebeu 26,2% das escolhas, com discriminação 0,238 e ponto-bisserial 0,223; C recebeu 37,5%, mas apresentou ponto-bisserial 0,077. A maior frequência de C não é acompanhada de maior associação ao desempenho. A ressalva de possível conflito deve ser examinada pedagogicamente, sem alterar automaticamente a chave oficial. Ambos foram mantidos na TRI.',
  'recommendations':['Revisar o enunciado, a resolução e o gabarito do item 24, registrando a ressalva e sua manutenção na TRI.','Revisar o funcionamento do item 14 e de suas alternativas por baixa discriminação e ponto-bisserial.','Examinar as dificuldades elevadas dos itens 13, 14 e 24 e a discriminação TRI baixa antes de reutilizar os itens.']},
 '2EM_MT': {
  'lead':'Leitura principal. O Simulado V de Matemática da 2ª série manteve os 26 itens nas análises TCM e TRI. A distribuição das habilidades é bimodal, com duas concentrações bem distintas. Os itens tendem a ser difíceis para o grupo, e o item 13 teve seus parâmetros liberados para estimação, conforme as informações gerais.',
  'diagnostic_items':[], 'status':{},
  'narrative':'Nenhum item apresentou discriminação ou correlação ponto-bisserial abaixo de 0,20 na TCM. As proporções de acertos variaram de 27,4% a 45,5%, indicando que todos os itens foram acertados por menos da metade dos avaliados. A ausência de indicadores TCM abaixo do limiar não dispensa a análise dos parâmetros TRI, especialmente a baixa discriminação dos itens 9, 15 e 24.',
  'recommendations':['Considerar a bimodalidade ao planejar intervenções: examinar as duas concentrações de habilidades e sua composição escolar, evitando apoiar decisões apenas na média.','Documentar a liberação dos parâmetros do item 13 e avaliar a estabilidade da ligação à escala nas comparações entre aplicações.','Revisar a cobertura de dificuldade da prova, principalmente para estudantes da concentração de menor habilidade, e examinar as curvas de informação antes de concluir sobre precisão.','Examinar os itens 9, 15 e 24 por discriminação TRI baixa e os itens com dificuldades acima de 400 pontos.']}
}

if __name__ == '__main__':
    n['main']()
    audit = {}
    for profile in n['PROFILES']:
        d = n['read_profile'](profile)
        scores, theta, m, items, dist = [d[k] for k in ['scores','base_theta','measures','items','dist']]
        np = n['np']
        assert len(scores) == len(theta) == int(dist['N de alunos'].sum())
        assert np.isclose(scores.iloc[:,1].mean(), m.Dificuldade.sum())
        assert np.isclose(theta.Theta.mean(), d['theta'].loc['media'])
        assert np.isclose(theta.Theta.std(), d['theta'].loc['dp'])
        assert set(m.Item) == set(items.Item) == set(range(1,27))
        assert np.allclose(items.bSAEB, 250+50*items.b)
        audit[profile] = {'N':len(scores), 'mean_score':scores.iloc[:,1].mean(), 'mean_EAP':theta.Theta.mean(), 'TCM':len(m),'TRI':len(items)}
        if profile == '2EM_MT':
            info = n['density_with_difficulty'](d)
            audit[profile]['density'] = {k: info[k] for k in ['bandwidth','peaks','valley','pct_350']}
    qa = ROOT / 'Relatorios/qa'
    qa.mkdir(exist_ok=True)
    (qa/'auditoria.json').write_text(json.dumps(audit,ensure_ascii=False,indent=2),encoding='utf-8')
    print(json.dumps(audit,ensure_ascii=False,indent=2))
