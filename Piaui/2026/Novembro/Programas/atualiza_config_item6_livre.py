from pathlib import Path
root=Path(__file__).resolve().parent
p=root/'gera_relatorios_simulado_vi.py'
s=p.read_text(encoding='utf-8')
s=s.replace('com onze itens de ancoragem e 28.727 avaliados','com dez itens de ancoragem e 28.727 avaliados')
s=s.replace('Os escores brutos e seus percentuais foram recalculados sobre 25 itens; as proficiências foram reestimadas.', 'O item 6 foi liberado para estimação de a, b e c em 26/11/2026; as proficiências foram reestimadas com dez âncoras. A TCM permanece calculada sobre os mesmos 25 itens.')
s=s.replace('sobretudo entre os onze itens fixados.', 'sobretudo entre os dez itens fixados. Documentar a liberação do item 6 e acompanhar seus parâmetros estimados e a estabilidade da ligação à escala.')
s=s.replace('A ancoragem usa aSAEB, bSAEB e c; diferenças', 'A ancoragem usa aSAEB, bSAEB e c; diferenças')
s=s.replace('com 25 itens válidos e numeração original preservada. ', 'com 25 itens válidos e numeração original preservada. O item 6 foi liberado para estimação de a, b e c, mantendo-se dez âncoras. ')
a=s.index("            old = n['pd'].read_excel(")
b=s.index('        assert np.allclose(items.bSAEB',a)
s=s[:a]+'''            prior_root = ROOT/'Historico/2026-11-26_MT_sem25_item6_fixo'
            prior_scores=n['pd'].read_excel(prior_root/'ResultadosTCM_2EM_MT/Simulado_VI/EscoresBrutos_Aluno.xlsx')
            assert scores.equals(prior_scores)
            assert items.loc[items.Item==6,'SAEB'].iloc[0] != 'Sim'
'''+s[b:]
s=s.replace("known=info.iloc[:,-1].notna()", "known=info.iloc[:,-1].notna()\n        if profile == '2EM_MT':\n            known = known & (info.Item != 6)")
s=s.replace("'itens_excluidos':[25] if profile=='2EM_MT' else []", "'itens_excluidos':[25] if profile=='2EM_MT' else [],'itens_liberados':[6] if profile=='2EM_MT' else []")
p.write_text(s,encoding='utf-8')
p=root/'verifica_relatorios.py'
s=p.read_text(encoding='utf-8').replace('2EM_MT_sem_item25_2026-11-26/final','2EM_MT_sem25_item6_livre_2026-11-26/final').replace('25 itens válidos, onze âncoras','25 itens válidos, dez âncoras; item 6 livre para estimação de a, b e c')
s=s.replace("assert '26 de novembro de 2026' in pdf_text", "assert '26 de novembro de 2026' in pdf_text\n        assert len(tri[tri.SAEB=='Sim'])==10\n        assert tri.loc[tri.Item==6,'SAEB'].iloc[0]!='Sim'\n        assert 'item 6' in pdf_text")
p.write_text(s,encoding='utf-8')
