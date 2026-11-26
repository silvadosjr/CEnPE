from pathlib import Path
from zipfile import ZipFile
import hashlib
import json
import shutil
import xml.etree.ElementTree as ET
from docx import Document
from PIL import Image, ImageDraw
import pandas as pd
import os
from pypdf import PdfReader

root=Path(__file__).resolve().parents[1]
qa=root/'Relatorios/qa'
result=json.loads((qa/'auditoria_documentos.json').read_text(encoding='utf-8')) if (qa/'auditoria_documentos.json').exists() else {}
for profile in [os.environ['REPORT_PROFILE']] if os.environ.get('REPORT_PROFILE') else ['2EM_LP','2EM_MT']:
    reference=root.parent/f'Setembro/Relatorios/{profile}/Relatorio_Tecnico_{profile}_S5_TCM_TRI.docx'
    final=root/f'Relatorios/{profile}/Relatorio_Tecnico_{profile}_S6_TCM_TRI.docx'
    ref,doc=Document(reference),Document(final)
    s=ref.sections[0]
    assert len(ref.sections)==len(doc.sections)==1
    for attr in ['page_width','page_height','top_margin','bottom_margin','left_margin','right_margin','header_distance','footer_distance']:
        assert getattr(s,attr)==getattr(doc.sections[0],attr)
    with ZipFile(reference) as a,ZipFile(final) as b:
        preserve=['word/styles.xml','word/numbering.xml','word/theme/theme1.xml','word/footer1.xml']
        assert all(a.read(p)==b.read(p) for p in preserve)
        parts={p:hashlib.sha256(a.read(p)).hexdigest() for p in a.namelist()}
        (qa/f'pacote_referencia_{profile}.json').write_text(json.dumps(parts,indent=2),encoding='utf-8')
        full=' '.join(''.join(ET.fromstring(b.read(p)).itertext()) for p in b.namelist() if p.startswith('word/') and p.endswith('.xml') and ('document' in p or 'header' in p))
        assert 'SIMULADO VI' in full and 'SIMULADO V |' not in full
        assert 'S5.xlsx' not in full and 'Informações gerais.odt' not in full
    tcm=pd.read_excel(root/f'ResultadosTCM_{profile}/Simulado_VI/MedidasTCM_Item.xlsx')
    tri=pd.read_excel(root/f'ResultadosTRI_{profile}/Simulado_VI/EstItens_{profile}_S6.xlsx')
    ap_tcm,ap_tri,ap_dist=doc.tables[-3:]
    def fmt(x,n): return f'{x:,.{n}f}'.replace(',','X').replace('.',',').replace('X','.')
    for row,(_,v) in zip(ap_tcm.rows[1:],tcm.iterrows(),strict=True):
        assert [c.text for c in row.cells]==[f'{int(v.Item):03d}']+[fmt(v[k],3) for k in ['Dificuldade','Discriminacao','CBisserial','CPBisserial']]
    for row,(_,v) in zip(ap_tri.rows[1:],tri.iterrows(),strict=True):
        assert [c.text for c in row.cells]==[f'{int(v.Item):03d}',fmt(v.a,3),fmt(v.b,3),fmt(v.bSAEB,1),fmt(v.c,3),'Sim' if v.SAEB=='Sim' else 'Não']
    dist=pd.read_excel(root/f'ResultadosTRI_{profile}/Simulado_VI/DistriAlunosClasse{profile[-2:]}2serie_S6.xlsx',index_col=0)
    for row,(classe,v) in zip(ap_dist.rows[1:],dist.iterrows(),strict=True):
        assert [c.text for c in row.cells]==[str(classe),fmt(v['N de alunos'],0),fmt(v.Percentual,3)+'%']
    folder=qa/('2EM_MT_sem25_item6_livre_2026-11-26/final' if profile=='2EM_MT' else profile)
    pdf=folder/f'Relatorio_Tecnico_{profile}_S6_TCM_TRI.pdf'
    reader=PdfReader(pdf)
    assert 6 <= len(reader.pages) <= 9
    assert all(len(p.extract_text())>100 for p in reader.pages)
    pdf_text='\n'.join(p.extract_text() for p in reader.pages)
    assert 'Informações gerais.odt' not in pdf_text and 'Simulado VI' in pdf_text
    pages=sorted(folder.glob('page*.png'))
    assert len(pages)==len(reader.pages)
    if profile=='2EM_MT':
        assert len(tcm)==len(tri)==25 and 25 not in tcm.Item.tolist() and 25 not in tri.Item.tolist()
        assert '26 de novembro de 2026' in pdf_text
        assert len(tri[tri.SAEB=='Sim'])==10
        assert tri.loc[tri.Item==6,'SAEB'].iloc[0]!='Sim'
        assert 'item 6' in pdf_text
        assert 'Mantido na TRI; possível conflito' not in pdf_text
        assert '26 / 26' not in pdf_text and '25 / 25' in pdf_text
    for start in range(0,len(pages),4):
        canvas=Image.new('RGB',(1700,2200),'#d0d0d0')
        for j,p in enumerate(pages[start:start+4]):
            im=Image.open(p); im.thumbnail((830,1070))
            x=(j%2)*850+10; y=(j//2)*1100+22
            canvas.paste(im,(x,y))
            ImageDraw.Draw(canvas).text((x,y-16),f'{profile} page {start+j+1}',fill='black')
        canvas.save(qa/f'{profile}_paginas_{start+1}_{start+4}.jpg')
    shutil.copy2(pdf,final.with_suffix('.pdf'))
    contract=f'''Referência: {reference}
SHA256: {hashlib.sha256(reference.read_bytes()).hexdigest()}
Uma seção Letter 8,5 x 11 pol.; margens 1 pol.; cabeçalho e rodapé 0,492 pol.
Corpo Calibri 11, entrelinha 1,10; H1 16 e H2 13 pt azuis. Tabelas 9360 DXA, corpo 9,2 pt, cabeçalhos cinza e rodapé PAGE.
Slots editáveis: corpo integral da aplicação, cabeçalho VI, tabelas, gráficos, referências e apêndices. Mantidos estilos, tema, numeração, rodapé e geometria, verificados byte a byte ou por propriedade.
Padrões: capa, síntese e método, resultados TCM, distribuição e parâmetros TRI, recomendações, referências e três apêndices numéricos. Gráficos e texto regenerados com S6; nenhuma ressalva factual de S5 reutilizada.
Evidência: pacote_referencia_{profile}.json; auditoria_resultados.json; PNGs e PDF em {folder}. Campos PAGE preservados e updateFields solicitado no DOCX.
Apêndices TCM, TRI e distribuição conferidos integralmente contra as planilhas. PDF com {len(reader.pages)} páginas; inspeção visual registrada separadamente após revisão das imagens.
Atualização MT em 26/11/2026: exclusão do item 25 da TCM e da TRI, 25 itens válidos, dez âncoras; item 6 livre para estimação de a, b e c e numeração original preservada. Referência de estilo mantida.
'''
    (qa/f'artifact_{profile}.md').write_text(contract,encoding='utf-8')
    result[profile]={'paginas':len(reader.pages),'apendices_TCM_TRI_conferidos':True,'template_preservado':True,'sha256_docx':hashlib.sha256(final.read_bytes()).hexdigest()}
(qa/'auditoria_documentos.json').write_text(json.dumps(result,indent=2),encoding='utf-8')
print(json.dumps(result,indent=2))
