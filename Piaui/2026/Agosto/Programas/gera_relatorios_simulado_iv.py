"""Relatórios de agosto com os componentes do gerador de julho e dados S4."""
from pathlib import Path
import hashlib
import json

ROOT = Path(__file__).resolve().parents[1]
PREVIOUS = ROOT.parent / 'Julho'
source = (PREVIOUS / 'Programas/gera_relatorios_simulado_iii.py').read_text(encoding='utf-8')
for old, new in [('Simulado III', 'Simulado IV'), ('SIMULADO III', 'SIMULADO IV'), ('Simulado_III', 'Simulado_IV'), ('_S3', '_S4'), ('relatorios_s3_', 'relatorios_s4_'), ('pasta Julho', 'pasta Agosto'), ('Impressões gerais..odt', 'Informações gerais.odt')]:
    source = source.replace(old, new)

# Preserve the reference package, page system, styles and page numbering.
source = source.replace('    doc = Document()\n    configure_document(doc, profile)', '''    reference = ROOT.parent / "Julho/Relatorios/2EM_LP/Relatorio_Tecnico_2EM_LP_S3_TCM_TRI.docx"
    doc = Document(reference)
    for child in list(doc._element.body):
        if child.tag != qn("w:sectPr"):
            doc._element.body.remove(child)
    for run in doc.sections[0].header.paragraphs[0].runs:
        run.text = run.text.replace("SIMULADO III", "SIMULADO IV").replace("2EM-LP", profile.replace("_", "-"))
    field_update = OxmlElement("w:updateFields")
    field_update.set(qn("w:val"), "true")
    doc.settings.element.append(field_update)''')
source = source.replace('exclusion_text = "6; item 25 anulado"', 'exclusion_text = "Item 2 anulado (TCM/TRI)"')
source = source.replace('if item_number not in data["choice"].index and item_number > len(data["choice"]):', 'if item_number not in data["choice"].index:')
source = source.replace('if item_number == 25 and profile == "3EM_MT":', 'if item_number == 2 and profile == "3EM_MT":')
for key in ['choice', 'disc_alt', 'pbis_alt']:
    source = source.replace(f'data["{key}"].iloc[item_number - 1]', f'data["{key}"].loc[item_number]')
source = source.replace('fill = "#B3261E" if idx in problem_indices else color', 'item_id = int(measures.iloc[idx - 1]["Item"])\n            fill = "#B3261E" if item_id in problem_indices else color')
source = source.replace('if idx % 2 == 0:', 'if True:').replace('str(idx), font=tiny', 'str(item_id), font=tiny')
source = source.replace('0.90, shade=', 'max(0.90, float(measures["Dificuldade"].max()) * 1.05), shade=')
source = source.replace('0.75, reference=', 'max(0.75, float(measures["Discriminacao"].max()) * 1.05), reference=')
# The density interpretation in July was application-specific, not a template fact.
a = source.index('    density_chart = None\n')
b = source.index('    return tcm_chart, theta_chart, density_chart', a)
source = source[:a] + '    density_chart = None\n\n' + source[b:]
a = source.index('    if profile == "3EM_MT" and density_chart is not None:')
b = source.index('    doc.add_heading("Parâmetros dos itens"', a)
source = source[:a] + source[b:]
a = source.index('    add_table(\n        doc,\n        ["Item", "Prop. acertos"')
b = source.index('    doc.add_paragraph(notes["narrative"])', a)
block = source[a:b]
source = source[:a] + '    if issue_rows:\n' + ''.join('    ' + line + '\n' for line in block.splitlines()) + source[b:]
source = source.replace('    doc.add_heading("Diagnóstico das alternativas", level=2)\n', '')
source = source.replace('    if alt_rows:\n        add_table(', '    if alt_rows:\n        doc.add_heading("Diagnóstico das alternativas", level=2)\n        add_table(')
source = source.replace('    doc.add_heading("6. Referências e arquivos-fonte", level=1)', '    doc.add_heading("6. Referências e arquivos-fonte", level=1).paragraph_format.page_break_before = True')
source = source.replace('    add_source(doc, "ResumoTheta e ResumoThetaIC.")', '''    add_source(doc, "ResumoTheta e ResumoThetaIC.")
    doc.add_paragraph("O intervalo de confiança é reproduzido do arquivo de resultados; não representa o erro individual de proficiência. As médias de disciplinas distintas não devem ser comparadas diretamente como medidas equivalentes de desempenho.")''')
namespace = {'__file__': __file__, '__name__': 'simulado_iv_components'}
exec(compile(source, str(__file__), 'exec'), namespace)

NOTES = {
    '2EM_LP': {
        'lead': 'Leitura principal. O Simulado IV de Língua Portuguesa da 2ª série manteve os 26 itens na TCM e na TRI. Nenhum item apresentou discriminação ou correlação ponto-bisserial abaixo de 0,20. A ausência desses sinais não dispensa a avaliação dos parâmetros TRI e a revisão pedagógica.',
        'diagnostic_items': [], 'status': {},
        'narrative': 'Todos os itens superaram os limiares TCM de 0,20 adotados neste relatório. Não há exclusão registrada nos resultados de agosto. A análise de parâmetros TRI deve complementar essa leitura, sem transformar os limiares em aprovação automática dos itens.',
        'recommendations': ['Manter documentada a presença dos 26 itens nas análises TCM e TRI.', 'Examinar os itens sinalizados pelos parâmetros TRI e suas curvas antes de reutilizá-los.', 'Planejar intervenções a partir da distribuição de proficiência e dos conteúdos avaliados, sem atribuir níveis pedagógicos a faixas não validadas.'],
    },
    '2EM_MT': {
        'lead': 'Leitura principal. O Simulado IV de Matemática da 2ª série teve o item 24 excluído da TRI por possível conflito de gabarito. A TCM contém 26 itens e a TRI, 25. O item 24 reúne baixa proporção de acertos, discriminação baixa e uma alternativa não oficial mais associada ao desempenho.',
        'diagnostic_items': [24], 'status': {24: 'Excluído da TRI; possível conflito de gabarito'},
        'narrative': 'O item 24 teve 13,7% de acertos, discriminação 0,076 e correlação ponto-bisserial 0,091. O gabarito E recebeu 13,7% das escolhas, enquanto C recebeu 28,5% e apresentou ponto-bisserial 0,216. Esse padrão é compatível com a ressalva de possível conflito de gabarito registrada pela equipe; não permite, isoladamente, alterar a chave oficial.',
        'recommendations': ['Revisar formalmente o enunciado, a resolução e o gabarito do item 24, com atenção à alternativa C.', 'Registrar a exclusão do item 24 apenas da TRI: os escores TCM apresentados continuam calculados sobre 26 itens.', 'Examinar parâmetros extremos e curvas dos itens antes de decisões de banco ou reaplicação.'],
    },
    '3EM_LP': {
        'lead': 'Leitura principal. O Simulado IV de Língua Portuguesa da 3ª série manteve os 26 itens na TRI. Os itens 18 e 24 merecem acompanhamento: ambos tiveram correlação ponto-bisserial abaixo de 0,20, e o item 24 também apresentou discriminação abaixo desse limiar.',
        'diagnostic_items': [18, 24], 'status': {18: 'Mantido na TRI; ponto-bisserial baixo', 24: 'Mantido na TRI; discriminação e ponto-bisserial baixos'},
        'narrative': 'No item 18, o gabarito A recebeu 30,4% das escolhas e ponto-bisserial 0,184; o distrator D recebeu 27,9% e ponto-bisserial 0,201. Recomenda-se examinar a concorrência entre A e D, sem concluir pela troca do gabarito. No item 24, o gabarito E recebeu 22,3%, com discriminação 0,120 e ponto-bisserial 0,121; o distrator D foi mais escolhido (24,3%), mas teve menor ponto-bisserial (0,073). Não houve exclusão desses itens na TRI.',
        'recommendations': ['Revisar o item 18, especialmente a distinção de conteúdo entre o gabarito A e o distrator D.', 'Revisar a clareza e o funcionamento das alternativas do item 24, registrando que foi mantido na TRI.', 'Combinar as ressalvas TCM com parâmetros, curvas e evidências pedagógicas antes de decidir sobre os itens.'],
    },
    '3EM_MT': {
        'lead': 'Leitura principal. O Simulado IV de Matemática da 3ª série teve o item 2 anulado e retirado das análises TCM e TRI. As duas análises utilizam 25 itens. Entre os itens válidos, nenhum apresentou discriminação ou correlação ponto-bisserial abaixo de 0,20.',
        'diagnostic_items': [2], 'status': {2: 'Anulado antes da TCM/TRI'},
        'narrative': 'A anulação do item 2 está registrada nas informações gerais e nos programas de agosto, e sua ausência foi confirmada nas tabelas TCM e TRI. Os escores deste relatório usam denominador de 25 itens. Os itens válidos não apresentaram os sinais TCM de baixa discriminação ou ponto-bisserial selecionados, mas permanecem sujeitos à avaliação dos parâmetros TRI.',
        'recommendations': ['Manter documentada a anulação do item 2 e usar 25 itens como denominador dos escores TCM.', 'Preservar a numeração original dos itens: a ausência do item 2 não renumera os demais.', 'Examinar os parâmetros TRI sinalizados e a cobertura da escala antes de reutilizar a prova.'],
    },
}
namespace['PROFILE_NOTES'] = NOTES

if __name__ == '__main__':
    namespace['main']()
