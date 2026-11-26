from pathlib import Path
import shutil
root=Path(__file__).resolve().parents[1]
dest=root/'Historico/2026-11-26_MT_sem25_item6_fixo'
if dest.exists():
    raise SystemExit('Histórico já existe; não sobrescrever.')
for p in ['ResultadosTCM_2EM_MT','ResultadosTRI_2EM_MT','Relatorios/2EM_MT']:
    shutil.copytree(root/p,dest/p)
for p in ['LEIA-ME.txt','Programas/Fit_3PL_Fix_2EM_MT_S6.R','Programas/gera_relatorios_simulado_vi.py','Relatorios/qa/auditoria_resultados.json','Relatorios/qa/auditoria_documentos.json']:
    (dest/p).parent.mkdir(parents=True,exist_ok=True)
    shutil.copy2(root/p,dest/p)
print(dest)
